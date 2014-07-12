{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module: SignatureV4
-- Copyright: Copyright © 2014 AlephCloud Systems, Inc.
-- License: MIT
-- Author: Lars Kuhtz <lars@alephcloud.com>
-- Maintainer: Lars Kuhtz <lars@alephcloud.com>
-- Stability: experimental
--
-- Unit tests for "Aws.SignatureV4"
--
module SignatureV4
( tests

-- * Test Properties
, prop_canonicalHeaders
, allTests

) where

import General (prop_textRoundtrip)

import Aws.Core
import Aws.General
import Aws.SignatureV4

import Control.Applicative
import Control.Error.Util
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Either
import Control.Monad.IO.Class

import qualified Data.Attoparsec.ByteString.Char8 as A8
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.CaseInsensitive as CI
import Data.IORef
import qualified Data.List as L
import Data.Maybe
import Data.Monoid
import Data.String
import qualified Data.Text.Encoding as T
import Data.Time.Clock (UTCTime)
import Data.Tagged

import qualified Network.HTTP.Types as HTTP

import qualified Test.QuickCheck as Q
import Test.QuickCheck.Instances ()

import qualified Text.Parser.Char as P
import qualified Text.Parser.Combinators as P

import System.Directory
import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty.QuickCheck
import Test.Tasty
import Test.Tasty.Providers

tests :: TestTree
tests = testGroup "SignatureV4"
    [ quickCheckTests
    , awsSignatureV4TestSuite
    ]

-- -------------------------------------------------------------------------- --
-- QuickCheck Properties

quickCheckTests :: TestTree
quickCheckTests = testGroup "quick queck tests"
    [ testProperty "canonical headers" prop_canonicalHeaders
    , testProperty "text roundtrip for CredentialScope"
        (prop_textRoundtrip :: CredentialScope -> Bool)
    ]

instance Q.Arbitrary (CI.CI B.ByteString) where
    arbitrary = CI.mk . T.encodeUtf8 <$> Q.arbitrary

prop_canonicalHeaders :: HTTP.RequestHeaders -> Bool
prop_canonicalHeaders h = let x = canonicalHeaders h in x `seq` True

-- -------------------------------------------------------------------------- --
-- AWS Signature V4 Test Suite
--
-- <http://docs.aws.amazon.com/general/1.0/gr/samples/aws4_testsuite.zip>
--
-- TODO use machinery from tasty-golden to run these tests
--

instance IsTest (IO Bool) where
    run _ t _ = t >>= \x -> return $ if x
        then testPassed ""
        else testFailed ""
    testOptions = Tagged []

simpleIOTest :: String -> IO Bool -> TestTree
simpleIOTest name = singleTest name

--

awsSignatureV4TestSuite :: TestTree
awsSignatureV4TestSuite = simpleIOTest "AWS Signature V4 Test Suite" allTests

baseDir :: String
baseDir = "./tests/signatureV4/aws4_testsuite"

testFileBase :: String -> String
testFileBase name = baseDir <> "/" <> name

readFileNormalized :: String -> IO B8.ByteString
readFileNormalized f = B8.filter (/= '\r') <$> B8.readFile f

amz_credentialScope :: CredentialScope
amz_credentialScope = either error id
    $ fromText "20110909/us-east-1/host/aws4_request"

amz_credentialsIO :: IO Credentials
amz_credentialsIO = Credentials "AKIDEXAMPLE" "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"
    <$> newIORef []

amz_credentials :: Credentials
amz_credentials = unsafePerformIO $ amz_credentialsIO
{-# NOINLINE amz_credentials #-}

-- | Note that the input is an invalid date. The day 2011-09-09 was a
-- a Friday in the Gregorian calendar.
--
amz_testDateStr :: IsString a => a
amz_testDateStr = "Mon, 09 Sep 2011 23:36:00 GMT"

amz_testDate :: UTCTime
amz_testDate = fromMaybe (error "failed to parse test date")
    $ parseHttpDate amz_testDateStr

-- | Test Parameters
--
data TestRequest = TestRequest
    { testRequestMethod :: !Method
    , testRequestPath :: !UriPath
    , testRequestQuery :: !UriQuery
    , testRequestHeaders :: !HTTP.RequestHeaders
    , testRequestPayload :: !B.ByteString
    }
    deriving (Show, Eq)

parseRequest :: (Monad m, P.CharParsing m) => m TestRequest
parseRequest = uncurry <$> (TestRequest <$> tok parseMethod)
        <*> parseTestUri
        <*> many parseTestHeader
        <*> (fromString <$> (line *> P.manyTill P.anyChar P.eof))
        P.<?> "TestRequest"

parseMethod :: P.CharParsing m => m Method
parseMethod =
        Get <$ P.text "GET"
    <|> Head <$ P.text "HEAD"
    <|> Post <$ P.text "POST"
    <|> Put <$ P.text "PUT"
    <|> Delete <$ P.text "DELETE"
    P.<?> "Method"

parseTestUri :: forall m . P.CharParsing m => m (UriPath, UriQuery)
parseTestUri = fmap HTTP.queryToQueryText . HTTP.decodePath . B8.pack
    <$> word <* (line :: m String)

parseTestHeader :: P.CharParsing m => m HTTP.Header
parseTestHeader = (,)
    <$> (fromString <$> P.manyTill P.anyChar (P.char ':'))
    <*> line

tok :: P.CharParsing m => m a -> m a
tok m = m <* P.spaces P.<?> "Token"

word :: (P.CharParsing m, IsString a) => m a
word = fromString <$> P.manyTill P.anyChar (some P.space) P.<?> "word"

line :: (P.CharParsing m, IsString a) => m a
line = fromString
    <$> P.manyTill P.anyChar (P.newline *> P.notFollowedBy (P.char ' '))
    P.<?> "line"

-- ** Test Methods

testCanonicalRequest
    :: String
    -> TestRequest
    -> EitherT String IO CanonicalRequest
testCanonicalRequest name r = do
    creq_ <- liftIO $ readFileNormalized f
    if creq == creq_
    then return result
    else left $ "test " <> name <> " failed to compute canonical request: "
        <> "\n  expected:" <> show creq_
        <> "\n  computed:" <> show creq
  where
    f = testFileBase name <> ".creq"
    result@(CanonicalRequest creq) = canonicalRequest
        (testRequestMethod r)
        (testRequestPath r)
        (testRequestQuery r)
        (testRequestHeaders r)
        (testRequestPayload r)

testStringToSign
    :: String
    -> TestRequest
    -> CanonicalRequest
    -> EitherT String IO StringToSign
testStringToSign name req creq = do
    sts_ <- liftIO $ readFileNormalized f
    if sts == sts_
    then return result
    else left $ "test " <> name <> " failed compute string to sgin: "
        <> "\n  expected:" <> show sts_
        <> "\n  computed:" <> show sts
  where
    f = testFileBase name <> ".sts"
    result@(StringToSign sts)  = stringToSign
        amz_testDate
        amz_credentialScope
        creq

testSignature :: String -> TestRequest -> StringToSign -> EitherT String IO Signature
testSignature name req str = do
    sig_ <- liftIO $ getSignature <$> readFileNormalized f
    if sig == sig_
    then return result
    else left $ "test " <> name <> " failed to compute signature: "
        <> "\n  expected:" <> show sig_
        <> "\n  computed:" <> show sig
  where
    f = testFileBase name <> ".authz"
    key = signingKey amz_credentials amz_credentialScope
    result@(Signature sig) = requestSignature key str
    getSignature = snd . B8.spanEnd (/= '=')

testAuthorization :: String -> TestRequest -> Signature -> EitherT String IO B8.ByteString
testAuthorization name req sig = do
    authz_ <- liftIO $ readFileNormalized f
    authz <- result
    if authz == authz_
    then return authz
    else left $ "test " <> name <> " failed to compute authorization info: "
        <> "\n  expected:" <> show authz_
        <> "\n  computed:" <> show authz
  where
    f = testFileBase name <> ".authz"
    result = failWith "authorization header is missing"
        . lookup "authorization"
        . authorizationInfoHeader
        $ authorizationInfo
            amz_credentials
            amz_credentialScope
            (signedHeaders $ testRequestHeaders req)
            amz_testDate
            sig

-- | Run a single Test
--
testMain :: String -> IO Bool
testMain name = do
    testRequest <- readFileNormalized reqFile >>= \x -> case A8.parseOnly parseRequest x of
        Left e -> error $ "failed to parse test request file " <> reqFile <> ": " <> e
        Right r -> return r
    eitherT (\e -> putStrLn e >> return False) (const $ return True) $ do
        creq <- testCanonicalRequest name testRequest
        sts <- testStringToSign name testRequest creq
        sig <- testSignature name testRequest sts
        authz <- testAuthorization name testRequest sig
        return True
  where
    reqFile = testFileBase name <> ".req"

-- | Run all Tests
--
allTests :: IO Bool
allTests = do
    testFiles <- filter (L.isSuffixOf ".req") <$> getDirectoryContents baseDir
    let tests = map (\x -> L.take (L.length x - 4) x) $ testFiles
    results <- forM tests $ \n ->
        testMain n `catch` \(e :: IOError) -> do
            putStrLn $ "test " <> n <> " failed with: " <> show e
            return False
    return $ all id results

-- -------------------------------------------------------------------------- --
-- Utils

