{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2014 AlephCloud Systems, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@alephcloud.com>
-- Stability: experimental
--
-- Tests for Haskell SNS bindings
--

import Aws
import Aws.Core
import Aws.General
import Aws.Sns

import Control.Error
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class

import Data.Monoid
import Data.String
import qualified Data.Text as T

import Test.Tasty

import System.IO

-- -------------------------------------------------------------------------- --
-- Main

-- | Since these tests generate costs there should be a warning and
-- we also should require an explicit command line argument that expresses
-- the concent of the user.
--
main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "SNS Tests"
    [ -- TODO
    ]

-- -------------------------------------------------------------------------- --
-- Static Test parameters
--
-- TODO make these configurable

testRegion :: Region
testRegion = UsWest2

testProtocol :: Protocol
testProtocol = HTTP

testAwsCredential :: T.Text
testAwsCredential = "default"

-- | This prefix is used for the IDs and names of all entities that are
-- created in the AWS account.
--
testDataPrefix :: IsString a => a
testDataPrefix = "__TEST_AWSHASKELLBINDINGS__"

-- -------------------------------------------------------------------------- --
-- Utils

snsConfiguration :: SnsConfiguration qt
snsConfiguration = SnsConfiguration testProtocol testRegion

simpleSns
    :: (AsMemoryResponse a, Transaction r a, ServiceConfiguration r ~ SnsConfiguration, MonadIO m)
    => r
    -> m (MemoryResponse a)
simpleSns command = do
    c <- baseConfiguration
    simpleAws c snsConfiguration command

testData :: (IsString a, Monoid a) => a -> a
testData a = testDataPrefix <> a

tryT :: IO a -> EitherT T.Text IO a
tryT = fmapLT (T.pack . show) . syncIO

-- |
--
-- /Costs as of 2014-07-18: $1 per 1,000,000 invocations/
--
-- Two API requests.
--
withTopic :: T.Text -> (Arn -> IO a) -> IO a
withTopic topicName = bracket createTopic deleteTopic
  where
    createTopic = do
        CreateTopicResponse arn <- simpleSns $ CreateTopic tTopicName
        return arn
    tTopicName = testData topicName
    deleteTopic arn = simpleSns (DeleteTopic arn) >> return ()


-- -------------------------------------------------------------------------- --
-- Tests

-- |
--
-- /Costs as of 2014-07-18: $1 per ~600,000 test invocations/
--
-- Costs incure for three API per test run. 1000000/3 test invocations will
-- cost $0.50.
--
-- TODO:
--
-- * use 'awsIteratedList' for parsing the topics list
--
test_topicCreateListDelete
    :: T.Text -- ^ topic name
    -> IO Bool
test_topicCreateListDelete topicName = do
    runEitherT run >>= \r -> case r of
        Left e -> do
            hPutStrLn stderr $ "failed to run test_topicCreateListDelete: " <> show e
            return False
        Right _ -> return True

  where
    run = do
        CreateTopicResponse topicArn <- tryT $ simpleSns $ CreateTopic tTopicName
        handleT (\e -> deleteTopic topicArn >> left e) $ do
            ListTopicsResponse _ allTopics <- tryT $ simpleSns (ListTopics Nothing)
            when (not $ elem topicArn allTopics) $
                left $ "topic " <> toText topicArn <> " not listed"
            deleteTopic topicArn

    tTopicName = testData topicName
    deleteTopic arn = tryT $ simpleSns (DeleteTopic arn) >> return ()

-- | Subscribe an email endpoint (don't wait for confirmation).
--
-- /Costs as of 2014-07-18: $1 per ~45,000 invocations/
--
-- From the AWS page it is not completely clear if costs incure from
-- the confirmation emails that are sent to the endpoint email address.
--
test_emailSubscribe
    :: T.Text -- ^ topic name
    -> SnsEndpoint -- ^ email addresss
    -> IO Bool
test_emailSubscribe topicName email = withTopic topicName $ \topicArn -> do
    r <- runEitherT $ do
        SubscribeResponse maybeSubArn <- tryT $ simpleSns $ Subscribe (Just email) SnsProtocolEmail topicArn
        case maybeSubArn of
            Nothing -> return ()
            Just subArn -> do
                let e = "unexpected subscription arn when 'confirmation pending' is expected"
                handleT (\e2 -> left (e <> " and " <> e2)) $ tryT $ simpleSns $ Unsubscribe subArn
                left e
    case r of
        Left e -> do
            hPutStrLn stderr $ "failed to run test_subscribeUnsubscribe: " <> show e
            return False
        Right _ -> return True

