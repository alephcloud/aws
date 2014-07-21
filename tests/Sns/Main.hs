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
import qualified Aws.Sqs as SQS

import Control.Concurrent (threadDelay)
import Control.Error
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class

import Data.Aeson (encode, object, (.=), eitherDecode)
import qualified Data.ByteString.Lazy as LB
import Data.Monoid
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

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

sshow :: (Show a, IsString b) => a -> b
sshow = fromString . show

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

-- | SQS endpoint
testSqsEndpoint :: SQS.Endpoint
testSqsEndpoint = SQS.sqsEndpointUsWest2

-- -------------------------------------------------------------------------- --
-- Utils

tryT :: MonadIO m => IO a -> EitherT T.Text m a
tryT = fmapLT (T.pack . show) . syncIO

testData :: (IsString a, Monoid a) => a -> a
testData a = testDataPrefix <> a

retryT :: MonadIO m => Int -> EitherT T.Text m a -> EitherT T.Text m a
retryT i f = run 1
  where
    run x
        | x >= i = fmapLT (\e -> "error after " <> sshow x <> " retries: " <> e) f
        | otherwise = f `catchT` \e -> do
            liftIO $ threadDelay (100000 * min 60 (2^(x-1)))
            run (succ x)

-- -------------------------------------------------------------------------- --
-- SNS Utils

snsConfiguration :: SnsConfiguration qt
snsConfiguration = SnsConfiguration testProtocol testRegion

simpleSns
    :: (AsMemoryResponse a, Transaction r a, ServiceConfiguration r ~ SnsConfiguration, MonadIO m)
    => r
    -> m (MemoryResponse a)
simpleSns command = do
    c <- baseConfiguration
    simpleAws c snsConfiguration command

simpleSnsT
    :: (AsMemoryResponse a, Transaction r a, ServiceConfiguration r ~ SnsConfiguration, MonadIO m)
    => r
    -> EitherT T.Text m (MemoryResponse a)
simpleSnsT = tryT . simpleSns

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
        CreateTopicResponse arn <- simpleSns $ CreateTopic topicName
        return arn
    deleteTopic arn = simpleSns (DeleteTopic arn) >> return ()

-- -------------------------------------------------------------------------- --
-- SQS Utils

sqsArn
    :: Region
    -> AccountId
    -> T.Text -- ^ Queue Name
    -> Arn
sqsArn region accountId queueName = Arn
    { arnService = ServiceNamespaceSqs
    , arnRegion = Just region
    , arnAccount = Just accountId
    , arnResource = [queueName]
    }

sqsConfiguration :: SQS.SqsConfiguration qt
sqsConfiguration = SQS.SqsConfiguration
    { SQS.sqsProtocol = testProtocol
    , SQS.sqsEndpoint = testSqsEndpoint
    , SQS.sqsPort = 80
    , SQS.sqsUseUri = False
    , SQS.sqsDefaultExpiry = 180
    }

simpleSqs
    :: (AsMemoryResponse a, Transaction r a, ServiceConfiguration r ~ SQS.SqsConfiguration, MonadIO m)
    => r
    -> m (MemoryResponse a)
simpleSqs command = do
    c <- baseConfiguration
    simpleAws c sqsConfiguration command

simpleSqsT
    :: (AsMemoryResponse a, Transaction r a, ServiceConfiguration r ~ SQS.SqsConfiguration, MonadIO m)
    => r
    -> EitherT T.Text m (MemoryResponse a)
simpleSqsT = tryT . simpleSqs

-- |
--
-- Throws an exception if the URL returned by 'CreateQueue' doesn't
-- include the HTTP protocol or the account ID as first path component.
--
withSqsQueue
    :: T.Text
    -- ^ queue name
    -> (T.Text -> SQS.QueueName -> Arn -> IO a)
    -- ^ the first argument is the queue URL,
    -- the second is the 'SQS.QueueName', and
    -- the third the queue 'Arn'.
    -> IO a
withSqsQueue queueName f = bracket createQueue deleteQueue $ \url -> do
    let
    f url (sqsQueueName url) Arn
        { arnService = ServiceNamespaceSqs
        , arnRegion = Just testRegion
        , arnAccount = Just $ AccountId (sqsAccountId url)
        , arnResource = [queueName]
        }
  where
    createQueue = do
        SQS.CreateQueueResponse url <- simpleSqs $ SQS.CreateQueue Nothing queueName
        return url
    deleteQueue url = simpleSqs (SQS.DeleteQueue (sqsQueueName url)) >> return ()
    sqsQueueName url = SQS.QueueName queueName (sqsAccountId url)
    sqsAccountId url = T.split (== '/') url !! 3

-- | Set queue policy attribute that allows an SNS Topic
-- to send notification to an SQS queue.
--
sqsAllowTopicAttribute
    :: Arn -- ^ Queue ARN
    -> T.Text -- ^ policy ID
    -> Arn -- ^ topic ARN
    -> SQS.SetQueueAttributes
sqsAllowTopicAttribute queueArn policyId topicArn = SQS.SetQueueAttributes
    { SQS.sqaAttribute = SQS.Policy
    , SQS.sqaValue = T.decodeUtf8 . LB.toStrict . encode $ object
        [ "Version" .= ("2012-10-17" :: T.Text)
        , "Statement" .= [object
            [ "Resource" .= queueArn
            , "Sid" .= policyId
            , "Effect" .= ("Allow" :: T.Text)
            , "Principal" .= object [ "AWS" .= ("*" :: T.Text) ]
            , "Action" .= ("sqs:SendMessage" :: T.Text)
            , "Condition" .= object
                [ "ArnEquals" .= object [ "aws:SourceArn" .= topicArn ]
                ]
            ]]
        ]
    , SQS.sqaQueueName = queueId
    }
  where
    queueId = SQS.QueueName
        { SQS.qAccountNumber = case arnAccount queueArn of
            Nothing -> error $ "Malformed SQS queue ARN: " <> toText queueArn
            Just (AccountId t) -> t
        , SQS.qName = if length (arnResource queueArn) /= 1
            then error $ "Malformed SQS queue ARN: " <> toText queueArn
            else head $ arnResource queueArn
        }

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
        CreateTopicResponse topicArn <- simpleSnsT $ CreateTopic tTopicName
        handleT (\e -> deleteTopic topicArn >> left e) $ do
            ListTopicsResponse _ allTopics <- simpleSnsT (ListTopics Nothing)
            when (not $ elem topicArn allTopics) $
                left $ "topic " <> toText topicArn <> " not listed"
            deleteTopic topicArn

    tTopicName = testData topicName
    deleteTopic arn = simpleSnsT (DeleteTopic arn) >> return ()

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
test_emailSubscribe topicName email = withTopic tTopicName $ \topicArn -> do
    r <- runEitherT $ do
        SubscribeResponse maybeSubArn <- simpleSnsT $ Subscribe (Just email) SnsProtocolEmail topicArn
        case maybeSubArn of
            Nothing -> return ()
            Just subArn -> do
                let e = "unexpected subscription arn when 'confirmation pending' is expected"
                handleT (\e2 -> left (e <> " and " <> e2)) $ simpleSnsT $ Unsubscribe subArn
                left e
    case r of
        Left e -> do
            hPutStrLn stderr $ "failed to run test_subscribeUnsubscribe: " <> show e
            return False
        Right _ -> return True
  where
    tTopicName = testData topicName

-- | Subscribe an SQS queue to an SNS topic
--
test_sqsSubscribePublishUnsubscribe
    :: T.Text -- ^ topic name
    -> T.Text -- ^ queue name
    -> IO Bool
test_sqsSubscribePublishUnsubscribe topicName queueName = withTopic tTopicName $ \topicArn ->
    withSqsQueue tQueueName $ \queueUrl queueId queueArn -> do

        r <- runEitherT $ do
            -- Add permission to send messages from SNS topic (identified by ARN) to queue
            simpleSqsT $ sqsAllowTopicAttribute queueArn sqsPermissionId topicArn

            -- subscribe Queue to SNS topci
            SubscribeResponse maybeSubArn <- simpleSnsT $ Subscribe (Just $ toText queueArn) SnsProtocolSqs topicArn
            subArn <- maybeSubArn ?? "Subscription failed: subscription Arn is missing probably because the confirmation is yet pending"

            -- Let's wait 5 seconds, just be on the safe side ...
            liftIO $ threadDelay (5*1000000)

            -- publish to topic
            PublishResponse msgId0 <- simpleSnsT $ Publish msg Nothing (Just subj) (Left topicArn)

            -- receive messages
            sqsMsg <- retryT 3 $ do
                SQS.ReceiveMessageResponse msgs <- simpleSqsT $ SQS.ReceiveMessage
                    { SQS.rmVisibilityTimeout = Nothing
                    , SQS.rmAttributes = []
                    , SQS.rmMaxNumberOfMessages = Just 1
                    , SQS.rmQueueName = queueId
                    , SQS.rmWaitTimeSeconds = Just 20
                    }
                when (length msgs < 1) $ left
                    $ "unexpected number of messages in queue; expected 1, got " <> sshow (length msgs)
                return $ head msgs

            -- parse notification
            notification :: SqsNotification <- fmapLT T.pack . hoistEither
                . eitherDecode . LB.fromStrict . T.encodeUtf8 $ SQS.mBody sqsMsg

            -- check result
            when (sqsNotificationMessageId notification /= msgId0) $ left
                $ "message IDs don't match; epxected " <> q (messageIdText msgId0)
                <> ", got " <> q (messageIdText $ sqsNotificationMessageId notification)
            when (sqsNotificationMessage notification /= snsMessageDefault msg) $ left
                $ "messages don't match; expected " <> q (snsMessageDefault msg)
                <> ", got " <> q (sqsNotificationMessage notification)

            -- unsubscribe queue
            simpleSnsT $ Unsubscribe subArn

        -- Check result
        case r of
            Left e -> do
                hPutStrLn stderr $ "failed to run test_sqsSubscribePublishUnsubscribe: " <> show e
                return False
            Right _ -> return True

  where
    q t = "\"" <> t <> "\""
    sqsPermissionId = testData topicName
    tQueueName = testData queueName
    tTopicName = testData topicName

    msg = snsMessage "message abc"
    subj = "subject abc"

