{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: Aws.Sns.Commands.ListTopics
-- Copyright: Copyright © 2014 AlephCloud Systems, Inc.
-- License: MIT
-- Author: Lars Kuhtz <lars@alephcloud.com>
-- Maintainer: Lars Kuhtz <lars@alephcloud.com>
-- Stability: experimental
--
-- /API Version: 2013-03-31/
--
-- Returns a list of the requester's topics. Each call returns a limited list
-- of topics, up to 100. If there are more topics, a NextToken is also
-- returned. Use the NextToken parameter in a new ListTopics call to get
-- further results.
--
-- Defined at <http://docs.aws.amazon.com/sns/2010-03-31/APIReference/API_ListTopics.html>
--
module Aws.Sns.Commands.ListTopics
( ListTopics(..)
, ListTopicsResponse(..)
, ListTopicsErrors(..)
) where

import Aws.Core
import Aws.General
import Aws.Sns.Core

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Resource (throwM)

import Data.Monoid
import qualified Data.Text as T
import Data.Typeable

import Text.XML.Cursor (($//), (&/))
import qualified Text.XML.Cursor as CU

listTopicsAction :: SnsAction
listTopicsAction = SnsActionListTopics

data ListTopics = ListTopics
    {}
    deriving (Show, Read, Eq, Ord, Typeable)

data ListTopicsResponse = ListTopicsResponse
    { listTopicsTopics :: ![Arn]
    -- ^ A list of topic ARNs.
    }
    deriving (Show, Read, Eq, Ord, Typeable)

instance ResponseConsumer r ListTopicsResponse where
    type ResponseMetadata ListTopicsResponse = SnsMetadata
    responseConsumer _ = snsXmlResponseConsumer p
      where
        p el = ListTopicsResponse <$> arns el
        arns el = do
            let t = el
                    $// CU.laxElement "ListTopicsResult"
                    &/ CU.laxElement "Topics"
                    &/ CU.laxElement "member"
                    &/ CU.laxElement "TopicArn"
                    &/ CU.content
            forM t $ \i -> case fromText i of
                Right a -> return a
                Left e -> throwM $ SnsResponseDecodeError $
                    "failed to parse topic ARN (" <> i <> "): " <> (T.pack . show) e

instance SignQuery ListTopics where
    type ServiceConfiguration ListTopics = SnsConfiguration
    signQuery ListTopics{..} = snsSignQuery SnsQuery
        { snsQueryMethod = Get
        , snsQueryAction = listTopicsAction
        , snsQueryParameters = []
        , snsQueryBody = Nothing
        }

instance Transaction ListTopics ListTopicsResponse

instance AsMemoryResponse ListTopicsResponse where
    type MemoryResponse ListTopicsResponse = ListTopicsResponse
    loadToMemory = return

-- -------------------------------------------------------------------------- --
-- Errors
--
-- Currently not used for requests. It's included for future usage
-- and as reference.

data ListTopicsErrors
    = ListTopicsAuthorizationError
    -- ^ Indicates that the user has been denied access to the requested resource.
    --
    -- /Code 403/

    | ListTopicsInternalError
    -- ^ Indicates an internal service error.
    --
    -- /Code 500/

    | ListTopicsInvalidParameter
    -- ^ Indicates that a request parameter does not comply with the associated constraints.
    --
    -- /Code 400/

    deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable)


