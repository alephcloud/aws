{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Aws.Sns
-- Copyright: Copyright © 2014 AlephCloud Systems, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@alephcloud.com>
-- Stability: experimental
--
-- /API Version: 2013-03-31/
--
-- <http://docs.aws.amazon.com/sns/2010-03-31/APIReference/Welcome.html>
--
module Aws.Sns
( module SNS
) where

import Aws.Sns.Core as SNS
import Aws.Sns.Commands.CreateTopic as SNS
import Aws.Sns.Commands.DeleteTopic as SNS
import Aws.Sns.Commands.ListTopics as SNS
import Aws.Sns.Commands.Subscribe as SNS
import Aws.Sns.Commands.Unsubscribe as SNS
import Aws.Sns.Commands.Publish as SNS
import Aws.Sns.Commands.ConfirmSubscription as SNS
import Aws.Sns.Commands.ListSubscriptionsByTopic as SNS
import Aws.Sns.Commands.GetSubscriptionAttributes as SNS

