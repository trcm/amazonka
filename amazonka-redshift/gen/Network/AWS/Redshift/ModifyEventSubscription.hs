{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.ModifyEventSubscription
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Modifies an existing Amazon Redshift event notification subscription.
module Network.AWS.Redshift
    (
    -- * Request
      ModifyEventSubscription
    -- ** Request constructor
    , mkModifyEventSubscription
    -- ** Request lenses
    , mesSubscriptionName
    , mesSnsTopicArn
    , mesSourceType
    , mesSourceIds
    , mesEventCategories
    , mesSeverity
    , mesEnabled

    -- * Response
    , ModifyEventSubscriptionResponse
    -- ** Response constructor
    , mkModifyEventSubscriptionResponse
    -- ** Response lenses
    , mesrEventSubscription
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import Network.AWS.Prelude

-- | 
data ModifyEventSubscription = ModifyEventSubscription
    { _mesSubscriptionName :: !Text
    , _mesSnsTopicArn :: !(Maybe Text)
    , _mesSourceType :: !(Maybe Text)
    , _mesSourceIds :: [Text]
    , _mesEventCategories :: [Text]
    , _mesSeverity :: !(Maybe Text)
    , _mesEnabled :: !(Maybe Bool)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifyEventSubscription' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SubscriptionName ::@ @Text@
--
-- * @SnsTopicArn ::@ @Maybe Text@
--
-- * @SourceType ::@ @Maybe Text@
--
-- * @SourceIds ::@ @[Text]@
--
-- * @EventCategories ::@ @[Text]@
--
-- * @Severity ::@ @Maybe Text@
--
-- * @Enabled ::@ @Maybe Bool@
--
mkModifyEventSubscription :: Text -- ^ 'mesSubscriptionName'
                          -> ModifyEventSubscription
mkModifyEventSubscription p1 = ModifyEventSubscription
    { _mesSubscriptionName = p1
    , _mesSnsTopicArn = Nothing
    , _mesSourceType = Nothing
    , _mesSourceIds = mempty
    , _mesEventCategories = mempty
    , _mesSeverity = Nothing
    , _mesEnabled = Nothing
    }

-- | The name of the modified Amazon Redshift event notification subscription.
mesSubscriptionName :: Lens' ModifyEventSubscription Text
mesSubscriptionName =
    lens _mesSubscriptionName (\s a -> s { _mesSubscriptionName = a })

-- | The Amazon Resource Name (ARN) of the SNS topic to be used by the event
-- notification subscription.
mesSnsTopicArn :: Lens' ModifyEventSubscription (Maybe Text)
mesSnsTopicArn = lens _mesSnsTopicArn (\s a -> s { _mesSnsTopicArn = a })

-- | The type of source that will be generating the events. For example, if you
-- want to be notified of events generated by a cluster, you would set this
-- parameter to cluster. If this value is not specified, events are returned
-- for all Amazon Redshift objects in your AWS account. You must specify a
-- source type in order to specify source IDs. Valid values: cluster,
-- cluster-parameter-group, cluster-security-group, and cluster-snapshot.
mesSourceType :: Lens' ModifyEventSubscription (Maybe Text)
mesSourceType = lens _mesSourceType (\s a -> s { _mesSourceType = a })

-- | A list of one or more identifiers of Amazon Redshift source objects. All of
-- the objects must be of the same type as was specified in the source type
-- parameter. The event subscription will return only events generated by the
-- specified objects. If not specified, then events are returned for all
-- objects within the source type specified. Example: my-cluster-1,
-- my-cluster-2 Example: my-snapshot-20131010.
mesSourceIds :: Lens' ModifyEventSubscription [Text]
mesSourceIds = lens _mesSourceIds (\s a -> s { _mesSourceIds = a })

-- | Specifies the Amazon Redshift event categories to be published by the event
-- notification subscription. Values: Configuration, Management, Monitoring,
-- Security.
mesEventCategories :: Lens' ModifyEventSubscription [Text]
mesEventCategories =
    lens _mesEventCategories (\s a -> s { _mesEventCategories = a })

-- | Specifies the Amazon Redshift event severity to be published by the event
-- notification subscription. Values: ERROR, INFO.
mesSeverity :: Lens' ModifyEventSubscription (Maybe Text)
mesSeverity = lens _mesSeverity (\s a -> s { _mesSeverity = a })

-- | A Boolean value indicating if the subscription is enabled. true indicates
-- the subscription is enabled.
mesEnabled :: Lens' ModifyEventSubscription (Maybe Bool)
mesEnabled = lens _mesEnabled (\s a -> s { _mesEnabled = a })

instance ToQuery ModifyEventSubscription where
    toQuery = genericQuery def

newtype ModifyEventSubscriptionResponse = ModifyEventSubscriptionResponse
    { _mesrEventSubscription :: Maybe EventSubscription
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifyEventSubscriptionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @EventSubscription ::@ @Maybe EventSubscription@
--
mkModifyEventSubscriptionResponse :: ModifyEventSubscriptionResponse
mkModifyEventSubscriptionResponse = ModifyEventSubscriptionResponse
    { _mesrEventSubscription = Nothing
    }

-- | 
mesrEventSubscription :: Lens' ModifyEventSubscriptionResponse (Maybe EventSubscription)
mesrEventSubscription =
    lens _mesrEventSubscription (\s a -> s { _mesrEventSubscription = a })

instance FromXML ModifyEventSubscriptionResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ModifyEventSubscription where
    type Sv ModifyEventSubscription = Redshift
    type Rs ModifyEventSubscription = ModifyEventSubscriptionResponse

    request = post "ModifyEventSubscription"
    response _ = xmlResponse
