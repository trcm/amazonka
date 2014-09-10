{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.AddUserToGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds the specified user to the specified group. https://iam.amazonaws.com/
-- ?Action=AddUserToGroup &GroupName=Managers &UserName=Bob &AUTHPARAMS
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM
    (
    -- * Request
      AddUserToGroup
    -- ** Request constructor
    , mkAddUserToGroup
    -- ** Request lenses
    , autgGroupName
    , autgUserName

    -- * Response
    , AddUserToGroupResponse
    -- ** Response constructor
    , mkAddUserToGroupResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data AddUserToGroup = AddUserToGroup
    { _autgGroupName :: !Text
    , _autgUserName :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AddUserToGroup' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GroupName ::@ @Text@
--
-- * @UserName ::@ @Text@
--
mkAddUserToGroup :: Text -- ^ 'autgGroupName'
                 -> Text -- ^ 'autgUserName'
                 -> AddUserToGroup
mkAddUserToGroup p1 p2 = AddUserToGroup
    { _autgGroupName = p1
    , _autgUserName = p2
    }

-- | Name of the group to update.
autgGroupName :: Lens' AddUserToGroup Text
autgGroupName = lens _autgGroupName (\s a -> s { _autgGroupName = a })

-- | Name of the user to add.
autgUserName :: Lens' AddUserToGroup Text
autgUserName = lens _autgUserName (\s a -> s { _autgUserName = a })

instance ToQuery AddUserToGroup where
    toQuery = genericQuery def

data AddUserToGroupResponse = AddUserToGroupResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AddUserToGroupResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkAddUserToGroupResponse :: AddUserToGroupResponse
mkAddUserToGroupResponse = AddUserToGroupResponse

instance AWSRequest AddUserToGroup where
    type Sv AddUserToGroup = IAM
    type Rs AddUserToGroup = AddUserToGroupResponse

    request = post "AddUserToGroup"
    response _ = nullaryResponse AddUserToGroupResponse
