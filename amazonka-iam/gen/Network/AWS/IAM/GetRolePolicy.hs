{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.GetRolePolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves the specified policy document for the specified role. For more
-- information about roles, go to Working with Roles. The returned policy is
-- URL-encoded according to RFC 3986. For more information about RFC 3986, go
-- to http://www.faqs.org/rfcs/rfc3986.html. https://iam.amazonaws.com/
-- ?Action=GetRolePolicy &PolicyName=S3AccessPolicy &RoleName=S3Access
-- &Version=2010-05-08 &AUTHPARAMS S3AccessPolicy S3Access
-- {"Version":"2012-10-17","Statement":[{"Effect":"Allow","Action":["s3:*"],"Resource":["*"]}]}
-- 7e7cd8bc-99ef-11e1-a4c3-27EXAMPLE804.
module Network.AWS.IAM
    (
    -- * Request
      GetRolePolicy
    -- ** Request constructor
    , mkGetRolePolicy
    -- ** Request lenses
    , grpRoleName
    , grpPolicyName

    -- * Response
    , GetRolePolicyResponse
    -- ** Response constructor
    , mkGetRolePolicyResponse
    -- ** Response lenses
    , grprRoleName
    , grprPolicyName
    , grprPolicyDocument
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data GetRolePolicy = GetRolePolicy
    { _grpRoleName :: !Text
    , _grpPolicyName :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetRolePolicy' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @RoleName ::@ @Text@
--
-- * @PolicyName ::@ @Text@
--
mkGetRolePolicy :: Text -- ^ 'grpRoleName'
                -> Text -- ^ 'grpPolicyName'
                -> GetRolePolicy
mkGetRolePolicy p1 p2 = GetRolePolicy
    { _grpRoleName = p1
    , _grpPolicyName = p2
    }

-- | Name of the role associated with the policy.
grpRoleName :: Lens' GetRolePolicy Text
grpRoleName = lens _grpRoleName (\s a -> s { _grpRoleName = a })

-- | Name of the policy document to get.
grpPolicyName :: Lens' GetRolePolicy Text
grpPolicyName = lens _grpPolicyName (\s a -> s { _grpPolicyName = a })

instance ToQuery GetRolePolicy where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the GetRolePolicy action.
data GetRolePolicyResponse = GetRolePolicyResponse
    { _grprRoleName :: !Text
    , _grprPolicyName :: !Text
    , _grprPolicyDocument :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetRolePolicyResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @RoleName ::@ @Text@
--
-- * @PolicyName ::@ @Text@
--
-- * @PolicyDocument ::@ @Text@
--
mkGetRolePolicyResponse :: Text -- ^ 'grprRoleName'
                        -> Text -- ^ 'grprPolicyName'
                        -> Text -- ^ 'grprPolicyDocument'
                        -> GetRolePolicyResponse
mkGetRolePolicyResponse p1 p2 p3 = GetRolePolicyResponse
    { _grprRoleName = p1
    , _grprPolicyName = p2
    , _grprPolicyDocument = p3
    }

-- | The role the policy is associated with.
grprRoleName :: Lens' GetRolePolicyResponse Text
grprRoleName = lens _grprRoleName (\s a -> s { _grprRoleName = a })

-- | The name of the policy.
grprPolicyName :: Lens' GetRolePolicyResponse Text
grprPolicyName = lens _grprPolicyName (\s a -> s { _grprPolicyName = a })

-- | The policy document.
grprPolicyDocument :: Lens' GetRolePolicyResponse Text
grprPolicyDocument =
    lens _grprPolicyDocument (\s a -> s { _grprPolicyDocument = a })

instance FromXML GetRolePolicyResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetRolePolicy where
    type Sv GetRolePolicy = IAM
    type Rs GetRolePolicy = GetRolePolicyResponse

    request = post "GetRolePolicy"
    response _ = xmlResponse
