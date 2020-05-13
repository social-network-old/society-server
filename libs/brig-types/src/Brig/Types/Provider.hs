-- This file is part of the Society Server implementation.
--

--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

-- | Types for the (internal) provider API.
--
-- TODO: proper export list
module Brig.Types.Provider
  ( -- * re-exports
    NewProvider (..),
    NewProviderResponse (..),
    Provider (..),
    ProviderProfile (..),
    UpdateProvider (..),
    ProviderActivationResponse (..),
    ProviderLogin (..),
    DeleteProvider (..),
    PasswordReset (..),
    CompletePasswordReset (..),
    PasswordChange (..),
    EmailUpdate (..),
    queryAnyTags,
    queryAllTags,
    QueryAnyTags (..),
    QueryAllTags (..),
    ServiceTagList (..),
    ServiceKeyPEM (..),
    ServiceKeyType (..),
    ServiceKey (..),
    NewService (..),
    NewServiceResponse (..),
    Service (..),
    ServiceProfile (..),
    ServiceProfilePage (..),
    UpdateService (..),
    UpdateServiceConn (..),
    mkUpdateServiceConn,
    DeleteService (..),
    UpdateServiceWhitelist (..),
    AddBot (..),
    AddBotResponse (..),
    RemoveBotResponse (..),
    UpdateBotPrekeys (..),
    module Common,
    HttpsUrl (..),
    ServiceToken (..),
    ServiceTag (..),
  )
where

import Brig.Types.Common as Common
import Society.API.Conversation.Bot
import Society.API.Provider
import Society.API.Provider.Service
import Society.API.Provider.Service.Tag
