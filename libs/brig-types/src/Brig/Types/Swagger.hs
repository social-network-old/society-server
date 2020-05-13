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

module Brig.Types.Swagger
  ( module Brig.Types.Swagger,
    pendingLoginError,
  )
where

import Data.Misc (modelLocation)
import Data.Swagger.Build.Api (DataType, Model)
import Society.API.Call.TURN (modelRtcConfiguration, modelRtcIceServer)
import Society.API.Connection (modelConnection, modelConnectionList, modelConnectionRequest, modelConnectionUpdate)
import Society.API.Properties (modelPropertyDictionary, modelPropertyValue)
import Society.API.Swagger (models)
import Society.API.Team.Invitation (modelTeamInvitation, modelTeamInvitationList, modelTeamInvitationRequest)
import Society.API.Team.Role (typeRole)
import Society.API.User (modelSelf, modelUser)
import Society.API.User (modelEmailUpdate, modelNewUser, modelPhoneUpdate, modelUserUpdate)
import Society.API.User (modelChangeHandle, modelChangeLocale, modelChangePassword)
import Society.API.User (modelDelete, modelVerifyDelete)
import Society.API.User.Activation (modelActivate, modelActivationResponse, modelSendActivationCode)
import Society.API.User.Auth (modelAccessToken, modelCookie, modelCookieList, modelLogin, modelLoginCodeResponse, modelRemoveCookies, modelSendLoginCode)
import Society.API.User.Client (modelClient, modelDeleteClient, modelNewClient, modelPubClient, modelSigkeys, modelUpdateClient)
import Society.API.User.Client.Prekey (modelClientPrekey, modelPrekey, modelPrekeyBundle)
import Society.API.User.Handle (modelCheckHandles, modelUserHandleInfo)
import Society.API.User.Password (modelCompletePasswordReset, modelNewPasswordReset)
import Society.API.User.Profile (modelAsset)
import Society.API.User.Profile (modelUserDisplayName)
import Society.API.User.Profile (typeManagedBy)
import Society.API.User.RichInfo (modelRichField, modelRichInfo)
import Society.API.User.Search (modelSearchContact, modelSearchResult)
import Society.Swagger (pendingLoginError)

-- | Actually all models of the whole API,
-- but it doesn't hurt and makes it less likely to forget one.
brigModels :: [Model]
brigModels = Society.API.Swagger.models

self :: Model
self = modelSelf

user :: Model
user = modelUser

managedBy :: DataType
managedBy = typeManagedBy

asset :: Model
asset = modelAsset

richField :: Model
richField = modelRichField

richInfo :: Model
richInfo = modelRichInfo

userDisplayName :: Model
userDisplayName = modelUserDisplayName

newUser :: Model
newUser = modelNewUser

userUpdate :: Model
userUpdate = modelUserUpdate

emailUpdate :: Model
emailUpdate = modelEmailUpdate

phoneUpdate :: Model
phoneUpdate = modelPhoneUpdate

newPasswordReset :: Model
newPasswordReset = modelNewPasswordReset

completePasswordReset :: Model
completePasswordReset = modelCompletePasswordReset

changePassword :: Model
changePassword = modelChangePassword

changeLocale :: Model
changeLocale = modelChangeLocale

changeHandle :: Model
changeHandle = modelChangeHandle

userHandleInfo :: Model
userHandleInfo = modelUserHandleInfo

checkHandles :: Model
checkHandles = modelCheckHandles

connection :: Model
connection = modelConnection

connectionUpdate :: Model
connectionUpdate = modelConnectionUpdate

connectionRequest :: Model
connectionRequest = modelConnectionRequest

connectionList :: Model
connectionList = modelConnectionList

role :: DataType
role = typeRole

teamInvitationRequest :: Model
teamInvitationRequest = modelTeamInvitationRequest

teamInvitation :: Model
teamInvitation = modelTeamInvitation

teamInvitationList :: Model
teamInvitationList = modelTeamInvitationList

activate :: Model
activate = modelActivate

sendActivationCode :: Model
sendActivationCode = modelSendActivationCode

activationResponse :: Model
activationResponse = modelActivationResponse

delete :: Model
delete = modelDelete

verifyDelete :: Model
verifyDelete = modelVerifyDelete

sendLoginCode :: Model
sendLoginCode = modelSendLoginCode

loginCodeResponse :: Model
loginCodeResponse = modelLoginCodeResponse

login :: Model
login = modelLogin

accessToken :: Model
accessToken = modelAccessToken

removeCookies :: Model
removeCookies = modelRemoveCookies

cookie :: Model
cookie = modelCookie

cookieList :: Model
cookieList = modelCookieList

newClient :: Model
newClient = modelNewClient

updateClient :: Model
updateClient = modelUpdateClient

deleteClient :: Model
deleteClient = modelDeleteClient

client :: Model
client = modelClient

pubClient :: Model
pubClient = modelPubClient

sigkeys :: Model
sigkeys = modelSigkeys

location :: Model
location = modelLocation

prekeyBundle :: Model
prekeyBundle = modelPrekeyBundle

clientPrekey :: Model
clientPrekey = modelClientPrekey

prekey :: Model
prekey = modelPrekey

propertyValue :: Model
propertyValue = modelPropertyValue

propertyDictionary :: Model
propertyDictionary = modelPropertyDictionary

searchResult :: Model
searchResult = modelSearchResult

searchContact :: Model
searchContact = modelSearchContact

rtcConfiguration :: Model
rtcConfiguration = modelRtcConfiguration

rtcIceServer :: Model
rtcIceServer = modelRtcIceServer
