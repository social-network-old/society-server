
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

module Society.API.Swagger where

import Data.Swagger.Build.Api (Model)
import qualified Society.API.Call.TURN as Call.TURN
import qualified Society.API.Connection as Connection
import qualified Society.API.Conversation.Code as Conversation.Code
import qualified Society.API.Conversation.Member as Conversation.Member
import qualified Society.API.Conversation.Role as Conversation.Role
import qualified Society.API.Conversation.Typing as Conversation.Typing
import qualified Society.API.CustomBackend as CustomBackend
import qualified Society.API.Event.Conversation as Event.Conversation
import qualified Society.API.Event.Team as Event.Team
import qualified Society.API.Message as Message
import qualified Society.API.Notification as Notification
import qualified Society.API.Properties as Properties
import qualified Society.API.Push.Token as Push.Token
import qualified Society.API.Team as Team
import qualified Society.API.Team.Conversation as Team.Conversation
import qualified Society.API.Team.Feature as Team.Feature
import qualified Society.API.Team.Invitation as Team.Invitation
import qualified Society.API.Team.Member as Team.Member
import qualified Society.API.Team.Permission as Team.Permission
import qualified Society.API.Team.SearchVisibility as Team.SearchVisibility
import qualified Society.API.User as User
import qualified Society.API.User.Activation as User.Activation
import qualified Society.API.User.Auth as User.Auth
import qualified Society.API.User.Client as User.Client
import qualified Society.API.User.Password as User.Password
import qualified Society.API.User.Profile as User.Profile
import qualified Society.API.User.RichInfo as User.RichInfo
import qualified Society.API.User.Search as User.Search

models :: [Model]
models =
  [ Call.TURN.modelRtcConfiguration,
    Call.TURN.modelRtcIceServer,
    Connection.modelConnectionList,
    Connection.modelConnection,
    Connection.modelConnectionRequest,
    Connection.modelConnectionUpdate,
    Conversation.Code.modelConversationCode,
    Conversation.Member.modelConversationMembers,
    Conversation.Member.modelOtherMember,
    Conversation.Member.modelMember,
    Conversation.Member.modelMemberUpdate,
    Conversation.Member.modelOtherMemberUpdate,
    Conversation.Role.modelConversationRole,
    Conversation.Role.modelConversationRolesList,
    Conversation.Typing.modelTyping,
    CustomBackend.modelCustomBackend,
    Event.Conversation.modelEvent,
    Event.Conversation.modelMemberEvent,
    Event.Conversation.modelConnectEvent,
    Event.Conversation.modelConversationReceiptModeUpdateEvent,
    Event.Conversation.modelConversationNameUpdateEvent,
    Event.Conversation.modelConversationAccessUpdateEvent,
    Event.Conversation.modelConversationMessageTimerUpdateEvent,
    Event.Conversation.modelConversationCodeUpdateEvent,
    Event.Conversation.modelConversationCodeDeleteEvent,
    Event.Conversation.modelMemberUpdateEvent,
    Event.Conversation.modelTypingEvent,
    Event.Conversation.modelOtrMessageEvent,
    Event.Conversation.modelMembers,
    Event.Conversation.modelConnect,
    Event.Conversation.modelMemberUpdateData,
    Event.Conversation.modelOtrMessage,
    Event.Team.modelEvent,
    Event.Team.modelMemberEvent,
    Event.Team.modelMemberData,
    Event.Team.modelConvEvent,
    Event.Team.modelConversationData,
    Event.Team.modelUpdateEvent,
    Message.modelNewOtrMessage,
    Message.modelOtrRecipients,
    Message.modelClientMismatch,
    Notification.modelEvent,
    Notification.modelNotification,
    Notification.modelNotificationList,
    Properties.modelPropertyValue,
    Properties.modelPropertyDictionary,
    Push.Token.modelPushToken,
    Push.Token.modelPushTokenList,
    Team.modelTeam,
    Team.modelTeamList,
    Team.modelNewBindingTeam,
    Team.modelNewNonBindingTeam,
    Team.modelUpdateData,
    Team.modelTeamDelete,
    Team.Conversation.modelTeamConversation,
    Team.Conversation.modelTeamConversationList,
    Team.Feature.modelLegalHoldTeamConfig,
    Team.Feature.modelSsoTeamConfig,
    Team.Invitation.modelTeamInvitation,
    Team.Invitation.modelTeamInvitationList,
    Team.Invitation.modelTeamInvitationRequest,
    Team.Member.modelTeamMember,
    Team.Member.modelTeamMemberList,
    Team.Member.modelTeamMemberDelete,
    Team.Member.modelNewTeamMember,
    Team.Permission.modelPermissions,
    Team.SearchVisibility.modelTeamSearchVisibility,
    Team.SearchVisibility.modelTeamSearchVisibilityAvailable,
    User.modelUserIdList,
    User.modelSelf,
    User.modelUser,
    User.modelNewUser,
    User.modelUserUpdate,
    User.modelChangePassword,
    User.modelChangeLocale,
    User.modelEmailUpdate,
    User.modelPhoneUpdate,
    User.modelChangeHandle,
    User.modelDelete,
    User.modelVerifyDelete,
    User.Activation.modelActivate,
    User.Activation.modelSendActivationCode,
    User.Activation.modelActivationResponse,
    User.Auth.modelSendLoginCode,
    User.Auth.modelLoginCodeResponse,
    User.Auth.modelLogin,
    User.Auth.modelRemoveCookies,
    User.Auth.modelCookie,
    User.Auth.modelCookieList,
    User.Auth.modelAccessToken,
    User.Client.modelOtrClientMap,
    User.Client.modelUserClients,
    User.Password.modelNewPasswordReset,
    User.Password.modelCompletePasswordReset,
    User.Profile.modelUserDisplayName,
    User.Profile.modelAsset,
    User.RichInfo.modelRichInfo,
    User.RichInfo.modelRichField,
    User.Search.modelSearchResult,
    User.Search.modelSearchContact
  ]
