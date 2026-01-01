/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

export * from './errors/AccessDeniedError';
export * from './errors/AccountSuspiciousActivityError';
export * from './errors/AclsMustBeNonEmptyError';
export * from './errors/AlreadyFriendsError';
export * from './errors/ApplicationNotFoundError';
export * from './errors/AuditLogIndexingError';
export * from './errors/BadRequestError';
export * from './errors/BannedFromGuildError';
export * from './errors/BetaCodeAllowanceExceededError';
export * from './errors/BetaCodeMaxUnclaimedError';
export * from './errors/BotAlreadyInGuildError';
export * from './errors/BotApplicationNotFoundError';
export * from './errors/BotIsPrivateError';
export * from './errors/BotsCannotHaveFriendsError';
export * from './errors/BotUserNotFoundError';
export * from './errors/CallAlreadyExistsError';
export * from './errors/CannotEditOtherUserMessageError';
export * from './errors/CannotEditSystemMessageError';
export * from './errors/CannotExecuteOnDmError';
export * from './errors/CannotRedeemPlutoniumWithVisionaryError';
export * from './errors/CannotRemoveOtherRecipientsError';
export * from './errors/CannotReportOwnGuildError';
export * from './errors/CannotReportOwnMessageError';
export * from './errors/CannotReportYourselfError';
export * from './errors/CannotSendEmptyMessageError';
export * from './errors/CannotSendFriendRequestToBlockedUserError';
export * from './errors/CannotSendFriendRequestToSelfError';
export * from './errors/CannotSendMessagesToUserError';
export * from './errors/CannotSendMessageToNonTextChannelError';
export * from './errors/CannotShrinkReservedSlotsError';
export * from './errors/CaptchaVerificationRequiredError';
export * from './errors/ChannelIndexingError';
export * from './errors/CommunicationDisabledError';
export * from './errors/CommunicationDisabledError';
export * from './errors/CreationFailedError';
export * from './errors/DeletionFailedError';
export * from './errors/EmailServiceNotTestableError';
export * from './errors/ErrorHandlers';
export * from './errors/ExplicitContentCannotBeSentError';
export * from './errors/FeatureTemporarilyDisabledError';
export * from './errors/FileSizeTooLargeError';
export * from './errors/FluxerAPIError';
export * from './errors/ForbiddenError';
export * from './errors/FriendRequestBlockedError';
export * from './errors/GiftCodeAlreadyRedeemedError';
export * from './errors/GuildDisallowsUnclaimedAccountsError';
export * from './errors/GuildVerificationRequiredError';
export * from './errors/HarvestExpiredError';
export * from './errors/HarvestFailedError';
export * from './errors/HarvestNotReadyError';
export * from './errors/HarvestOnCooldownError';
export * from './errors/InputValidationError';
export * from './errors/InternalServerError';
export * from './errors/InvalidAclsFormatError';
export * from './errors/InvalidBotFlagError';
export * from './errors/InvalidCaptchaError';
export * from './errors/InvalidChannelTypeError';
export * from './errors/InvalidChannelTypeForCallError';
export * from './errors/InvalidClientError';
export * from './errors/InvalidDiscriminatorError';
export * from './errors/InvalidDsaReportTargetError';
export * from './errors/InvalidDsaTicketError';
export * from './errors/InvalidDsaVerificationCodeError';
export * from './errors/InvalidFlagsFormatError';
export * from './errors/InvalidGrantError';
export * from './errors/InvalidPhoneNumberError';
export * from './errors/InvalidPhoneVerificationCodeError';
export * from './errors/InvalidRequestError';
export * from './errors/InvalidScopeError';
export * from './errors/InvalidSuspiciousFlagsFormatError';
export * from './errors/InvalidSystemFlagError';
export * from './errors/InvalidTimestampError';
export * from './errors/InvalidTokenError';
export * from './errors/InvitesDisabledError';
export * from './errors/IpBannedError';
export * from './errors/IpBannedFromGuildError';
export * from './errors/LockedError';
export * from './errors/MaxBookmarksError';
export * from './errors/MaxCategoryChannelsError';
export * from './errors/MaxFavoriteMemesError';
export * from './errors/MaxGroupDmRecipientsError';
export * from './errors/MaxGroupDmsError';
export * from './errors/MaxGuildChannelsError';
export * from './errors/MaxGuildEmojisAnimatedError';
export * from './errors/MaxGuildEmojisStaticError';
export * from './errors/MaxGuildInvitesError';
export * from './errors/MaxGuildMembersError';
export * from './errors/MaxGuildRolesError';
export * from './errors/MaxGuildStickersStaticError';
export * from './errors/MaxGuildsError';
export * from './errors/MaxReactionsPerMessageError';
export * from './errors/MaxRelationshipsError';
export * from './errors/MaxUsersPerMessageReactionError';
export * from './errors/MaxWebhooksPerChannelError';
export * from './errors/MaxWebhooksPerGuildError';
export * from './errors/MediaMetadataError';
export * from './errors/MfaNotDisabledError';
export * from './errors/MfaNotEnabledError';
export * from './errors/MissingACLError';
export * from './errors/MissingAccessError';
export * from './errors/MissingOAuthFieldsError';
export * from './errors/MissingPermissionsError';
export * from './errors/NoActiveCallError';
export * from './errors/NoActiveSubscriptionError';
export * from './errors/NoPendingDeletionError';
export * from './errors/NotABotApplicationError';
export * from './errors/NotFoundError';
export * from './errors/NotFriendsWithUserError';
export * from './errors/NoUsersWithFluxertagError';
export * from './errors/NoVisionarySlotsAvailableError';
export * from './errors/NsfwContentRequiresAgeVerificationError';
export * from './errors/OAuth2Error';
export * from './errors/PhoneAlreadyUsedError';
export * from './errors/PhoneRequiredForSmsMfaError';
export * from './errors/PhoneVerificationRequiredError';
export * from './errors/PremiumPurchaseBlockedError';
export * from './errors/PremiumRequiredError';
export * from './errors/ProcessingFailedError';
export * from './errors/RateLimitError';
export * from './errors/ReportAlreadyResolvedError';
export * from './errors/ReportBannedError';
export * from './errors/ResourceLockedError';
export * from './errors/SlowmodeRateLimitError';
export * from './errors/SmsMfaNotEnabledError';
export * from './errors/SmsMfaRequiresTotpError';
export * from './errors/StripeError';
export * from './errors/StripeWebhookSignatureInvalidError';
export * from './errors/StripeWebhookSignatureMissingError';
export * from './errors/TagAlreadyTakenError';
export * from './errors/TemporaryInviteRequiresPresenceError';
export * from './errors/TestHarnessDisabledError';
export * from './errors/TestHarnessForbiddenError';
export * from './errors/UnauthorizedError';
export * from './errors/UnclaimedAccountRestrictedError';
export * from './errors/UnknownApplicationError';
export * from './errors/UnknownChannelError';
export * from './errors/UnknownFavoriteMemeError';
export * from './errors/UnknownGiftCodeError';
export * from './errors/UnknownGuildEmojiError';
export * from './errors/UnknownGuildError';
export * from './errors/UnknownGuildMemberError';
export * from './errors/UnknownGuildRoleError';
export * from './errors/UnknownGuildStickerError';
export * from './errors/UnknownHarvestError';
export * from './errors/UnknownInviteError';
export * from './errors/UnknownMessageError';
export * from './errors/UnknownReportError';
export * from './errors/UnknownSuspiciousFlagError';
export * from './errors/UnknownUserError';
export * from './errors/UnknownUserFlagError';
export * from './errors/UnknownVoiceRegionError';
export * from './errors/UnknownVoiceServerError';
export * from './errors/UnknownWebhookError';
export * from './errors/UnsupportedGrantTypeError';
export * from './errors/UpdateFailedError';
export * from './errors/UserNotInVoiceError';
export * from './errors/UserOwnsGuildsError';
export * from './errors/ValidationError';
