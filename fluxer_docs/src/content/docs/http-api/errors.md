---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Errors
description: Error responses, status codes and every error code.
---

When a request fails, the response has a non-2xx status and a JSON body with `code` and `message`. Match on `code`, which never changes. `message` is translated text for people, and you shouldn't parse it.

## Error response

```json
{
  "code": "MISSING_PERMISSIONS",
  "message": "You don't have the permissions required to perform this action."
}
```

Some codes add more top-level fields, and a request that fails a field check also has an `errors` array. Ignore any field you don't recognize. Every response has an `X-Request-ID` header, and you should include it when you report a problem.

`message` uses the account's locale or the `Accept-Language` header and falls back to English, as described in [Locales](/topics/locales/).

[OAuth2](/http-api/oauth2/) protocol endpoints return the RFC 6749 `error` and `error_description` fields instead.

## Status codes

| Status | Meaning | What to do |
| --- | --- | --- |
| 400 | The request is invalid or failed a check | Fix the request before you send it again. On `SLOWMODE_RATE_LIMITED`, wait the seconds in `retry_after` |
| 401 | The token is missing, invalid or expired, or a passkey sign-in failed | Get a new token and stop using the rejected one, unless `code` is `PASSKEY_AUTHENTICATION_FAILED` |
| 403 | The account, token or network isn't allowed to make this request | Check `code` and your permissions, and don't send the same request again |
| 404 | The resource or route doesn't exist, or you can't see it | Check the ID and the route |
| 409 | The request conflicts with the current state | Fetch the current state and change the request to match it |
| 423 | Another request holds a lock on the resource | Wait the number of seconds in the `Retry-After` header and try again |
| 429 | You're rate limited, or another request is changing the same resource (`RESOURCE_LOCKED`) | Wait the number of seconds in `retry_after`, which can be a fraction, and try again. See [Rate limits](/topics/rate-limits/) |
| 500, 502, 503, 504 | Something failed on the instance | Wait the number of seconds in `Retry-After` if it's sent, or use exponential backoff. Give up after a few tries |

If your IP address is blocked for a while because of abusive traffic, requests return 403 `GLOBAL_IP_TEMPORARILY_BANNED`, and `expires_at` says when the block ends.

## Validation errors

When a field fails a check, the request returns 400 `INVALID_FORM_BODY` with an `errors` array. Each element is one failed check, with `path`, `message` and usually `code`.

```json
{
  "code": "INVALID_FORM_BODY",
  "message": "Invalid form body.",
  "errors": [
    {
      "path": "embeds.0.title",
      "code": "STRING_LENGTH_INVALID",
      "message": "String length must be between 0 and 256 characters."
    }
  ]
}
```

`path` joins field names and array indexes with dots, as in `embeds.0.title`. A `path` of `root` means the error is about the whole request.

On [Modify current user settings](/http-api/users/settings/#modify-current-user-settings), the trusted domain, age restriction and synced preferences checks return 400 `VALIDATION_ERROR` with the same `errors` structure. Other field failures there return `INVALID_FORM_BODY`.

## Extra fields

This table has the most common extra fields. Any other extra fields are listed on the page for the endpoint that returns them.

| Field | Meaning |
| --- | --- |
| `retry_after`, `global` | On `RATE_LIMITED`, `SLOWMODE_RATE_LIMITED` and other cooldowns, the seconds to wait. `global` is only on `RATE_LIMITED` |
| `required_scope` | On `MISSING_OAUTH_SCOPE`, the OAuth2 scope the token is missing |
| `has_mfa`, `methods` | On `SUDO_MODE_REQUIRED`, the proofs the account can give for [sudo mode](/http-api/users/mfa/#sudo-mode) |
| `captcha_provider`, `altcha_challenge` | On `CAPTCHA_REQUIRED` and `INVALID_CAPTCHA`, the challenge to solve, as described in [CAPTCHA handling](/topics/captcha/) |
| `step_index` | On `INVALID_REPORT_FLOW_ANSWERS`, the index of the first [report flow step](/http-api/reports/#report-flow-step-object) whose answer isn't valid |
| `ticket`, `email`, `resend_available_in` | On `IP_AUTHORIZATION_REQUIRED`, the ticket for [authorizing a new IP address](/http-api/authentication/#log-in-with-a-password), and `ip_authorization_required` is `true` |
| `action`, `tier`, `need` | On `ACCOUNT_VERIFICATION_REQUIRED`, what was refused and which step is missing, as described in [account tiers](/http-api/users/#account-tiers) |
| `ban_kind`, `expires_at`, `reason` | On `GLOBAL_IP_BANNED` and `GLOBAL_IP_TEMPORARILY_BANNED`, details of the block, listed in full under [network block response](/http-api/authentication/#network-block-response) |
| `notice` | On `ACCOUNT_SUSPENDED_TEMPORARILY` and `ACCOUNT_SUSPENDED_PERMANENTLY`, the [enforcement notice](/http-api/authentication/#enforcement-notice-object) for the account |

## API error codes

These are the values of the top-level `code`, and the [OpenAPI document](/http-api/instance/#get-openapi-document) lists them as the `APIErrorCode` schema. The Message column shows the English text, with values to fill in shown in braces. The `message` you get back has those values filled in and can have an extra sentence at the end. Some messages change with the reason, and the table shows the general one.

| Code | Message |
| --- | --- |
| `ACCESS_DENIED` | You don't have access to this resource or feature |
| `ACCOUNT_ALREADY_CLAIMED` | This account is already set up to sign in from other devices |
| `ACCOUNT_BELOW_MINIMUM_AGE` | You aren't old enough to have an account here |
| `ACCOUNT_IDENTITY_LOCKED` | The sign-in method is already set and can't be changed |
| `ACCOUNT_LIMITED` | Messaging is paused on your account |
| `ACCOUNT_SUSPENDED_PERMANENTLY` | This account has been permanently suspended |
| `ACCOUNT_SUSPENDED_TEMPORARILY` | This account has been temporarily suspended |
| `ACCOUNT_TOO_NEW_FOR_GUILD` | Your account is too new to send messages in this community |
| `ACCOUNT_VERIFICATION_REQUIRED` | Claim your account to do this |
| `ACLS_MUST_BE_NON_EMPTY` | ACLs must be non-empty |
| `ADMIN_API_KEY_NOT_FOUND` | Admin API key wasn't found |
| `ALREADY_FRIENDS` | You're already friends with this user |
| `ANNOUNCEMENT_CHANNEL_REQUIRED` | This action is only available in announcement channels |
| `BAD_GATEWAY` | Bad gateway |
| `BAD_REQUEST` | Bad request |
| `BLUESKY_OAUTH_AUTHORIZATION_FAILED` | We couldn't resolve that Bluesky handle |
| `BLUESKY_OAUTH_NOT_ENABLED` | Bluesky connections are not enabled on this instance |
| `BOTS_CANNOT_CREATE_GUILDS` | Bots can't create communities |
| `BOTS_CANNOT_SEND_FRIEND_REQUESTS` | Bots can't send friend requests |
| `BOT_ALREADY_IN_GUILD` | This bot is already in the community |
| `BOT_IS_PRIVATE` | This bot is private |
| `BOT_USER_AUTH_ENDPOINT_ACCESS_DENIED` | Bot users can't use auth endpoints |
| `BOT_USER_AUTH_SESSION_CREATION_DENIED` | Bot users can't create auth sessions |
| `BOT_USER_GENERATION_FAILED` | Bot user generation failed |
| `BOT_USER_NOT_FOUND` | Bot user wasn't found |
| `CALL_ALREADY_EXISTS` | A call is already in progress in this channel |
| `CANNOT_BLOCK_SYSTEM_USER` | You can't block the system user |
| `CANNOT_EDIT_OTHER_USER_MESSAGE` | You can't edit another user's message |
| `CANNOT_EXECUTE_ON_DM` | This action can't be performed in a DM |
| `CANNOT_MODIFY_SYSTEM_WEBHOOK` | You can't edit system messages |
| `CANNOT_REDEEM_PLUTONIUM_WITH_VISIONARY` | You already have lifetime {premium_tier_name} |
| `CANNOT_REPORT_GUILD` | You can't report this community |
| `CANNOT_REPORT_OWN_GUILD` | You can't report your own community |
| `CANNOT_REPORT_OWN_MESSAGE` | You can't report your own message |
| `CANNOT_REPORT_YOURSELF` | You can't report yourself |
| `CANNOT_SEND_EMPTY_MESSAGE` | You can't send an empty message |
| `CANNOT_SEND_FRIEND_REQUEST_TO_BLOCKED_USER` | You can't send a friend request to a blocked user |
| `CANNOT_SEND_FRIEND_REQUEST_TO_SELF` | You can't send a friend request to yourself |
| `CANNOT_SEND_MESSAGES_IN_NON_TEXT_CHANNEL` | You can't send messages in a non-text channel |
| `CANNOT_SEND_MESSAGES_TO_USER` | You can't send messages to this user |
| `CANNOT_TRANSFER_OWNERSHIP_TO_BOT` | Community ownership can't be transferred to a bot |
| `CAPTCHA_REQUIRED` | Verification required |
| `CHANNEL_ALREADY_FOLLOWED` | This channel already receives updates from that announcement channel |
| `CHANNEL_HAS_FOLLOWED_CHANNELS` | Remove the followed channels posting here before converting it to an announcement channel |
| `CHANNEL_HAS_THREADS` | Channels with threads can't be converted to announcement channels |
| `CHANNEL_TYPE_CONVERSION_NOT_SUPPORTED` | Only text and announcement channels can be converted into each other |
| `COMMUNICATION_DISABLED` | Communication is disabled |
| `CONFLICT` | Conflict |
| `CONNECTION_ALREADY_EXISTS` | A connection of this type with this identifier already exists |
| `CONNECTION_INITIATION_TOKEN_INVALID` | The connection initiation token is invalid or has expired |
| `CONNECTION_INVALID_TYPE` | The connection type is not supported |
| `CONNECTION_LIMIT_REACHED` | You've reached the maximum number of connections ({limit}) |
| `CONNECTION_NOT_FOUND` | Unknown connection |
| `CONNECTION_VERIFICATION_FAILED` | Connection verification failed |
| `CONTENT_BLOCKED` | This content was blocked by safety systems |
| `DATE_OF_BIRTH_ALREADY_SET` | Your date of birth is already set |
| `DATE_OF_BIRTH_REQUIRED` | Enter your date of birth to continue |
| `DELETION_FAILED` | We couldn't delete the resource |
| `DIRECT_MESSAGES_DISABLED` | Direct messages are disabled on this instance |
| `DIRECT_MESSAGE_EMAIL_VERIFICATION_REQUIRED` | Email verification is required for this action |
| `DISCOVERY_ALREADY_APPLIED` | This community has already applied to be listed in Explore |
| `DISCOVERY_APPLICATION_ALREADY_REVIEWED` | This application to Explore has already been reviewed |
| `DISCOVERY_APPLICATION_NOT_FOUND` | The application to Explore wasn't found |
| `DISCOVERY_DISABLED` | Explore isn't available on this instance |
| `DISCOVERY_INSUFFICIENT_MEMBERS` | This community doesn't have enough members to be listed in Explore |
| `DISCOVERY_NOT_DISCOVERABLE` | This community isn't listed in Explore |
| `DISCRIMINATOR_REQUIRED` | Discriminator is required |
| `EMAIL_CODES_UNAVAILABLE` | This instance can't send email codes |
| `EMAIL_CODE_RESEND_COOLDOWN` | Wait a moment before asking for another code |
| `EMAIL_DELIVERY_OFF` | This instance doesn't send email |
| `EMAIL_SERVICE_NOT_TESTABLE` | Email service is temporarily unavailable |
| `EMAIL_UNAVAILABLE_ON_INSTANCE` | This instance doesn't use email |
| `EMAIL_VERIFICATION_REQUIRED` | Email verification is required for this action |
| `ENFORCEMENT_ACCOUNT_CLOSED` | This account is closed, so it can't be suspended |
| `ENFORCEMENT_DECISION_ENDED` | This decision has already ended |
| `ENFORCEMENT_DECISION_OWNS_STATE` | An enforcement decision owns this restriction |
| `ENFORCEMENT_DECISION_REPLACED` | A later decision replaced this one, so it can no longer be ended |
| `ENFORCEMENT_DELETION_STARTED` | The deletion of this account has already started, so the decision can no longer be ended |
| `EXPLICIT_CONTENT_CANNOT_BE_SENT` | Explicit content can't be sent |
| `FEATURE_NOT_AVAILABLE_SELF_HOSTED` | This feature isn't available on self-hosted instances |
| `FEATURE_TEMPORARILY_DISABLED` | This feature is temporarily disabled |
| `FILE_SIZE_TOO_LARGE` | File size is too large |
| `FOLLOW_TARGET_CONTENT_WARNING_REQUIRED` | Updates from a channel with a content warning can only go to a channel with a content warning or an age restriction |
| `FOLLOW_TARGET_NOT_AGE_RESTRICTED` | Updates from an age-restricted channel can only go to an age-restricted channel |
| `FORBIDDEN` | Forbidden |
| `FORUM_TAG_NAMES_MUST_BE_UNIQUE` | Tag names must be unique |
| `FORUM_TAG_REQUIRED` | A tag is required to create a post in this channel |
| `FRIENDS_DISABLED` | Friend requests are disabled on this instance |
| `FRIEND_REQUEST_BLOCKED` | User does not accept friend requests at this time |
| `FRIEND_REQUEST_EMAIL_VERIFICATION_REQUIRED` | Email verification is required for this action |
| `GATEWAY_TIMEOUT` | Gateway timeout |
| `GENERAL_ERROR` | Something went wrong |
| `GIFT_CODE_ALREADY_REDEEMED` | Gift code is already redeemed |
| `GLOBAL_IP_BANNED` | {product_name} blocked access from this network |
| `GLOBAL_IP_TEMPORARILY_BANNED` | {product_name} blocked access from this network |
| `GONE` | Gone |
| `GROUP_DM_MATURE_CONTENT_INELIGIBLE` | Not everyone in this group is eligible for mature content |
| `GROUP_DM_RECIPIENTS_NOT_ADDABLE` | One or more selected users can't be added to this group DM |
| `GUILD_CREATION_EMAIL_VERIFICATION_REQUIRED` | Email verification is required for this action |
| `GUILD_CREATION_PERMISSION_REQUIRED` | You don't have permission to create communities on this instance |
| `GUILD_EMAIL_VERIFICATION_REQUIRED` | Email verification is required for this action |
| `GUILD_TEMPLATE_INVALID` | The community template data is invalid or malformed |
| `GUILD_VERIFICATION_REQUIRED` | Claim your account to talk in this community |
| `GUILD_VISITS_DISABLED` | This community asks people to join before previewing it |
| `GUILD_VISIT_AGE_RESTRICTED` | This community is for adults only |
| `GUILD_VISIT_NOT_FOUND` | You're not previewing this community anymore |
| `HANDOFF_ATTEMPTS_EXCEEDED` | Too many sign-in attempts from your network |
| `HANDOFF_CODE_ALREADY_USED` | This code was already used |
| `HANDOFF_CODE_EXPIRED` | Handoff code expired |
| `HARVEST_EXPIRED` | Your data export request has expired |
| `HARVEST_FAILED` | Your data export request failed |
| `HARVEST_NOT_READY` | Your data export is still being prepared |
| `HIDE_MEDIA_DOWNLOAD_OPTION_MEDIA_ONLY` | Hiding media download options is only available in media channels |
| `INSTANCE_POLICY_TRANSITION_NOT_ALLOWED` | This instance policy change is not allowed |
| `INSTANT_ACCOUNTS_DISABLED` | Creating an account from just a display name isn't available right now |
| `INTERNAL_SERVER_ERROR` | Internal server error |
| `INVALID_ACCOUNT_HANDOFF_NONCE` | This sign-up link isn't valid |
| `INVALID_ACLS_FORMAT` | Invalid ACL format |
| `INVALID_API_ORIGIN` | Invalid API origin |
| `INVALID_AUTH_TOKEN` | Invalid or expired authorization token |
| `INVALID_BOT_FLAG` | Invalid bot flag |
| `INVALID_CAPTCHA` | Verification failed |
| `INVALID_CHANNEL_TYPE` | Invalid channel type |
| `INVALID_CHANNEL_TYPE_FOR_CALL` | Calls can only be made in direct messages or group DMs |
| `INVALID_DSA_REPORT_TARGET` | Invalid DSA report target |
| `INVALID_DSA_TICKET` | Invalid DSA ticket |
| `INVALID_DSA_VERIFICATION_CODE` | Invalid DSA verification code |
| `INVALID_FLAGS_FORMAT` | Invalid flags format |
| `INVALID_FOLLOW_TARGET_CHANNEL` | Followed channels can only post into text channels |
| `INVALID_FORM_BODY` | Invalid form body |
| `INVALID_HANDOFF_CODE` | Invalid handoff code |
| `INVALID_ORIGIN_HANDOFF_NONCE` | This sign-in transfer doesn't match the one you started |
| `INVALID_PERMISSIONS_INTEGER` | Permissions must be a valid integer |
| `INVALID_PERMISSIONS_NEGATIVE` | Permissions must be non-negative |
| `INVALID_REPORT_FLOW_ANSWERS` | These answers don't match the report form |
| `INVALID_REQUEST` | Invalid request |
| `INVALID_SIGN_IN_CODE` | That code isn't right |
| `INVALID_STREAM_KEY_FORMAT` | Invalid stream key format |
| `INVALID_STREAM_THUMBNAIL_PAYLOAD` | Invalid stream thumbnail payload |
| `INVALID_SYSTEM_FLAG` | Invalid system flag |
| `INVALID_THREAD_NOTIFICATION_SETTINGS` | Invalid thread notification settings |
| `INVALID_TIMESTAMP` | Invalid timestamp |
| `INVALID_TOKEN` | Invalid or expired authorization token |
| `INVALID_WEBAUTHN_AUTHENTICATION_COUNTER` | Invalid WebAuthn authentication counter |
| `INVALID_WEBAUTHN_CREDENTIAL` | Failed to verify WebAuthn credential |
| `INVALID_WEBAUTHN_CREDENTIAL_COUNTER` | Invalid WebAuthn credential counter |
| `INVALID_WEBAUTHN_PUBLIC_KEY_FORMAT` | Invalid WebAuthn public key format |
| `INVITES_DISABLED` | Invites are disabled |
| `IP_AUTHORIZATION_REQUIRED` | IP authorization is required |
| `IP_AUTHORIZATION_RESEND_COOLDOWN` | IP authorization resend is on cooldown |
| `IP_AUTHORIZATION_RESEND_LIMIT_EXCEEDED` | IP authorization resend limit exceeded |
| `IP_BAN_DECLINED` | This IP address cannot be added to the blocklist |
| `IP_BAN_LINKED_TO_DECISION` | This network block belongs to an enforcement decision |
| `MAX_ACTIVE_THREADS` | This community has reached its active thread limit of {count} |
| `MAX_APPLICATIONS` | You've reached the maximum of {limit} applications |
| `MAX_BADGES` | You've reached the maximum of {count} badges for this type |
| `MAX_BOOKMARKS` | You've reached the maximum of {count} bookmarks |
| `MAX_CATEGORY_CHANNELS` | You've reached the maximum of {count} category channels |
| `MAX_EMOJIS` | You've reached the maximum of {count} emojis |
| `MAX_FAVORITE_MEMES` | You've reached the maximum of {count} saved media items |
| `MAX_FORUM_TAGS` | This channel has reached its tag limit of {count} |
| `MAX_FRIENDS` | You've reached the maximum of {count} friends |
| `MAX_GROUP_DMS` | You've reached the maximum of {count} group DMs |
| `MAX_GROUP_DM_RECIPIENTS` | You've reached the maximum of {count} group DM recipients |
| `MAX_GUILDS` | You've reached the maximum of {count} communities |
| `MAX_GUILD_CHANNELS` | You've reached the maximum of {count} community channels |
| `MAX_GUILD_MEMBERS` | You've reached the maximum of {count} community members |
| `MAX_GUILD_ROLES` | You've reached the maximum of {count} community roles |
| `MAX_INVITES` | You've reached the maximum of {count} invites |
| `MAX_PINNED_THREADS_IN_FORUM` | This channel has reached its pinned post limit of {count} |
| `MAX_REACTIONS` | You've reached the maximum of {count} reactions |
| `MAX_STICKERS` | You've reached the maximum of {count} stickers |
| `MAX_THREAD_MEMBERS` | This thread has reached its member limit of {count} |
| `MAX_WEBHOOKS_PER_CHANNEL` | You've reached the maximum of {count} webhooks per channel |
| `MAX_WEBHOOKS_PER_GUILD` | You've reached the maximum of {count} webhooks per community |
| `MEDIA_METADATA_ERROR` | Media metadata error |
| `MESSAGE_ALREADY_CROSSPOSTED` | This message has already been published |
| `MESSAGE_CROSSPOST_RATE_LIMITED` | This channel has reached its publishing limit |
| `MESSAGE_NOT_CROSSPOSTABLE` | This message cannot be published |
| `METHOD_NOT_ALLOWED` | Method not allowed |
| `MFA_EMAIL_VERIFICATION_REQUIRED` | Confirm your email address to use two-factor authentication or passkeys |
| `MISSING_ACCESS` | You don't have access to this resource or feature |
| `MISSING_ACL` | Missing ACL |
| `MISSING_AUTHORIZATION` | Missing or invalid authorization header |
| `MISSING_OAUTH_SCOPE` | The requested scope isn't supported or you don't have permission to request it |
| `MISSING_PERMISSIONS` | You don't have the permissions required to perform this action |
| `NEW_CONVERSATIONS_LIMITED` | You can't start new conversations right now |
| `NOT_A_BOT_APPLICATION` | This application isn't a bot |
| `NOT_FOUND` | Not found |
| `NOT_FRIENDS_WITH_USER` | You're not friends with this user |
| `NOT_IMPLEMENTED` | Not implemented |
| `NO_ACTIVE_CALL` | No active call |
| `NO_ACTIVE_SUBSCRIPTION` | No active subscription |
| `NO_PASSKEYS_REGISTERED` | No passkeys are registered |
| `NO_PENDING_DELETION` | Invalid request |
| `NO_TAGS_AVAILABLE_TO_NON_MODERATORS` | A tag is required, but only moderators can apply the tags in this channel |
| `NO_USERS_WITH_FLUXERTAG_EXIST` | There are too many users with this username |
| `NSFW_CONTENT_AGE_RESTRICTED` | NSFW content is age restricted |
| `PASSKEY_AUTHENTICATION_FAILED` | Passkey authentication failed |
| `PASSWORD_REQUIRED_TO_CLAIM` | Set a password to keep this account |
| `PASSWORD_SIGN_IN_REQUIRED` | Sign in to this account with its password |
| `PERSONAL_NOTES_DISABLED` | Personal notes are turned off on this instance |
| `PREMIUM_PURCHASE_BLOCKED` | No active subscription |
| `PREVIEW_MUST_BE_JPEG` | Preview must be JPEG |
| `PROCESSING_FAILED` | We couldn't process the request |
| `PROFILE_EMAIL_VERIFICATION_REQUIRED` | Email verification is required for this action |
| `PUBLISHED_MESSAGE_EDIT_RATE_LIMITED` | This published message has reached its editing limit |
| `PURCHASE_EMAIL_VERIFICATION_REQUIRED` | Email verification is required for this action |
| `RATE_LIMITED` | You're being rate limited |
| `REACTION_EMAIL_VERIFICATION_REQUIRED` | Email verification is required for this action |
| `REGISTRATION_CLOSED` | Registration is closed on this instance |
| `REGISTRATION_CREDENTIALS_REQUIRED` | This instance reviews new accounts |
| `REGISTRATION_CREDENTIAL_REQUIRED` | Choose a password to sign up |
| `REGISTRATION_PENDING_APPROVAL` | This registration is waiting for admin approval |
| `REGISTRATION_REJECTED` | This registration request was rejected |
| `REGISTRATION_URL_INVALID` | This registration link is invalid or has expired |
| `REPORT_ALREADY_RESOLVED` | Report already resolved |
| `REPORT_BANNED` | You've been banned from submitting reports |
| `REPORT_EMAIL_VERIFICATION_REQUIRED` | Email verification is required for this action |
| `REPORT_FLOW_OUTDATED` | The report form changed while you were filling it in |
| `REPORT_UNDER_LEGAL_HOLD` | This report is under a legal hold and can't be deleted |
| `RESOURCE_LOCKED` | This resource is being modified |
| `RESPONSE_VALIDATION_ERROR` | Response validation failed: {errors} |
| `SEARCH_INDEX_NOT_READY` | The search index isn't ready yet |
| `SERVICE_UNAVAILABLE` | Service unavailable |
| `SESSION_TOKEN_MISMATCH` | Session token mismatch |
| `SIGN_IN_CODE_EXPIRED` | This code has expired |
| `SINGLE_COMMUNITY_CANNOT_CREATE_GUILDS` | This instance is a single community, so additional communities cannot be created |
| `SINGLE_COMMUNITY_CANNOT_DELETE` | The community for this instance cannot be deleted |
| `SINGLE_COMMUNITY_CANNOT_LEAVE` | You cannot leave the community for this instance |
| `SLOWMODE_RATE_LIMITED` | Slowmode rate limited |
| `SSO_REQUIRED` | This instance requires single sign-on |
| `STREAM_KEY_CHANNEL_MISMATCH` | Stream key channel mismatch |
| `STREAM_KEY_SCOPE_MISMATCH` | Stream key scope mismatch |
| `STRIPE_ERROR` | Payment processing encountered an error |
| `STRIPE_GIFT_REDEMPTION_IN_PROGRESS` | Gift code redemption is in progress |
| `STRIPE_INVALID_PRODUCT` | Invalid product selection |
| `STRIPE_INVALID_PRODUCT_CONFIGURATION` | Invalid product configuration |
| `STRIPE_NO_ACTIVE_SUBSCRIPTION` | No active subscription found |
| `STRIPE_NO_PURCHASE_HISTORY` | No purchase history found |
| `STRIPE_NO_SUBSCRIPTION` | No subscription found |
| `STRIPE_PAYMENT_NOT_AVAILABLE` | Payment processing is temporarily unavailable |
| `STRIPE_SUBSCRIPTION_ALREADY_CANCELING` | Subscription is already set to cancel at period end |
| `STRIPE_SUBSCRIPTION_NOT_CANCELING` | Subscription isn't set to cancel |
| `STRIPE_WEBHOOK_NOT_AVAILABLE` | Webhook processing isn't available |
| `STRIPE_WEBHOOK_SIGNATURE_INVALID` | Stripe webhook signature is invalid |
| `STRIPE_WEBHOOK_SIGNATURE_MISSING` | Stripe webhook signature is missing |
| `SUDO_MODE_REQUIRED` | Sudo mode is required |
| `SUDO_SECOND_FACTOR_REQUIRED` | Confirm it's you with your authenticator app, passkey or a backup code |
| `SYNCED_PREFERENCES_CONFLICT` | Your preferences changed on another device |
| `TAG_ALREADY_TAKEN` | This tag is already taken |
| `TEMPORARY_INVITE_REQUIRES_PRESENCE` | Temporary invite requires presence |
| `TEST_HARNESS_DISABLED` | Test harness is disabled |
| `TEST_HARNESS_FORBIDDEN` | Test harness is forbidden |
| `THREAD_ALREADY_CREATED_FOR_MESSAGE` | A thread has already been created for this message |
| `THREAD_ARCHIVED` | This thread is archived |
| `THREAD_LOCKED` | This thread is locked |
| `TWO_FACTOR_REQUIRED` | Two-factor authentication is required |
| `TWO_FA_ALREADY_ENABLED` | Two-factor authentication is already enabled |
| `UNAUTHORIZED` | Unauthorized |
| `UNCLAIMED_ACCOUNT_CANNOT_ACCEPT_FRIEND_REQUESTS` | Claim your account to accept friend requests |
| `UNCLAIMED_ACCOUNT_CANNOT_ADD_REACTIONS` | Claim your account to add reactions |
| `UNCLAIMED_ACCOUNT_CANNOT_CREATE_APPLICATIONS` | Claim your account to create applications |
| `UNCLAIMED_ACCOUNT_CANNOT_CREATE_GUILDS` | Claim your account to create communities |
| `UNCLAIMED_ACCOUNT_CANNOT_JOIN_GROUP_DMS` | Claim your account to join group chats |
| `UNCLAIMED_ACCOUNT_CANNOT_JOIN_ONE_ON_ONE_VOICE_CALLS` | Claim your account to start or join calls |
| `UNCLAIMED_ACCOUNT_CANNOT_JOIN_VOICE_CHANNELS` | Claim your account to join voice channels |
| `UNCLAIMED_ACCOUNT_CANNOT_MAKE_PURCHASES` | Claim your account to make purchases |
| `UNCLAIMED_ACCOUNT_CANNOT_SEND_DIRECT_MESSAGES` | Claim your account to send direct messages |
| `UNCLAIMED_ACCOUNT_CANNOT_SEND_FRIEND_REQUESTS` | Claim your account to send friend requests |
| `UNCLAIMED_ACCOUNT_CANNOT_SEND_MESSAGES` | Claim your account to send messages |
| `UNCLAIMED_ACCOUNT_CANNOT_SUBMIT_REPORTS` | Claim your account to submit reports |
| `UNKNOWN_ACCOUNT_HANDOFF` | This sign-up link has expired or was already used |
| `UNKNOWN_APPLICATION` | Unknown application |
| `UNKNOWN_BADGE` | Badge wasn't found |
| `UNKNOWN_CHANNEL` | Channel wasn't found |
| `UNKNOWN_EMOJI` | Unknown emoji |
| `UNKNOWN_ENFORCEMENT_DECISION` | Unknown enforcement decision |
| `UNKNOWN_FAVORITE_MEME` | Unknown saved media |
| `UNKNOWN_FORUM_TAG` | Tag wasn't found |
| `UNKNOWN_GIFT_CODE` | Unknown gift code |
| `UNKNOWN_GUILD` | Community wasn't found |
| `UNKNOWN_HARVEST` | Unknown data export |
| `UNKNOWN_INVITE` | Invite wasn't found or is no longer valid |
| `UNKNOWN_MEMBER` | Member wasn't found in this community |
| `UNKNOWN_MESSAGE` | Message wasn't found |
| `UNKNOWN_ORIGIN_HANDOFF` | This sign-in transfer has expired or was already used |
| `UNKNOWN_REPORT` | Unknown report |
| `UNKNOWN_ROLE` | Role wasn't found |
| `UNKNOWN_SIGN_IN_TICKET` | This sign-in attempt has expired |
| `UNKNOWN_STICKER` | Unknown sticker |
| `UNKNOWN_THREAD_MEMBER` | Thread member wasn't found |
| `UNKNOWN_USER` | User wasn't found |
| `UNKNOWN_USER_FLAG` | The specified user flag isn't recognized |
| `UNKNOWN_VOICE_REGION` | Unknown voice region |
| `UNKNOWN_VOICE_SERVER` | Unknown voice server |
| `UNKNOWN_WEBAUTHN_CREDENTIAL` | Unknown WebAuthn credential |
| `UNKNOWN_WEBHOOK` | Unknown webhook |
| `UPDATE_FAILED` | We couldn't update the resource |
| `USERNAME_NOT_AVAILABLE` | This username is not available |
| `USERNAME_SIGN_IN_ONLY` | This is only available on instances where people sign in with a username |
| `USER_BANNED_FROM_GUILD` | This user is banned from this community |
| `USER_IP_BANNED_FROM_GUILD` | This user's IP is banned from this community |
| `USER_NOT_IN_VOICE` | This user isn't in voice |
| `USER_OWNS_GUILDS` | This user owns communities |
| `USE_CLAIM_ROUTE` | Add an email address to your account first |
| `VALIDATION_ERROR` | Validation failed |
| `VOICE_CHANNEL_FULL` | Voice channel is full |
| `WEBAUTHN_CREDENTIAL_LIMIT_REACHED` | You've reached the maximum of {count} WebAuthn credentials |
| `WEBHOOK_FORUM_TARGET_CONFLICT` | You can't specify both `thread_id` and `thread_name` |
| `WEBHOOK_FORUM_TARGET_REQUIRED` | Webhooks posting to this channel need a `thread_id` or a `thread_name` |
| `WEBHOOK_SERVICE_FORUM_UNSUPPORTED` | This webhook service can't post to a forum or media channel without a `thread_id` |
| `WEBHOOK_THREAD_NAME_REQUIRES_FORUM` | `thread_name` and `applied_tags` only work in forum and media channels |

## Validation error codes

These are the values of `code` inside `errors`.

| Code | Message |
| --- | --- |
| `ACCENT_COLOR_CHANGED_TOO_MANY_TIMES` | You've changed your accent color too often recently |
| `AFK_CHANNEL_MUST_BE_IN_GUILD` | AFK channel must be in this community |
| `AFK_CHANNEL_MUST_BE_VOICE` | AFK channel must be a voice channel |
| `AGE_RESTRICTED` | This sensitive content filter isn't available for your age group |
| `ALL_CHANNELS_MUST_BELONG_TO_GUILD` | All channels must belong to this community |
| `ANIMATED_AVATARS_REQUIRE_PREMIUM` | Animated avatars require Premium |
| `ANIMATED_GUILD_BANNER_REQUIRES_FEATURE` | Animated community banner requires ANIMATED_BANNER feature |
| `ATTACHMENTS_NOT_ALLOWED_FOR_MESSAGE` | Attachments aren't allowed for this message |
| `ATTACHMENT_ID_NOT_FOUND_IN_MESSAGE` | Attachment with ID {attachmentId} wasn't found in the message |
| `ATTACHMENT_MUST_BE_IMAGE` | Attachment "{filename}" must be an image file (png, jpg, jpeg, webp, or gif) |
| `AVATAR_CHANGED_TOO_MANY_TIMES` | You've changed your avatar too often recently |
| `BANNERS_REQUIRE_PREMIUM` | Banners require Premium |
| `BANNER_CHANGED_TOO_MANY_TIMES` | You've changed your banner too often recently |
| `BASE64_LENGTH_INVALID` | Base64 string length must be between {min} and {maxLength} characters |
| `BIO_CHANGED_TOO_MANY_TIMES` | You've changed your bio too often recently |
| `BOT_DISCRIMINATOR_CANNOT_BE_CHANGED` | Bot discriminator can't be changed |
| `BOT_SEARCH_SCOPE_UNAVAILABLE` | Bots can only search within a single community or channel |
| `CANNOT_ADD_YOURSELF_TO_GROUP_DM` | You can't add yourself to a group DM |
| `CANNOT_DELETE_MORE_THAN_100_MESSAGES` | You can't delete more than {max} messages at once |
| `CANNOT_DM_YOURSELF` | You can't DM yourself |
| `CANNOT_EDIT_ATTACHMENT_METADATA` | Users with MANAGE_MESSAGES can only edit attachment descriptions, not other metadata |
| `CANNOT_LEAVE_GUILD_AS_OWNER` | You can't leave a community as the owner |
| `CANNOT_POSITION_CHANNEL_RELATIVE_TO_ITSELF` | You can't position a channel relative to itself or its descendants |
| `CANNOT_PRELOAD_MORE_THAN_100_CHANNELS` | You can't preload more than {max} channels at once |
| `CANNOT_REFERENCE_ATTACHMENTS_WITHOUT_ATTACHMENTS` | You can't reference attachments when no attachments are provided |
| `CANNOT_REORDER_EVERYONE_ROLE` | You can't reorder the @everyone role |
| `CANNOT_REPLY_TO_SYSTEM_MESSAGE` | You can't reply to a system message |
| `CANNOT_SET_HOIST_FOR_EVERYONE_ROLE` | You can't set hoist position for the @everyone role |
| `CANNOT_SPECIFY_BOTH_BEFORE_AND_AFTER` | You can't specify both `before` and `after` |
| `CAPTCHA_REQUIRED_BY_INSTANT_ACCOUNTS` | Turn off instant accounts before turning off the captcha |
| `CATEGORIES_CANNOT_HAVE_PARENTS` | Categories can't have parents |
| `CATEGORIES_CANNOT_HAVE_PARENT_CHANNEL` | Categories can't have a parent channel |
| `CHANGING_DISCRIMINATOR_REQUIRES_PREMIUM` | Changing your discriminator requires Premium |
| `CHANNEL_DOES_NOT_EXIST` | Channel not found |
| `CHANNEL_ID_IS_REQUIRED` | `channel_id` is required |
| `CHANNEL_MUST_BE_DM_OR_GROUP_DM` | Channel must be a DM or a group DM |
| `CHANNEL_MUST_BE_VOICE` | Channel must be a voice channel |
| `CHANNEL_NOT_FOUND` | Channel not found |
| `COLOR_VALUE_TOO_HIGH` | Color value must not exceed 0xffffff |
| `COLOR_VALUE_TOO_LOW` | Color value must be at least 0x000000 |
| `CONTENT_EXCEEDS_MAX_LENGTH` | Text must not exceed {maxLength} characters |
| `CONTEXT_CHANNEL_OR_GUILD_ID_REQUIRED` | A context channel or community ID is required |
| `CUSTOM_EMOJIS_REQUIRE_PREMIUM_OUTSIDE_SOURCE` | You can't use custom emojis outside their source communities without Premium |
| `CUSTOM_EMOJI_NOT_FOUND` | Custom emoji wasn't found |
| `CUSTOM_STICKERS_IN_DMS_REQUIRE_PREMIUM` | You can't use custom stickers in DMs without Premium |
| `CUSTOM_STICKERS_REQUIRE_PREMIUM_OUTSIDE_SOURCE` | You can't use custom stickers outside their source communities without Premium |
| `CUSTOM_STICKER_NOT_FOUND` | Custom sticker wasn't found |
| `DISCOVERABLE_GUILD_VERIFICATION_LEVEL_TOO_LOW` | Communities listed in Explore must have a verification level of at least Low |
| `DISCRIMINATOR_INVALID_FORMAT` | Discriminator must be {min}–{max} digits |
| `DISCRIMINATOR_NOT_SUPPORTED_ON_INSTANCE` | This instance doesn't use tags |
| `DISCRIMINATOR_OUT_OF_RANGE` | Discriminator must be between {min} and {max} |
| `DUPLICATE_ATTACHMENT_IDS_NOT_ALLOWED` | Duplicate attachment IDs aren't allowed |
| `DUPLICATE_FILE_INDEX` | Duplicate file index: {index} |
| `DUPLICATE_RECIPIENTS_NOT_ALLOWED` | Duplicate recipients aren't allowed |
| `EMAIL_ALREADY_IN_USE` | Email is already in use |
| `EMAIL_DELIVERY_REQUIRES_EMAIL_IDENTITY` | Email delivery only works when people sign in with an email |
| `EMAIL_DOMAIN_NOT_ALLOWED` | This instance only accepts email addresses from certain domains |
| `EMAIL_DOMAIN_NOT_ALLOWED_FOR_SSO` | This email domain isn't allowed for single sign-on |
| `EMAIL_IS_REQUIRED` | Email is required |
| `EMAIL_LENGTH_INVALID` | Email address must be between {min} and {max} characters |
| `EMAIL_MUST_BE_CHANGED_VIA_TOKEN` | Email must be changed via token |
| `EMAIL_TOKEN_EXPIRED` | Email token expired |
| `EMBEDS_EXCEED_MAX_CHARACTERS` | Embeds must not exceed {maxCharacters} characters in total |
| `EMBED_INDEX_OUT_OF_BOUNDS` | Embed index {embedIndex} is out of bounds (message has {embedCount} embeds) |
| `EMBED_SPLASH_REQUIRES_FEATURE` | Embed splash requires INVITE_SPLASH feature |
| `ENFORCEMENT_CONFIRMATIONS_REQUIRED` | Confirm that the evidence is preserved and that any required report is filed |
| `ENFORCEMENT_EVIDENCE_DM_UNREPORTED` | Messages in direct messages can only be used as evidence when a linked report covers that conversation |
| `ENFORCEMENT_EVIDENCE_NOT_ALLOWED` | The messages being removed are saved as evidence, so leave out separate evidence messages |
| `ENFORCEMENT_EVIDENCE_NOT_FOUND` | One of the evidence messages wasn't found |
| `ENFORCEMENT_EVIDENCE_NOT_SUBJECT` | Every evidence message must be from the account or community this decision is about |
| `ENFORCEMENT_EVIDENCE_REQUIRED` | This decision needs at least one evidence message |
| `ENFORCEMENT_FACTS_REQUIRED` | Add an evidence message or a note to the user |
| `ENFORCEMENT_INTERNAL_NOTE_REQUIRED` | Add an internal note in the X-Audit-Log-Reason header |
| `ENFORCEMENT_KIND_NOT_ALLOWED` | This reason can't be used for this action |
| `ENFORCEMENT_LIMIT_END_INVALID` | Only a reports limit has an end date, and it must be within the next 365 days |
| `ENFORCEMENT_LIMIT_NOT_ALLOWED` | This reason can't be used for this limit |
| `ENFORCEMENT_NETWORK_BLOCK_NOT_ALLOWED` | Only a permanent suspension or a closure for a broken rule can block networks |
| `ENFORCEMENT_NETWORK_SHARED` | Other accounts that aren't restricted were last active on this network, so it can't be blocked |
| `ENFORCEMENT_NETWORK_UNKNOWN` | This account hasn't used this network in the last 30 days |
| `ENFORCEMENT_OUTCOME_NOT_ALLOWED` | Only a reversal can end this kind of decision, because there is nothing to lift |
| `ENFORCEMENT_PERMANENT_REQUIRED` | Suspensions for this reason must be permanent |
| `ENFORCEMENT_REPORT_MISMATCH` | Every linked report must exist and be about the account or community this decision is about |
| `ENFORCEMENT_REPORT_REQUIRED` | Link at least one report when the source is a report |
| `ENFORCEMENT_RIGHTS_HOLDER_NOT_ALLOWED` | A rights holder can only be given for copyright decisions |
| `ENFORCEMENT_RIGHTS_HOLDER_REQUIRED` | Copyright decisions need the rights holder or the right concerned |
| `ENFORCEMENT_STATEMENT_NOT_ALLOWED` | Decisions for this reason can't include a note to the user |
| `ENTRANCE_SOUND_DURATION_EXCEEDS_LIMIT` | Entrance sounds can be at most {max_ms, number}ms long |
| `ENTRANCE_SOUND_INVALID_FORMAT` | Entrance sounds must be MP3, OGG, M4A, or WAV audio |
| `ENTRANCE_SOUND_INVALID_SCOPE` | Invalid entrance sound scope |
| `ENTRANCE_SOUND_NAME_LENGTH_INVALID` | Entrance sound names must be between 1 and {max, number} characters |
| `ENTRANCE_SOUND_NOT_FOUND` | Entrance sound not found |
| `ENTRANCE_SOUND_QUOTA_REACHED` | You've reached the limit of {max, number} saved entrance sounds |
| `ENTRANCE_SOUND_SIZE_EXCEEDS_LIMIT` | Entrance sounds can be at most {max_bytes, number} bytes |
| `FAILED_TO_FETCH_SSO_USER_INFO` | We couldn't fetch your SSO user info |
| `FAILED_TO_PARSE_MULTIPART_FORM_DATA` | We couldn't parse the multipart form data |
| `FAILED_TO_PARSE_SSO_USER_INFO` | We couldn't read the response from your SSO provider |
| `FAILED_TO_UPLOAD_IMAGE` | We couldn't upload the image |
| `FAVORITE_MEME_NAME_REQUIRED` | Saved media name is required |
| `FAVORITE_MEME_NOT_FOUND` | Saved media wasn't found |
| `FILENAME_LENGTH_INVALID` | Filename must be between {min} and {max} characters |
| `FILE_INDEX_EXCEEDS_MAXIMUM` | File index {index} exceeds the maximum allowed index of {maxIndex} |
| `FILE_NOT_FOUND` | File not found |
| `FLAG_REQUIRES_ENFORCEMENT_ROUTE` | This flag is part of a restriction |
| `FORWARD_MESSAGES_CANNOT_CONTAIN_CONTENT` | Forwarded messages can't include content, embeds, attachments, or stickers |
| `FORWARD_REFERENCE_REQUIRES_CHANNEL_AND_MESSAGE` | Forward message reference must include `channel_id` and `message_id` |
| `GLOBAL_NAME_CANNOT_CONTAIN_RESERVED_TERMS` | Display name can't contain "system message" |
| `GLOBAL_NAME_LENGTH_INVALID` | Global name must be between {min} and {max} characters |
| `GLOBAL_NAME_RESERVED_VALUE` | Global name can't be "everyone" or "here" |
| `GUILD_BANNER_REQUIRES_FEATURE` | Community banner requires BANNER feature |
| `GUILD_FEATURE_NOT_TOGGLEABLE` | This feature cannot be toggled |
| `GUILD_ID_MUST_MATCH_REFERENCED_MESSAGE` | Community ID must match the community containing the channel the referenced message was fetched from |
| `GUILD_ID_REQUIRED_FOR_SEARCH_INDEX` | Community ID is required for channel message and member search indexes |
| `IDENTIFIER_FIRST_REQUIRES_EMAIL_IDENTITY` | Email-first sign-in only works when people sign in with an email |
| `IMAGE_SIZE_EXCEEDS_LIMIT` | Image size exceeds {maxSize} bytes |
| `INSTANCE_ADDRESS_REQUIRED` | This instance uses usernames |
| `INSTANT_ACCOUNTS_REQUIRE_CAPTCHA` | Turn on the captcha before allowing instant accounts |
| `INTEGER_OUT_OF_INT64_RANGE` | Integer value is out of the valid int64 range |
| `INVALID_ALLOWED_DOMAIN` | "{domain}" isn't a valid domain |
| `INVALID_AUDIT_LOG_REASON` | Invalid audit log reason |
| `INVALID_BASE64_FORMAT` | The provided value is in an invalid format |
| `INVALID_CHANNEL_ID` | Invalid channel ID: {channelId} |
| `INVALID_CODE` | Invalid code |
| `INVALID_DATE_OF_BIRTH_FORMAT` | Enter your real date of birth |
| `INVALID_EMAIL_ADDRESS` | Invalid email address format |
| `INVALID_EMAIL_FORMAT` | Invalid email address format |
| `INVALID_EMAIL_LOCAL_PART` | Invalid email address format |
| `INVALID_EMAIL_OR_PASSWORD` | Check your email and password |
| `INVALID_EMAIL_TOKEN` | Invalid email token |
| `INVALID_FILE_FIELD_NAME` | Invalid file field name: {key} |
| `INVALID_FORMAT` | The provided value is in an invalid format |
| `INVALID_IMAGE_FORMAT` | Invalid image format |
| `INVALID_INTEGER_FORMAT` | Invalid integer format |
| `INVALID_ISO_TIMESTAMP` | Must be a valid ISO timestamp |
| `INVALID_JSON_IN_PAYLOAD_JSON` | Invalid JSON in `payload_json` |
| `INVALID_LOGIN_OR_PASSWORD` | Invalid username or password |
| `INVALID_MESSAGE_DATA` | Invalid message data |
| `INVALID_MFA_CODE` | Invalid MFA code |
| `INVALID_OR_EXPIRED_AUTHORIZATION_TICKET` | Invalid or expired authorization ticket |
| `INVALID_OR_EXPIRED_AUTHORIZATION_TOKEN` | Invalid or expired authorization token |
| `INVALID_OR_EXPIRED_RESET_TOKEN` | Invalid or expired password reset token |
| `INVALID_OR_EXPIRED_REVERT_TOKEN` | Invalid or expired revert token |
| `INVALID_OR_EXPIRED_SSO_STATE` | Invalid or expired SSO state |
| `INVALID_OR_EXPIRED_TICKET` | Invalid or expired ticket |
| `INVALID_OR_EXPIRED_VERIFICATION_TOKEN` | Invalid or expired verification token |
| `INVALID_OR_RESTRICTED_RTC_REGION` | Invalid or restricted RTC region: {region} |
| `INVALID_PARENT_CHANNEL` | Invalid parent channel |
| `INVALID_PASSWORD` | Check your email and password |
| `INVALID_PROOF_TOKEN` | Invalid proof token |
| `INVALID_RECOVERY_KEY` | Invalid username or recovery key |
| `INVALID_ROLE_ID` | Invalid role ID: {roleId} |
| `INVALID_RTC_REGION` | Invalid RTC region: {region} |
| `INVALID_SNOWFLAKE` | Invalid snowflake |
| `INVALID_SNOWFLAKE_FORMAT` | Invalid snowflake |
| `INVALID_SSO_AUTHORIZATION_CODE` | Invalid SSO authorization code |
| `INVALID_SSO_TOKEN` | Invalid SSO token |
| `INVALID_TIMEOUT_VALUE` | Invalid timeout value |
| `INVALID_TIMEZONE_IDENTIFIER` | Invalid time zone identifier |
| `INVALID_TRUSTED_DOMAINS` | The wildcard (*) can't be combined with specific domains |
| `INVALID_URL_FORMAT` | Invalid URL format |
| `INVALID_URL_OR_ATTACHMENT_FORMAT` | Invalid URL format or attachment reference |
| `INVALID_VERIFICATION_CODE` | Invalid verification code |
| `INVITE_SPLASH_REQUIRES_FEATURE` | Invite splash requires INVITE_SPLASH feature |
| `MAX_FAVORITE_MEME_TAGS_EXCEEDED` | You can only add up to {limit} tags per saved media item |
| `MEDIA_ALREADY_IN_FAVORITE_MEMES` | This media is already in your saved media |
| `MESSAGES_ARRAY_REQUIRED_AND_NOT_EMPTY` | `messages` array is required and must not be empty |
| `MESSAGES_WITH_SNAPSHOTS_CANNOT_BE_EDITED` | Messages with snapshots can't be edited |
| `MESSAGE_HISTORY_CUTOFF_BEFORE_GUILD_CREATION` | Message history cutoff can't be before the community was created |
| `MESSAGE_HISTORY_CUTOFF_IN_FUTURE` | Message history cutoff can't be in the future |
| `MESSAGE_IDS_CANNOT_BE_EMPTY` | `message_ids` can't be empty |
| `MODERATION_BASIS_REQUIRED` | Give a basis to delete the messages |
| `MULTIPLE_FILES_FOR_INDEX_NOT_ALLOWED` | Multiple files for index {index} aren't allowed |
| `MUST_AGREE_TO_TOS_AND_PRIVACY_POLICY` | You need to accept the terms of service to create an account |
| `MUST_BE_MINIMUM_AGE` | You must be at least {minAge} years old to create an account |
| `MUST_ENABLE_2FA_BEFORE_REQUIRING_FOR_MODS` | You must enable 2FA on your account before requiring it for moderators |
| `MUST_HAVE_EMAIL_TO_CHANGE_IT` | You must have an email address to change it |
| `MUST_START_SESSION_BEFORE_SENDING` | You must start a session before sending messages |
| `NAME_EMPTY_AFTER_NORMALIZATION` | Name can't be empty after normalization |
| `NEW_EMAIL_MUST_BE_DIFFERENT` | New email must be different from your current email |
| `NOT_A_VALID_UNICODE_EMOJI` | Not a valid Unicode emoji |
| `NO_FILE_FOR_ATTACHMENT` | No file was uploaded for attachment with ID {attachmentId} |
| `NO_FILE_FOR_ATTACHMENT_METADATA` | No file was uploaded for attachment metadata with ID {attachmentId} |
| `NO_NEW_EMAIL_REQUESTED` | No new email was requested |
| `NO_ORIGINAL_EMAIL_ON_RECORD` | No original email is on record |
| `NO_UPLOADED_PARTS_TO_FINALIZE` | No uploaded parts are available to finalize |
| `NO_VALID_MEDIA_IN_MESSAGE` | No valid media was found in the message |
| `ORIGINAL_EMAIL_ALREADY_VERIFIED` | Original email is already verified |
| `ORIGINAL_EMAIL_MUST_BE_VERIFIED_FIRST` | Original email must be verified first |
| `ORIGINAL_VERIFICATION_NOT_REQUIRED` | Original verification isn't required for this flow |
| `PARENT_CHANNEL_NOT_IN_GUILD` | Parent channel isn't present in the community |
| `PARENT_MUST_BE_CATEGORY` | Parent must be a category |
| `PARSE_AND_USERS_OR_ROLES_CANNOT_BE_USED_TOGETHER` | You can't use `parse` together with `allowed_mentions.users` or `allowed_mentions.roles` |
| `PASSWORD_IS_TOO_COMMON` | Your password is too common |
| `PASSWORD_LENGTH_INVALID` | String length must be between {min} and {max} characters |
| `PASSWORD_NOT_SET` | Password isn't set |
| `PRECEDING_CHANNEL_MUST_SHARE_PARENT` | Preceding channel must share the same parent as the moved channel |
| `PRECEDING_CHANNEL_NOT_IN_GUILD` | Preceding channel isn't present in the community |
| `PRONOUNS_CHANGED_TOO_MANY_TIMES` | You've changed your pronouns too often recently |
| `RECIPIENT_IDS_CANNOT_BE_EMPTY` | Recipient IDs can't be empty |
| `REFERENCED_ATTACHMENT_NOT_FOUND` | Referenced attachment "{filename}" wasn't found in message attachments |
| `REPORT_OUTCOME_INVALID` | An actioned resolution needs an outcome other than no violation |
| `SEARCH_FILTER_NEEDS_MEMBERSHIP` | Join this community to search by person or mention |
| `SESSION_TIMEOUT` | Session timed out |
| `SETUP_TOKEN_INVALID` | That setup token is not correct |
| `SETUP_TOKEN_REQUIRED` | This instance is not set up yet |
| `SMTP_HOST_REQUIRED` | Enter an SMTP server before turning on email delivery |
| `SNOWFLAKE_OUT_OF_RANGE` | Invalid snowflake |
| `SSO_ACCOUNT_LINKED_TO_ANOTHER_IDENTITY` | This account is already linked to a different SSO identity |
| `SSO_EMAIL_NOT_VERIFIED` | Your SSO provider has not verified your email address |
| `SSO_IDENTITY_LINKED_TO_ANOTHER_ACCOUNT` | This SSO identity is already linked to a different account |
| `SSO_IDENTITY_MISMATCH` | SSO identity mismatch between id_token and userinfo |
| `SSO_MISCONFIGURED` | SSO is misconfigured |
| `SSO_PROVIDER_DID_NOT_RETURN_EMAIL` | Your SSO provider didn't return an email address |
| `SSO_TEST_CODE_MISSING_EMAIL` | SSO test code is missing the email payload |
| `SSO_UNABLE_TO_ALLOCATE_DISCRIMINATOR` | We couldn't allocate a discriminator for your SSO account |
| `STRING_LENGTH_EXACT` | String must be exactly {length} characters |
| `STRING_LENGTH_INVALID` | String length must be between {min} and {max} characters |
| `SYSTEM_CHANNEL_MUST_BE_IN_GUILD` | System channel must be in this community |
| `SYSTEM_CHANNEL_MUST_BE_TEXT` | System channel must be a text channel |
| `TAG_ALREADY_TAKEN` | This tag is already taken |
| `TAG_STYLE_REQUIRES_EMAIL_SIGN_IN` | Tags are only available when people sign in with email |
| `THIS_VANITY_URL_IS_ALREADY_TAKEN` | This vanity URL is already taken |
| `TICKET_ALREADY_COMPLETED` | This ticket has already been completed |
| `TIMEOUT_CANNOT_EXCEED_365_DAYS` | Timeout can't be longer than {maxDays} days from now |
| `TOO_LARGE` | Synced preferences exceed {max_bytes} bytes |
| `TOO_MANY_EMBEDS` | Too many embeds |
| `TOO_MANY_FILES` | Too many files |
| `TOO_MANY_USERS_WITH_THIS_USERNAME` | There are too many users with this username |
| `TOO_MANY_USERS_WITH_USERNAME_TRY_DIFFERENT` | There are too many users with this username |
| `TOTP_NOT_ENABLED` | Two-factor authentication using an authenticator app isn't enabled for this account |
| `UNCLAIMED_ACCOUNTS_CAN_ONLY_SET_EMAIL_VIA_TOKEN` | Claim your account before changing your profile |
| `UNRESOLVED_ATTACHMENT_URL` | Unresolved `attachment://` URL detected |
| `UPLOADED_ATTACHMENT_NOT_FOUND` | Uploaded attachment {filename} wasn't found |
| `URL_LENGTH_INVALID` | URL must be between {min} and {max} characters |
| `URL_MUST_USE_HTTPS` | URL must start with https:// |
| `URL_NOT_PUBLICLY_ROUTABLE` | URL must resolve to a publicly routable address |
| `USERNAME_ALREADY_TAKEN` | This username is already taken |
| `USERNAME_CANNOT_CONTAIN_RESERVED_TERMS` | Username can't contain "fluxer" or "system message" |
| `USERNAME_CHANGED_TOO_MANY_TIMES` | You've changed your username too often recently |
| `USERNAME_INVALID_CHARACTERS` | Username can only contain Latin letters (a-z, A-Z), numbers (0-9), and underscores (_) |
| `USERNAME_LENGTH_INVALID` | Username must be between {min} and {max} characters |
| `USERNAME_RESERVED_VALUE` | Username can't be "everyone" or "here" |
| `USER_DOES_NOT_HAVE_AN_EMAIL_ADDRESS` | This user doesn't have an email address |
| `USER_IS_NOT_BANNED` | This user isn't banned |
| `USER_MUST_BE_A_BOT_TO_BE_MARKED_AS_A_SYSTEM_USER` | User must be a bot to be marked as a system user |
| `USER_NOT_IN_CHANNEL` | This user isn't in the channel |
| `VALUE_MUST_BE_INTEGER_IN_RANGE` | The value for `{name}` must be an integer between {minValue} and {maxValue} |
| `VANITY_URL_CODE_ALREADY_TAKEN` | Vanity URL code is already taken |
| `VANITY_URL_CODE_CANNOT_CONTAIN_FLUXER` | Vanity URL code can't contain "fluxer" |
| `VANITY_URL_CODE_LENGTH_INVALID` | Vanity URL code must be between {min} and {max} characters |
| `VANITY_URL_INVALID_CHARACTERS` | Vanity URL can only contain lowercase letters (a-z), digits (0-9), and hyphens (-) |
| `VANITY_URL_REQUIRES_FEATURE` | Vanity URL requires VANITY_URL feature |
| `VERIFICATION_BASIS_REQUIRES_EMAIL_DELIVERY` | Accounts can only be verified by a confirmed email while email delivery is on |
| `VERIFICATION_CODE_EXPIRED` | Verification code has expired |
| `VERIFICATION_CODE_NOT_ISSUED` | No verification code has been issued |
| `VISIONARY_REQUIRED_FOR_DISCRIMINATOR` | You must be on the Visionary lifetime plan to use that discriminator |
| `VOICE_ACTIVITY_SHARING_ON_COOLDOWN` | You can only update your voice activity sharing default once every 24 hours |
| `VOICE_CHANNELS_CANNOT_BE_ABOVE_TEXT_CHANNELS` | Voice channels can't be positioned above text channels within the same category |
| `VOICE_MESSAGES_ATTACHMENT_DURATION_REQUIRED` | Voice message attachments must specify a duration |
| `VOICE_MESSAGES_ATTACHMENT_MUST_BE_AUDIO` | Voice message attachments must be audio files |
| `VOICE_MESSAGES_ATTACHMENT_WAVEFORM_REQUIRED` | Voice message attachments must include waveform data |
| `VOICE_MESSAGES_CANNOT_HAVE_CONTENT` | Voice messages can't have text content |
| `VOICE_MESSAGES_CANNOT_HAVE_EMBEDS` | Voice messages can't have embeds |
| `VOICE_MESSAGES_CANNOT_HAVE_FAVORITE_MEMES` | Voice messages can't have saved media |
| `VOICE_MESSAGES_CANNOT_HAVE_STICKERS` | Voice messages can't have stickers |
| `VOICE_MESSAGES_DURATION_EXCEEDS_LIMIT` | Voice message duration can't exceed {maxDuration} seconds |
| `VOICE_MESSAGES_REQUIRE_SINGLE_ATTACHMENT` | Voice messages must contain exactly one attachment |
| `WEBHOOK_NAME_LENGTH_INVALID` | Webhook name must be between {min} and {max} characters |
