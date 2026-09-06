---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Errors
description: The error envelope, the status fallback mapping, and both code registries.
---

A failed request returns a JSON [error response](/http-api/#error-response). It has a stable `code` a client matches on and a localised `message` for a person to read. Every HTTP API and Admin API failure uses this shape. An OAuth2 protocol failure is the one exception.

:::caution[Match on `code` alone]
The `message` field is rendered from a template, is translated per request, and can change wording without notice. An HTTP status is not enough on its own, because many distinct codes share one status.
:::

## Selecting the error code

An error response always has `code` and `message`. A failure with more to report adds its own members beside those two. Every added member sits at the top level of the response. A rate limit denial adds `global` and `retry_after`, a missing scope adds `required_scope`, and a validation failure adds `errors`.

:::note[OAuth2 endpoints use a different envelope]
An OAuth2 protocol failure raised by the [OAuth2 resource](/http-api/oauth2/) answers with the RFC 6749 shape. That body is `error` and `error_description` and nothing else, so its values appear in no registry here.
:::

## Supplementary members

The members a failure adds are fixed by its code, so a client reads only the members documented for the code it matched. `errors` reports field violations. `retry_after` states the delay before another attempt is admitted, `global` distinguishes a global rate limit denial from a route one, `required_scope` names a missing OAuth2 scope, and `has_mfa` and `methods` state the [sudo mode](/http-api/users/mfa/#sudo-mode) proofs an account can supply.

The two IP ban codes have their own members. `GLOBAL_IP_BANNED` and `GLOBAL_IP_TEMPORARILY_BANNED` both have `ip_address` with the normalised client address, `appeal_email` with the address an appeal is sent to, `appeals_supported` which is `true` only for the permanent ban, and `ban_kind` which is `permanent` or `temporary_24h`. `expires_at` is an ISO 8601 timestamp when the ban records an expiry and `null` otherwise, including on every permanent ban.

## Validation failure codes

Fluxer answers a field-level failure with 400 and a top-level `errors` array. Each element identifies one failed input field. The [validation error object](/http-api/#validation-error-object) documents the element shape.

A boundary schema validates one of four request targets: the JSON body, the form body, the query string, and the path parameters. A failure on any of them returns the top-level code `INVALID_FORM_BODY`, and each element has a `code` drawn from the [validation error code registry](#validation-error-code-registry) together with a localised `message`.

An operation can also report against a named field without the boundary schema. That failure returns `INVALID_FORM_BODY` as well. Its element has an enumerated `code` and localised `message` when the failure declares a registry code. Otherwise it has a fixed English `message` written at the failure site and no `code`. Every element `code` a client observes is a registry value.

[Modify current user settings](/http-api/users/settings/#modify-current-user-settings) is the one operation that answers such a failure with the top-level code `VALIDATION_ERROR` instead of `INVALID_FORM_BODY`. Its trusted domain, age restriction, and synced preferences decisions each report one element with a registry `code` and a fixed English `message`.

The `path` of an element is the dot-joined position of the failed value, so a nested field appears as `embeds.0.title`. A failure with no field position uses the literal path `root`. Fluxer produces at most one element for each distinct pair of `path` and `code`.

### Example

```json
{
  "code": "INVALID_FORM_BODY",
  "message": "Invalid form body.",
  "errors": [
    {
      "path": "embeds.0.title",
      "code": "CONTENT_EXCEEDS_MAX_LENGTH",
      "message": "Text must not exceed 256 characters."
    }
  ]
}
```

:::note[An empty body reports the missing fields]
An empty or whitespace-only body becomes `{}`, so the response reports the fields the schema then finds missing. A body that does not parse as JSON returns 400 `INVALID_FORM_BODY` with one element at path `body` and code `INVALID_FORMAT`.
:::

Fluxer normalises empty values on all four targets before validation runs. An empty string becomes `null` wherever it appears, including inside an array element. A nested object becomes `null` when it holds no members, and it becomes `null` when every one of its members is `null` after Fluxer has applied the same rule to each of them. The top-level object itself is never replaced, so a request that sends nothing still reaches the schema as an object and fails on the fields the schema requires.

:::caution[An enumerated validation failure answers 400 alone]
A validation failure whose elements have enumerated codes answers 400 with its elements in `errors`, and its top-level code is `INVALID_FORM_BODY` everywhere except [Modify current user settings](/http-api/users/settings/#modify-current-user-settings). Any other status, retry guidance, or response header from the original failure is dropped.
:::

### Default schema failure codes

A boundary schema constraint can name its own [validation code](#validation-error-code-registry). When it names none, Fluxer maps the failure to one of the codes below by the kind of constraint that failed.

| Constraint | Code | Description |
| --- | --- | --- |
| String longer than its maximum | CONTENT_EXCEEDS_MAX_LENGTH<sup>1</sup> | A string value exceeded the maximum length the schema declares |
| Email format | INVALID_EMAIL_ADDRESS | A value declared as an email address did not parse as one |
| UUID format | INVALID_SNOWFLAKE | A value declared as a UUID did not parse as one |
| Any other constraint | INVALID_FORMAT<sup>2</sup> | Every constraint kind not listed above |

<sup>1</sup> A string shorter than its minimum reports `INVALID_FORMAT`, and so does a length bound on anything other than a string

<sup>2</sup> Covers a type, union, enumerated value, unknown key, minimum length, numeric or date bound, multiple, key, element, or custom constraint, a constraint kind the mapping does not recognise, and a constraint that names a code outside the registry

## HTTP status fallback codes

A failure that has an HTTP status and no recognised Fluxer error code takes the fallback `code` its status selects below. An unclassified 401, 413, 422, or 429 falls back to `GENERAL_ERROR`. An error named by an operation or by one of the registries below keeps that more specific code.

An unrecognised failure returns 500 with `INTERNAL_SERVER_ERROR` and a generic message that names no detail. A failure that has a registered API code but no status returns 400, except `GENERAL_ERROR`, which returns 500.

| Status | Code | Description |
| --- | --- | --- |
| 400 | BAD_REQUEST | The request was malformed or failed a precondition |
| 403 | FORBIDDEN | The caller lacks permission for the operation |
| 404 | NOT_FOUND<sup>1</sup> | The target resource does not exist or is not visible |
| 405 | METHOD_NOT_ALLOWED | The HTTP method is not accepted on the route |
| 409 | CONFLICT | The request conflicts with the current state of the resource |
| 410 | GONE | The resource is permanently unavailable |
| 500 | INTERNAL_SERVER_ERROR<sup>2</sup> | An unexpected internal failure occurred |
| 501 | NOT_IMPLEMENTED | The requested capability is not implemented |
| 502 | BAD_GATEWAY | An upstream dependency returned an invalid response |
| 503 | SERVICE_UNAVAILABLE | The service is temporarily unable to handle the request |
| 504 | GATEWAY_TIMEOUT | An upstream dependency did not respond in time |
| default<sup>3</sup> | GENERAL_ERROR | Any status without a specific mapping |

<sup>1</sup> Also the code a request that matches no route receives, so an unrouted path and an unreadable resource are indistinguishable from the body alone

<sup>2</sup> Also the code an unrecognised failure receives, with the generic localised message

<sup>3</sup> A failure that lands on this row keeps its own status

## Client errors as an abuse signal

Every `4xx` response to a request that resolved no authenticated user contributes a weighted abuse signal keyed by the client IP identity. A 429 weighs 3, a 401 weighs 0.75, a 403 weighs 0.5, and every other 4xx weighs 0.25. Fluxer records a signal only when the client address is public and not exempt. An IPv4 client is keyed by its exact address and an IPv6 client by its `/64`. One request records at most one client error signal. A denial produced by an existing IP ban records no signal.

Fluxer records a second signal of weight 1 for a credential that fails to resolve. A request that presents an unrecognised token and is answered 401 contributes both. That signal also records a hash of the credential presented, and Fluxer tracks the number of distinct hashes seen for one IP identity beside the score.

The score and the credential hashes accumulate inside a fixed window, and the window restarts once it elapses. Two triggers fire an automatic ban. The credential trigger fires the first time the count of distinct rejected credentials reaches its threshold. The score trigger fires only after the score has crossed its threshold in three separate windows, which is the default. Both thresholds depend on the address classification, which is datacentre, anonymising, mobile, or residential. An unclassified address takes the residential thresholds. Fluxer never bans a mobile address automatically.

:::caution[An automatic ban answers every request for 24 hours]
The window length, both thresholds, and the number of windows the score trigger requires are instance configuration. A tripped ban lasts 24 hours by default. While it holds, Fluxer answers every request from that identity with 403 and the code `GLOBAL_IP_TEMPORARILY_BANNED`.
:::

## API error code registry

These codes appear in the top-level `code` field of an error response, transmitted as the exact JSON string shown. The registry is closed and holds exactly 257 codes. Each entry states the leading sentence of the English source message, without its final full stop. Those messages call a [guild](/http-api/guilds/) a community.

:::note[The rendered `message` fills in the braced values]
A description containing a value in braces is an ICU MessageFormat template. `You've reached the maximum of {count, plural, one {# emoji} other {# emojis}}` renders as a complete sentence with the applicable limit.
:::

:::note[A rendered `message` can run longer than the registry entry]
Several source messages append a further recovery sentence with a template value such as a maximum size, a format list, or a retry delay. A client branches on `code` and reads the members the operation documents.
:::

### `ACCESS_DENIED`

You don't have access to this resource or feature

### `ACCOUNT_SUSPENDED_PERMANENTLY`

This account has been permanently suspended

### `ACCOUNT_SUSPENDED_TEMPORARILY`

This account has been temporarily suspended

### `ACCOUNT_SUSPICIOUS_ACTIVITY`

Your account is locked due to suspicious activity

### `ACCOUNT_TOO_NEW_FOR_GUILD`

Your account is too new to send messages in this community

### `ACLS_MUST_BE_NON_EMPTY`

ACLs must be non-empty

### `ADMIN_API_KEY_NOT_FOUND`

Admin API key wasn't found

### `AGE_VERIFICATION_ALREADY_VERIFIED`

You've already completed age verification

### `ALREADY_FRIENDS`

You're already friends with this user

### `APPLICATION_NOT_OWNED`

You don't own this application

### `BAD_GATEWAY`

Bad gateway

### `BAD_REQUEST`

Bad request

### `BLUESKY_OAUTH_AUTHORIZATION_FAILED`

We couldn't resolve that Bluesky handle

### `BLUESKY_OAUTH_CALLBACK_FAILED`

We couldn't complete the Bluesky connection

### `BLUESKY_OAUTH_NOT_ENABLED`

Bluesky connections are not enabled on this instance

### `BLUESKY_OAUTH_STATE_INVALID`

The authorization request has expired or is invalid

### `BOTS_CANNOT_CREATE_GUILDS`

Bots can't create communities

### `BOTS_CANNOT_SEND_FRIEND_REQUESTS`

Bots can't send friend requests

### `BOT_ALREADY_IN_GUILD`

This bot is already in the community

### `BOT_IS_PRIVATE`

This bot is private

### `BOT_USER_AUTH_ENDPOINT_ACCESS_DENIED`

Bot users can't use auth endpoints

### `BOT_USER_AUTH_SESSION_CREATION_DENIED`

Bot users can't create auth sessions

### `BOT_USER_GENERATION_FAILED`

Bot user generation failed

### `BOT_USER_NOT_FOUND`

Bot user wasn't found

### `CALL_ALREADY_EXISTS`

A call is already in progress in this channel

### `CANNOT_BLOCK_SYSTEM_USER`

You can't block the system user

### `CANNOT_EDIT_OTHER_USER_MESSAGE`

You can't edit another user's message

### `CANNOT_EXECUTE_ON_DM`

This action can't be performed in a DM

### `CANNOT_MODIFY_SYSTEM_WEBHOOK`

You can't edit system messages

### `CANNOT_REDEEM_PLUTONIUM_WITH_VISIONARY`

You can't redeem {premium_tier_name} with Visionary

### `CANNOT_REPORT_GUILD`

You can't report this community

### `CANNOT_REPORT_OWN_GUILD`

You can't report your own community

### `CANNOT_REPORT_OWN_MESSAGE`

You can't report your own message

### `CANNOT_REPORT_YOURSELF`

You can't report yourself

### `CANNOT_SEND_EMPTY_MESSAGE`

You can't send an empty message

### `CANNOT_SEND_FRIEND_REQUEST_TO_BLOCKED_USER`

You can't send a friend request to a blocked user

### `CANNOT_SEND_FRIEND_REQUEST_TO_SELF`

You can't send a friend request to yourself

### `CANNOT_SEND_MESSAGES_IN_NON_TEXT_CHANNEL`

You can't send messages in a non-text channel

### `CANNOT_SEND_MESSAGES_TO_USER`

You can't send messages to this user

### `CANNOT_SHRINK_RESERVED_SLOTS`

You can't shrink reserved slots

### `CANNOT_TRANSFER_OWNERSHIP_TO_BOT`

Community ownership can't be transferred to a bot

### `CAPTCHA_REQUIRED`

Captcha is required

### `COMMUNICATION_DISABLED`

Communication is disabled

### `CONFLICT`

Conflict

### `CONNECTION_ALREADY_EXISTS`

A connection of this type with this identifier already exists

### `CONNECTION_INITIATION_TOKEN_INVALID`

The connection initiation token is invalid or has expired

### `CONNECTION_INVALID_TYPE`

The connection type is not supported

### `CONNECTION_LIMIT_REACHED`

You've reached the maximum number of connections ({limit})

### `CONNECTION_NOT_FOUND`

Unknown connection

### `CONNECTION_VERIFICATION_FAILED`

Connection verification failed

### `CONTENT_BLOCKED`

This content was blocked by safety systems

### `DELETION_FAILED`

We couldn't delete the resource

### `DIRECT_MESSAGES_DISABLED`

Direct messages and friend requests are disabled on this instance

### `DIRECT_MESSAGE_EMAIL_VERIFICATION_REQUIRED`

Email verification is required for this action

### `DISCOVERY_ALREADY_APPLIED`

This community has already applied for discovery

### `DISCOVERY_APPLICATION_ALREADY_REVIEWED`

This discovery application has already been reviewed

### `DISCOVERY_APPLICATION_NOT_FOUND`

Discovery application wasn't found

### `DISCOVERY_DISABLED`

Discovery isn't available on this instance

### `DISCOVERY_INSUFFICIENT_MEMBERS`

This community doesn't meet the minimum member count for discovery

### `DISCOVERY_NOT_DISCOVERABLE`

This community isn't listed in discovery

### `DISCRIMINATOR_REQUIRED`

Discriminator is required

### `DONATION_AMOUNT_INVALID`

Donation amount is invalid

### `DONATION_MAGIC_LINK_EXPIRED`

Magic link has expired

### `DONATION_MAGIC_LINK_INVALID`

Magic link is invalid

### `DONATION_MAGIC_LINK_USED`

Magic link has already been used

### `EMAIL_SERVICE_NOT_TESTABLE`

Email service is temporarily unavailable

### `EMAIL_VERIFICATION_REQUIRED`

Email verification is required for this action

### `EXPLICIT_CONTENT_CANNOT_BE_SENT`

Explicit content can't be sent

### `FEATURE_NOT_AVAILABLE_SELF_HOSTED`

This feature isn't available on self-hosted instances

### `FEATURE_TEMPORARILY_DISABLED`

This feature is temporarily disabled

### `FILE_SIZE_TOO_LARGE`

File size is too large

### `FORBIDDEN`

Forbidden

### `FRIEND_REQUEST_BLOCKED`

User does not accept friend requests at this time

### `FRIEND_REQUEST_EMAIL_VERIFICATION_REQUIRED`

Email verification is required for this action

### `GATEWAY_TIMEOUT`

Gateway timeout

### `GENERAL_ERROR`

Something went wrong

### `GIFT_CODE_ALREADY_REDEEMED`

Gift code is already redeemed

### `GLOBAL_IP_BANNED`

Your IP address {ipAddress} has been permanently blocked from the Fluxer API by platform administrators

### `GLOBAL_IP_TEMPORARILY_BANNED`

Your IP address {ipAddress} has been temporarily blocked from the Fluxer API for 24 hours because of abusive or unusual access patterns

### `GONE`

Gone

### `GROUP_DM_RECIPIENTS_NOT_ADDABLE`

One or more selected users can't be added to this group DM

### `GUILD_CREATION_EMAIL_VERIFICATION_REQUIRED`

Email verification is required for this action

### `GUILD_EMAIL_VERIFICATION_REQUIRED`

Email verification is required for this action

### `GUILD_PHONE_VERIFICATION_REQUIRED`

You need to add a phone number to send messages in this community

### `GUILD_TEMPLATE_INVALID`

The community template data is invalid or malformed

### `GUILD_VERIFICATION_REQUIRED`

Community verification is required

### `HANDOFF_CODE_EXPIRED`

Handoff code expired

### `HARVEST_EXPIRED`

Your data export request has expired

### `HARVEST_FAILED`

Your data export request failed

### `HARVEST_NOT_READY`

Your data export is still being prepared

### `INSTANCE_POLICY_TRANSITION_NOT_ALLOWED`

This instance policy change is not allowed

### `INTERNAL_SERVER_ERROR`

Internal server error

### `INVALID_ACLS_FORMAT`

Invalid ACLs format

### `INVALID_API_ORIGIN`

Invalid API origin

### `INVALID_AUTH_TOKEN`

Invalid or expired authorization token

### `INVALID_BOT_FLAG`

Invalid bot flag

### `INVALID_CAPTCHA`

Invalid captcha

### `INVALID_CHANNEL_TYPE`

Invalid channel type

### `INVALID_CHANNEL_TYPE_FOR_CALL`

Calls can only be made in direct messages or group DMs

### `INVALID_DSA_REPORT_TARGET`

Invalid DSA report target

### `INVALID_DSA_TICKET`

Invalid DSA ticket

### `INVALID_DSA_VERIFICATION_CODE`

Invalid DSA verification code

### `INVALID_FLAGS_FORMAT`

Invalid flags format

### `INVALID_FORM_BODY`

Invalid form body

### `INVALID_HANDOFF_CODE`

Invalid handoff code

### `INVALID_PERMISSIONS_INTEGER`

Permissions must be a valid integer

### `INVALID_PERMISSIONS_NEGATIVE`

Permissions must be non-negative

### `INVALID_PHONE_NUMBER`

Invalid phone number

### `INVALID_PHONE_VERIFICATION_CODE`

Invalid phone verification code

### `INVALID_REQUEST`

Invalid request

### `INVALID_STREAM_KEY_FORMAT`

Invalid stream key format

### `INVALID_STREAM_THUMBNAIL_PAYLOAD`

Invalid stream thumbnail payload

### `INVALID_SUSPICIOUS_FLAGS_FORMAT`

Invalid suspicious flags format

### `INVALID_SYSTEM_FLAG`

Invalid system flag

### `INVALID_TIMESTAMP`

Invalid timestamp

### `INVALID_TOKEN`

Invalid or expired authorization token

### `INVALID_WEBAUTHN_AUTHENTICATION_COUNTER`

Invalid WebAuthn authentication counter

### `INVALID_WEBAUTHN_CREDENTIAL`

Failed to verify WebAuthn credential

### `INVALID_WEBAUTHN_CREDENTIAL_COUNTER`

Invalid WebAuthn credential counter

### `INVALID_WEBAUTHN_PUBLIC_KEY_FORMAT`

Invalid WebAuthn public key format

### `INVITES_DISABLED`

Invites are disabled

### `IP_AUTHORIZATION_REQUIRED`

IP authorization is required

### `IP_AUTHORIZATION_RESEND_COOLDOWN`

IP authorization resend is on cooldown

### `IP_AUTHORIZATION_RESEND_LIMIT_EXCEEDED`

IP authorization resend limit exceeded

### `IP_BAN_DECLINED`

This IP address cannot be added to the blocklist

### `MAX_APPLICATIONS`

You've reached the maximum of {limit, plural, one {# application} other {# applications}}

### `MAX_BOOKMARKS`

You've reached the maximum of {count, plural, one {# bookmark} other {# bookmarks}}

### `MAX_CATEGORY_CHANNELS`

You've reached the maximum of {count, plural, one {# category channel} other {# category channels}}

### `MAX_EMOJIS`

You've reached the maximum of {count, plural, one {# emoji} other {# emojis}}

### `MAX_FAVORITE_MEMES`

You've reached the maximum of {count, plural, one {# favorite meme} other {# favorite memes}}

### `MAX_FRIENDS`

You've reached the maximum of {count, plural, one {# friend} other {# friends}}

### `MAX_GROUP_DMS`

You've reached the maximum of {count, plural, one {# group DM} other {# group DMs}}

### `MAX_GROUP_DM_RECIPIENTS`

You've reached the maximum of {count, plural, one {# group DM recipient} other {# group DM recipients}}

### `MAX_GUILDS`

You've reached the maximum of {count, plural, one {# community} other {# communities}}

### `MAX_GUILD_CHANNELS`

You've reached the maximum of {count, plural, one {# community channel} other {# community channels}}

### `MAX_GUILD_MEMBERS`

You've reached the maximum of {count, plural, one {# community member} other {# community members}}

### `MAX_GUILD_ROLES`

You've reached the maximum of {count, plural, one {# community role} other {# community roles}}

### `MAX_INVITES`

You've reached the maximum of {count, plural, one {# invite} other {# invites}}

### `MAX_REACTIONS`

You've reached the maximum of {count, plural, one {# reaction} other {# reactions}}

### `MAX_STICKERS`

You've reached the maximum of {count, plural, one {# sticker} other {# stickers}}

### `MAX_WEBHOOKS_PER_CHANNEL`

You've reached the maximum of {count, plural, one {# webhook} other {# webhooks}} per channel

### `MAX_WEBHOOKS_PER_GUILD`

You've reached the maximum of {count, plural, one {# webhook} other {# webhooks}} per community

### `MEDIA_METADATA_ERROR`

Media metadata error

### `METHOD_NOT_ALLOWED`

Method not allowed

### `MFA_EMAIL_VERIFICATION_REQUIRED`

Email verification is required for this action

### `MISSING_ACCESS`

You don't have access to this resource or feature

### `MISSING_ACL`

Missing ACL

### `MISSING_AUTHORIZATION`

Missing or invalid authorization header

### `MISSING_OAUTH_SCOPE`

The requested scope isn't supported or you don't have permission to request it

### `MISSING_PERMISSIONS`

You don't have the permissions required to perform this action

### `NCMEC_ALREADY_SUBMITTED`

This content has already been submitted to NCMEC

### `NCMEC_SUBMISSION_FAILED`

We couldn't submit the report to NCMEC

### `NOT_A_BOT_APPLICATION`

This application isn't a bot

### `NOT_FOUND`

Not found

### `NOT_FRIENDS_WITH_USER`

You're not friends with this user

### `NOT_IMPLEMENTED`

Not implemented

### `NO_ACTIVE_CALL`

No active call

### `NO_ACTIVE_SUBSCRIPTION`

No active subscription

### `NO_PASSKEYS_REGISTERED`

No passkeys are registered

### `NO_PENDING_DELETION`

Invalid request

### `NO_USERS_WITH_FLUXERTAG_EXIST`

There are too many users with this username

### `NSFW_CONTENT_AGE_RESTRICTED`

NSFW content is age restricted

### `PASSKEY_AUTHENTICATION_FAILED`

Passkey authentication failed

### `PHONE_ADD_NOT_ELIGIBLE`

You are not eligible to add a phone number to your account

### `PHONE_ALREADY_USED`

Phone number is already in use

### `PHONE_COUNTRY_NOT_SUPPORTED`

We don't send verification texts to this country. Use a mobile number from another country, or email support@fluxer.app and a person will review your account

### `PHONE_GATE_ESCAPE_UNAVAILABLE`

This account cannot postpone the phone verification check

### `PHONE_INBOUND_VERIFICATION_REQUIRED`

This number is verified by texting us instead of us texting you. Start phone verification again to get the code and the number to text

### `PHONE_LOOKUP_UNAVAILABLE`

Our phone number check is down right now, so we stopped before sending your code. This is on us, not your number. Wait a few minutes and try the same number again

### `PHONE_NUMBER_NOT_IN_SERVICE`

Your carrier says this number isn't in service. Check the number and try again, or email support@fluxer.app if it's correct

### `PHONE_NUMBER_NOT_MOBILE`

This isn't a mobile number, so it can't receive our text. Use a mobile number, or email support@fluxer.app if you think that's wrong

### `PHONE_RATE_LIMIT_EXCEEDED`

Phone rate limit exceeded

### `PHONE_VERIFICATION_NEEDS_REVIEW`

We couldn't verify this number automatically. Email support@fluxer.app and a person will review your account

### `PHONE_VERIFICATION_REQUIRED`

Phone verification is required

### `PREMIUM_PURCHASE_BLOCKED`

No active subscription

### `PREVIEW_MUST_BE_JPEG`

Preview must be JPEG

### `PROCESSING_FAILED`

We couldn't process the request

### `PROFILE_EMAIL_VERIFICATION_REQUIRED`

Email verification is required for this action

### `PURCHASE_EMAIL_VERIFICATION_REQUIRED`

Email verification is required for this action

### `RATE_LIMITED`

You're being rate limited

### `REACTION_EMAIL_VERIFICATION_REQUIRED`

Email verification is required for this action

### `REGISTRATION_CLOSED`

Registration is closed on this instance

### `REGISTRATION_PENDING_APPROVAL`

This registration is waiting for admin approval

### `REGISTRATION_REJECTED`

This registration request was rejected

### `REGISTRATION_URL_INVALID`

This registration link is invalid or has expired

### `REPORT_ALREADY_RESOLVED`

Report already resolved

### `REPORT_BANNED`

You've been banned from submitting reports

### `REPORT_EMAIL_VERIFICATION_REQUIRED`

Email verification is required for this action

### `RESOURCE_LOCKED`

This resource is being modified

### `RESPONSE_VALIDATION_ERROR`

Response validation failed: {errors}

### `SERVICE_UNAVAILABLE`

Service unavailable

### `SESSION_TOKEN_MISMATCH`

Session token mismatch

### `SINGLE_COMMUNITY_CANNOT_CREATE_GUILDS`

This instance is a single community, so additional communities cannot be created

### `SINGLE_COMMUNITY_CANNOT_DELETE`

The community for this instance cannot be deleted

### `SINGLE_COMMUNITY_CANNOT_LEAVE`

You cannot leave the community for this instance

### `SLOWMODE_RATE_LIMITED`

Slowmode rate limited

### `SMS_VERIFICATION_UNAVAILABLE`

Service unavailable

### `SSO_REQUIRED`

Invalid request

### `STREAM_KEY_CHANNEL_MISMATCH`

Stream key channel mismatch

### `STREAM_KEY_SCOPE_MISMATCH`

Stream key scope mismatch

### `STRIPE_ERROR`

Payment processing encountered an error

### `STRIPE_GIFT_REDEMPTION_IN_PROGRESS`

Gift code redemption is in progress

### `STRIPE_INVALID_PRODUCT`

Invalid product selection

### `STRIPE_INVALID_PRODUCT_CONFIGURATION`

Invalid product configuration

### `STRIPE_NO_ACTIVE_SUBSCRIPTION`

No active subscription found

### `STRIPE_NO_PURCHASE_HISTORY`

No purchase history found

### `STRIPE_NO_SUBSCRIPTION`

No subscription found

### `STRIPE_PAYMENT_NOT_AVAILABLE`

Payment processing is temporarily unavailable

### `STRIPE_REFUND_COOLDOWN_ACTIVE`

You can self-serve refund once every 30 days

### `STRIPE_REFUND_OUTSIDE_WINDOW`

This purchase is outside the 3-day self-serve refund window

### `STRIPE_SUBSCRIPTION_ALREADY_CANCELING`

Subscription is already set to cancel at period end

### `STRIPE_SUBSCRIPTION_NOT_CANCELING`

Subscription isn't set to cancel

### `STRIPE_WEBHOOK_NOT_AVAILABLE`

Webhook processing isn't available

### `STRIPE_WEBHOOK_SIGNATURE_INVALID`

Stripe webhook signature is invalid

### `STRIPE_WEBHOOK_SIGNATURE_MISSING`

Stripe webhook signature is missing

### `SUDO_MODE_REQUIRED`

Sudo mode is required

### `TAG_ALREADY_TAKEN`

This tag is already taken

### `TEMPORARY_INVITE_REQUIRES_PRESENCE`

Temporary invite requires presence

### `TEST_HARNESS_DISABLED`

Test harness is disabled

### `TEST_HARNESS_FORBIDDEN`

Test harness is forbidden

### `TWO_FACTOR_REQUIRED`

Two-factor authentication is required

### `TWO_FA_NOT_ENABLED`

Two-factor authentication isn't enabled

### `UNAUTHORIZED`

Unauthorized

### `UNCLAIMED_ACCOUNT_CANNOT_ACCEPT_FRIEND_REQUESTS`

You're not friends with this user

### `UNCLAIMED_ACCOUNT_CANNOT_ADD_REACTIONS`

Invalid request

### `UNCLAIMED_ACCOUNT_CANNOT_CREATE_APPLICATIONS`

Invalid request

### `UNCLAIMED_ACCOUNT_CANNOT_CREATE_GUILDS`

You need to complete your account setup before you can create communities

### `UNCLAIMED_ACCOUNT_CANNOT_JOIN_GROUP_DMS`

You can't add yourself to a group DM

### `UNCLAIMED_ACCOUNT_CANNOT_JOIN_ONE_ON_ONE_VOICE_CALLS`

Invalid request

### `UNCLAIMED_ACCOUNT_CANNOT_JOIN_VOICE_CHANNELS`

Invalid request

### `UNCLAIMED_ACCOUNT_CANNOT_MAKE_PURCHASES`

Invalid request

### `UNCLAIMED_ACCOUNT_CANNOT_SEND_DIRECT_MESSAGES`

You can't send messages to this user

### `UNCLAIMED_ACCOUNT_CANNOT_SEND_FRIEND_REQUESTS`

Bots can't send friend requests

### `UNCLAIMED_ACCOUNT_CANNOT_SEND_MESSAGES`

Invalid request

### `UNCLAIMED_ACCOUNT_CANNOT_SUBMIT_REPORTS`

You need to complete your account setup before you can submit reports

### `UNKNOWN_APPLICATION`

Unknown application

### `UNKNOWN_CHANNEL`

Channel wasn't found

### `UNKNOWN_EMOJI`

Unknown emoji

### `UNKNOWN_FAVORITE_MEME`

Unknown favorite meme

### `UNKNOWN_GIFT_CODE`

Unknown gift code

### `UNKNOWN_GUILD`

Community wasn't found

### `UNKNOWN_HARVEST`

Unknown harvest

### `UNKNOWN_INVITE`

Invite wasn't found or is no longer valid

### `UNKNOWN_MEMBER`

Member wasn't found in this community

### `UNKNOWN_MESSAGE`

Message wasn't found

### `UNKNOWN_REPORT`

Unknown report

### `UNKNOWN_ROLE`

Role wasn't found

### `UNKNOWN_STICKER`

Unknown sticker

### `UNKNOWN_SUSPICIOUS_FLAG`

Unknown suspicious flag

### `UNKNOWN_USER`

User wasn't found

### `UNKNOWN_USER_FLAG`

The specified user flag isn't recognized

### `UNKNOWN_VOICE_REGION`

Unknown voice region

### `UNKNOWN_VOICE_SERVER`

Unknown voice server

### `UNKNOWN_WEBAUTHN_CREDENTIAL`

Unknown WebAuthn credential

### `UNKNOWN_WEBHOOK`

Unknown webhook

### `UPDATE_FAILED`

We couldn't update the resource

### `USERNAME_NOT_AVAILABLE`

This username is not available

### `USER_BANNED_FROM_GUILD`

This user is banned from this community

### `USER_IP_BANNED_FROM_GUILD`

This user's IP is banned from this community

### `USER_NOT_IN_VOICE`

This user isn't in voice

### `USER_OWNS_GUILDS`

This user owns communities

### `VALIDATION_ERROR`

Validation failed

### `VOICE_CHANNEL_FULL`

Voice channel is full

### `WEBAUTHN_CREDENTIAL_LIMIT_REACHED`

You've reached the maximum of {count, plural, one {# WebAuthn credential} other {# WebAuthn credentials}}


## Validation error code registry

These codes appear in the `code` field of an element in the top-level `errors` array on an `INVALID_FORM_BODY` response. Each names one specific input constraint. A schema failure whose constraint names no code of its own reports one of the [default schema failure codes](#default-schema-failure-codes). The registry is closed and holds exactly 236 codes. Each entry states the leading sentence of the English source message, the same way.

### `ACCENT_COLOR_CHANGED_TOO_MANY_TIMES`

You've changed your accent color too often recently

### `AFK_CHANNEL_MUST_BE_IN_GUILD`

AFK channel must be in this community

### `AFK_CHANNEL_MUST_BE_VOICE`

AFK channel must be a voice channel

### `AGE_RESTRICTED`

This sensitive content filter isn't available for your age

### `ALL_CHANNELS_MUST_BELONG_TO_GUILD`

All channels must belong to this community

### `ANIMATED_AVATARS_REQUIRE_PREMIUM`

Animated avatars require Premium

### `ANIMATED_GUILD_BANNER_REQUIRES_FEATURE`

Animated community banner requires ANIMATED_BANNER feature

### `ATTACHMENTS_NOT_ALLOWED_FOR_MESSAGE`

Attachments aren't allowed for this message

### `ATTACHMENT_FIELDS_REQUIRED`

`attachment_id`, `channel_id`, `message_id`, and `expires_at` are required

### `ATTACHMENT_IDS_MUST_BE_VALID_INTEGERS`

`attachment_id`, `channel_id`, and `message_id` must be valid integers

### `ATTACHMENT_ID_NOT_FOUND_IN_MESSAGE`

Attachment with ID {attachmentId} wasn't found in the message

### `ATTACHMENT_MUST_BE_IMAGE`

Attachment "{filename}" must be an image file (png, jpg, jpeg, webp, or gif)

### `AT_LEAST_ONE_ENTRY_IS_REQUIRED`

At least one entry is required

### `AT_LEAST_ONE_RECIPIENT_REQUIRED`

At least one recipient is required

### `AVATAR_CHANGED_TOO_MANY_TIMES`

You've changed your avatar too often recently

### `BANNERS_REQUIRE_PREMIUM`

Banners require Premium

### `BANNER_CHANGED_TOO_MANY_TIMES`

You've changed your banner too often recently

### `BASE64_LENGTH_INVALID`

Base64 string length must be between {min} and {maxLength} characters

### `BIO_CHANGED_TOO_MANY_TIMES`

You've changed your bio too often recently

### `BOT_DISCRIMINATOR_CANNOT_BE_CHANGED`

Bot discriminator can't be changed

### `BOT_SEARCH_SCOPE_UNAVAILABLE`

Bots can only search within a single community or channel

### `BUCKET_IS_REQUIRED`

`bucket` is required

### `CANNOT_ADD_YOURSELF_TO_GROUP_DM`

You can't add yourself to a group DM

### `CANNOT_DELETE_MORE_THAN_100_MESSAGES`

You can't delete more than {max, plural, one {# message} other {# messages}} at once

### `CANNOT_DM_YOURSELF`

You can't DM yourself

### `CANNOT_EDIT_ATTACHMENT_METADATA`

Users with MANAGE_MESSAGES can only edit attachment descriptions, not other metadata

### `CANNOT_LEAVE_GUILD_AS_OWNER`

You can't leave a community as the owner

### `CANNOT_POSITION_CHANNEL_RELATIVE_TO_ITSELF`

You can't position a channel relative to itself or its descendants

### `CANNOT_PRELOAD_MORE_THAN_100_CHANNELS`

You can't preload more than {max, plural, one {# channel} other {# channels}} at once

### `CANNOT_REFERENCE_ATTACHMENTS_WITHOUT_ATTACHMENTS`

You can't reference attachments when no attachments are provided

### `CANNOT_REORDER_EVERYONE_ROLE`

You can't reorder the @everyone role

### `CANNOT_REPLY_TO_SYSTEM_MESSAGE`

You can't reply to a system message

### `CANNOT_SET_HOIST_FOR_EVERYONE_ROLE`

You can't set hoist position for the @everyone role

### `CANNOT_SPECIFY_BOTH_BEFORE_AND_AFTER`

You can't specify both `before` and `after`

### `CATEGORIES_CANNOT_HAVE_PARENTS`

Categories can't have parents

### `CATEGORIES_CANNOT_HAVE_PARENT_CHANNEL`

Categories can't have a parent channel

### `CHANGING_DISCRIMINATOR_REQUIRES_PREMIUM`

Changing your discriminator requires Premium

### `CHANNEL_DOES_NOT_EXIST`

Channel not found

### `CHANNEL_ID_IS_REQUIRED`

`channel_id` is required

### `CHANNEL_MUST_BE_DM_OR_GROUP_DM`

Channel must be a DM or a group DM

### `CHANNEL_MUST_BE_VOICE`

Channel must be a voice channel

### `CHANNEL_NAME_EMPTY_AFTER_NORMALIZATION`

Channel name can't be empty after normalization

### `CHANNEL_NOT_FOUND`

Channel not found

### `COLOR_VALUE_TOO_HIGH`

Color value must not exceed 0xffffff

### `COLOR_VALUE_TOO_LOW`

Color value must be at least 0x000000

### `CONTENT_EXCEEDS_MAX_LENGTH`

Text must not exceed {maxLength, plural, one {# character} other {# characters}}

### `CONTEXT_CHANNEL_OR_GUILD_ID_REQUIRED`

A context channel or community ID is required

### `CUSTOM_EMOJIS_REQUIRE_PREMIUM_OUTSIDE_SOURCE`

You can't use custom emojis outside their source communities without Premium

### `CUSTOM_EMOJI_NOT_FOUND`

Custom emoji wasn't found

### `CUSTOM_STICKERS_IN_DMS_REQUIRE_PREMIUM`

You can't use custom stickers in DMs without Premium

### `CUSTOM_STICKERS_REQUIRE_PREMIUM_OUTSIDE_SOURCE`

You can't use custom stickers outside their source communities without Premium

### `CUSTOM_STICKER_NOT_FOUND`

Custom sticker wasn't found

### `DISCOVERABLE_GUILD_VERIFICATION_LEVEL_TOO_LOW`

Discoverable communities must have a verification level of at least Low

### `DISCRIMINATOR_INVALID_FORMAT`

Discriminator must be {min}–{max} digits

### `DISCRIMINATOR_OUT_OF_RANGE`

Discriminator must be between {min} and {max}

### `DUPLICATE_ATTACHMENT_IDS_NOT_ALLOWED`

Duplicate attachment IDs aren't allowed

### `DUPLICATE_FILE_INDEX`

Duplicate file index: {index}

### `DUPLICATE_RECIPIENTS_NOT_ALLOWED`

Duplicate recipients aren't allowed

### `EMAIL_ALREADY_IN_USE`

Email is already in use

### `EMAIL_DOMAIN_NOT_ALLOWED_FOR_SSO`

This email domain isn't allowed for single sign-on

### `EMAIL_IS_REQUIRED`

Email is required

### `EMAIL_LENGTH_INVALID`

Email address must be between {min} and {max} characters

### `EMAIL_MUST_BE_CHANGED_VIA_TOKEN`

Email must be changed via token

### `EMAIL_TOKEN_EXPIRED`

Email token expired

### `EMBEDS_EXCEED_MAX_CHARACTERS`

Embeds must not exceed {maxCharacters, plural, one {# character} other {# characters}} in total

### `EMBED_INDEX_OUT_OF_BOUNDS`

Embed index {embedIndex} is out of bounds (message has {embedCount, plural, one {# embed} other {# embeds}})

### `EMBED_SPLASH_REQUIRES_FEATURE`

Embed splash requires INVITE_SPLASH feature

### `ENTRANCE_SOUND_DURATION_EXCEEDS_LIMIT`

Entrance sounds can be at most {max_ms, number}ms long

### `ENTRANCE_SOUND_INVALID_FORMAT`

Entrance sounds must be MP3, OGG, M4A, or WAV audio

### `ENTRANCE_SOUND_INVALID_SCOPE`

Invalid entrance sound scope

### `ENTRANCE_SOUND_NAME_LENGTH_INVALID`

Entrance sound names must be between 1 and {max, number} characters

### `ENTRANCE_SOUND_NOT_FOUND`

Entrance sound not found

### `ENTRANCE_SOUND_QUOTA_REACHED`

You've reached the limit of {max, number} saved entrance sounds

### `ENTRANCE_SOUND_SIZE_EXCEEDS_LIMIT`

Entrance sounds can be at most {max_bytes, number} bytes

### `FAILED_TO_FETCH_SSO_USER_INFO`

We couldn't fetch your SSO user info

### `FAILED_TO_PARSE_MULTIPART_FORM_DATA`

We couldn't parse the multipart form data

### `FAILED_TO_PARSE_SSO_USER_INFO`

We couldn't read the response from your SSO provider

### `FAILED_TO_UPLOAD_IMAGE`

We couldn't upload the image

### `FAVORITE_MEME_NAME_REQUIRED`

Favorite meme name is required

### `FAVORITE_MEME_NOT_FOUND`

Favorite meme wasn't found

### `FILENAME_EMPTY_AFTER_NORMALIZATION`

Filename can't be empty after normalization

### `FILENAME_INVALID_CHARACTERS`

Filename contains invalid characters

### `FILENAME_LENGTH_INVALID`

Filename must be between {min} and {max} characters

### `FILE_INDEX_EXCEEDS_MAXIMUM`

File index {index} exceeds the maximum allowed index of {maxIndex}

### `FILE_NOT_FOUND`

File not found

### `FORWARD_MESSAGES_CANNOT_CONTAIN_CONTENT`

Forwarded messages can't include content, embeds, attachments, or stickers

### `FORWARD_REFERENCE_REQUIRES_CHANNEL_AND_MESSAGE`

Forward message reference must include `channel_id` and `message_id`

### `GLOBAL_NAME_CANNOT_CONTAIN_RESERVED_TERMS`

Display name can't contain "system message"

### `GLOBAL_NAME_LENGTH_INVALID`

Global name must be between {min} and {max} characters

### `GLOBAL_NAME_RESERVED_VALUE`

Global name can't be "everyone" or "here"

### `GUILD_BANNER_REQUIRES_FEATURE`

Community banner requires BANNER feature

### `GUILD_FEATURE_NOT_TOGGLEABLE`

This feature cannot be toggled

### `GUILD_ID_MUST_MATCH_REFERENCED_MESSAGE`

Community ID must match the channel the referenced message was fetched from

### `GUILD_ID_REQUIRED_FOR_SEARCH_INDEX`

Community ID is required for channel message and member search indexes

### `IMAGE_SIZE_EXCEEDS_LIMIT`

Image size exceeds {maxSize} bytes

### `INTEGER_OUT_OF_INT64_RANGE`

Integer value is out of the valid int64 range

### `INVALID_AUDIT_LOG_REASON`

Invalid audit log reason

### `INVALID_BASE64_FORMAT`

The provided value is in an invalid format

### `INVALID_CHANNEL_ID`

Invalid channel ID: {channelId}

### `INVALID_CODE`

Invalid code

### `INVALID_DATE_OF_BIRTH_FORMAT`

Invalid date of birth format

### `INVALID_EMAIL_ADDRESS`

Invalid email address format

### `INVALID_EMAIL_FORMAT`

Invalid email address format

### `INVALID_EMAIL_LOCAL_PART`

Invalid email address format

### `INVALID_EMAIL_OR_PASSWORD`

Invalid email or password

### `INVALID_EMAIL_TOKEN`

Invalid email token

### `INVALID_FILE_FIELD_NAME`

Invalid file field name: {key}

### `INVALID_FORMAT`

The provided value is in an invalid format

### `INVALID_IMAGE_FORMAT`

Invalid image format

### `INVALID_INTEGER_FORMAT`

Invalid integer format

### `INVALID_ISO_TIMESTAMP`

Must be a valid ISO timestamp

### `INVALID_JSON_IN_PAYLOAD_JSON`

Invalid JSON in `payload_json`

### `INVALID_MESSAGE_DATA`

Invalid message data

### `INVALID_MFA_CODE`

Invalid MFA code

### `INVALID_OR_EXPIRED_AUTHORIZATION_TICKET`

Invalid or expired authorization ticket

### `INVALID_OR_EXPIRED_AUTHORIZATION_TOKEN`

Invalid or expired authorization token

### `INVALID_OR_EXPIRED_RESET_TOKEN`

Invalid or expired password reset token

### `INVALID_OR_EXPIRED_REVERT_TOKEN`

Invalid or expired revert token

### `INVALID_OR_EXPIRED_SSO_STATE`

Invalid or expired SSO state

### `INVALID_OR_EXPIRED_TICKET`

Invalid or expired ticket

### `INVALID_OR_EXPIRED_VERIFICATION_TOKEN`

Invalid or expired verification token

### `INVALID_OR_RESTRICTED_RTC_REGION`

Invalid or restricted RTC region: {region}

### `INVALID_PARENT_CHANNEL`

Invalid parent channel

### `INVALID_PASSWORD`

Invalid email or password

### `INVALID_PROOF_TOKEN`

Invalid proof token

### `INVALID_ROLE_ID`

Invalid role ID: {roleId}

### `INVALID_RTC_REGION`

Invalid RTC region: {region}

### `INVALID_SNOWFLAKE`

Invalid snowflake

### `INVALID_SNOWFLAKE_FORMAT`

Invalid snowflake

### `INVALID_SSO_AUTHORIZATION_CODE`

Invalid SSO authorization code

### `INVALID_SSO_TOKEN`

Invalid SSO token

### `INVALID_TIMEOUT_VALUE`

Invalid timeout value

### `INVALID_TIMEZONE_IDENTIFIER`

Invalid timezone identifier

### `INVALID_TRUSTED_DOMAINS`

The wildcard (*) can't be combined with specific domains

### `INVALID_URL_FORMAT`

Invalid URL format

### `INVALID_URL_OR_ATTACHMENT_FORMAT`

Invalid URL format or attachment reference

### `INVALID_VERIFICATION_CODE`

Invalid verification code

### `INVITE_SPLASH_REQUIRES_FEATURE`

Invite splash requires INVITE_SPLASH feature

### `MAX_FAVORITE_MEME_TAGS_EXCEEDED`

You can only add up to {limit, plural, one {# tag} other {# tags}} per favorite meme

### `MEDIA_ALREADY_IN_FAVORITE_MEMES`

This media is already in your favorite memes

### `MESSAGES_ARRAY_REQUIRED_AND_NOT_EMPTY`

`messages` array is required and must not be empty

### `MESSAGES_WITH_SNAPSHOTS_CANNOT_BE_EDITED`

Messages with snapshots can't be edited

### `MESSAGE_HISTORY_CUTOFF_BEFORE_GUILD_CREATION`

Message history cutoff can't be before the community was created

### `MESSAGE_HISTORY_CUTOFF_IN_FUTURE`

Message history cutoff can't be in the future

### `MESSAGE_IDS_CANNOT_BE_EMPTY`

`message_ids` can't be empty

### `MULTIPLE_FILES_FOR_INDEX_NOT_ALLOWED`

Multiple files for index {index} aren't allowed

### `MUST_AGREE_TO_TOS_AND_PRIVACY_POLICY`

You must agree to the Terms of Service and Privacy Policy

### `MUST_BE_MINIMUM_AGE`

You must be at least {minAge, plural, one {# year} other {# years}} old to create an account

### `MUST_ENABLE_2FA_BEFORE_REQUIRING_FOR_MODS`

You must enable 2FA on your account before requiring it for moderators

### `MUST_HAVE_EMAIL_TO_CHANGE_IT`

You must have an email address to change it

### `MUST_START_SESSION_BEFORE_SENDING`

You must start a session before sending messages

### `NAME_EMPTY_AFTER_NORMALIZATION`

Name can't be empty after normalization

### `NCMEC_ATTACHMENT_MUST_BE_IMAGE_OR_VIDEO`

Only image or video attachments can be reported to NCMEC

### `NEW_EMAIL_MUST_BE_DIFFERENT`

New email must be different from your current email

### `NOT_A_VALID_UNICODE_EMOJI`

Not a valid unicode emoji

### `NO_FILE_FOR_ATTACHMENT`

No file was uploaded for attachment with ID {attachmentId}

### `NO_FILE_FOR_ATTACHMENT_METADATA`

No file was uploaded for attachment metadata with ID {attachmentId}

### `NO_NEW_EMAIL_REQUESTED`

No new email was requested

### `NO_ORIGINAL_EMAIL_ON_RECORD`

No original email is on record

### `NO_UPLOADED_PARTS_TO_FINALIZE`

No uploaded parts are available to finalize

### `NO_VALID_MEDIA_IN_MESSAGE`

No valid media was found in the message

### `ORIGINAL_EMAIL_ALREADY_VERIFIED`

Original email is already verified

### `ORIGINAL_EMAIL_MUST_BE_VERIFIED_FIRST`

Original email must be verified first

### `ORIGINAL_VERIFICATION_NOT_REQUIRED`

Original verification isn't required for this flow

### `PARENT_CHANNEL_NOT_IN_GUILD`

Parent channel isn't present in the community

### `PARENT_MUST_BE_CATEGORY`

Parent must be a category

### `PARSE_AND_USERS_OR_ROLES_CANNOT_BE_USED_TOGETHER`

You can't use `parse` together with `allowed_mentions.users` or `allowed_mentions.roles`

### `PASSWORD_IS_TOO_COMMON`

Your password is too common

### `PASSWORD_LENGTH_INVALID`

String length must be between {min} and {max} characters

### `PASSWORD_NOT_SET`

Password isn't set

### `PHONE_NUMBER_INVALID_FORMAT`

Phone number must be in E.164 format (for example, +1234567890)

### `PRECEDING_CHANNEL_MUST_SHARE_PARENT`

Preceding channel must share the same parent as the moved channel

### `PRECEDING_CHANNEL_NOT_IN_GUILD`

Preceding channel isn't present in the community

### `PRONOUNS_CHANGED_TOO_MANY_TIMES`

You've changed your pronouns too often recently

### `RECIPIENT_IDS_CANNOT_BE_EMPTY`

Recipient IDs can't be empty

### `RECIPIENT_IDS_MUST_BE_STRINGS`

Recipient IDs must be strings

### `RECIPIENT_IDS_MUST_BE_VALID_SNOWFLAKES`

Recipient IDs must be valid snowflakes

### `REFERENCED_ATTACHMENT_NOT_FOUND`

Referenced attachment "{filename}" wasn't found in message attachments

### `ROWS_IS_REQUIRED`

`rows` is required

### `SESSION_TIMEOUT`

Session timed out

### `SIZE_BYTES_MUST_BE_VALID_INTEGER`

`size_bytes` must be a valid integer

### `SNOWFLAKE_OUT_OF_RANGE`

Invalid snowflake

### `SSO_IDENTITY_MISMATCH`

SSO identity mismatch between id_token and userinfo

### `SSO_MISCONFIGURED`

SSO is misconfigured

### `SSO_PROVIDER_DID_NOT_RETURN_EMAIL`

Your SSO provider didn't return an email address

### `SSO_TEST_CODE_MISSING_EMAIL`

SSO test code is missing the email payload

### `SSO_UNABLE_TO_ALLOCATE_DISCRIMINATOR`

We couldn't allocate a discriminator for your SSO account

### `STRING_LENGTH_EXACT`

String must be exactly {length} characters

### `STRING_LENGTH_INVALID`

String length must be between {min} and {max} characters

### `SYSTEM_CHANNEL_MUST_BE_IN_GUILD`

System channel must be in this community

### `SYSTEM_CHANNEL_MUST_BE_TEXT`

System channel must be a text channel

### `TAG_ALREADY_TAKEN`

This tag is already taken

### `THIS_VANITY_URL_IS_ALREADY_TAKEN`

This vanity URL is already taken

### `TICKET_ALREADY_COMPLETED`

This ticket has already been completed

### `TIMEOUT_CANNOT_EXCEED_365_DAYS`

Timeout can't be longer than {maxDays, plural, one {# day} other {# days}} from now

### `TOO_LARGE`

Synced preferences exceed {max_bytes} bytes

### `TOO_MANY_EMBEDS`

Too many embeds

### `TOO_MANY_FILES`

Too many files

### `TOO_MANY_USERS_WITH_THIS_USERNAME`

There are too many users with this username

### `TOO_MANY_USERS_WITH_USERNAME_TRY_DIFFERENT`

There are too many users with this username

### `TOTP_NOT_ENABLED`

Authenticator app two-factor isn't enabled for this account

### `UNCLAIMED_ACCOUNTS_CAN_ONLY_SET_EMAIL_VIA_TOKEN`

Unclaimed accounts can only set email via token

### `UNRESOLVED_ATTACHMENT_URL`

Unresolved `attachment://` URL detected

### `UPLOADED_ATTACHMENT_NOT_FOUND`

Uploaded attachment {filename} wasn't found

### `URL_LENGTH_INVALID`

URL must be between {min} and {max} characters

### `URL_NOT_PUBLICLY_ROUTABLE`

URL must resolve to a publicly routable address

### `USERNAME_CANNOT_CONTAIN_RESERVED_TERMS`

Username can't contain "fluxer" or "system message"

### `USERNAME_CHANGED_TOO_MANY_TIMES`

You've changed your username too often recently

### `USERNAME_INVALID_CHARACTERS`

Username can only contain Latin letters (a-z, A-Z), numbers (0-9), and underscores (_)

### `USERNAME_LENGTH_INVALID`

Username must be between {min} and {max} characters

### `USERNAME_RESERVED_VALUE`

Username can't be "everyone" or "here"

### `USER_DOES_NOT_HAVE_AN_EMAIL_ADDRESS`

This user doesn't have an email address

### `USER_IS_NOT_BANNED`

This user isn't banned

### `USER_MUST_BE_A_BOT_TO_BE_MARKED_AS_A_SYSTEM_USER`

User must be a bot to be marked as a system user

### `USER_NOT_IN_CHANNEL`

This user isn't in the channel

### `VALUE_MUST_BE_INTEGER_IN_RANGE`

The value for `{name}` must be an integer between {minValue} and {maxValue}

### `VANITY_URL_CODE_ALREADY_TAKEN`

Vanity URL code is already taken

### `VANITY_URL_CODE_CANNOT_CONTAIN_FLUXER`

Vanity URL code can't contain "fluxer"

### `VANITY_URL_CODE_LENGTH_INVALID`

Vanity URL code must be between {min} and {max} characters

### `VANITY_URL_INVALID_CHARACTERS`

Vanity URL can only contain lowercase letters (a-z), digits (0-9), and hyphens (-)

### `VANITY_URL_REQUIRES_FEATURE`

Vanity URL requires VANITY_URL feature

### `VERIFICATION_CODE_EXPIRED`

Verification code has expired

### `VERIFICATION_CODE_NOT_ISSUED`

No verification code has been issued

### `VISIONARY_REQUIRED_FOR_DISCRIMINATOR`

You must be on the Visionary lifetime plan to use that discriminator

### `VOICE_ACTIVITY_SHARING_ON_COOLDOWN`

You can only update your voice activity sharing default once every 24 hours

### `VOICE_CHANNELS_CANNOT_BE_ABOVE_TEXT_CHANNELS`

Voice channels can't be positioned above text channels within the same category

### `VOICE_MESSAGES_ATTACHMENT_DURATION_REQUIRED`

Voice message attachments must specify a duration

### `VOICE_MESSAGES_ATTACHMENT_MUST_BE_AUDIO`

Voice message attachments must be audio files

### `VOICE_MESSAGES_ATTACHMENT_WAVEFORM_REQUIRED`

Voice message attachments must include waveform data

### `VOICE_MESSAGES_CANNOT_HAVE_CONTENT`

Voice messages can't have text content

### `VOICE_MESSAGES_CANNOT_HAVE_EMBEDS`

Voice messages can't have embeds

### `VOICE_MESSAGES_CANNOT_HAVE_FAVORITE_MEMES`

Voice messages can't have favorite memes

### `VOICE_MESSAGES_CANNOT_HAVE_STICKERS`

Voice messages can't have stickers

### `VOICE_MESSAGES_DURATION_EXCEEDS_LIMIT`

Voice message duration can't exceed {maxDuration, plural, one {# second} other {# seconds}}

### `VOICE_MESSAGES_REQUIRE_SINGLE_ATTACHMENT`

Voice messages must contain exactly one attachment

### `WEBHOOK_NAME_LENGTH_INVALID`

Webhook name must be between {min} and {max} characters


## Localisation

An error `message` is rendered in one resolved locale. Fluxer uses the configured locale of the authenticated account when the request is authenticated and the account has one. Otherwise it negotiates the request `Accept-Language` value against the canonical [supported locale registry](/topics/locales/#supported-locales). [Locales](/topics/locales/#negotiation) defines the resolution algorithm, including weights, language subtag reduction, and the `en-US` result.

A failure raised before the request locale is resolved, such as an IP ban denial, cannot see the account locale and negotiates `Accept-Language` on its own. That negotiation reads the header entries in order, ignores quality weights, and falls back to `en-US`.

Fluxer localises the `message` of a validation element that has a `code` the same way. A validation element with no `code` has the fixed English string written at the failure site. The `code` field is never localised, either in the envelope or in a validation element.

A code whose catalogue entry is missing for the resolved locale falls back to its English source template. A code with no registered template at all falls back to the message the failure supplied or to the code itself.
