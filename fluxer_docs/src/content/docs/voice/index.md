---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Voice
description: Voice placement, guild voice channels, calls, Go Live, and entrance sounds.
---

Fluxer runs voice over LiveKit. The [main Gateway](/gateway/overview/) places a session into a voice channel and hands it one credential, the client presents that credential to a media server, and every track goes over the connection it opens there. Microphone, camera, and screen share are track sources on that one connection, so going live opens nothing new.

[Client commands](/gateway/commands/) and [Gateway events](/gateway/events/) define the placement protocol.

:::note[A voice rewrite is in progress]
Every statement on this page is what an instance serves today, and a later release can change it. Re-read the page after an instance upgrade.
:::

## Voice surfaces

| Surface | What it is | Reference |
| --- | --- | --- |
| Guild voice channel | A channel of [type](/http-api/channels/#channel-types) `2`, which also has messages, pins, and slowmode | [Channels](/http-api/channels/) |
| Private call | The direct message and group direct message counterpart of a voice channel | [Calls](/http-api/calls/) |
| Go Live stream<sup>1</sup> | Screen share published as an extra track on a voice connection the member already holds | [Streams](/http-api/streams/) |
| Entrance sound<sup>2</sup> | A short clip announced to everyone already connected to a voice channel | [Entrance sounds](/http-api/entrance-sounds/) |
| Voice activity sharing | Whether a friend is told which voice channel the account is in | [User settings](/http-api/users/settings/#modify-voice-activity-sharing) |

<sup>1</sup> Going live opens no second connection and mints no second credential

<sup>2</sup> The one surface that publishes no media track

## Media transport

Media never crosses the HTTP API. No route returns an audio or video track, the credential a media connection presents, or the key material an end-to-end encrypted channel uses.

Fluxer runs media over LiveKit and publishes no signalling protocol of its own. There is no voice websocket, no voice opcode set, no UDP discovery step, and no separate encryption handshake. A client connects to the `endpoint` the [Voice Server Update](/gateway/events/#voice-server-update) grant names, presents the grant `token`, and speaks the LiveKit protocol from there.

| Concept | Value |
| --- | --- |
| Room name, guild voice channel | `guild_{guild_id}_channel_{channel_id}` |
| Room name, private call | `dm_channel_{channel_id}` |
| Participant identity | `user_{user_id}_{connection_id}` |

The grant lives for 600 seconds.

One room is one voice channel, and one participant is one voice connection. A member holding several connections in the same channel is several participants in the same room, which is how one account is present from more than one device.

The grant also names the track sources the connection may publish. SPEAK admits the microphone source, and STREAM admits the camera source together with the two screen share sources. A server-deafened connection may neither publish nor subscribe.

## Deployment feature state

A deployment can be configured without voice. The [instance discovery document](/http-api/instance/#instance-features-object) reports that state as `features.voice_enabled`. No other surface warns a client in advance.

A deployment that reports false mints no media credential, so a placement request is refused with `VOICE_TOKEN_FAILED`. [List RTC regions](/http-api/channels/#list-rtc-regions) answers 200 with an empty array before it resolves the channel, and [Modify call region](/http-api/calls/#modify-call-region) accepts any region string.

## Placement

[Voice State Update](/gateway/commands/#voice-state-update) declares where a session wants to be. It has the target `guild_id`, `channel_id`, and `connection_id` together with `self_mute`, `self_deaf`, `self_video`, and `self_stream`. A null `channel_id` leaves, and a call placement always has a null `guild_id`.

`channel_id` and `connection_id` together select the operation.

| Command shape | Result |
| --- | --- |
| `channel_id` and no `connection_id` | Opens a new connection |
| `channel_id` and a `connection_id` | Updates or moves that connection |
| Null `channel_id` and a `connection_id`, in a guild | Drops that connection |
| Null `channel_id` and no `connection_id`, in a guild | Refused with `VOICE_MISSING_CONNECTION_ID` |
| Null `channel_id` and no `connection_id`, in a call | Drops every voice membership the session holds |
| Null `channel_id` with a non-string, non-null `connection_id` | Refused with `VALIDATION_INVALID_PARAMS` |

The server answers with [Voice State Update](/gateway/events/#voice-state-update) for the resulting membership, delivered to every session that can view the channel, and with [Voice Server Update](/gateway/events/#voice-server-update) for the requesting session alone. That grant has the `token`, `endpoint`, `channel_id`, and `connection_id` of the media server the deployment selected. `guild_id` is present for a guild voice channel and omitted for a call, so a client reads the scope of the grant from that field. It also has an `e2ee_key` when the channel is end-to-end encrypted.

A grant is issued when a connection opens, when it moves to another channel, and when the region changes. An update that stays in the same channel reissues nothing, so toggling `self_mute`, `self_video`, or `self_stream` produces one [Voice State Update](/gateway/events/#voice-state-update) and no new grant.

The grant `token` is minted for the media server and consumed by the media connection alone. No route on this API accepts it.

Fluxer reports a guild refusal as [Voice State Ack](/gateway/events/#voice-state-ack) with a `status` of `rejected` and an `error_code` naming the exact reason, and only a command with a `mutation_id` receives one. A refused placement without it produces no Dispatch, so a client that needs to observe a guild failure MUST send `mutation_id`.

A call never acks. A refused placement into a direct message or group direct message call produces no Dispatch whether or not the command had `mutation_id`, so a client observes that refusal only as the absence of a grant.

## Guild voice channels

A guild voice channel stores its `bitrate`, `user_limit`, `voice_connection_limit`, and `rtc_region` on the [channel object](/http-api/channels/#channel-object). It also has ordinary messages, pins, and slowmode, so its text history is read and written through the [Messages resource](/http-api/messages/).

### Permissions

| Permission | Effect on voice |
| --- | --- |
| VIEW_CHANNEL and CONNECT<sup>1</sup> | Both are required to hold a voice connection in the channel |
| SPEAK<sup>2</sup> | Publish audio |
| STREAM<sup>3</sup> | Publish camera video and Go Live media |
| MUTE_MEMBERS<sup>4</sup> | Apply and clear a moderator mute |
| DEAFEN_MEMBERS<sup>4</sup> | Apply and clear a moderator deafen |
| MOVE_MEMBERS | Move a member to another guild voice channel, or disconnect it |
| UPDATE_RTC_REGION | Change the channel's `rtc_region` |

<sup>1</sup> A member missing either bit is refused with `VOICE_PERMISSION_DENIED`

<sup>2</sup> A member without it is admitted, and its voice state is published with `suppress` true

<sup>3</sup> One bit gates both, so there is no separate camera permission

<sup>4</sup> MUTE_MEMBERS covers the `mute` field and DEAFEN_MEMBERS the `deaf` field of the [guild member update object](/http-api/guild-members/#guild-member-update-object). `PRIORITY_SPEAKER` and `USE_VAD` are defined and assignable [permission bits](/http-api/permissions/#permissions) that no HTTP route and no Gateway command evaluates

[ADMINISTRATOR](/http-api/permissions/) resolves to the complete mask before any channel overwrite is applied, so it satisfies every row of that table. Two states skip the VIEW_CHANNEL and CONNECT check. A member the guild is already moving is admitted. So is a member holding virtual access to the channel, which the guild grants to a connected member that loses VIEW_CHANNEL or that a moderator moves into a channel it cannot see. Virtual access also grants SPEAK and STREAM in that channel on its own.

A grant is evaluated when it is minted, and the guild re-evaluates a connection that is already open. Joining, moving, a region change, a role edit, an overwrite edit, and a member role change each recompute SPEAK and STREAM.

The guild applies the new result to a live connection and issues no new [Voice Server Update](/gateway/events/#voice-server-update). The media server mutes a published microphone, camera, or screen share track the member may no longer publish, and drops a connection that fails the VIEW_CHANNEL and CONNECT check.

Moderation is an HTTP operation on the guild membership. [Modify guild member](/http-api/guild-members/#modify-guild-member) and [Modify current guild member](/http-api/guild-members/#modify-current-guild-member) share one request body. Each applies a moderator mute, applies a moderator deafen, moves a member between guild voice channels, and forces a disconnect. Both hold the caller to MUTE_MEMBERS, DEAFEN_MEMBERS, and MOVE_MEMBERS, including when the target is the caller itself. No other HTTP route and no Gateway command does any of it.

That mute and that deafen reach the media server without a new credential. The change applies to every connection the account holds in that channel, and no [Voice Server Update](/gateway/events/#voice-server-update) follows.

### Capacity

| Bound | Refusal |
| --- | --- |
| `user_limit`<sup>1</sup> | `VOICE_CHANNEL_FULL` |
| `voice_connection_limit`<sup>2</sup> | `VOICE_CONNECTION_LIMIT_REACHED` |
| 25 members with a camera on<sup>3</sup> | `VOICE_CAMERA_USER_LIMIT` |

<sup>1</sup> A stored `0` means no limit. While any member in the channel has a camera on, the effective occupancy limit becomes the lower of `user_limit` and 25, and a channel with no limit is capped at 25 for as long as that holds

<sup>2</sup> The ceiling on simultaneous connections one member may hold in the channel. A channel that stores no usable value is evaluated at 5, and a stored value above 100 is evaluated at 100

<sup>3</sup> One member with several connections counts once, and the requesting member is counted before the comparison

A private call reads no `voice_connection_limit` and applies a fixed ceiling of 5 connections for each member.

A member whose `communication_disabled_until` is still in the future is refused with `VOICE_MEMBER_TIMED_OUT` before any permission or capacity check runs. An account that has never claimed its credentials is refused with `VOICE_UNCLAIMED_ACCOUNT` for a one-on-one direct message call and for any guild voice channel whose guild it does not own. A group direct message call is not refused. A session that did not identify with `e2ee_capable` is refused with `VOICE_E2EE_REQUIRED` while the guild has voice encryption enabled and every connection already in the channel is capable, and a bot is exempt from that one.

### Regions

[List RTC regions](/http-api/channels/#list-rtc-regions) returns the [RTC region objects](/http-api/channels/#rtc-region-object) the caller MAY select for one guild voice channel. A region is returned only when the caller passes every restriction configured for it and at least one voice server accessible to the caller is active in it, so the array can be empty.

`rtc_region` is written by [Modify channel](/http-api/channels/#modify-channel) and requires UPDATE_RTC_REGION. A null value selects automatic routing, and so does a stored value the placing account cannot reach.

The first placement in the channel pins one voice server for it, and every later placement inherits that pinned server whatever its own coordinates are. A placement that finds no usable pin takes the accessible server nearest to the `latitude` and `longitude` the placement command supplied. A command that supplied no usable coordinates falls back to the deployment's default region, and then to the first accessible region.

The pin drops when the channel's `rtc_region` changes, when a call changes region, when the pinned server stops being accessible, or when the media server reports the room finished. That last case also disconnects every connection in a guild voice channel.

The literal `automatic` is not a channel region. Only the `region` field of [Modify call region](/http-api/calls/#modify-call-region) accepts it, as a synonym for null.

An operator manages the regions and the voice servers registered inside them through the [Admin voice resource](/admin-api/voice/).

## Private calls

A private call is the direct message and group direct message counterpart of a voice channel. It has no moderator, no permission overwrites, and no moderator mute, deafen, or disconnect.

The [Calls resource](/http-api/calls/) owns its HTTP surface, which reads whether the caller may ring, changes the region of an active call, rings recipients, and stops ringing them. [End call session](/http-api/calls/#end-call-session) ends no call. [Call Create](/gateway/events/#call-create), [Call Update](/gateway/events/#call-update), and [Call Delete](/gateway/events/#call-delete) publish the call state, and both joining and leaving are the same [Voice State Update](/gateway/commands/#voice-state-update) a guild voice channel uses.

A recipient's [incoming call flags](/http-api/users/#incoming-call-flags) decide whether it is rung. The flags can admit nobody, friends only, friends of friends, guild members, or everyone, and they can admit everyone silently.

[Get call eligibility](/http-api/calls/#get-call-eligibility) applies two conditions of the caller's own before it reads that policy. A caller already connected to the channel's call is reported as not ringable. So is an unclaimed account in a direct message. The operation applies no recipient policy to a group direct message, and reports one as ringable unless the caller is already connected to its call.

[Ring call recipients](/http-api/calls/#ring-call-recipients) applies neither of those two conditions and evaluates the policy once per targeted recipient, in a group direct message as well as in a direct message. The result selects who is rung, so a recipient the policy excludes and a recipient it admits silently are both left out of the ringing set while the request still answers 204.

## Go Live streams

Going live publishes a screen share track, and its screen share audio track, on the LiveKit participant the member already holds in the channel. There is no second connection, no second participant, and no second `connection_id`. A member that held STREAM at placement already has both screen share sources in its grant, so no new credential is minted and no [Voice Server Update](/gateway/events/#voice-server-update) follows.

The publisher advertises the stream by setting `self_stream` on that connection with [Voice State Update](/gateway/commands/#voice-state-update), naming the connection's own `connection_id`. The server bumps the voice state `version` and rebroadcasts the state as one [Voice State Update](/gateway/events/#voice-state-update).

The voice state of a connection without STREAM in its channel has `self_stream` and `self_video` false, whatever the client sent. The guild clears both when a live connection loses STREAM, and rebroadcasts the state.

A viewer declares which streams it is watching with `viewer_stream_keys` on its own voice state, and a channel move resets that list to empty.

That connection's `connection_id` is the last segment of the [stream key](/http-api/streams/#stream-key), which is `{guild_id}:{channel_id}:{connection_id}` for a guild voice channel and `dm:{channel_id}:{connection_id}` for a private call.

The [Streams resource](/http-api/streams/) owns the operations addressed by that key, which record a region preference and read, upload, and delete a JPEG preview image. Reading a preview takes the same access that lets a member join the channel, so any member holding CONNECT there MAY read it without owning the connection. Mutating one additionally requires STREAM on a guild channel and a voice state matching exactly the channel and the connection the key names.

:::caution[An oversized track loses only its own source]
On an instance that is not self-hosted, Fluxer mutes a camera or screen share track above 1280x720 from a member without the higher video quality entitlement, and removes that source from the connection's grant. The voice connection stays up.
:::

Fluxer removes screen share audio from the grant together with screen share, and removes camera on its own. The member keeps publishing its remaining sources, and a connection whose grant has no source left may publish nothing. A track published without a `sid` is neither muted nor revoked.

## Entrance sounds

An entrance sound is a short clip an account plays for everyone already connected to a voice channel. An account keeps a personal library of at most eight clips, each 100 through 5200 milliseconds and at most 1048576 decoded bytes, stored as `mp3`, `ogg`, `m4a`, or `wav`. It then assigns one clip per [scope](/http-api/entrance-sounds/#entrance-sound-scopes), and the four scopes are `global`, `guilds`, `dms`, and `guild:{guild_id}`.

Fluxer records which clip belongs to which scope and nothing more. [Play entrance sound](/http-api/entrance-sounds/#play-entrance-sound) names the clip explicitly, so the client decides which selection applies to a given channel.

Playback requires a voice state in the target channel and no channel permission. A caller that holds none is refused at the `channel_id` path with the validation code `ENTRANCE_SOUND_INVALID_SCOPE`, and a channel ID naming no channel is refused the same way. A successful call fans one [ENTRANCE_SOUND_PLAY](/gateway/events/#entrance-sound-play) Dispatch out to every other account with a voice state in the channel, at most once per account and never back to the caller. That Dispatch has the clip's CDN URL, and each recipient fetches and plays it locally, so no audio track is published for it.

Every session the account holds receives the Dispatch, including sessions that are not in the channel. A client filters on `channel_id`.

## Voice activity sharing

An account chooses whether a friend is told which voice channel it is in. [Modify voice activity sharing](/http-api/users/settings/#modify-voice-activity-sharing) writes the account's default and rewrites the caller's side of every existing friendship to the same value in one operation. It then holds a 24 hour cooldown, and a second attempt inside that window is refused at the `share_voice_activity` path with the validation code `VOICE_ACTIVITY_SHARING_ON_COOLDOWN` and a `retry_after` in seconds.

The stored result is `share_voice_activity` on the caller's own [relationship object](/http-api/users/relationships/#relationship-object), and `friend_shares_voice_activity` reports the reciprocal record. That reciprocal is resolved by [List relationships](/http-api/users/relationships/#list-relationships) and by the two [Relationship Update](/gateway/events/#relationship-update) Dispatches this operation emits for each rewritten friendship, one to the caller and one to the friend. Every other operation that returns a relationship object reports it as true.

:::caution[The Gateway does not enforce voice activity sharing]
A [voice state](/gateway/events/#voice-state-object) reaches every session that can view the channel whatever `share_voice_activity` holds, so an account that shares nothing is still visible there. A client MUST NOT present the flag as concealment.
:::
