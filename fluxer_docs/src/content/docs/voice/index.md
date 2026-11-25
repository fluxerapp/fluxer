---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Voice
description: How sessions join voice channels and calls, and the limits and permissions that apply.
---

Fluxer runs voice over LiveKit. The Gateway places a session in a voice channel and sends it a grant, which the client uses to connect to a media server.

| Surface | Description | Reference |
| --- | --- | --- |
| Guild voice channel | A channel of [type](/http-api/channels/#channel-types) `2` | [Channels](/http-api/channels/) |
| Private call | The voice channel of a direct message or group DM | [Calls](/http-api/calls/) |
| Stream | A screen share published on an existing connection | [Streams](/http-api/streams/) |
| Entrance sound | A clip played for everyone in a voice channel when you join | [Entrance sounds](/http-api/entrance-sounds/) |
| Voice activity sharing | Whether friends see which voice channel you are in | [User settings](/http-api/users/settings/#modify-voice-activity-sharing) |

## Media transport

No HTTP route returns media or the grant. A client sends [Voice State Update](/gateway/commands/#voice-state-update) and gets a [Voice Server Update](/gateway/events/#voice-server-update). The client connects to its `endpoint` with its `token` over LiveKit. The grant also has an `e2ee_key` when the channel encrypts call media. The server creates that key, and this isn't end-to-end encryption. End-to-end encrypted calls are planned with the [new voice and video system](https://fluxer.com/roadmap#voice-and-video).

A [peer-to-peer](/gateway/events/#peer-to-peer-voice) grant has ICE servers and no `endpoint` or `token`.

LiveKit room and participant names look like this.

| Name | Format |
| --- | --- |
| Guild voice channel room | `guild_{guild_id}_channel_{channel_id}` |
| Private call room | `dm_channel_{channel_id}` |
| Participant identity | `user_{user_id}_{connection_id}` |

The grant is valid for 600 seconds. It lets the connection publish the microphone with SPEAK, and the camera and screen share with STREAM.

A connection a moderator has deafened can't publish or subscribe.

If `features.voice_enabled` is false in the [instance features](/http-api/instance/#instance-features-object), joining voice fails with `VOICE_TOKEN_FAILED` on that instance.

## Placement

The `channel_id` and `connection_id` you send in Voice State Update decide what happens. For a private call, send a null `guild_id`.

| What you send | Result |
| --- | --- |
| `channel_id` and no `connection_id` | Opens a new connection |
| `channel_id` and a `connection_id` | Updates or moves that connection |
| Null `channel_id` and a `connection_id` | Drops that connection |
| Null `channel_id` and no `connection_id`, in a guild | Refused with `VOICE_MISSING_CONNECTION_ID` |
| Null `channel_id` and no `connection_id`, in a call | Drops every voice connection of the session |

The server sends [Voice State Update](/gateway/events/#voice-state-update) to every session that can view the channel. It sends a new grant only when a connection opens, moves or changes region. Changing `self_mute`, `self_video` or `self_stream` sends no grant.

## Guild voice channels

A voice channel has `bitrate`, `user_limit`, `voice_connection_limit`, `rtc_region` and `rtc_p2p` on the [channel object](/http-api/channels/#channel-object).

The default `bitrate` is 64000 and the maximum is 96000. The `AUDIO_BITRATE_128_KBPS`, `AUDIO_BITRATE_256_KBPS` and `AUDIO_BITRATE_384_KBPS` [guild features](/http-api/guilds/#guild-features) raise the maximum to 128000, 256000 and 384000.

### Permissions

| Permission | Effect on voice |
| --- | --- |
| VIEW_CHANNEL and CONNECT | Both are needed to connect, or the join fails with `VOICE_PERMISSION_DENIED` |
| SPEAK | Publish audio, and without it the member joins with `suppress` true |
| STREAM | Publish camera video and streams |
| MUTE_MEMBERS | Apply a moderator mute with the `mute` member field |
| DEAFEN_MEMBERS | Apply a moderator deafen with the `deaf` member field |
| MOVE_MEMBERS | Move a member to another voice channel or disconnect them |
| UPDATE_RTC_REGION | Change the channel's `rtc_region` |

[Modify guild member](/http-api/guild-members/#modify-guild-member) and [Modify current guild member](/http-api/guild-members/#modify-current-guild-member) apply mute, deafen, move and disconnect to every connection the member has in the channel. When a role or overwrite change removes SPEAK or STREAM, the media server mutes the affected tracks at once.

### Capacity

| Condition | Code |
| --- | --- |
| The channel has `user_limit` members (0 is no limit, and 25 is the limit while anyone has a camera on) | `VOICE_CHANNEL_FULL` |
| The member has `voice_connection_limit` connections in the channel (5 by default, 100 at most, 5 in a private call) | `VOICE_CONNECTION_LIMIT_REACHED` |
| 25 members already have a camera on | `VOICE_CAMERA_USER_LIMIT` |
| The member has an active timeout | `VOICE_MEMBER_TIMED_OUT` |
| An unclaimed account joins a one-on-one call or a voice channel in a guild it doesn't own | `VOICE_UNCLAIMED_ACCOUNT` |
| Voice encryption is on, everyone in the channel is `e2ee_capable`, and the session (not a bot) didn't identify with it | `VOICE_E2EE_REQUIRED` |
| The instance's `interaction_tier` is `enforced` and the account isn't [verified](/http-api/users/#account-tiers) | `VOICE_ACCOUNT_VERIFICATION_REQUIRED` |
| Fluxer couldn't issue a voice token | `VOICE_TOKEN_FAILED` |

A refused join sends no Voice Server Update. The session that asked gets a [Gateway Error](/gateway/opcodes-and-close-codes/#gateway-error) with the code, and the connection stays open.

### Regions

[List RTC regions](/http-api/channels/#list-rtc-regions) returns the regions the caller can pick for a channel, and the list can be empty. It leaves out [hidden regions](/admin-api/voice/#hidden-regions), which placement still uses. [Modify channel](/http-api/channels/#modify-channel) sets `rtc_region`, which needs the UPDATE_RTC_REGION permission, and a null region lets Fluxer pick one. Instance admins manage regions and voice servers through the [Admin voice resource](/admin-api/voice/), and [run more voice servers](/operator/voice-servers/).

## Private calls

A private call has no moderators, permission overwrites or moderator actions. The [Calls resource](/http-api/calls/) rings recipients and changes the region, and [Call Create](/gateway/events/#call-create), [Call Update](/gateway/events/#call-update) and [Call Delete](/gateway/events/#call-delete) send the call state. Joining and leaving use the Voice State Update command. A private call has no `bitrate` and runs at 64000.

A recipient's [incoming call flags](/http-api/users/#incoming-call-flags) decide whether the call rings for them. [Ring call recipients](/http-api/calls/#ring-call-recipients) skips recipients the flags exclude and still returns 204.

## Streams

A stream publishes screen share tracks on the member's existing connection, with no new connection or grant. The client sets `self_stream` on that connection with Voice State Update. A connection without STREAM always has `self_stream` and `self_video` false.

A viewer's Voice State Update lists the streams it watches in `viewer_stream_keys`, and moving channels clears the list. The [stream key](/http-api/streams/#stream-key) ends in the publisher's `connection_id`. The [Streams resource](/http-api/streams/) sets a stream's region and uploads and returns preview images.

## Entrance sounds

An account uploads [entrance sounds](/http-api/entrance-sounds/#clip-limits) and assigns one to each [scope](/http-api/entrance-sounds/#entrance-sound-scopes), and the client picks which one to play. [Play entrance sound](/http-api/entrance-sounds/#play-entrance-sound) needs a voice state in the channel and sends [ENTRANCE_SOUND_PLAY](/gateway/events/#entrance-sound-play) to every other account in it. Each of those accounts gets it on every session, and a client checks `channel_id` before playing it.

## Voice activity sharing

[Modify voice activity sharing](/http-api/users/settings/#modify-voice-activity-sharing) sets the default and changes every existing friendship to match. The values show as `share_voice_activity` and `friend_shares_voice_activity` on the [relationship object](/http-api/users/relationships/#relationship-object). The setting doesn't hide voice state, and every session that can view the channel still gets the [voice state](/gateway/events/#voice-state-object).
