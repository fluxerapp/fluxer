# Events

All events are dispatched as op 0 (`dispatch`) frames. The `"t"` field carries the event name as an uppercase string; `"d"` carries the payload; `"s"` is the monotonically increasing sequence number used for [Resume](session-lifecycle.md#resume-flow).

Event names are normalised internally via `event_atoms:normalize/1`. See [shared-utilities.md](shared-utilities.md#event_atomserl) for how unknown event names are handled.

For how events travel through the dispatch pipeline once they reach a guild process, see [event-dispatch-pipeline.md](event-dispatch-pipeline.md).

---

## Session

Events sent once on connect or reconnect, or to synchronise session-level state.

| Event | Description |
|---|---|
| `READY` | Sent after a successful Identify. Contains the initial guild list, user data, session ID, and timing metadata under `_timings_gw`. |
| `RESUMED` | Sent after a successful Resume. Contains missed events replayed in sequence, followed by this marker. Also carries `_timings_gw`. |
| `SESSIONS_REPLACE` | Replaces the full list of active sessions for the current user across all devices. |
| `AUTH_SESSION_CHANGE` | Notifies the client that the auth session has changed (e.g. password reset). |

## Presence

| Event | Description |
|---|---|
| `PRESENCE_UPDATE` | A user's online status, activities, or custom status changed. Sent to sessions that are subscribed to that user's presence. |
| `TYPING_START` | A user started typing in a channel. |

## Relationships

| Event | Description |
|---|---|
| `RELATIONSHIP_ADD` | A friend relationship was created or a friend request was sent/received. |
| `RELATIONSHIP_UPDATE` | An existing relationship changed state. |
| `RELATIONSHIP_REMOVE` | A relationship was removed. |

## Channels

| Event | Description |
|---|---|
| `CHANNEL_CREATE` | A new channel was created in a guild, or a DM channel was opened. |
| `CHANNEL_UPDATE` | A channel's settings changed. |
| `CHANNEL_UPDATE_BULK` | Multiple channels in a guild were updated atomically (e.g. after a category reorder). |
| `CHANNEL_DELETE` | A channel was deleted. Visibility is evaluated against the pre-delete state so only users who could see the channel receive this event. |
| `CHANNEL_PINS_UPDATE` | The pinned messages in a channel changed. |
| `CHANNEL_PINS_ACK` | The client acknowledged pinned messages for a channel. |
| `CHANNEL_RECIPIENT_ADD` | A user was added to a group DM. |
| `CHANNEL_RECIPIENT_REMOVE` | A user was removed from a group DM. |

## Guilds

| Event | Description |
|---|---|
| `GUILD_CREATE` | Sent on connect for each guild the user belongs to. Contains full guild state: channels, roles, members, voice states. |
| `GUILD_UPDATE` | Guild settings changed (name, icon, features, etc.). Always dispatched even for guilds with unavailability flags. |
| `GUILD_DELETE` | The user left or was removed from a guild, or the guild was deleted. |
| `GUILD_MEMBER_ADD` | A user joined the guild. |
| `GUILD_MEMBER_UPDATE` | A member's roles, nickname, or other per-guild properties changed. |
| `GUILD_MEMBER_REMOVE` | A member left, was kicked, or was banned. |
| `GUILD_MEMBER_LIST_UPDATE` | An incremental update to the sorted member list (used by the member sidebar). |
| `GUILD_BAN_ADD` | A user was banned from the guild. |
| `GUILD_BAN_REMOVE` | A ban was lifted. |
| `GUILD_ROLE_CREATE` | A new role was created. |
| `GUILD_ROLE_UPDATE` | A role's properties changed. |
| `GUILD_ROLE_UPDATE_BULK` | Multiple roles changed in a single operation. |
| `GUILD_ROLE_DELETE` | A role was deleted. |
| `GUILD_EMOJIS_UPDATE` | The guild's custom emoji list changed. |
| `GUILD_STICKERS_UPDATE` | The guild's custom sticker list changed. |
| `GUILD_AUDIT_LOG_ENTRY_CREATE` | A new audit log entry was created. Only dispatched to sessions whose user has the `VIEW_AUDIT_LOG` permission. |

## Messages

| Event | Description |
|---|---|
| `MESSAGE_CREATE` | A new message was sent. Only dispatched to sessions that can view the channel. |
| `MESSAGE_UPDATE` | A message was edited. |
| `MESSAGE_DELETE` | A single message was deleted. |
| `MESSAGE_DELETE_BULK` | Multiple messages were deleted at once. |
| `MESSAGE_ACK` | The client acknowledged reading up to a message. Used to update unread state across sessions. |
| `MESSAGE_REACTION_ADD` | A reaction was added to a message. The originating session's `session_id` is stripped before dispatch to avoid echoing back to the sender. |
| `MESSAGE_REACTION_ADD_MANY` | Multiple reactions were added in bulk. |
| `MESSAGE_REACTION_REMOVE` | A reaction was removed. Same session stripping as `MESSAGE_REACTION_ADD`. |
| `MESSAGE_REACTION_REMOVE_ALL` | All reactions were cleared from a message. |
| `MESSAGE_REACTION_REMOVE_EMOJI` | All reactions for a specific emoji were removed. |

## Voice

| Event | Description |
|---|---|
| `VOICE_STATE_UPDATE` | A user joined, moved, or left a voice channel, or their mute/deaf/video state changed. |
| `VOICE_SERVER_UPDATE` | Sent to the joining user with the LiveKit endpoint and token needed to connect. Also sent during a channel move. |

## Calls (DM voice)

Calls use the same `VOICE_STATE_UPDATE` and `VOICE_SERVER_UPDATE` events as guild voice, but are managed by the `call` gen_server rather than a guild process. See [calls.md](calls.md) and [voice.md](voice.md) for details.

## User settings

Events sent only to the authenticated user's own sessions.

| Event | Description |
|---|---|
| `USER_UPDATE` | The user's own account data changed (username, avatar, etc.). |
| `USER_SETTINGS_UPDATE` | The user's client settings changed. |
| `USER_GUILD_SETTINGS_UPDATE` | The user's per-guild notification settings changed. |
| `USER_NOTE_UPDATE` | The user's note on another user changed. |
| `USER_CONNECTIONS_UPDATE` | The user's linked external accounts changed. |
| `USER_PINNED_DMS_UPDATE` | The user's pinned DM list changed. |
| `WEBAUTHN_CREDENTIALS_UPDATE` | The user's WebAuthn credentials changed. |

## Invites

| Event | Description |
|---|---|
| `INVITE_CREATE` | A new invite was created. Only dispatched to sessions whose user has the `MANAGE_CHANNELS` permission for the invite's channel. |
| `INVITE_DELETE` | An invite was revoked. Same permission filter as `INVITE_CREATE`. |

## Miscellaneous

| Event | Description |
|---|---|
| `ENTRANCE_SOUND_PLAY` | Triggers an entrance sound effect for a user joining a voice channel. |
| `FAVORITE_MEME_CREATE` | A meme was added to the user's favourites. |
| `FAVORITE_MEME_UPDATE` | A favourited meme was updated. |
| `FAVORITE_MEME_DELETE` | A meme was removed from the user's favourites. |
| `SAVED_MESSAGE_CREATE` | A message was saved. |
| `SAVED_MESSAGE_DELETE` | A saved message was removed. |
| `RECENT_MENTION_DELETE` | A recent mention was dismissed. |

---

## Visibility and filtering

Not every session receives every event. The dispatch pipeline applies per-session visibility checks before delivery:

- **Channel-scoped events**: only sessions whose user can `view_channel` for the relevant channel receive the event. `CHANNEL_DELETE` uses pre-deletion state for this check.
- **Audit log events**: only sessions with `VIEW_AUDIT_LOG` permission.
- **Invite events**: only sessions with `MANAGE_CHANNELS` permission for the channel.
- **Presence events**: only sessions subscribed to the user via `presence_bus`.
- **User-settings events**: only the authenticated user's own sessions.

See [event-dispatch-pipeline.md](event-dispatch-pipeline.md) for the full filtering pipeline and [permissions.md](permissions.md) for how permission bits are evaluated.
