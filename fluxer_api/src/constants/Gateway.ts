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

export type GatewayDispatchEvent =
	| 'READY'
	| 'RESUMED'
	| 'SESSIONS_REPLACE'
	| 'USER_UPDATE'
	| 'USER_PINNED_DMS_UPDATE'
	| 'USER_SETTINGS_UPDATE'
	| 'USER_GUILD_SETTINGS_UPDATE'
	| 'USER_NOTE_UPDATE'
	| 'RECENT_MENTION_DELETE'
	| 'SAVED_MESSAGE_CREATE'
	| 'SAVED_MESSAGE_DELETE'
	| 'FAVORITE_MEME_CREATE'
	| 'FAVORITE_MEME_UPDATE'
	| 'FAVORITE_MEME_DELETE'
	| 'AUTH_SESSION_CHANGE'
	| 'PRESENCE_UPDATE'
	| 'GUILD_CREATE'
	| 'GUILD_UPDATE'
	| 'GUILD_DELETE'
	| 'GUILD_MEMBER_ADD'
	| 'GUILD_MEMBER_UPDATE'
	| 'GUILD_MEMBER_REMOVE'
	| 'GUILD_ROLE_CREATE'
	| 'GUILD_ROLE_UPDATE'
	| 'GUILD_ROLE_UPDATE_BULK'
	| 'GUILD_ROLE_DELETE'
	| 'GUILD_EMOJIS_UPDATE'
	| 'GUILD_STICKERS_UPDATE'
	| 'GUILD_BAN_ADD'
	| 'GUILD_BAN_REMOVE'
	| 'CHANNEL_CREATE'
	| 'CHANNEL_UPDATE'
	| 'CHANNEL_UPDATE_BULK'
	| 'CHANNEL_DELETE'
	| 'CHANNEL_PINS_UPDATE'
	| 'CHANNEL_PINS_ACK'
	| 'CHANNEL_RECIPIENT_ADD'
	| 'CHANNEL_RECIPIENT_REMOVE'
	| 'MESSAGE_CREATE'
	| 'MESSAGE_UPDATE'
	| 'MESSAGE_DELETE'
	| 'MESSAGE_DELETE_BULK'
	| 'MESSAGE_REACTION_ADD'
	| 'MESSAGE_REACTION_REMOVE'
	| 'MESSAGE_REACTION_REMOVE_ALL'
	| 'MESSAGE_REACTION_REMOVE_EMOJI'
	| 'MESSAGE_ACK'
	| 'TYPING_START'
	| 'WEBHOOKS_UPDATE'
	| 'INVITE_CREATE'
	| 'INVITE_DELETE'
	| 'RELATIONSHIP_ADD'
	| 'RELATIONSHIP_UPDATE'
	| 'RELATIONSHIP_REMOVE'
	| 'VOICE_STATE_UPDATE'
	| 'VOICE_SERVER_UPDATE';
