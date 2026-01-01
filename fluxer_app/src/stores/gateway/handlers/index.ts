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

import type {GatewaySocket} from '~/lib/GatewaySocket';

export interface GatewayHandlerContext {
	socket: GatewaySocket | null;
	previousSessionId: string | null;
	setPreviousSessionId: (id: string) => void;
	setReady: () => void;
}

export type GatewayEventHandler<T = unknown> = (data: T, context: GatewayHandlerContext) => void;

export type GatewayHandlerRegistry = Map<string, GatewayEventHandler>;

import {handleCallCreate} from './call/callCreate';
import {handleCallDelete} from './call/callDelete';
import {handleCallUpdate} from './call/callUpdate';
import {handleChannelCreate} from './channel/channelCreate';
import {handleChannelDelete} from './channel/channelDelete';
import {handleChannelPinsAck} from './channel/channelPinsAck';
import {handleChannelPinsUpdate} from './channel/channelPinsUpdate';
import {handleChannelRecipientAdd} from './channel/channelRecipientAdd';
import {handleChannelRecipientRemove} from './channel/channelRecipientRemove';
import {handleChannelUpdate} from './channel/channelUpdate';
import {handleChannelUpdateBulk} from './channel/channelUpdateBulk';
import {handleGuildBan} from './guild/guildBan';
import {handleGuildCreate} from './guild/guildCreate';
import {handleGuildDelete} from './guild/guildDelete';
import {handleGuildEmojisUpdate} from './guild/guildEmojisUpdate';
import {handleGuildMemberAdd} from './guild/guildMemberAdd';
import {handleGuildMemberListUpdate} from './guild/guildMemberListUpdate';
import {handleGuildMemberRemove} from './guild/guildMemberRemove';
import {handleGuildMembersChunk} from './guild/guildMembersChunk';
import {handleGuildMemberUpdate} from './guild/guildMemberUpdate';
import {handleGuildRoleCreate} from './guild/guildRoleCreate';
import {handleGuildRoleDelete} from './guild/guildRoleDelete';
import {handleGuildRoleUpdate} from './guild/guildRoleUpdate';
import {handleGuildRoleUpdateBulk} from './guild/guildRoleUpdateBulk';
import {handleGuildStickersUpdate} from './guild/guildStickersUpdate';
import {handleGuildSync} from './guild/guildSync';
import {handleGuildUpdate} from './guild/guildUpdate';
import {handlePassiveUpdates} from './guild/passiveUpdates';
import {handleInviteCreate} from './invite/inviteCreate';
import {handleInviteDelete} from './invite/inviteDelete';
import {handleMessageAck} from './message/messageAck';
import {handleMessageCreate} from './message/messageCreate';
import {handleMessageDelete} from './message/messageDelete';
import {handleMessageDeleteBulk} from './message/messageDeleteBulk';
import {handleMessageReactionAdd} from './message/messageReactionAdd';
import {handleMessageReactionRemove} from './message/messageReactionRemove';
import {handleMessageReactionRemoveAll} from './message/messageReactionRemoveAll';
import {handleMessageReactionRemoveEmoji} from './message/messageReactionRemoveEmoji';
import {handleMessageUpdate} from './message/messageUpdate';
import {handleRecentMentionDelete} from './message/recentMentionDelete';
import {handleSavedMessageCreate} from './message/savedMessageCreate';
import {handleSavedMessageDelete} from './message/savedMessageDelete';
import {handleTypingStart} from './message/typingStart';
import {handleFavoriteMemeCreate} from './misc/favoriteMemeCreate';
import {handleFavoriteMemeDelete} from './misc/favoriteMemeDelete';
import {handleFavoriteMemeUpdate} from './misc/favoriteMemeUpdate';
import {handleWebhooksUpdate} from './misc/webhooksUpdate';
import {handlePresenceUpdate} from './presence/presenceUpdate';
import {handlePresenceUpdateBulk} from './presence/presenceUpdateBulk';
import {handleReady} from './ready';
import {handleRelationshipAdd} from './relationship/relationshipAdd';
import {handleRelationshipRemove} from './relationship/relationshipRemove';
import {handleRelationshipUpdate} from './relationship/relationshipUpdate';
import {handleResumed} from './resumed';
import {handleAuthSessionChange} from './user/authSessionChange';
import {handleUserGuildSettingsUpdate} from './user/userGuildSettingsUpdate';
import {handleUserNoteUpdate} from './user/userNoteUpdate';
import {handleUserPinnedDmsUpdate} from './user/userPinnedDmsUpdate';
import {handleUserSettingsUpdate} from './user/userSettingsUpdate';
import {handleUserUpdate} from './user/userUpdate';
import {handleVoiceServerUpdate} from './voice/voiceServerUpdate';
import {handleVoiceStateUpdate} from './voice/voiceStateUpdate';

export function createHandlerRegistry(): GatewayHandlerRegistry {
	const registry: GatewayHandlerRegistry = new Map();

	registry.set('READY', handleReady as GatewayEventHandler);
	registry.set('RESUMED', handleResumed as GatewayEventHandler);

	registry.set('AUTH_SESSION_CHANGE', handleAuthSessionChange as GatewayEventHandler);
	registry.set('USER_UPDATE', handleUserUpdate as GatewayEventHandler);
	registry.set('USER_SETTINGS_UPDATE', handleUserSettingsUpdate as GatewayEventHandler);
	registry.set('USER_GUILD_SETTINGS_UPDATE', handleUserGuildSettingsUpdate as GatewayEventHandler);
	registry.set('USER_PINNED_DMS_UPDATE', handleUserPinnedDmsUpdate as GatewayEventHandler);
	registry.set('USER_NOTE_UPDATE', handleUserNoteUpdate as GatewayEventHandler);

	registry.set('GUILD_CREATE', handleGuildCreate as GatewayEventHandler);
	registry.set('GUILD_UPDATE', handleGuildUpdate as GatewayEventHandler);
	registry.set('GUILD_DELETE', handleGuildDelete as GatewayEventHandler);
	registry.set('GUILD_BAN_ADD', handleGuildBan as GatewayEventHandler);
	registry.set('GUILD_BAN_REMOVE', handleGuildBan as GatewayEventHandler);
	registry.set('GUILD_EMOJIS_UPDATE', handleGuildEmojisUpdate as GatewayEventHandler);
	registry.set('GUILD_STICKERS_UPDATE', handleGuildStickersUpdate as GatewayEventHandler);
	registry.set('GUILD_SYNC', handleGuildSync as GatewayEventHandler);
	registry.set('GUILD_MEMBER_ADD', handleGuildMemberAdd as GatewayEventHandler);
	registry.set('GUILD_MEMBER_UPDATE', handleGuildMemberUpdate as GatewayEventHandler);
	registry.set('GUILD_MEMBER_REMOVE', handleGuildMemberRemove as GatewayEventHandler);
	registry.set('GUILD_MEMBERS_CHUNK', handleGuildMembersChunk as GatewayEventHandler);
	registry.set('GUILD_MEMBER_LIST_UPDATE', handleGuildMemberListUpdate as GatewayEventHandler);
	registry.set('GUILD_ROLE_CREATE', handleGuildRoleCreate as GatewayEventHandler);
	registry.set('GUILD_ROLE_UPDATE', handleGuildRoleUpdate as GatewayEventHandler);
	registry.set('GUILD_ROLE_DELETE', handleGuildRoleDelete as GatewayEventHandler);
	registry.set('GUILD_ROLE_UPDATE_BULK', handleGuildRoleUpdateBulk as GatewayEventHandler);

	registry.set('CHANNEL_CREATE', handleChannelCreate as GatewayEventHandler);
	registry.set('CHANNEL_UPDATE', handleChannelUpdate as GatewayEventHandler);
	registry.set('CHANNEL_UPDATE_BULK', handleChannelUpdateBulk as GatewayEventHandler);
	registry.set('CHANNEL_DELETE', handleChannelDelete as GatewayEventHandler);
	registry.set('PASSIVE_UPDATES', handlePassiveUpdates as GatewayEventHandler);
	registry.set('CHANNEL_PINS_UPDATE', handleChannelPinsUpdate as GatewayEventHandler);
	registry.set('CHANNEL_PINS_ACK', handleChannelPinsAck as GatewayEventHandler);
	registry.set('CHANNEL_RECIPIENT_ADD', handleChannelRecipientAdd as GatewayEventHandler);
	registry.set('CHANNEL_RECIPIENT_REMOVE', handleChannelRecipientRemove as GatewayEventHandler);

	registry.set('MESSAGE_CREATE', handleMessageCreate as GatewayEventHandler);
	registry.set('MESSAGE_UPDATE', handleMessageUpdate as GatewayEventHandler);
	registry.set('MESSAGE_DELETE', handleMessageDelete as GatewayEventHandler);
	registry.set('MESSAGE_DELETE_BULK', handleMessageDeleteBulk as GatewayEventHandler);
	registry.set('MESSAGE_ACK', handleMessageAck as GatewayEventHandler);
	registry.set('MESSAGE_REACTION_ADD', handleMessageReactionAdd as GatewayEventHandler);
	registry.set('MESSAGE_REACTION_REMOVE', handleMessageReactionRemove as GatewayEventHandler);
	registry.set('MESSAGE_REACTION_REMOVE_ALL', handleMessageReactionRemoveAll as GatewayEventHandler);
	registry.set('MESSAGE_REACTION_REMOVE_EMOJI', handleMessageReactionRemoveEmoji as GatewayEventHandler);
	registry.set('TYPING_START', handleTypingStart as GatewayEventHandler);
	registry.set('RECENT_MENTION_DELETE', handleRecentMentionDelete as GatewayEventHandler);
	registry.set('SAVED_MESSAGE_CREATE', handleSavedMessageCreate as GatewayEventHandler);
	registry.set('SAVED_MESSAGE_DELETE', handleSavedMessageDelete as GatewayEventHandler);

	registry.set('PRESENCE_UPDATE', handlePresenceUpdate as GatewayEventHandler);
	registry.set('PRESENCE_UPDATE_BULK', handlePresenceUpdateBulk as GatewayEventHandler);

	registry.set('VOICE_STATE_UPDATE', handleVoiceStateUpdate as GatewayEventHandler);
	registry.set('VOICE_SERVER_UPDATE', handleVoiceServerUpdate as GatewayEventHandler);

	registry.set('CALL_CREATE', handleCallCreate as GatewayEventHandler);
	registry.set('CALL_UPDATE', handleCallUpdate as GatewayEventHandler);
	registry.set('CALL_DELETE', handleCallDelete as GatewayEventHandler);

	registry.set('INVITE_CREATE', handleInviteCreate as GatewayEventHandler);
	registry.set('INVITE_DELETE', handleInviteDelete as GatewayEventHandler);

	registry.set('RELATIONSHIP_ADD', handleRelationshipAdd as GatewayEventHandler);
	registry.set('RELATIONSHIP_UPDATE', handleRelationshipUpdate as GatewayEventHandler);
	registry.set('RELATIONSHIP_REMOVE', handleRelationshipRemove as GatewayEventHandler);

	registry.set('WEBHOOKS_UPDATE', handleWebhooksUpdate as GatewayEventHandler);
	registry.set('FAVORITE_MEME_CREATE', handleFavoriteMemeCreate as GatewayEventHandler);
	registry.set('FAVORITE_MEME_UPDATE', handleFavoriteMemeUpdate as GatewayEventHandler);
	registry.set('FAVORITE_MEME_DELETE', handleFavoriteMemeDelete as GatewayEventHandler);

	registry.set('SESSIONS_REPLACE', () => {});

	return registry;
}
