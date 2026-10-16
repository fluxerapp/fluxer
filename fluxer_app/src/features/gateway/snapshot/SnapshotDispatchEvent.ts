// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelDeletePayload} from '@app/features/channel/events/ChannelDelete';
import type {ChannelPinsAckPayload} from '@app/features/channel/events/ChannelPinsAck';
import type {ChannelPinsUpdatePayload} from '@app/features/channel/events/ChannelPinsUpdate';
import type {ChannelRecipientAddPayload} from '@app/features/channel/events/ChannelRecipientAdd';
import type {ChannelRecipientRemovePayload} from '@app/features/channel/events/ChannelRecipientRemove';
import type {ChannelUpdatePayload} from '@app/features/channel/events/ChannelUpdate';
import type {ChannelUpdateBulkPayload} from '@app/features/channel/events/ChannelUpdateBulk';
import type {FavoriteMemeDeletePayload} from '@app/features/expressions/events/FavoriteMemeDelete';
import type {FavoriteMemeWire} from '@app/features/expressions/models/FavoriteMeme';
import type {ReadyPayload} from '@app/features/gateway/events/GatewayReady';
import type {
	SnapshotThreadDeletePayload,
	SnapshotThreadListSyncPayload,
	SnapshotThreadMembersUpdatePayload,
	SnapshotThreadMemberUpdatePayload,
	SnapshotThreadWire,
} from '@app/features/gateway/snapshot/SnapshotChannelReducer';
import type {GuildReadyData} from '@app/features/gateway/types/GatewayGuildTypes';
import type {PresenceRecord} from '@app/features/gateway/types/GatewayPresenceTypes';
import type {VoiceState} from '@app/features/gateway/types/GatewayVoiceTypes';
import type {GuildCountsUpdatePayload} from '@app/features/guild/events/GuildCountsUpdate';
import type {GuildDeletePayload} from '@app/features/guild/events/GuildDelete';
import type {GuildEmojisUpdatePayload} from '@app/features/guild/events/GuildEmojisUpdate';
import type {GuildMemberAddPayload} from '@app/features/guild/events/GuildMemberAdd';
import type {GuildMemberListUpdatePayload} from '@app/features/guild/events/GuildMemberListUpdate';
import type {GuildMemberRemovePayload} from '@app/features/guild/events/GuildMemberRemove';
import type {GuildMemberUpdatePayload} from '@app/features/guild/events/GuildMemberUpdate';
import type {GuildRoleCreatePayload} from '@app/features/guild/events/GuildRoleCreate';
import type {GuildRoleDeletePayload} from '@app/features/guild/events/GuildRoleDelete';
import type {GuildRoleUpdatePayload} from '@app/features/guild/events/GuildRoleUpdate';
import type {GuildRoleUpdateBulkPayload} from '@app/features/guild/events/GuildRoleUpdateBulk';
import type {GuildStickersUpdatePayload} from '@app/features/guild/events/GuildStickersUpdate';
import type {PassiveUpdatesPayload} from '@app/features/guild/events/PassiveUpdates';
import type {MessageAckPayload} from '@app/features/messaging/events/MessageAck';
import type {PresenceUpdateBulkPayload} from '@app/features/presence/events/PresenceUpdateBulk';
import type {RelationshipWire} from '@app/features/relationship/models/Relationship';
import type {AuthSessionChangePayload} from '@app/features/user/events/AuthSessionChange';
import type {UserNoteUpdatePayload} from '@app/features/user/events/UserNoteUpdate';
import type {UserSettingsPayload} from '@app/features/user/events/UserSettingsUpdate';
import type {UserUpdatePayload} from '@app/features/user/events/UserUpdate';
import type {GatewayGuildSettings} from '@app/features/user/state/UserGuildSettings';
import type {WebAuthnCredential} from '@app/features/user/state/WebAuthnCredentials';
import type {Channel as WireChannel} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {ConnectionListResponse} from '@fluxer/schema/src/domains/connection/ConnectionSchemas';
import type {Guild as WireGuild} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {Message} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';

export type SnapshotAuthSessionChangePayload = AuthSessionChangePayload;

export type SnapshotUserNoteUpdatePayload = UserNoteUpdatePayload;
export type SnapshotUserSettingsUpdatePayload = UserSettingsPayload;
export type SnapshotUserUpdatePayload = UserUpdatePayload;

export interface SnapshotUserConnectionsUpdatePayload {
	readonly connections: ConnectionListResponse;
}

export type SnapshotFavoriteMemeDeletePayload = FavoriteMemeDeletePayload;
export type SnapshotChannelUpdatePayload = ChannelUpdatePayload;
export type SnapshotChannelDeletePayload = ChannelDeletePayload;
export type SnapshotChannelPinsPayload = ChannelPinsAckPayload | ChannelPinsUpdatePayload;
export type SnapshotChannelRecipientPayload = ChannelRecipientAddPayload | ChannelRecipientRemovePayload;
export type SnapshotPassiveUpdatesPayload = PassiveUpdatesPayload;
export type SnapshotMessageCreatePayload = Message;
export type SnapshotMessageAckPayload = MessageAckPayload;

export type SnapshotGuildDeletePayload = GuildDeletePayload;
export type SnapshotGuildCountsUpdatePayload = GuildCountsUpdatePayload;
export type SnapshotGuildMemberListUpdatePayload = GuildMemberListUpdatePayload;
export type SnapshotGuildMemberPayload = GuildMemberAddPayload | GuildMemberUpdatePayload;
export type SnapshotGuildMemberRemovePayload = GuildMemberRemovePayload;

export interface SnapshotVoiceStateUpdatePayload extends Omit<VoiceState, 'guild_id' | 'channel_id' | 'connection_id'> {
	readonly guild_id?: string | null;
	readonly channel_id?: string | null;
	readonly connection_id?: string;
}

export interface SnapshotRelationshipRemovePayload {
	readonly id: string;
}

export interface SnapshotDispatchDataMap {
	readonly GUILD_CREATE: GuildReadyData;
	readonly GUILD_SYNC: GuildReadyData;
	readonly GUILD_UPDATE: WireGuild;
	readonly GUILD_DELETE: GuildDeletePayload;
	readonly GUILD_ROLE_CREATE: GuildRoleCreatePayload;
	readonly GUILD_ROLE_UPDATE: GuildRoleUpdatePayload;
	readonly GUILD_ROLE_DELETE: GuildRoleDeletePayload;
	readonly GUILD_ROLE_UPDATE_BULK: GuildRoleUpdateBulkPayload;
	readonly GUILD_EMOJIS_UPDATE: GuildEmojisUpdatePayload;
	readonly GUILD_STICKERS_UPDATE: GuildStickersUpdatePayload;
	readonly GUILD_COUNTS_UPDATE: GuildCountsUpdatePayload;
	readonly GUILD_MEMBER_LIST_UPDATE: GuildMemberListUpdatePayload;
	readonly GUILD_MEMBER_ADD: GuildMemberAddPayload;
	readonly GUILD_MEMBER_UPDATE: GuildMemberUpdatePayload;
	readonly GUILD_MEMBER_REMOVE: GuildMemberRemovePayload;
	readonly CHANNEL_CREATE: WireChannel;
	readonly CHANNEL_UPDATE: ChannelUpdatePayload;
	readonly CHANNEL_UPDATE_BULK: ChannelUpdateBulkPayload;
	readonly CHANNEL_DELETE: ChannelDeletePayload;
	readonly CHANNEL_PINS_UPDATE: ChannelPinsUpdatePayload;
	readonly CHANNEL_PINS_ACK: ChannelPinsAckPayload;
	readonly CHANNEL_RECIPIENT_ADD: ChannelRecipientAddPayload;
	readonly CHANNEL_RECIPIENT_REMOVE: ChannelRecipientRemovePayload;
	readonly PASSIVE_UPDATES: PassiveUpdatesPayload;
	readonly MESSAGE_CREATE: Message;
	readonly MESSAGE_ACK: MessageAckPayload;
	readonly AUTH_SESSION_CHANGE: AuthSessionChangePayload;
	readonly USER_SETTINGS_UPDATE: UserSettingsPayload;
	readonly USER_NOTE_UPDATE: UserNoteUpdatePayload;
	readonly USER_PINNED_DMS_UPDATE: ReadonlyArray<string>;
	readonly USER_CONNECTIONS_UPDATE: SnapshotUserConnectionsUpdatePayload;
	readonly WEBAUTHN_CREDENTIALS_UPDATE: ReadonlyArray<WebAuthnCredential>;
	readonly USER_GUILD_SETTINGS_UPDATE: GatewayGuildSettings;
	readonly RELATIONSHIP_ADD: RelationshipWire;
	readonly RELATIONSHIP_UPDATE: RelationshipWire;
	readonly RELATIONSHIP_REMOVE: SnapshotRelationshipRemovePayload;
	readonly USER_UPDATE: UserUpdatePayload;
	readonly PRESENCE_UPDATE: PresenceRecord;
	readonly PRESENCE_UPDATE_BULK: PresenceUpdateBulkPayload;
	readonly FAVORITE_MEME_CREATE: FavoriteMemeWire;
	readonly FAVORITE_MEME_UPDATE: FavoriteMemeWire;
	readonly FAVORITE_MEME_DELETE: FavoriteMemeDeletePayload;
	readonly VOICE_STATE_UPDATE: SnapshotVoiceStateUpdatePayload;
	readonly THREAD_CREATE: SnapshotThreadWire;
	readonly THREAD_UPDATE: SnapshotThreadWire;
	readonly THREAD_DELETE: SnapshotThreadDeletePayload;
	readonly THREAD_LIST_SYNC: SnapshotThreadListSyncPayload;
	readonly THREAD_MEMBER_UPDATE: SnapshotThreadMemberUpdatePayload;
	readonly THREAD_MEMBERS_UPDATE: SnapshotThreadMembersUpdatePayload;
}

export const SnapshotDispatchEvent = Object.freeze({
	GUILD_CREATE: 'GUILD_CREATE',
	GUILD_SYNC: 'GUILD_SYNC',
	GUILD_UPDATE: 'GUILD_UPDATE',
	GUILD_DELETE: 'GUILD_DELETE',
	GUILD_ROLE_CREATE: 'GUILD_ROLE_CREATE',
	GUILD_ROLE_UPDATE: 'GUILD_ROLE_UPDATE',
	GUILD_ROLE_DELETE: 'GUILD_ROLE_DELETE',
	GUILD_ROLE_UPDATE_BULK: 'GUILD_ROLE_UPDATE_BULK',
	GUILD_EMOJIS_UPDATE: 'GUILD_EMOJIS_UPDATE',
	GUILD_STICKERS_UPDATE: 'GUILD_STICKERS_UPDATE',
	GUILD_COUNTS_UPDATE: 'GUILD_COUNTS_UPDATE',
	GUILD_MEMBER_LIST_UPDATE: 'GUILD_MEMBER_LIST_UPDATE',
	GUILD_MEMBER_ADD: 'GUILD_MEMBER_ADD',
	GUILD_MEMBER_UPDATE: 'GUILD_MEMBER_UPDATE',
	GUILD_MEMBER_REMOVE: 'GUILD_MEMBER_REMOVE',
	CHANNEL_CREATE: 'CHANNEL_CREATE',
	CHANNEL_UPDATE: 'CHANNEL_UPDATE',
	CHANNEL_UPDATE_BULK: 'CHANNEL_UPDATE_BULK',
	CHANNEL_DELETE: 'CHANNEL_DELETE',
	CHANNEL_PINS_UPDATE: 'CHANNEL_PINS_UPDATE',
	CHANNEL_PINS_ACK: 'CHANNEL_PINS_ACK',
	CHANNEL_RECIPIENT_ADD: 'CHANNEL_RECIPIENT_ADD',
	CHANNEL_RECIPIENT_REMOVE: 'CHANNEL_RECIPIENT_REMOVE',
	PASSIVE_UPDATES: 'PASSIVE_UPDATES',
	MESSAGE_CREATE: 'MESSAGE_CREATE',
	MESSAGE_ACK: 'MESSAGE_ACK',
	AUTH_SESSION_CHANGE: 'AUTH_SESSION_CHANGE',
	USER_SETTINGS_UPDATE: 'USER_SETTINGS_UPDATE',
	USER_NOTE_UPDATE: 'USER_NOTE_UPDATE',
	USER_PINNED_DMS_UPDATE: 'USER_PINNED_DMS_UPDATE',
	USER_CONNECTIONS_UPDATE: 'USER_CONNECTIONS_UPDATE',
	WEBAUTHN_CREDENTIALS_UPDATE: 'WEBAUTHN_CREDENTIALS_UPDATE',
	USER_GUILD_SETTINGS_UPDATE: 'USER_GUILD_SETTINGS_UPDATE',
	RELATIONSHIP_ADD: 'RELATIONSHIP_ADD',
	RELATIONSHIP_UPDATE: 'RELATIONSHIP_UPDATE',
	RELATIONSHIP_REMOVE: 'RELATIONSHIP_REMOVE',
	USER_UPDATE: 'USER_UPDATE',
	PRESENCE_UPDATE: 'PRESENCE_UPDATE',
	PRESENCE_UPDATE_BULK: 'PRESENCE_UPDATE_BULK',
	FAVORITE_MEME_CREATE: 'FAVORITE_MEME_CREATE',
	FAVORITE_MEME_UPDATE: 'FAVORITE_MEME_UPDATE',
	FAVORITE_MEME_DELETE: 'FAVORITE_MEME_DELETE',
	VOICE_STATE_UPDATE: 'VOICE_STATE_UPDATE',
	THREAD_CREATE: 'THREAD_CREATE',
	THREAD_UPDATE: 'THREAD_UPDATE',
	THREAD_DELETE: 'THREAD_DELETE',
	THREAD_LIST_SYNC: 'THREAD_LIST_SYNC',
	THREAD_MEMBER_UPDATE: 'THREAD_MEMBER_UPDATE',
	THREAD_MEMBERS_UPDATE: 'THREAD_MEMBERS_UPDATE',
} as const);

export type SnapshotDispatchType = (typeof SnapshotDispatchEvent)[keyof typeof SnapshotDispatchEvent];

export type SnapshotDispatch = {
	readonly [Type in SnapshotDispatchType]: {
		readonly type: Type;
		readonly data: SnapshotDispatchDataMap[Type];
	};
}[SnapshotDispatchType];

export type SnapshotGatewayEvent = {readonly type: 'READY'; readonly data: ReadyPayload} | SnapshotDispatch;

const SNAPSHOT_DISPATCH_TYPES: ReadonlySet<string> = new Set(Object.values(SnapshotDispatchEvent));

const SNAPSHOT_ARRAY_DISPATCH_TYPES: ReadonlySet<SnapshotDispatchType> = new Set([
	'USER_PINNED_DMS_UPDATE',
	'WEBAUTHN_CREDENTIALS_UPDATE',
]);

export function isSnapshotDispatchType(type: string): type is SnapshotDispatchType {
	return SNAPSHOT_DISPATCH_TYPES.has(type);
}

export function parseSnapshotGatewayEvent(type: string, data: unknown): SnapshotGatewayEvent | null {
	if (type !== 'READY' && !isSnapshotDispatchType(type)) {
		return null;
	}
	const expectsArray = type !== 'READY' && SNAPSHOT_ARRAY_DISPATCH_TYPES.has(type);
	if (expectsArray ? !Array.isArray(data) : data === null || typeof data !== 'object' || Array.isArray(data)) {
		throw new Error(`Gateway ${type} payload has an invalid top-level shape`);
	}
	return {type, data} as SnapshotGatewayEvent;
}
