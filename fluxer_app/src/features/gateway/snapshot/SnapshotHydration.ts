// SPDX-License-Identifier: AGPL-3.0-or-later

import AccountAccess from '@app/features/auth/state/AccountAccess';
import AuthSession from '@app/features/auth/state/AuthSession';
import type {ChannelWire} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import UserConnection from '@app/features/connection/state/UserConnection';
import Emoji from '@app/features/emoji/state/Emoji';
import Sticker from '@app/features/emoji/state/EmojiSticker';
import FavoriteMemes from '@app/features/expressions/state/FavoriteMemes';
import {
	parseSnapshotEntity,
	parseSnapshotGuildMemberKey,
	parseSnapshotGuildRoleKey,
	parseSnapshotSingleton,
	SNAPSHOT_SCHEMA_EPOCH,
	type SnapshotAccountMetadataRow,
	type SnapshotEntityRowMap,
	type SnapshotGuildMemberKey,
	type SnapshotGuildRoleKey,
	type SnapshotPresenceRow,
	type SnapshotReadStateRow,
	type SnapshotUnavailableGuildRow,
	type SnapshotUserRow,
	snapshotUserKey,
} from '@app/features/gateway/snapshot/SnapshotEntities';
import SnapshotSync, {
	disposeAndEvictDesktopSnapshot,
	isDesktopSnapshotAvailable,
} from '@app/features/gateway/snapshot/SnapshotSync';
import type {StateSnapshotEntries, StateSyncCursor} from '@app/features/gateway/snapshot/SnapshotTypes';
import type {GuildReadyData} from '@app/features/gateway/types/GatewayGuildTypes';
import type {VoiceState} from '@app/features/gateway/types/GatewayVoiceTypes';
import GuildAvailability from '@app/features/guild/state/GuildAvailability';
import GuildCount from '@app/features/guild/state/GuildCount';
import GuildList from '@app/features/guild/state/GuildList';
import GuildReadState from '@app/features/guild/state/GuildReadState';
import Guilds from '@app/features/guild/state/Guilds';
import GuildVerification from '@app/features/guild/state/GuildVerification';
import GuildMembers from '@app/features/member/state/GuildMembers';
import MemberSearch from '@app/features/member/state/MemberSearch';
import Permission from '@app/features/permissions/state/Permission';
import SessionManager from '@app/features/platform/state/AuthSession';
import {Logger} from '@app/features/platform/utils/AppLogger';
import Presence from '@app/features/presence/state/Presence';
import ReadStates from '@app/features/read_state/state/ReadStates';
import type {RelationshipWire} from '@app/features/relationship/models/Relationship';
import Relationships from '@app/features/relationship/state/Relationships';
import ChannelThreads from '@app/features/threads/state/ChannelThreads';
import ThreadGuilds from '@app/features/threads/state/ThreadGuilds';
import UserGuildSettings, {type GatewayGuildSettings} from '@app/features/user/state/UserGuildSettings';
import UserNote from '@app/features/user/state/UserNote';
import UserPinnedDM from '@app/features/user/state/UserPinnedDM';
import UserSettings from '@app/features/user/state/UserSettings';
import Users from '@app/features/user/state/Users';
import WebAuthnCredentials from '@app/features/user/state/WebAuthnCredentials';
import RtcRegions from '@app/features/voice/state/RtcRegions';
import {seedVoiceStatesFromReady} from '@app/features/voice/VoiceGatewayLifecycle';
import {THREAD_CHANNEL_TYPES} from '@fluxer/constants/src/ThreadConstants';
import type {RtcRegionResponse, Channel as WireChannel} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {ConnectionListResponse} from '@fluxer/schema/src/domains/connection/ConnectionSchemas';
import type {
	GuildEmoji as WireGuildEmoji,
	GuildSticker as WireGuildSticker,
} from '@fluxer/schema/src/domains/guild/GuildEmojiSchemas';
import type {GuildMemberData} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {GuildRole as WireGuildRole} from '@fluxer/schema/src/domains/guild/GuildRoleSchemas';
import type {UserPrivate} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {runInAction} from 'mobx';

interface ParsedSnapshot {
	readonly selfUser: UserPrivate;
	readonly users: Array<SnapshotUserRow>;
	readonly guilds: Array<GuildReadyData>;
	readonly unavailableGuilds: Array<SnapshotUnavailableGuildRow>;
	readonly channels: Array<WireChannel>;
	readonly relationships: Array<RelationshipWire>;
	readonly userSettings: SnapshotEntityRowMap['user_settings'] | null;
	readonly accountMetadata: SnapshotAccountMetadataRow;
	readonly userConnections: ConnectionListResponse | null;
	readonly userGuildSettings: Array<GatewayGuildSettings>;
	readonly readStates: Map<string, SnapshotReadStateRow>;
	readonly guildEmojis: Array<{guildId: string; emojis: ReadonlyArray<WireGuildEmoji>}>;
	readonly guildStickers: Array<{guildId: string; stickers: ReadonlyArray<WireGuildSticker>}>;
	readonly presences: Map<string, SnapshotPresenceRow>;
	readonly voiceStates: Map<string, ReadonlyArray<VoiceState>>;
}

export interface SnapshotHydrationDependencies {
	readonly readSnapshot: (storageKey: string) => Promise<StateSnapshotEntries> | null;
	readonly readCursor: (storageKey: string) => Promise<StateSyncCursor | null> | null;
	readonly resolveSelfUserId: (storageKey: string) => string | null | Promise<string | null>;
	readonly resolveCheckedSelfUser: (storageKey: string) => UserPrivate | null;
	readonly evictSnapshot: (storageKey: string) => void | Promise<void>;
}

class SnapshotHydrationUnavailableError extends Error {
	constructor(storageKey: string) {
		super(`Snapshot hydration is unavailable for ${storageKey}`);
		this.name = 'SnapshotHydrationUnavailableError';
	}
}

const logger = new Logger('SnapshotHydration');

const defaultDependencies: SnapshotHydrationDependencies = {
	readSnapshot: (storageKey) => SnapshotSync.readSnapshot(storageKey),
	readCursor: (storageKey) => SnapshotSync.readCursor(storageKey),
	resolveSelfUserId: (storageKey) => SessionManager.getAccount(storageKey)?.userId ?? null,
	resolveCheckedSelfUser: (storageKey) => AccountAccess.getCheckedUser(storageKey),
	evictSnapshot: (storageKey) => disposeAndEvictDesktopSnapshot(storageKey),
};

let hydrationEpoch = 0;

function groupRolesByGuild(roleRows: Map<SnapshotGuildRoleKey, WireGuildRole>): Map<string, Array<WireGuildRole>> {
	const rolesByGuild = new Map<string, Array<WireGuildRole>>();
	for (const [key, role] of roleRows) {
		const {guildId} = parseSnapshotGuildRoleKey(key);
		let roles = rolesByGuild.get(guildId);
		if (roles == null) {
			roles = [];
			rolesByGuild.set(guildId, roles);
		}
		roles.push(role);
	}
	return rolesByGuild;
}

function groupMembersByGuild(
	memberRows: Map<SnapshotGuildMemberKey, GuildMemberData>,
): Map<string, Array<GuildMemberData>> {
	const membersByGuild = new Map<string, Array<GuildMemberData>>();
	for (const [key, member] of memberRows) {
		const {guildId} = parseSnapshotGuildMemberKey(key);
		let members = membersByGuild.get(guildId);
		if (members == null) {
			members = [];
			membersByGuild.set(guildId, members);
		}
		members.push(member);
	}
	return membersByGuild;
}

function parseSnapshot(entries: StateSnapshotEntries, selfUserId: string): ParsedSnapshot {
	const usersById = parseSnapshotEntity(entries, 'user');
	const selfUser = usersById.get(snapshotUserKey(selfUserId));
	if (selfUser == null || selfUser.id !== selfUserId) {
		throw new Error(`Snapshot is missing private self user ${selfUserId}`);
	}
	if (!('email' in selfUser)) {
		throw new Error(`Snapshot self user ${selfUserId} is not a private user record`);
	}
	const accountMetadata = parseSnapshotSingleton(entries, 'account_metadata');
	if (accountMetadata == null) {
		throw new Error('Snapshot is missing account metadata');
	}
	const rolesByGuild = groupRolesByGuild(parseSnapshotEntity(entries, 'guild_role'));
	const membersByGuild = groupMembersByGuild(parseSnapshotEntity(entries, 'guild_member'));
	const channels: Array<WireChannel> = [];
	const threadsByGuild = new Map<string, Array<ChannelWire>>();
	for (const channel of parseSnapshotEntity(entries, 'channel').values()) {
		if (!THREAD_CHANNEL_TYPES.has(channel.type) || channel.guild_id == null) {
			channels.push(channel);
			continue;
		}
		let threads = threadsByGuild.get(channel.guild_id);
		if (threads == null) {
			threads = [];
			threadsByGuild.set(channel.guild_id, threads);
		}
		threads.push(channel);
	}
	const guilds: Array<GuildReadyData> = [];
	for (const [guildId, row] of parseSnapshotEntity(entries, 'guild')) {
		if (row.joined_at == null) {
			throw new Error(`Snapshot guild ${guildId} is missing joined_at`);
		}
		guilds.push({
			id: row.id,
			properties: row.properties,
			channels: [],
			emojis: [],
			members: membersByGuild.get(guildId) ?? [],
			member_count: row.member_count,
			online_count: row.online_count,
			roles: rolesByGuild.get(guildId) ?? [],
			joined_at: row.joined_at,
			unavailable: false,
			...(row.threads_active === true ? {threads: threadsByGuild.get(guildId) ?? []} : {}),
		});
	}
	const guildEmojis: Array<{guildId: string; emojis: ReadonlyArray<WireGuildEmoji>}> = [];
	for (const [guildId, emojis] of parseSnapshotEntity(entries, 'guild_emoji')) {
		guildEmojis.push({guildId, emojis});
	}
	const guildStickers: Array<{guildId: string; stickers: ReadonlyArray<WireGuildSticker>}> = [];
	for (const [guildId, stickers] of parseSnapshotEntity(entries, 'guild_sticker')) {
		guildStickers.push({guildId, stickers});
	}
	return {
		selfUser: selfUser as UserPrivate,
		users: Array.from(usersById.values()),
		guilds,
		unavailableGuilds: Array.from(parseSnapshotEntity(entries, 'unavailable_guild').values()),
		channels,
		relationships: Array.from(parseSnapshotEntity(entries, 'relationship').values()),
		userSettings: parseSnapshotSingleton(entries, 'user_settings'),
		accountMetadata,
		userConnections: parseSnapshotSingleton(entries, 'user_connections'),
		userGuildSettings: Array.from(parseSnapshotEntity(entries, 'user_guild_settings').values()),
		readStates: parseSnapshotEntity(entries, 'read_state'),
		guildEmojis,
		guildStickers,
		presences: parseSnapshotEntity(entries, 'presence'),
		voiceStates: parseSnapshotEntity(entries, 'voice_state'),
	};
}

function hydrateAccountMetadata(metadata: SnapshotAccountMetadataRow): void {
	UserNote.loadNotes(metadata.notes);
	UserPinnedDM.setPinnedDMs(metadata.pinnedDmIds);
	FavoriteMemes.loadFavoriteMemes(metadata.favoriteMemes);
	RtcRegions.setRegions(metadata.rtcRegions as Array<RtcRegionResponse>);
	AuthSession.handleGatewayReady(metadata.authSessionIdHash);
	WebAuthnCredentials.setCredentials(metadata.webAuthnCredentials);
}

function hydrateThreads(parsed: ParsedSnapshot): void {
	const channelsByGuild = new Map<string, Array<WireChannel>>();
	for (const channel of parsed.channels) {
		if (channel.guild_id == null) continue;
		let channels = channelsByGuild.get(channel.guild_id);
		if (channels == null) {
			channels = [];
			channelsByGuild.set(channel.guild_id, channels);
		}
		channels.push(channel);
	}
	const guilds = parsed.guilds.map((guild) => ({...guild, channels: channelsByGuild.get(guild.id) ?? []}));
	ThreadGuilds.handleGatewayReady(guilds);
	ChannelThreads.handleGatewayReady(guilds);
}

function applyParsedSnapshot(storageKey: string, parsed: ParsedSnapshot, selfUserId: string): void {
	runInAction(() => {
		hydrateAccountMetadata(parsed.accountMetadata);
		Users.hydrateFromSnapshot(storageKey, parsed.selfUser, parsed.users);
		Relationships.hydrateFromSnapshot(parsed.relationships);
		Guilds.hydrateFromSnapshot(parsed.guilds);
		if (parsed.userSettings != null) {
			UserSettings.handleGatewayReady(parsed.userSettings);
		}
		GuildList.handleGatewayReady(parsed.guilds);
		GuildCount.handleGatewayReady(parsed.guilds);
		GuildMembers.handleGatewayReady(parsed.guilds);
		GuildAvailability.hydrateFromSnapshot(parsed.unavailableGuilds);
		Channels.hydrateFromSnapshot(parsed.channels, selfUserId);
		hydrateThreads(parsed);
		GuildVerification.handleGatewayReady();
		Permission.handleGatewayReady();
		MemberSearch.handleGatewayReady();
		UserGuildSettings.hydrateFromSnapshot(parsed.userGuildSettings);
		ReadStates.hydrateFromSnapshot(parsed.readStates);
		Emoji.hydrateFromSnapshot(parsed.guildEmojis);
		Sticker.hydrateFromSnapshot(parsed.guildStickers);
		Presence.hydrateFromSnapshot(parsed.presences, selfUserId);
		seedVoiceStatesFromReady(
			parsed.guilds.map((guild) => ({...guild, voice_states: parsed.voiceStates.get(guild.id) ?? []})),
		);
		GuildReadState.handleGatewayReady();
		if (parsed.userConnections == null) {
			UserConnection.handleGatewayReady();
		} else {
			UserConnection.setConnections(parsed.userConnections);
		}
	});
}

function overlayMentionCounts(
	readStates: ReadonlyMap<string, SnapshotReadStateRow>,
	mentionCounts: ReadonlyMap<string, number>,
): Map<string, SnapshotReadStateRow> {
	const overlaid = new Map<string, SnapshotReadStateRow>();
	for (const [channelId, row] of readStates) {
		const mentionCount = mentionCounts.get(channelId) ?? 0;
		overlaid.set(channelId, row.mentionCount === mentionCount ? row : {...row, mentionCount});
	}
	return overlaid;
}

export function captureForegroundMentionCounts(): Map<string, number> {
	return ReadStates.captureMentionCounts();
}

export async function runSnapshotScopeHydration(
	storageKey: string,
	dependencies: SnapshotHydrationDependencies,
	mentionCounts: ReadonlyMap<string, number> | null,
): Promise<void> {
	const epoch = ++hydrationEpoch;
	const selfUserId = await dependencies.resolveSelfUserId(storageKey);
	if (epoch !== hydrationEpoch) {
		return;
	}
	if (selfUserId == null) {
		throw new SnapshotHydrationUnavailableError(storageKey);
	}
	const cursorPending = dependencies.readCursor(storageKey);
	const cursor = cursorPending == null ? null : await cursorPending;
	if (epoch !== hydrationEpoch) {
		return;
	}
	if (cursor == null || cursor.schemaEpoch !== SNAPSHOT_SCHEMA_EPOCH) {
		await dependencies.evictSnapshot(storageKey);
		throw new SnapshotHydrationUnavailableError(storageKey);
	}
	const snapshotPending = dependencies.readSnapshot(storageKey);
	if (snapshotPending == null) {
		throw new SnapshotHydrationUnavailableError(storageKey);
	}
	const entries = await snapshotPending;
	if (epoch !== hydrationEpoch) {
		return;
	}
	try {
		const snapshot = parseSnapshot(entries, selfUserId);
		const parsed =
			mentionCounts === null
				? snapshot
				: {...snapshot, readStates: overlayMentionCounts(snapshot.readStates, mentionCounts)};
		const checkedSelfUser = dependencies.resolveCheckedSelfUser(storageKey);
		if (checkedSelfUser !== null) {
			if (checkedSelfUser.id !== selfUserId) {
				throw new Error(`Checked self user ${checkedSelfUser.id} does not match snapshot user ${selfUserId}`);
			}
			applyParsedSnapshot(storageKey, {...parsed, selfUser: checkedSelfUser}, selfUserId);
		} else {
			applyParsedSnapshot(storageKey, parsed, selfUserId);
		}
	} catch (error) {
		await dependencies.evictSnapshot(storageKey);
		throw error;
	}
}

export async function hydrateStoresFromSnapshotScope(
	storageKey: string,
	mentionCounts: ReadonlyMap<string, number> | null,
): Promise<void> {
	if (!isDesktopSnapshotAvailable()) {
		throw new SnapshotHydrationUnavailableError(storageKey);
	}
	try {
		await runSnapshotScopeHydration(storageKey, defaultDependencies, mentionCounts);
	} catch (error) {
		logger.warn('Failed to hydrate stores from app-shell snapshot', {storageKey, error});
		throw error;
	}
}
