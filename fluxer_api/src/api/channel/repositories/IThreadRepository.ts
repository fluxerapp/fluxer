// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, MessageID, UserID} from '@app/api/BrandedTypes';
import type {ChannelRow} from '@app/api/database/types/ChannelTypes';
import type {GuildThreadStateRow, ThreadParentConfigRow, ThreadStateRow} from '@app/api/database/types/ThreadTypes';
import type {MuteConfig} from '@app/api/database/types/UserTypes';
import type {ThreadMember} from '@app/api/models/ThreadMember';
import type {ThreadParentConfig} from '@app/api/models/ThreadParentConfig';
import type {ThreadState} from '@app/api/models/ThreadState';
import type {ThreadStats} from '@app/api/models/ThreadStats';

export interface CreateThreadMember {
	userId: UserID;
	flags: number;
}

export interface CreateThreadParams {
	channel: ChannelRow;
	parentType: number;
	autoArchiveDuration: number;
	invitable: boolean | null;
	flags: number;
	appliedTags: Array<bigint>;
	hasStarter: boolean;
	createdAt: Date;
	members: Array<CreateThreadMember>;
}

export type ThreadStatePatch = Partial<
	Pick<
		ThreadStateRow,
		'archived' | 'locked' | 'invitable' | 'auto_archive_duration' | 'archive_timestamp' | 'flags' | 'applied_tags'
	>
>;

export interface ThreadStateTransition {
	previous: ThreadState;
	state: ThreadState;
}

export interface ArchivedThreadPage {
	threads: Array<ThreadState>;
	hasMore: boolean;
}

export interface ThreadMemberSettingsPatch {
	flags?: number;
	muted?: boolean;
	muteConfig?: MuteConfig | null;
}

export interface ThreadMemberAddResult {
	added: Array<ThreadMember>;
	state: ThreadState;
}

export interface ThreadMemberRemoveResult {
	removed: Array<ThreadMember>;
	state: ThreadState | null;
}

export type ThreadParentConfigPatch = Partial<Omit<ThreadParentConfigRow, 'guild_id' | 'channel_id'>>;

export abstract class IThreadRepository {
	abstract getState(threadId: ChannelID): Promise<ThreadState | null>;

	abstract getStates(threadIds: Array<ChannelID>): Promise<Array<ThreadState>>;

	abstract getStats(threadId: ChannelID): Promise<ThreadStats>;

	abstract getStatsMany(threadIds: Array<ChannelID>): Promise<Map<ChannelID, ThreadStats>>;

	abstract adjustMessageCount(threadId: ChannelID, delta: number): Promise<void>;

	abstract create(params: CreateThreadParams): Promise<ThreadState>;

	abstract updateState(
		threadId: ChannelID,
		mutate: (current: ThreadState) => ThreadStatePatch | null,
	): Promise<ThreadStateTransition | null>;

	abstract claimForumPin(parentId: ChannelID, threadId: ChannelID): Promise<boolean>;

	abstract releaseForumPin(parentId: ChannelID, threadId: ChannelID): Promise<void>;

	abstract getForumPin(parentId: ChannelID): Promise<ChannelID | null>;

	abstract countActiveThreads(guildId: GuildID): Promise<number>;

	abstract listActiveThreads(guildId: GuildID): Promise<Array<ThreadState>>;

	abstract listArchivedThreads(
		parentId: ChannelID,
		isPrivate: boolean,
		opts: {before?: Date; limit: number},
	): Promise<ArchivedThreadPage>;

	abstract listJoinedPrivateArchivedThreads(
		userId: UserID,
		guildId: GuildID,
		parentId: ChannelID,
		opts: {before?: ChannelID; limit: number},
	): Promise<ArchivedThreadPage>;

	abstract listJoinedThreadIds(userId: UserID, guildId: GuildID): Promise<Array<ChannelID>>;

	abstract listJoinedPrivateThreadIds(
		userId: UserID,
		guildId: GuildID,
		parentId: ChannelID,
		limit: number,
	): Promise<Array<ChannelID>>;

	abstract repairThreadIndexes(threadIds: Array<ChannelID>): Promise<void>;

	abstract getMember(threadId: ChannelID, userId: UserID): Promise<ThreadMember | null>;

	abstract getMembers(threadId: ChannelID, userIds: Array<UserID>): Promise<Array<ThreadMember>>;

	abstract listMembers(threadId: ChannelID, opts: {after?: UserID; limit: number}): Promise<Array<ThreadMember>>;

	abstract addMembers(
		threadId: ChannelID,
		members: Array<CreateThreadMember>,
		opts?: {joinTimestamp?: Date},
	): Promise<ThreadMemberAddResult | null>;

	abstract removeMembers(threadId: ChannelID, userIds: Array<UserID>): Promise<ThreadMemberRemoveResult>;

	abstract updateMemberSettings(expected: ThreadMember, patch: ThreadMemberSettingsPatch): Promise<ThreadMember | null>;

	abstract listThreadIdsByParent(
		parentId: ChannelID,
		opts: {after?: ChannelID; limit: number},
	): Promise<Array<ChannelID>>;

	abstract listParentThreads(parentId: ChannelID): Promise<Array<{threadId: ChannelID; type: number}>>;

	abstract setThreadType(threadId: ChannelID, type: number): Promise<ThreadState | null>;

	abstract listGuildThreadIds(
		guildId: GuildID,
		opts?: {activeSince?: Date; parents?: ReadonlyArray<{id: ChannelID; type: number}>},
	): Promise<Array<ChannelID>>;

	abstract purgeThread(threadId: ChannelID): Promise<void>;

	abstract revertParentLastMessageId(
		parentId: ChannelID,
		threadId: ChannelID,
		previous: MessageID | null,
	): Promise<void>;

	abstract purgeGuild(guildId: GuildID): Promise<void>;

	abstract getGuildMarker(guildId: GuildID): Promise<GuildThreadStateRow | null>;

	abstract ensureGuildMarker(guildId: GuildID, opts?: {permsSeededAt?: Date}): Promise<void>;

	abstract markGuildPermsSeeded(guildId: GuildID, at: Date): Promise<void>;

	abstract markGuildSearchBackfilled(guildId: GuildID, at: Date): Promise<void>;
	abstract clearGuildSearchBackfilled(guildId: GuildID): Promise<void>;

	abstract getParentConfig(guildId: GuildID, channelId: ChannelID): Promise<ThreadParentConfig | null>;

	abstract listParentConfigs(guildId: GuildID): Promise<Array<ThreadParentConfig>>;

	abstract patchParentConfig(guildId: GuildID, channelId: ChannelID, patch: ThreadParentConfigPatch): Promise<void>;

	abstract deleteParentConfig(guildId: GuildID, channelId: ChannelID): Promise<void>;
}
