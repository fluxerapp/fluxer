// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, UserID} from '../../BrandedTypes';
import type {
	ChannelRow,
	ThreadArchiveDueRow,
	ThreadMemberRow,
	ThreadMetadata,
	ThreadsByGuildRow,
	ThreadsByParentRow,
} from '../../database/types/ChannelTypes';
import type {Channel} from '../../models/Channel';

export interface ThreadChannelPatch {
	name?: string;
	rate_limit_per_user?: number;
	thread_metadata?: ThreadMetadata;
	message_count?: number;
	total_message_sent?: number;
	member_count?: number;
}

export abstract class IThreadRepository {
	abstract createThread(row: ChannelRow): Promise<Channel>;

	abstract patchThreadChannel(threadId: ChannelID, patch: ThreadChannelPatch): Promise<void>;

	abstract listThreadRefsByParent(parentId: ChannelID): Promise<Array<ThreadsByParentRow>>;

	abstract listThreadRefsByGuild(guildId: GuildID): Promise<Array<ThreadsByGuildRow>>;

	abstract setThreadArchivedRefs(thread: Channel, archived: boolean): Promise<void>;

	abstract addThreadMember(params: {
		threadId: ChannelID;
		guildId: GuildID;
		userId: UserID;
		joinTimestamp: Date;
	}): Promise<void>;

	abstract removeThreadMember(params: {threadId: ChannelID; guildId: GuildID; userId: UserID}): Promise<void>;

	abstract getThreadMember(threadId: ChannelID, userId: UserID): Promise<ThreadMemberRow | null>;

	abstract listThreadMembers(threadId: ChannelID): Promise<Array<ThreadMemberRow>>;

	abstract deleteThread(thread: Channel): Promise<void>;

	abstract upsertArchiveDue(row: ThreadArchiveDueRow): Promise<void>;

	abstract fetchArchiveDueByBucket(bucket: number, dueBefore: Date): Promise<Array<ThreadArchiveDueRow>>;

	abstract deleteArchiveDueRow(
		row: Pick<ThreadArchiveDueRow, 'due_bucket' | 'archive_due_at' | 'thread_id'>,
	): Promise<void>;
}
