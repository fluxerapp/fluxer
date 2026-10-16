// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, UserID} from '@app/api/BrandedTypes';
import type {ThreadStateRow} from '@app/api/database/types/ThreadTypes';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {ChannelFlags} from '@fluxer/constants/src/ThreadConstants';

export class ThreadState {
	readonly threadId: ChannelID;
	readonly guildId: GuildID;
	readonly parentId: ChannelID;
	readonly type: number;
	readonly archived: boolean;
	readonly locked: boolean;
	readonly invitable: boolean | null;
	readonly autoArchiveDuration: number;
	readonly archiveTimestamp: Date | null;
	readonly createdAt: Date;
	readonly flags: number;
	readonly appliedTags: Array<bigint>;
	readonly memberCount: number;
	readonly memberIdsPreview: Array<UserID>;
	readonly hasStarter: boolean;
	readonly stateVersion: number;

	constructor(row: ThreadStateRow) {
		this.threadId = row.thread_id;
		this.guildId = row.guild_id;
		this.parentId = row.parent_id;
		this.type = row.type;
		this.archived = row.archived ?? false;
		this.locked = row.locked ?? false;
		this.invitable = row.type === ChannelTypes.PRIVATE_THREAD ? (row.invitable ?? true) : null;
		this.autoArchiveDuration = row.auto_archive_duration;
		this.archiveTimestamp = row.archive_timestamp ?? null;
		this.createdAt = row.created_at;
		this.flags = row.flags ?? 0;
		this.appliedTags = row.applied_tags ?? [];
		this.memberCount = row.member_count ?? 0;
		this.memberIdsPreview = row.member_ids_preview ?? [];
		this.hasStarter = row.has_starter ?? false;
		this.stateVersion = row.state_version;
	}

	get isPrivate(): boolean {
		return this.type === ChannelTypes.PRIVATE_THREAD;
	}

	get isPinned(): boolean {
		return (this.flags & ChannelFlags.PINNED) !== 0;
	}

	toRow(): ThreadStateRow {
		return {
			thread_id: this.threadId,
			guild_id: this.guildId,
			parent_id: this.parentId,
			type: this.type,
			archived: this.archived,
			locked: this.locked,
			invitable: this.invitable,
			auto_archive_duration: this.autoArchiveDuration,
			archive_timestamp: this.archiveTimestamp,
			created_at: this.createdAt,
			flags: this.flags,
			applied_tags: this.appliedTags.length > 0 ? this.appliedTags : null,
			member_count: this.memberCount,
			member_ids_preview: this.memberIdsPreview.length > 0 ? this.memberIdsPreview : null,
			has_starter: this.hasStarter,
			state_version: this.stateVersion,
		};
	}
}
