// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, UserID} from '@app/api/BrandedTypes';
import type {ThreadMemberRow} from '@app/api/database/types/ThreadTypes';
import {MuteConfiguration} from '@app/api/models/MuteConfiguration';

export class ThreadMember {
	readonly threadId: ChannelID;
	readonly userId: UserID;
	readonly guildId: GuildID;
	readonly parentId: ChannelID;
	readonly joinTimestamp: Date;
	readonly flags: number;
	readonly muted: boolean;
	readonly muteConfig: MuteConfiguration | null;

	constructor(row: ThreadMemberRow) {
		this.threadId = row.thread_id;
		this.userId = row.user_id;
		this.guildId = row.guild_id;
		this.parentId = row.parent_id;
		this.joinTimestamp = row.join_timestamp;
		this.flags = row.flags ?? 0;
		this.muted = row.muted ?? false;
		this.muteConfig = row.mute_config ? new MuteConfiguration(row.mute_config) : null;
	}

	toRow(): ThreadMemberRow {
		return {
			thread_id: this.threadId,
			user_id: this.userId,
			guild_id: this.guildId,
			parent_id: this.parentId,
			join_timestamp: this.joinTimestamp,
			flags: this.flags,
			muted: this.muted,
			mute_config: this.muteConfig?.toMuteConfig() ?? null,
		};
	}
}
