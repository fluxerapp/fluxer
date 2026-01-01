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

import {ChannelTypes} from '~/Constants';
import type {UserPartial} from '~/records/UserRecord';
import UserPinnedDMStore from '~/stores/UserPinnedDMStore';
import UserStore from '~/stores/UserStore';
import * as SnowflakeUtils from '~/utils/SnowflakeUtils';

type ChannelOverwrite = Readonly<{
	id: string;
	type: number;
	allow: string;
	deny: string;
}>;

export class ChannelOverwriteRecord {
	readonly id: string;
	readonly type: number;
	readonly allow: bigint;
	readonly deny: bigint;

	constructor(overwrite: ChannelOverwrite) {
		this.id = overwrite.id;
		this.type = overwrite.type;
		this.allow = BigInt(overwrite.allow);
		this.deny = BigInt(overwrite.deny);
	}

	withUpdates(overwrite: Partial<ChannelOverwrite>): ChannelOverwriteRecord {
		return new ChannelOverwriteRecord({
			id: this.id,
			type: overwrite.type ?? this.type,
			allow: overwrite.allow ?? this.allow.toString(),
			deny: overwrite.deny ?? this.deny.toString(),
		});
	}

	equals(other: ChannelOverwriteRecord): boolean {
		return this.id === other.id && this.type === other.type && this.allow === other.allow && this.deny === other.deny;
	}

	toJSON(): ChannelOverwrite {
		return {
			id: this.id,
			type: this.type,
			allow: this.allow.toString(),
			deny: this.deny.toString(),
		};
	}
}

export type DefaultReactionEmoji = Readonly<{
	emoji_id: string | null;
	emoji_name: string | null;
}>;

export type Channel = Readonly<{
	id: string;
	guild_id?: string;
	name?: string;
	topic?: string | null;
	url?: string | null;
	icon?: string | null;
	owner_id?: string | null;
	type: number;
	position?: number;
	parent_id?: string | null;
	bitrate?: number | null;
	user_limit?: number | null;
	rtc_region?: string | null;
	last_message_id?: string | null;
	last_pin_timestamp?: string | null;
	permission_overwrites?: ReadonlyArray<ChannelOverwrite>;
	recipients?: ReadonlyArray<UserPartial>;
	nsfw?: boolean;
	rate_limit_per_user?: number;
	nicks?: Readonly<Record<string, string>>;
	flags?: number;
	member_count?: number;
	message_count?: number;
	total_message_sent?: number;
	default_reaction_emoji?: DefaultReactionEmoji | null;
}>;

export class ChannelRecord {
	readonly id: string;
	readonly guildId?: string;
	readonly name?: string;
	readonly topic: string | null;
	readonly url: string | null;
	readonly icon: string | null;
	readonly ownerId: string | null;
	readonly type: number;
	readonly position?: number;
	readonly parentId: string | null;
	readonly bitrate: number | null;
	readonly userLimit: number | null;
	readonly rtcRegion: string | null;
	readonly lastMessageId: string | null;
	readonly lastPinTimestamp: Date | null;
	readonly permissionOverwrites: Readonly<Record<string, ChannelOverwriteRecord>>;
	readonly recipientIds: ReadonlyArray<string>;
	readonly nsfw: boolean;
	readonly rateLimitPerUser: number;
	readonly nicks: Readonly<Record<string, string>>;
	readonly flags: number;
	readonly memberCount?: number;
	readonly messageCount?: number;
	readonly totalMessageSent?: number;
	readonly defaultReactionEmoji?: DefaultReactionEmoji | null;

	constructor(channel: Channel) {
		this.id = channel.id;
		this.guildId = channel.guild_id;
		this.name = channel.name;
		this.topic = channel.topic ?? null;
		this.url = channel.url ?? null;
		this.icon = channel.icon ?? null;
		this.ownerId = channel.owner_id ?? null;
		this.type = channel.type;
		this.position = channel.position;
		this.parentId = channel.parent_id ?? null;
		this.bitrate = channel.bitrate ?? null;
		this.userLimit = channel.user_limit ?? null;
		this.rtcRegion = channel.rtc_region ?? null;
		this.lastMessageId = channel.last_message_id ?? null;
		this.lastPinTimestamp = channel.last_pin_timestamp ? new Date(channel.last_pin_timestamp) : null;
		this.nsfw = channel.nsfw ?? false;
		this.rateLimitPerUser = channel.rate_limit_per_user ?? 0;
		this.flags = channel.flags ?? 0;
		this.nicks = channel.nicks ?? {};

		this.memberCount = channel.member_count;
		this.messageCount = channel.message_count;
		this.totalMessageSent = channel.total_message_sent;

		this.defaultReactionEmoji = channel.default_reaction_emoji;

		if ((this.type === ChannelTypes.DM || this.type === ChannelTypes.GROUP_DM) && channel.recipients) {
			UserStore.cacheUsers(Array.from(channel.recipients));
		}

		if (this.type === ChannelTypes.DM_PERSONAL_NOTES) {
			const currentUser = UserStore.getCurrentUser();
			this.recipientIds = currentUser ? [currentUser.id] : [];
		} else if ((this.type === ChannelTypes.DM || this.type === ChannelTypes.GROUP_DM) && channel.recipients) {
			this.recipientIds = channel.recipients.map((user) => user.id);
		} else {
			this.recipientIds = [];
		}

		this.permissionOverwrites =
			!this.isPrivate() && channel.permission_overwrites
				? channel.permission_overwrites.reduce(
						(acc, overwrite) => {
							acc[overwrite.id] = new ChannelOverwriteRecord(overwrite);
							return acc;
						},
						{} as Record<string, ChannelOverwriteRecord>,
					)
				: {};
	}

	hasFlag(flag: number): boolean {
		return (this.flags & flag) === flag;
	}

	get isPinned(): boolean {
		return UserPinnedDMStore.pinnedDMs.includes(this.id);
	}

	isPrivate(): boolean {
		return (
			this.type === ChannelTypes.DM ||
			this.type === ChannelTypes.GROUP_DM ||
			this.type === ChannelTypes.DM_PERSONAL_NOTES
		);
	}

	isDM(): boolean {
		return this.type === ChannelTypes.DM;
	}

	isGroupDM(): boolean {
		return this.type === ChannelTypes.GROUP_DM;
	}

	isPersonalNotes(): boolean {
		return this.type === ChannelTypes.DM_PERSONAL_NOTES;
	}

	isGuildText(): boolean {
		return this.type === ChannelTypes.GUILD_TEXT;
	}

	isGuildVoice(): boolean {
		return this.type === ChannelTypes.GUILD_VOICE;
	}

	isGuildCategory(): boolean {
		return this.type === ChannelTypes.GUILD_CATEGORY;
	}

	isVoice(): boolean {
		return this.type === ChannelTypes.GUILD_VOICE;
	}

	isText(): boolean {
		return this.type === ChannelTypes.GUILD_TEXT;
	}

	isNSFW(): boolean {
		return this.nsfw;
	}

	getRecipientId(): string | undefined {
		if (this.type !== ChannelTypes.DM) return undefined;
		return this.recipientIds[0];
	}

	get createdAt(): Date {
		return new Date(SnowflakeUtils.extractTimestamp(this.id));
	}

	withUpdates(updates: Partial<Channel>): ChannelRecord {
		let newRecipients: Array<UserPartial> = [];

		if (
			updates.type === ChannelTypes.DM_PERSONAL_NOTES ||
			(this.type === ChannelTypes.DM_PERSONAL_NOTES && updates.type === undefined)
		) {
			const currentUser = UserStore.getCurrentUser();
			if (currentUser) {
				newRecipients = [currentUser.toJSON()];
			}
		} else if ((this.type === ChannelTypes.DM || this.type === ChannelTypes.GROUP_DM) && updates.recipients) {
			newRecipients = Array.from(updates.recipients);
			UserStore.cacheUsers(newRecipients);
		} else if (this.type === ChannelTypes.DM || this.type === ChannelTypes.GROUP_DM) {
			newRecipients = this.recipientIds.map((id) => UserStore.getUser(id)!.toJSON());
		}

		return new ChannelRecord({
			id: this.id,
			guild_id: updates.guild_id ?? this.guildId,
			name: updates.name ?? this.name,
			topic: updates.topic !== undefined ? updates.topic : this.topic,
			url: updates.url !== undefined ? updates.url : this.url,
			icon: updates.icon !== undefined ? updates.icon : this.icon,
			owner_id: updates.owner_id !== undefined ? updates.owner_id : this.ownerId,
			type: updates.type ?? this.type,
			position: updates.position ?? this.position,
			parent_id: updates.parent_id !== undefined ? updates.parent_id : this.parentId,
			bitrate: updates.bitrate !== undefined ? updates.bitrate : this.bitrate,
			user_limit: updates.user_limit !== undefined ? updates.user_limit : this.userLimit,
			rtc_region: updates.rtc_region !== undefined ? updates.rtc_region : this.rtcRegion,
			last_message_id: updates.last_message_id !== undefined ? updates.last_message_id : this.lastMessageId,
			last_pin_timestamp: updates.last_pin_timestamp ?? this.lastPinTimestamp?.toISOString() ?? undefined,
			permission_overwrites: !this.isPrivate()
				? (updates.permission_overwrites ?? Object.values(this.permissionOverwrites).map((o) => o.toJSON()))
				: undefined,
			recipients: newRecipients.length > 0 ? newRecipients : undefined,
			nsfw: updates.nsfw ?? this.nsfw,
			rate_limit_per_user: updates.rate_limit_per_user ?? this.rateLimitPerUser,
			nicks: updates.nicks ?? this.nicks,
			flags: updates.flags ?? this.flags,
			member_count: updates.member_count ?? this.memberCount,
			message_count: updates.message_count ?? this.messageCount,
			total_message_sent: updates.total_message_sent ?? this.totalMessageSent,
			default_reaction_emoji: updates.default_reaction_emoji ?? this.defaultReactionEmoji,
		});
	}

	withOverwrite(overwrite: ChannelOverwriteRecord): ChannelRecord {
		if (this.isPrivate()) {
			return this;
		}

		return new ChannelRecord({
			...this.toJSON(),
			permission_overwrites: Object.values({
				...this.permissionOverwrites,
				[overwrite.id]: overwrite,
			}).map((o) => o.toJSON()),
		});
	}

	equals(other: ChannelRecord): boolean {
		if (this === other) return true;

		if (this.id !== other.id) return false;
		if (this.guildId !== other.guildId) return false;
		if (this.name !== other.name) return false;
		if (this.topic !== other.topic) return false;
		if (this.url !== other.url) return false;
		if (this.icon !== other.icon) return false;
		if (this.ownerId !== other.ownerId) return false;
		if (this.type !== other.type) return false;
		if (this.position !== other.position) return false;
		if (this.parentId !== other.parentId) return false;
		if (this.bitrate !== other.bitrate) return false;
		if (this.userLimit !== other.userLimit) return false;
		if (this.rtcRegion !== other.rtcRegion) return false;
		if (this.lastMessageId !== other.lastMessageId) return false;
		if (this.lastPinTimestamp?.getTime() !== other.lastPinTimestamp?.getTime()) return false;
		if (this.nsfw !== other.nsfw) return false;
		if (this.rateLimitPerUser !== other.rateLimitPerUser) return false;
		if (this.flags !== other.flags) return false;

		if (this.recipientIds.length !== other.recipientIds.length) return false;
		for (let i = 0; i < this.recipientIds.length; i++) {
			if (this.recipientIds[i] !== other.recipientIds[i]) return false;
		}

		const thisOverwrites = Object.keys(this.permissionOverwrites);
		const otherOverwrites = Object.keys(other.permissionOverwrites);
		if (thisOverwrites.length !== otherOverwrites.length) return false;
		for (const key of thisOverwrites) {
			if (!this.permissionOverwrites[key].equals(other.permissionOverwrites[key])) {
				return false;
			}
		}

		return true;
	}

	toJSON(): Channel {
		return {
			id: this.id,
			guild_id: this.guildId,
			name: this.name,
			topic: this.topic,
			url: this.url,
			icon: this.icon,
			owner_id: this.ownerId,
			type: this.type,
			position: this.position,
			parent_id: this.parentId,
			bitrate: this.bitrate,
			user_limit: this.userLimit,
			rtc_region: this.rtcRegion,
			last_message_id: this.lastMessageId,
			last_pin_timestamp: this.lastPinTimestamp?.toISOString() ?? undefined,
			permission_overwrites: Object.values(this.permissionOverwrites).map((o) => o.toJSON()),
			recipients:
				this.type === ChannelTypes.DM || this.type === ChannelTypes.GROUP_DM
					? this.recipientIds.map((id) => UserStore.getUser(id)!.toJSON())
					: undefined,
			nsfw: this.nsfw,
			rate_limit_per_user: this.rateLimitPerUser,
			nicks: this.nicks,
			flags: this.flags,
			member_count: this.memberCount,
			message_count: this.messageCount,
			total_message_sent: this.totalMessageSent,
			default_reaction_emoji: this.defaultReactionEmoji,
		};
	}
}
