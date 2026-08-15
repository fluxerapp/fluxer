// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {noteText} from '@app/features/theme/fonts/ScriptFontLoader';
import UserPinnedDM from '@app/features/user/state/UserPinnedDM';
import Users from '@app/features/user/state/Users';
import {ChannelTypes, GUILD_TEXT_BASED_CHANNEL_TYPES, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {VOICE_CHANNEL_CONNECTION_LIMIT_DEFAULT} from '@fluxer/constants/src/LimitConstants';
import type {ChannelOverwrite, Channel as WireChannel} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {UserPartial} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import * as SnowflakeUtils from '@fluxer/snowflake/src/SnowflakeUtils';

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

interface ChannelRecordOptions {
	instanceId?: string;
}

function getRecipientPartials(recipientIds: ReadonlyArray<string>): Array<UserPartial> {
	return recipientIds
		.map((id) => Users?.getUser(id)?.toJSON())
		.filter((user): user is UserPartial => user !== undefined);
}

export class Channel {
	readonly instanceId: string;
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
	readonly voiceConnectionLimit: number | null;
	readonly rtcRegion: string | null;
	readonly lastMessageId: string | null;
	readonly lastPinTimestamp: Date | null;
	readonly permissionOverwrites: Readonly<Record<string, ChannelOverwriteRecord>>;
	readonly recipientIds: ReadonlyArray<string>;
	readonly nsfw: boolean;
	readonly nsfwOverride: boolean | null;
	readonly contentWarningLevel: number;
	readonly contentWarningText: string | null;
	readonly rateLimitPerUser: number;
	readonly nicks: Readonly<Record<string, string>>;

	constructor(channel: WireChannel, options?: ChannelRecordOptions) {
		this.instanceId = options?.instanceId ?? RuntimeConfig.localInstanceDomain;
		this.id = channel.id;
		this.guildId = channel.guild_id;
		this.name = channel.name;
		noteText(this.name);
		this.topic = channel.topic ?? null;
		this.url = channel.url ?? null;
		this.icon = channel.icon ?? null;
		this.ownerId = channel.owner_id ?? null;
		this.type = channel.type;
		this.position = channel.position;
		this.parentId = channel.parent_id ?? null;
		this.bitrate = channel.bitrate ?? null;
		this.userLimit = channel.user_limit ?? null;
		this.voiceConnectionLimit =
			channel.voice_connection_limit ??
			(this.type === ChannelTypes.GUILD_VOICE ? VOICE_CHANNEL_CONNECTION_LIMIT_DEFAULT : null);
		this.rtcRegion = channel.rtc_region ?? null;
		this.lastMessageId = channel.last_message_id ?? null;
		this.lastPinTimestamp = channel.last_pin_timestamp ? new Date(channel.last_pin_timestamp) : null;
		this.nsfw = channel.nsfw ?? false;
		this.nsfwOverride = channel.nsfw_override ?? null;
		this.contentWarningLevel = channel.content_warning_level ?? 0;
		this.contentWarningText = channel.content_warning_text ?? null;
		this.rateLimitPerUser = channel.rate_limit_per_user ?? 0;
		this.nicks = channel.nicks ?? {};
		if ((this.type === ChannelTypes.DM || this.type === ChannelTypes.GROUP_DM) && channel.recipients) {
			Users?.cacheUsers(Array.from(channel.recipients));
		}
		if (this.type === ChannelTypes.DM_PERSONAL_NOTES) {
			this.recipientIds = channel.recipients?.map((user) => user.id) ?? [channel.id];
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

	get isPinned(): boolean {
		return UserPinnedDM.pinnedDMs.includes(this.id);
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
		return GUILD_TEXT_BASED_CHANNEL_TYPES.has(this.type);
	}

	isMature(): boolean {
		return this.nsfw;
	}

	isRoleRequired(): boolean {
		if (
			this.guildId == null ||
			(this.type !== ChannelTypes.GUILD_TEXT &&
				this.type !== ChannelTypes.GUILD_VOICE &&
				this.type !== ChannelTypes.GUILD_LINK)
		) {
			return false;
		}
		const flag = this.type === ChannelTypes.GUILD_VOICE ? Permissions.CONNECT : Permissions.VIEW_CHANNEL;
		const overwrite = this.permissionOverwrites[this.guildId];
		return overwrite != null && (overwrite.deny & flag) === flag;
	}

	getRecipientId(): string | undefined {
		if (this.type !== ChannelTypes.DM) return undefined;
		return this.recipientIds[0];
	}

	get createdAt(): Date {
		return new Date(SnowflakeUtils.extractTimestamp(this.id));
	}

	withUpdates(updates: Partial<WireChannel>): Channel {
		let newRecipients: Array<UserPartial> = [];
		if (
			updates.type === ChannelTypes.DM_PERSONAL_NOTES ||
			(this.type === ChannelTypes.DM_PERSONAL_NOTES && updates.type === undefined)
		) {
			if (updates.recipients) {
				newRecipients = Array.from(updates.recipients);
				Users?.cacheUsers(newRecipients);
			}
		} else if ((this.type === ChannelTypes.DM || this.type === ChannelTypes.GROUP_DM) && updates.recipients) {
			newRecipients = Array.from(updates.recipients);
			Users?.cacheUsers(newRecipients);
		} else if (this.type === ChannelTypes.DM || this.type === ChannelTypes.GROUP_DM) {
			newRecipients = getRecipientPartials(this.recipientIds);
		}
		return new Channel(
			{
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
				voice_connection_limit:
					updates.voice_connection_limit !== undefined ? updates.voice_connection_limit : this.voiceConnectionLimit,
				rtc_region: updates.rtc_region !== undefined ? updates.rtc_region : this.rtcRegion,
				last_message_id: updates.last_message_id !== undefined ? updates.last_message_id : this.lastMessageId,
				last_pin_timestamp: updates.last_pin_timestamp ?? this.lastPinTimestamp?.toISOString() ?? undefined,
				permission_overwrites: !this.isPrivate()
					? (updates.permission_overwrites ?? Object.values(this.permissionOverwrites).map((o) => o.toJSON()))
					: undefined,
				recipients: newRecipients.length > 0 ? newRecipients : undefined,
				nsfw: updates.nsfw ?? this.nsfw,
				nsfw_override: updates.nsfw_override !== undefined ? updates.nsfw_override : this.nsfwOverride,
				content_warning_level:
					updates.content_warning_level !== undefined ? updates.content_warning_level : this.contentWarningLevel,
				content_warning_text:
					updates.content_warning_text !== undefined ? updates.content_warning_text : this.contentWarningText,
				rate_limit_per_user: updates.rate_limit_per_user ?? this.rateLimitPerUser,
				nicks: updates.nicks ?? this.nicks,
			},
			{instanceId: this.instanceId},
		);
	}

	withOverwrite(overwrite: ChannelOverwriteRecord): Channel {
		if (this.isPrivate()) {
			return this;
		}
		return new Channel(
			{
				...this.toJSON(),
				permission_overwrites: Object.values({
					...this.permissionOverwrites,
					[overwrite.id]: overwrite,
				}).map((o) => o.toJSON()),
			},
			{instanceId: this.instanceId},
		);
	}

	equals(other: Channel): boolean {
		if (this === other) return true;
		if (this.instanceId !== other.instanceId) return false;
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
		if (this.voiceConnectionLimit !== other.voiceConnectionLimit) return false;
		if (this.rtcRegion !== other.rtcRegion) return false;
		if (this.lastMessageId !== other.lastMessageId) return false;
		if (this.lastPinTimestamp?.getTime() !== other.lastPinTimestamp?.getTime()) return false;
		if (this.nsfw !== other.nsfw) return false;
		if (this.nsfwOverride !== other.nsfwOverride) return false;
		if (this.contentWarningLevel !== other.contentWarningLevel) return false;
		if (this.contentWarningText !== other.contentWarningText) return false;
		if (this.rateLimitPerUser !== other.rateLimitPerUser) return false;
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

	toJSON(): WireChannel {
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
			voice_connection_limit: this.voiceConnectionLimit,
			rtc_region: this.rtcRegion,
			last_message_id: this.lastMessageId,
			last_pin_timestamp: this.lastPinTimestamp?.toISOString() ?? undefined,
			permission_overwrites: Object.values(this.permissionOverwrites).map((o) => o.toJSON()),
			recipients:
				this.type === ChannelTypes.DM || this.type === ChannelTypes.GROUP_DM
					? getRecipientPartials(this.recipientIds)
					: undefined,
			nsfw: this.nsfw,
			nsfw_override: this.nsfwOverride,
			content_warning_level: this.contentWarningLevel,
			content_warning_text: this.contentWarningText,
			rate_limit_per_user: this.rateLimitPerUser,
			nicks: this.nicks,
		};
	}
}
