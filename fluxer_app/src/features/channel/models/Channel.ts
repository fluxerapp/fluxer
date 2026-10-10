// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {noteText} from '@app/features/theme/fonts/ScriptFontLoader';
import UserPinnedDM from '@app/features/user/state/UserPinnedDM';
import Users from '@app/features/user/state/Users';
import {ChannelTypes, GUILD_TEXT_BASED_CHANNEL_TYPES, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {VOICE_CHANNEL_CONNECTION_LIMIT_DEFAULT} from '@fluxer/constants/src/LimitConstants';
import {THREAD_CHANNEL_TYPES, THREAD_ONLY_CHANNEL_TYPES} from '@fluxer/constants/src/ThreadConstants';
import type {ChannelOverwrite, Channel as WireChannel} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {
	DefaultReactionEmojiResponse,
	ForumTagResponse,
	ThreadMemberResponse,
	ThreadMetadataResponse,
} from '@fluxer/schema/src/domains/channel/ThreadSchemas';
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

export interface ThreadChannelFields {
	readonly flags?: number;
	readonly thread_metadata?: ThreadMetadataResponse;
	readonly applied_tags?: ReadonlyArray<string>;
	readonly message_count?: number;
	readonly total_message_sent?: number;
	readonly member_count?: number;
	readonly default_auto_archive_duration?: number | null;
	readonly default_thread_rate_limit_per_user?: number;
	readonly available_tags?: ReadonlyArray<ForumTagResponse>;
	readonly default_reaction_emoji?: DefaultReactionEmojiResponse | null;
	readonly default_sort_order?: number | null;
	readonly default_forum_layout?: number;
	readonly default_tag_setting?: 'match_some' | 'match_all';
}

export interface ChannelWire extends WireChannel, ThreadChannelFields {
	readonly member?: ThreadMemberResponse;
	readonly newly_created?: boolean;
}

const THREAD_FIELD_KEYS: ReadonlyArray<keyof ThreadChannelFields> = [
	'flags',
	'thread_metadata',
	'applied_tags',
	'message_count',
	'total_message_sent',
	'member_count',
	'default_auto_archive_duration',
	'default_thread_rate_limit_per_user',
	'available_tags',
	'default_reaction_emoji',
	'default_sort_order',
	'default_forum_layout',
	'default_tag_setting',
];

function pickThreadFields(source: ThreadChannelFields, base?: ThreadChannelFields | null): ThreadChannelFields | null {
	let picked: Record<string, unknown> | null = base ? {...base} : null;
	for (const key of THREAD_FIELD_KEYS) {
		const value = source[key];
		if (value !== undefined) {
			picked ??= {};
			picked[key] = value;
		}
	}
	return picked as ThreadChannelFields | null;
}

function threadFieldsEqual(a: ThreadChannelFields | null, b: ThreadChannelFields | null): boolean {
	if (a === b) return true;
	if (a == null || b == null) return false;
	for (const key of THREAD_FIELD_KEYS) {
		const left = a[key];
		const right = b[key];
		if (left === right) continue;
		if (typeof left !== 'object' || typeof right !== 'object' || left == null || right == null) return false;
		if (JSON.stringify(left) !== JSON.stringify(right)) return false;
	}
	return true;
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
	readonly rtcP2p: boolean;
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
	readonly threadFields: ThreadChannelFields | null;

	constructor(channel: ChannelWire, options?: ChannelRecordOptions) {
		this.instanceId = options?.instanceId ?? RuntimeConfig.localInstanceDomain;
		this.id = channel.id;
		this.guildId = channel.guild_id;
		this.name = channel.name ?? undefined;
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
		this.rtcP2p = channel.rtc_p2p ?? false;
		this.lastMessageId = channel.last_message_id ?? null;
		this.lastPinTimestamp = channel.last_pin_timestamp ? new Date(channel.last_pin_timestamp) : null;
		this.nsfw = channel.nsfw ?? false;
		this.nsfwOverride = channel.nsfw_override ?? null;
		this.contentWarningLevel = channel.content_warning_level ?? 0;
		this.contentWarningText = channel.content_warning_text ?? null;
		this.rateLimitPerUser = channel.rate_limit_per_user ?? 0;
		this.nicks = channel.nicks ?? {};
		this.threadFields = pickThreadFields(channel);
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

	isGuildAnnouncement(): boolean {
		return this.type === ChannelTypes.GUILD_ANNOUNCEMENT;
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

	isThread(): boolean {
		return THREAD_CHANNEL_TYPES.has(this.type);
	}

	isPrivateThread(): boolean {
		return this.type === ChannelTypes.PRIVATE_THREAD;
	}

	isThreadOnly(): boolean {
		return THREAD_ONLY_CHANNEL_TYPES.has(this.type);
	}

	get flags(): number {
		return this.threadFields?.flags ?? 0;
	}

	get threadMetadata(): ThreadMetadataResponse | null {
		return this.threadFields?.thread_metadata ?? null;
	}

	get isArchived(): boolean {
		return this.threadFields?.thread_metadata?.archived ?? false;
	}

	get isLocked(): boolean {
		return this.threadFields?.thread_metadata?.locked ?? false;
	}

	get messageCount(): number {
		return this.threadFields?.message_count ?? 0;
	}

	get totalMessageSent(): number {
		return this.threadFields?.total_message_sent ?? 0;
	}

	get memberCount(): number {
		return this.threadFields?.member_count ?? 0;
	}

	get appliedTags(): ReadonlyArray<string> {
		return this.threadFields?.applied_tags ?? [];
	}

	get availableTags(): ReadonlyArray<ForumTagResponse> {
		return this.threadFields?.available_tags ?? [];
	}

	get defaultAutoArchiveDuration(): number | null {
		return this.threadFields?.default_auto_archive_duration ?? null;
	}

	get defaultThreadRateLimitPerUser(): number {
		return this.threadFields?.default_thread_rate_limit_per_user ?? 0;
	}

	isRoleRequired(): boolean {
		if (
			this.guildId == null ||
			(this.type !== ChannelTypes.GUILD_TEXT &&
				this.type !== ChannelTypes.GUILD_ANNOUNCEMENT &&
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

	withUpdates(updates: Partial<ChannelWire>): Channel {
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
				name: updates.name !== undefined ? updates.name : this.name,
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
				rtc_p2p: updates.rtc_p2p ?? this.rtcP2p,
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
				...pickThreadFields(updates, this.threadFields),
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
		if (this.rtcP2p !== other.rtcP2p) return false;
		if (this.lastMessageId !== other.lastMessageId) return false;
		if (this.lastPinTimestamp?.getTime() !== other.lastPinTimestamp?.getTime()) return false;
		if (this.nsfw !== other.nsfw) return false;
		if (this.nsfwOverride !== other.nsfwOverride) return false;
		if (this.contentWarningLevel !== other.contentWarningLevel) return false;
		if (this.contentWarningText !== other.contentWarningText) return false;
		if (this.rateLimitPerUser !== other.rateLimitPerUser) return false;
		if (!threadFieldsEqual(this.threadFields, other.threadFields)) return false;
		if (this.recipientIds.length !== other.recipientIds.length) return false;
		for (let i = 0; i < this.recipientIds.length; i++) {
			if (this.recipientIds[i] !== other.recipientIds[i]) return false;
		}
		const thisOverwrites = Object.keys(this.permissionOverwrites);
		const otherOverwrites = Object.keys(other.permissionOverwrites);
		if (thisOverwrites.length !== otherOverwrites.length) return false;
		for (const key of thisOverwrites) {
			const otherOverwrite = other.permissionOverwrites[key];
			if (otherOverwrite == null || !this.permissionOverwrites[key].equals(otherOverwrite)) {
				return false;
			}
		}
		return true;
	}

	toJSON(): ChannelWire {
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
			rtc_p2p: this.rtcP2p,
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
			...this.threadFields,
		};
	}
}
