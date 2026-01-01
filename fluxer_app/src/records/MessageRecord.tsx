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

import {MessageFlags, MessageStates, MessageTypes} from '~/Constants';
import type {GuildMember} from '~/records/GuildMemberRecord';
import type {UserPartial} from '~/records/UserRecord';
import {UserRecord} from '~/records/UserRecord';
import AuthenticationStore from '~/stores/AuthenticationStore';
import ChannelStore from '~/stores/ChannelStore';
import GuildMemberStore from '~/stores/GuildMemberStore';
import GuildStore from '~/stores/GuildStore';
import RelationshipStore from '~/stores/RelationshipStore';
import UserStore from '~/stores/UserStore';
import type {Invite as InviteType} from '~/types/InviteTypes';
import * as GiftCodeUtils from '~/utils/giftCodeUtils';
import * as InviteUtils from '~/utils/InviteUtils';
import {emojiEquals, type ReactionEmoji} from '~/utils/ReactionUtils';
import * as ThemeUtils from '~/utils/ThemeUtils';

export type Invite = InviteType;

export interface EmbedAuthor {
	name: string;
	url?: string;
	icon_url?: string;
	proxy_icon_url?: string;
}

export interface EmbedFooter {
	text: string;
	icon_url?: string;
	proxy_icon_url?: string;
}

export interface EmbedMedia {
	url: string;
	proxy_url?: string;
	content_type?: string;
	content_hash?: string | null;
	width?: number;
	height?: number;
	placeholder?: string;
	flags: number;
	description?: string;
	duration?: number;
	nsfw?: boolean;
}

export interface EmbedField {
	name: string;
	value: string;
	inline: boolean;
}

export interface MessageEmbed {
	id: string;
	type: string;
	url?: string;
	title?: string;
	color?: number;
	timestamp?: string;
	description?: string;
	author?: EmbedAuthor;
	image?: EmbedMedia;
	thumbnail?: EmbedMedia;
	footer?: EmbedFooter;
	fields?: ReadonlyArray<EmbedField>;
	provider?: EmbedAuthor;
	video?: EmbedMedia;
	audio?: EmbedMedia;
	flags?: number;
}

export interface MessageReference {
	message_id: string;
	channel_id: string;
	guild_id?: string;
	type?: number;
}

export interface MessageReaction {
	emoji: ReactionEmoji;
	count: number;
	me?: true;
	me_burst?: boolean;
	count_details?: {
		burst: number;
		normal: number;
	};
}

export interface MessageAttachment {
	id: string;
	filename: string;
	title?: string;
	description?: string;
	caption?: string;
	content_type?: string;
	size: number;
	url: string | null;
	proxy_url: string | null;
	width?: number;
	height?: number;
	placeholder?: string;
	placeholder_version?: number;
	flags: number;
	duration_secs?: number;
	duration?: number;
	waveform?: string;
	content_hash?: string | null;
	nsfw?: boolean;
	expires_at?: string | null;
	expired?: boolean;
}

export interface MessageCall {
	participants: Array<string>;
	ended_timestamp?: string | null;
}

export interface MessageSnapshot {
	type: number;
	content: string;
	embeds?: ReadonlyArray<MessageEmbed>;
	attachments?: ReadonlyArray<MessageAttachment>;
	timestamp: string;
}

export interface MessageStickerItem {
	id: string;
	name: string;
	format_type: number;
}

export interface ChannelMention {
	id: string;
	guild_id: string;
	type: number;
	name: string;
	parent_id?: string | null;
}

export interface AllowedMentions {
	parse?: ReadonlyArray<'roles' | 'users' | 'everyone'>;
	roles?: ReadonlyArray<string>;
	users?: ReadonlyArray<string>;
	replied_user?: boolean;
}

export interface MessageMention extends UserPartial {
	member?: Omit<GuildMember, 'user'>;
}

export type Message = Readonly<{
	id: string;
	channel_id: string;
	guild_id?: string;
	author: UserPartial;
	member?: Omit<GuildMember, 'user'>;
	webhook_id?: string;
	application_id?: string;
	type: number;
	flags: number;
	pinned: boolean;
	tts?: boolean;
	mention_everyone: boolean;
	content: string;
	timestamp: string;
	edited_timestamp?: string;
	mentions?: ReadonlyArray<MessageMention>;
	mention_roles?: ReadonlyArray<string>;
	mention_channels?: ReadonlyArray<ChannelMention>;
	embeds?: ReadonlyArray<MessageEmbed>;
	attachments?: ReadonlyArray<MessageAttachment>;
	stickers?: ReadonlyArray<MessageStickerItem>;
	reactions?: ReadonlyArray<MessageReaction>;
	message_reference?: MessageReference;
	referenced_message?: Message | null;
	message_snapshots?: ReadonlyArray<MessageSnapshot>;
	call?: MessageCall | null;
	state?: string;
	nonce?: string;
	blocked?: boolean;
	loggingName?: string;
	_allowedMentions?: AllowedMentions;
	_favoriteMemeId?: string;
}>;

interface TransformedMessageCall {
	participants: Array<string>;
	endedTimestamp: Date | null;
}

function transformMessageCall(call?: MessageCall | null): TransformedMessageCall | null {
	if (call != null) {
		return {
			participants: call.participants,
			endedTimestamp: call.ended_timestamp != null ? new Date(call.ended_timestamp) : null,
		};
	}
	return null;
}

let embedIdCounter = 0;

const generateEmbedId = (): string => {
	return `embed_${embedIdCounter++}`;
};

const areEmbedsEqual = (embed1: MessageEmbed, embed2: MessageEmbed): boolean => {
	const {id: _id1, ...embed1WithoutId} = embed1;
	const {id: _id2, ...embed2WithoutId} = embed2;
	return JSON.stringify(embed1WithoutId) === JSON.stringify(embed2WithoutId);
};

const embedCache: Array<{embed: MessageEmbed; id: string}> = [];

const getOrCreateEmbedId = (embed: Omit<MessageEmbed, 'id'>): string => {
	const existingEmbed = embedCache.find((cached) => areEmbedsEqual(cached.embed, embed as MessageEmbed));

	if (existingEmbed) {
		return existingEmbed.id;
	}

	const newId = generateEmbedId();
	embedCache.push({
		embed: {...embed, id: newId} as MessageEmbed,
		id: newId,
	});

	return newId;
};

interface MessageRecordOptions {
	skipUserCache?: boolean;
}

export class MessageRecord {
	readonly id: string;
	readonly channelId: string;
	readonly guildId?: string;
	readonly author: UserRecord;
	readonly webhookId?: string;
	readonly applicationId?: string;
	readonly type: number;
	readonly flags: number;
	readonly pinned: boolean;
	readonly mentionEveryone: boolean;
	readonly content: string;
	readonly timestamp: Date;
	readonly editedTimestamp: Date | null;
	readonly mentions: ReadonlyArray<UserRecord>;
	readonly mentionRoles: ReadonlyArray<string>;
	readonly mentionChannels: ReadonlyArray<ChannelMention>;
	readonly embeds: ReadonlyArray<MessageEmbed>;
	readonly attachments: ReadonlyArray<MessageAttachment>;
	readonly stickerItems: ReadonlyArray<MessageStickerItem>;
	readonly reactions: ReadonlyArray<MessageReaction>;
	readonly messageReference?: MessageReference;
	readonly referencedMessage?: MessageRecord | null;
	readonly messageSnapshots?: ReadonlyArray<MessageSnapshot>;
	readonly call: TransformedMessageCall | null;
	readonly state: string;
	readonly nonce?: string;
	readonly blocked: boolean;
	readonly loggingName?: string;

	readonly invites: ReadonlyArray<string>;
	readonly gifts: ReadonlyArray<string>;
	readonly themes: ReadonlyArray<string>;

	readonly _allowedMentions?: AllowedMentions;
	readonly _favoriteMemeId?: string;
	readonly stickers?: ReadonlyArray<MessageStickerItem>;

	constructor(message: Message, options?: MessageRecordOptions) {
		if (!options?.skipUserCache) {
			UserStore.cacheUsers([message.author, ...(message.mentions ?? [])]);
		}

		const isBlocked = RelationshipStore.isBlocked(message.author.id);

		if (message.webhook_id) {
			this.author = new UserRecord(message.author);
		} else {
			this.author = UserStore.getUser(message.author.id) || new UserRecord(message.author);
		}

		this.id = message.id;
		this.channelId = message.channel_id;
		this.guildId = message.guild_id;
		this.webhookId = message.webhook_id;
		this.applicationId = message.application_id;
		this.type = message.type;
		this.flags = message.flags;
		this.pinned = message.pinned;
		this.mentionEveryone = message.mention_everyone;
		this.content = message.content;
		this.timestamp = new Date(message.timestamp);
		this.editedTimestamp = message.edited_timestamp ? new Date(message.edited_timestamp) : null;
		this.state = message.state ?? MessageStates.SENT;
		this.nonce = message.nonce;
		this.blocked = message.blocked ?? isBlocked;
		this.loggingName = message.loggingName;

		this.mentions = Object.freeze((message.mentions ?? []).map((user) => new UserRecord(user)));
		this.mentionRoles = Object.freeze(message.mention_roles ?? []);
		this.mentionChannels = Object.freeze(message.mention_channels ?? []);
		this.embeds = Object.freeze(
			(message.embeds ?? []).map((embed) => ({
				...embed,
				id: getOrCreateEmbedId(embed),
			})),
		);
		this.attachments = Object.freeze(message.attachments ?? []);
		this.stickerItems = Object.freeze(message.stickers ?? []);
		this.reactions = Object.freeze(message.reactions ?? []);

		this.messageReference = message.message_reference;
		this.referencedMessage = message.referenced_message
			? new MessageRecord(message.referenced_message, {skipUserCache: true})
			: undefined;
		this.messageSnapshots = message.message_snapshots ? Object.freeze(message.message_snapshots) : undefined;

		this.call = transformMessageCall(message.call);

		this.invites = Object.freeze(InviteUtils.findInvites(message.content));
		this.gifts = Object.freeze(GiftCodeUtils.findGifts(message.content));
		this.themes = Object.freeze(ThemeUtils.findThemes(message.content));

		this._allowedMentions = message._allowedMentions;
		this._favoriteMemeId = message._favoriteMemeId;
		this.stickers = message.stickers ? Object.freeze(message.stickers) : undefined;
	}

	hasFlag(flag: number): boolean {
		return (this.flags & flag) === flag;
	}

	get suppressEmbeds(): boolean {
		return this.hasFlag(MessageFlags.SUPPRESS_EMBEDS);
	}

	get suppressNotifications(): boolean {
		return this.hasFlag(MessageFlags.SUPPRESS_NOTIFICATIONS);
	}

	get isSilent(): boolean {
		return this.hasFlag(MessageFlags.SUPPRESS_NOTIFICATIONS);
	}

	isUserMessage(): boolean {
		return (
			this.type === MessageTypes.DEFAULT || this.type === MessageTypes.REPLY || this.type === MessageTypes.CLIENT_SYSTEM
		);
	}

	isSystemMessage(): boolean {
		return !this.isUserMessage();
	}

	isAuthor(userId?: string | null): boolean {
		return userId != null && this.author.id === userId;
	}

	isCurrentUserAuthor(): boolean {
		return this.isAuthor(AuthenticationStore.currentUserId);
	}

	isMentioned(): boolean {
		return messageMentionsCurrentUser(this.toJSON());
	}

	get isSending(): boolean {
		return this.state === MessageStates.SENDING;
	}

	get isSent(): boolean {
		return this.state === MessageStates.SENT;
	}

	get hasFailed(): boolean {
		return this.state === MessageStates.FAILED;
	}

	get isEditing(): boolean {
		return this.state === MessageStates.EDITING;
	}

	withUpdates(updates: Partial<Message>): MessageRecord {
		return new MessageRecord(
			{
				id: this.id,
				channel_id: this.channelId,
				guild_id: updates.guild_id ?? this.guildId,
				author: updates.author ?? this.author.toJSON(),
				webhook_id: updates.webhook_id ?? this.webhookId,
				application_id: updates.application_id ?? this.applicationId,
				type: updates.type ?? this.type,
				flags: updates.flags ?? this.flags,
				pinned: updates.pinned ?? this.pinned,
				mention_everyone: 'mention_everyone' in updates ? (updates.mention_everyone ?? false) : this.mentionEveryone,
				content: updates.content ?? this.content,
				timestamp: this.timestamp.toISOString(),
				edited_timestamp: updates.edited_timestamp ?? this.editedTimestamp?.toISOString(),
				mentions: 'mentions' in updates ? updates.mentions : this.mentions.map((m) => m.toJSON()),
				mention_roles: 'mention_roles' in updates ? updates.mention_roles : this.mentionRoles,
				mention_channels: updates.mention_channels ?? this.mentionChannels,
				embeds: updates.embeds ?? this.embeds,
				attachments: updates.attachments ?? this.attachments,
				stickers: updates.stickers ?? this.stickerItems,
				reactions: updates.reactions ?? this.reactions,
				message_reference: updates.message_reference ?? this.messageReference,
				referenced_message: updates.referenced_message ?? this.referencedMessage?.toJSON(),
				message_snapshots: updates.message_snapshots ?? this.messageSnapshots,
				call: updates.call ?? this.call,
				state: updates.state ?? this.state,
				nonce: updates.nonce ?? this.nonce,
				blocked: updates.blocked ?? this.blocked,
				loggingName: updates.loggingName ?? this.loggingName,
			},
			{skipUserCache: true},
		);
	}

	withReaction(emoji: ReactionEmoji, add = true, me = false): MessageRecord {
		const existingReaction = this.getReaction(emoji);

		if (!existingReaction && !add) {
			return this;
		}

		let newReactions: Array<MessageReaction>;

		if (existingReaction) {
			if (add) {
				newReactions = this.reactions.map((reaction) =>
					emojiEquals(reaction.emoji, emoji)
						? {
								...reaction,
								count: me && reaction.me ? reaction.count : reaction.count + 1,
								me: me || reaction.me ? true : undefined,
							}
						: reaction,
				);
			} else {
				const updatedCount = existingReaction.count - (me && !existingReaction.me ? 0 : 1);
				if (updatedCount <= 0) {
					newReactions = this.reactions.filter((reaction) => !emojiEquals(reaction.emoji, emoji));
				} else {
					newReactions = this.reactions.map((reaction) =>
						emojiEquals(reaction.emoji, emoji)
							? {
									...reaction,
									count: updatedCount,
									me: me ? undefined : reaction.me,
								}
							: reaction,
					);
				}
			}
		} else {
			newReactions = [
				...this.reactions,
				{
					emoji,
					count: 1,
					me: me ? true : undefined,
				},
			];
		}

		return this.withUpdates({reactions: newReactions});
	}

	withoutReactionEmoji(emoji: ReactionEmoji): MessageRecord {
		return this.withUpdates({
			reactions: this.reactions.filter((reaction) => !emojiEquals(reaction.emoji, emoji)),
		});
	}

	getReaction(emoji: ReactionEmoji): MessageReaction | undefined {
		return this.reactions.find((r) => emojiEquals(r.emoji, emoji));
	}

	equals(other: MessageRecord): boolean {
		if (this === other) return true;

		if (this.id !== other.id) return false;
		if (this.channelId !== other.channelId) return false;
		if (this.guildId !== other.guildId) return false;
		if (this.type !== other.type) return false;
		if (this.flags !== other.flags) return false;
		if (this.pinned !== other.pinned) return false;
		if (this.mentionEveryone !== other.mentionEveryone) return false;
		if (this.content !== other.content) return false;
		if (this.state !== other.state) return false;
		if (this.nonce !== other.nonce) return false;
		if (this.blocked !== other.blocked) return false;
		if (this.webhookId !== other.webhookId) return false;
		if (this.applicationId !== other.applicationId) return false;
		if (this.loggingName !== other.loggingName) return false;

		if (this.timestamp.getTime() !== other.timestamp.getTime()) return false;
		if (this.editedTimestamp?.getTime() !== other.editedTimestamp?.getTime()) return false;

		if (!this.author.equals(other.author)) return false;

		if (this.mentions.length !== other.mentions.length) return false;
		if (this.mentionRoles.length !== other.mentionRoles.length) return false;
		if (this.mentionChannels.length !== other.mentionChannels.length) return false;
		if (this.embeds.length !== other.embeds.length) return false;
		if (this.attachments.length !== other.attachments.length) return false;
		if (this.stickerItems.length !== other.stickerItems.length) return false;
		if (this.reactions.length !== other.reactions.length) return false;
		if (this.invites.length !== other.invites.length) return false;
		if (this.gifts.length !== other.gifts.length) return false;
		if (this.themes.length !== other.themes.length) return false;

		for (let i = 0; i < this.mentions.length; i++) {
			if (!this.mentions[i].equals(other.mentions[i])) return false;
		}

		for (let i = 0; i < this.mentionRoles.length; i++) {
			if (this.mentionRoles[i] !== other.mentionRoles[i]) return false;
		}

		if (this.mentionChannels.length > 0) {
			for (let i = 0; i < this.mentionChannels.length; i++) {
				if (JSON.stringify(this.mentionChannels[i]) !== JSON.stringify(other.mentionChannels[i])) {
					return false;
				}
			}
		}

		for (let i = 0; i < this.embeds.length; i++) {
			if (this.embeds[i].id !== other.embeds[i].id) return false;
		}

		for (let i = 0; i < this.attachments.length; i++) {
			const a1 = this.attachments[i];
			const a2 = other.attachments[i];
			if (
				a1.id !== a2.id ||
				a1.filename !== a2.filename ||
				a1.size !== a2.size ||
				a1.url !== a2.url ||
				a1.proxy_url !== a2.proxy_url ||
				a1.width !== a2.width ||
				a1.height !== a2.height ||
				a1.content_type !== a2.content_type ||
				a1.flags !== a2.flags
			) {
				return false;
			}
		}

		for (let i = 0; i < this.stickerItems.length; i++) {
			const s1 = this.stickerItems[i];
			const s2 = other.stickerItems[i];
			if (s1.id !== s2.id || s1.name !== s2.name || s1.format_type !== s2.format_type) {
				return false;
			}
		}

		for (let i = 0; i < this.reactions.length; i++) {
			const r1 = this.reactions[i];
			const r2 = other.reactions[i];
			if (!emojiEquals(r1.emoji, r2.emoji) || r1.count !== r2.count || r1.me !== r2.me || r1.me_burst !== r2.me_burst) {
				return false;
			}
		}

		for (let i = 0; i < this.invites.length; i++) {
			if (this.invites[i] !== other.invites[i]) return false;
		}

		for (let i = 0; i < this.gifts.length; i++) {
			if (this.gifts[i] !== other.gifts[i]) return false;
		}

		for (let i = 0; i < this.themes.length; i++) {
			if (this.themes[i] !== other.themes[i]) return false;
		}

		if (this.messageReference !== other.messageReference) {
			if (!this.messageReference || !other.messageReference) return false;
			if (
				this.messageReference.message_id !== other.messageReference.message_id ||
				this.messageReference.channel_id !== other.messageReference.channel_id ||
				this.messageReference.guild_id !== other.messageReference.guild_id ||
				this.messageReference.type !== other.messageReference.type
			) {
				return false;
			}
		}

		if (this.referencedMessage !== other.referencedMessage) {
			if (!this.referencedMessage || !other.referencedMessage) return false;
			if (!this.referencedMessage.equals(other.referencedMessage)) return false;
		}

		if (this.messageSnapshots !== other.messageSnapshots) {
			if (!this.messageSnapshots || !other.messageSnapshots) return false;
			if (this.messageSnapshots.length !== other.messageSnapshots.length) return false;
			for (let i = 0; i < this.messageSnapshots.length; i++) {
				if (JSON.stringify(this.messageSnapshots[i]) !== JSON.stringify(other.messageSnapshots[i])) {
					return false;
				}
			}
		}

		if (this.call !== other.call) {
			if (!this.call || !other.call) return false;
			if (
				this.call.participants.length !== other.call.participants.length ||
				this.call.endedTimestamp?.getTime() !== other.call.endedTimestamp?.getTime()
			) {
				return false;
			}
			for (let i = 0; i < this.call.participants.length; i++) {
				if (this.call.participants[i] !== other.call.participants[i]) return false;
			}
		}

		return true;
	}

	static hasRenderChanges(prev: MessageRecord | undefined, next: MessageRecord | undefined): boolean {
		if (!prev && !next) return false;
		if (!prev || !next) return true;
		return !prev.equals(next);
	}

	toJSON(): Message {
		return {
			id: this.id,
			channel_id: this.channelId,
			guild_id: this.guildId,
			author: this.author.toJSON(),
			webhook_id: this.webhookId,
			application_id: this.applicationId,
			type: this.type,
			flags: this.flags,
			pinned: this.pinned,
			mention_everyone: this.mentionEveryone,
			content: this.content,
			timestamp: this.timestamp.toISOString(),
			edited_timestamp: this.editedTimestamp?.toISOString(),
			mentions: this.mentions.map((user) => user.toJSON()),
			mention_roles: this.mentionRoles,
			mention_channels: this.mentionChannels,
			embeds: this.embeds,
			attachments: this.attachments,
			stickers: this.stickerItems,
			reactions: this.reactions,
			message_reference: this.messageReference,
			referenced_message: this.referencedMessage?.toJSON(),
			message_snapshots: this.messageSnapshots,
			call: this.call,
			state: this.state,
			nonce: this.nonce,
			blocked: this.blocked,
			loggingName: this.loggingName,
		};
	}
}

export const messageMentionsCurrentUser = (message: Message): boolean => {
	const channel = ChannelStore.getChannel(message.channel_id);
	if (!channel) return false;

	if (message.mention_everyone) return true;

	if (message.mentions?.some((user) => user.id === AuthenticationStore.currentUserId)) {
		return true;
	}

	if (!channel.guildId) return false;

	const guild = GuildStore.getGuild(channel.guildId);
	if (!guild) return false;

	const guildMember = GuildMemberStore.getMember(guild.id, AuthenticationStore.currentUserId);
	if (!guildMember) return false;

	return message.mention_roles?.some((roleId) => guildMember.roles.has(roleId)) ?? false;
};
