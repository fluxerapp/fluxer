// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import Authentication from '@app/features/auth/state/Authentication';
import Channels from '@app/features/channel/state/Channels';
import * as GiftCodeUtils from '@app/features/gift/utils/GiftCodeUtils';
import Guilds from '@app/features/guild/state/Guilds';
import * as InviteUtils from '@app/features/invite/utils/InviteUtils';
import GuildMembers from '@app/features/member/state/GuildMembers';
import MessageReactions from '@app/features/messaging/state/MessageReactions';
import {extractEmbeddableCodeLinkContent} from '@app/features/messaging/utils/EmbeddableCodeLinkContent';
import {emojiEquals} from '@app/features/messaging/utils/ReactionUtils';
import Relationships from '@app/features/relationship/state/Relationships';
import * as ThemeUtils from '@app/features/theme/utils/ThemeUtils';
import {User} from '@app/features/user/models/User';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import Users from '@app/features/user/state/Users';
import {LRUMap} from '@app/lib/list/ListLruMap';
import {MessageFlags, MessageStates, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import type {MessageEmbed} from '@fluxer/schema/src/domains/message/EmbedSchemas';
import type {
	AllowedMentions,
	ChannelMention,
	MessageAttachment,
	MessageCall,
	MessageMention,
	MessageReaction,
	MessageReference,
	MessageSnapshot,
	MessageStickerItem,
	ReactionEmoji,
	Message as WireMessage,
} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';

type MessageInput = Omit<WireMessage, 'mentions' | 'mention_roles' | 'tts'> &
	Partial<Pick<WireMessage, 'mentions' | 'mention_roles' | 'tts'>>;

interface TransformedMessageCall {
	participants: ReadonlyArray<string>;
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
const canonicalize = (value: unknown): string => {
	if (value === null || typeof value !== 'object') {
		return JSON.stringify(value);
	}
	if (Array.isArray(value)) {
		return `[${value.map(canonicalize).join(',')}]`;
	}
	const obj = value as Record<string, unknown>;
	const keys = Object.keys(obj).sort();
	const parts = new Array<string>(keys.length);
	for (let i = 0; i < keys.length; i++) {
		const k = keys[i];
		if (obj[k] === undefined) continue;
		parts[i] = `${JSON.stringify(k)}:${canonicalize(obj[k])}`;
	}
	return `{${parts.filter(Boolean).join(',')}}`;
};
const EMBED_CACHE_CAPACITY = 4096;
const embedCache = new LRUMap<string, string>(EMBED_CACHE_CAPACITY);
const getOrCreateEmbedId = (embed: MessageEmbed): string => {
	const {id: _existingId, ...rest} = embed;
	const key = canonicalize(rest);
	const cached = embedCache.get(key);
	if (cached !== undefined) {
		return cached;
	}
	const newId = generateEmbedId();
	embedCache.set(key, newId);
	return newId;
};
const stringArraysEqual = (a?: ReadonlyArray<string>, b?: ReadonlyArray<string>): boolean => {
	if (a === b) return true;
	if (!a || !b || a.length !== b.length) return false;
	for (let i = 0; i < a.length; i++) {
		if (a[i] !== b[i]) return false;
	}
	return true;
};
const areMessageSnapshotsEqual = (a: MessageSnapshot, b: MessageSnapshot): boolean => {
	if (
		a.type !== b.type ||
		a.content !== b.content ||
		a.flags !== b.flags ||
		a.edited_timestamp !== b.edited_timestamp ||
		a.timestamp !== b.timestamp
	) {
		return false;
	}
	if (!stringArraysEqual(a.mentions, b.mentions)) return false;
	if (!stringArraysEqual(a.mention_roles, b.mention_roles)) return false;
	const aEmbeds = a.embeds ?? [];
	const bEmbeds = b.embeds ?? [];
	if (aEmbeds.length !== bEmbeds.length) return false;
	for (let i = 0; i < aEmbeds.length; i++) {
		if (aEmbeds[i].id !== bEmbeds[i].id) return false;
	}
	const aAttachments = a.attachments ?? [];
	const bAttachments = b.attachments ?? [];
	if (aAttachments.length !== bAttachments.length) return false;
	for (let i = 0; i < aAttachments.length; i++) {
		if (aAttachments[i].id !== bAttachments[i].id) return false;
		if ((aAttachments[i].description ?? null) !== (bAttachments[i].description ?? null)) return false;
		if ((aAttachments[i].title ?? null) !== (bAttachments[i].title ?? null)) return false;
	}
	const aStickers = a.stickers ?? [];
	const bStickers = b.stickers ?? [];
	if (aStickers.length !== bStickers.length) return false;
	for (let i = 0; i < aStickers.length; i++) {
		if (aStickers[i].id !== bStickers[i].id) return false;
	}
	return true;
};

interface MessageRecordOptions {
	skipUserCache?: boolean;
	instanceId?: string;
	missingReactions?: 'empty' | 'preserve';
	skipReactionHydration?: boolean;
}

export class Message {
	readonly instanceId: string;
	readonly id: string;
	readonly channelId: string;
	readonly guildId?: string;
	readonly author: User;
	readonly webhookId?: string;
	readonly type: number;
	readonly flags: number;
	readonly pinned: boolean;
	readonly mentionEveryone: boolean;
	readonly tts: boolean;
	readonly content: string;
	readonly timestamp: Date;
	readonly editedTimestamp: Date | null;
	readonly mentions: ReadonlyArray<User>;
	readonly mentionRoles: ReadonlyArray<string>;
	readonly mentionChannels: ReadonlyArray<ChannelMention>;
	readonly embeds: ReadonlyArray<MessageEmbed>;
	readonly attachments: ReadonlyArray<MessageAttachment>;
	readonly stickerItems: ReadonlyArray<MessageStickerItem>;
	readonly nsfwEmojis: ReadonlySet<string>;
	readonly messageReference?: MessageReference;
	readonly referencedMessage?: Message | null;
	readonly messageSnapshots?: ReadonlyArray<MessageSnapshot>;
	readonly call: TransformedMessageCall | null;
	readonly state: string;
	readonly nonce?: string;
	readonly blocked: boolean;
	readonly invites: ReadonlyArray<string>;
	readonly gifts: ReadonlyArray<string>;
	readonly themes: ReadonlyArray<string>;
	readonly _allowedMentions?: AllowedMentions;
	readonly _favoriteMemeId?: string;
	readonly stickers?: ReadonlyArray<MessageStickerItem>;

	constructor(message: MessageInput, options?: MessageRecordOptions) {
		this.instanceId = options?.instanceId ?? RuntimeConfig.localInstanceDomain;
		const shouldCacheAuthor = !message.webhook_id;
		if (!options?.skipUserCache) {
			const authorsToCache = [
				...(shouldCacheAuthor ? [message.author] : []),
				...(message.mentions ?? []),
				...(message.users ?? []),
			].filter(Boolean);
			if (authorsToCache.length > 0) {
				Users.cacheUsers(authorsToCache);
			}
		}
		const isBlocked = Relationships.isBlocked(message.author.id);
		if (message.webhook_id) {
			this.author = new User(message.author, {instanceId: this.instanceId});
		} else {
			this.author = Users.getUser(message.author.id) || new User(message.author, {instanceId: this.instanceId});
		}
		this.id = message.id;
		this.channelId = message.channel_id;
		this.guildId = message.guild_id;
		this.webhookId = message.webhook_id;
		this.type = message.type;
		this.flags = message.flags;
		this.pinned = message.pinned;
		this.mentionEveryone = message.mention_everyone;
		this.tts = message.tts ?? false;
		this.content = message.content;
		this.timestamp = new Date(message.timestamp);
		this.editedTimestamp = message.edited_timestamp ? new Date(message.edited_timestamp) : null;
		this.state = message.state ?? MessageStates.SENT;
		this.nonce = message.nonce;
		this.blocked = message.blocked ?? isBlocked;
		this.mentions = Object.freeze((message.mentions ?? []).map((user) => new User(user)));
		this.mentionRoles = Object.freeze(message.mention_roles ?? []);
		this.mentionChannels = Object.freeze(message.mention_channels ?? []);
		this.embeds = Object.freeze(
			(message.embeds ?? []).map((embed) => {
				const id = embed.id ?? getOrCreateEmbedId(embed);
				return embed.id === id ? embed : {...embed, id};
			}),
		);
		this.attachments = Object.freeze(message.attachments ?? []);
		this.stickerItems = Object.freeze(message.stickers ?? []);
		this.nsfwEmojis = Object.freeze(new Set(message.nsfw_emojis ?? []));
		if (!options?.skipReactionHydration) {
			if ('reactions' in message) {
				MessageReactions.hydrateMessageReactions(this.id, message.reactions);
			} else if (options?.missingReactions !== 'preserve') {
				MessageReactions.hydrateMessageReactions(this.id, []);
			}
		}
		this.messageReference = message.message_reference;
		this.referencedMessage = message.referenced_message
			? new Message(message.referenced_message, {
					skipUserCache: true,
					missingReactions: 'preserve',
					skipReactionHydration: options?.skipReactionHydration,
				})
			: undefined;
		this.messageSnapshots = message.message_snapshots ? Object.freeze(message.message_snapshots) : undefined;
		this.call = transformMessageCall(message.call);
		const embeddableCodeLinkContent = extractEmbeddableCodeLinkContent(message.content);
		this.invites = Object.freeze(InviteUtils.findInvites(embeddableCodeLinkContent));
		this.gifts = Object.freeze(GiftCodeUtils.findGifts(embeddableCodeLinkContent));
		this.themes = Object.freeze(ThemeUtils.findThemes(embeddableCodeLinkContent));
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

	isClientSystemMessage(): boolean {
		return this.type === MessageTypes.CLIENT_SYSTEM;
	}

	isSystemMessage(): boolean {
		return !this.isUserMessage();
	}

	isAuthor(userId?: string | null): boolean {
		return userId != null && this.author.id === userId;
	}

	isCurrentUserAuthor(): boolean {
		return this.isAuthor(Authentication.currentUserId);
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

	get reactions(): ReadonlyArray<MessageReaction> {
		return MessageReactions.getMessageReactions(this.id);
	}

	withUpdates(updates: Partial<WireMessage>): Message {
		if ('reactions' in updates) {
			MessageReactions.replaceMessageReactions(this.id, updates.reactions ?? []);
		}
		return new Message(
			{
				id: this.id,
				channel_id: this.channelId,
				guild_id: updates.guild_id ?? this.guildId,
				author: updates.author ?? this.author.toJSON(),
				webhook_id: updates.webhook_id ?? this.webhookId,
				type: updates.type ?? this.type,
				flags: updates.flags ?? this.flags,
				pinned: updates.pinned ?? this.pinned,
				mention_everyone: 'mention_everyone' in updates ? (updates.mention_everyone ?? false) : this.mentionEveryone,
				tts: 'tts' in updates ? (updates.tts ?? false) : this.tts,
				content: updates.content ?? this.content,
				timestamp: this.timestamp.toISOString(),
				edited_timestamp: updates.edited_timestamp ?? this.editedTimestamp?.toISOString(),
				mentions: ('mentions' in updates ? updates.mentions : this.mentions.map((m) => m.toJSON())) ?? [],
				mention_roles: ('mention_roles' in updates ? updates.mention_roles : this.mentionRoles) ?? [],
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
				_allowedMentions: updates._allowedMentions ?? this._allowedMentions,
				_favoriteMemeId: updates._favoriteMemeId ?? this._favoriteMemeId,
			},
			{skipUserCache: true, instanceId: this.instanceId},
		);
	}

	withReaction(emoji: ReactionEmoji, add = true, me = false): Message {
		const existing = this.getReaction(emoji);
		let newReactions: Array<MessageReaction>;
		if (add) {
			if (!existing) {
				newReactions = [...this.reactions, {emoji, count: 1, me: me ? true : undefined}];
			} else if (me && existing.me) {
				return this;
			} else {
				newReactions = this.reactions.map((r) =>
					emojiEquals(r.emoji, emoji) ? {...r, count: r.count + 1, me: me || r.me ? true : undefined} : r,
				);
			}
		} else {
			if (!existing) return this;
			if (me && !existing.me) return this;
			const nextCount = Math.max(0, existing.count - 1);
			if (nextCount === 0) {
				newReactions = this.reactions.filter((r) => !emojiEquals(r.emoji, emoji));
			} else {
				newReactions = this.reactions.map((r) =>
					emojiEquals(r.emoji, emoji) ? {...r, count: nextCount, me: me ? undefined : r.me} : r,
				);
			}
		}
		return this.withUpdates({reactions: newReactions});
	}

	withoutReactionEmoji(emoji: ReactionEmoji): Message {
		return this.withUpdates({
			reactions: this.reactions.filter((reaction) => !emojiEquals(reaction.emoji, emoji)),
		});
	}

	getReaction(emoji: ReactionEmoji): MessageReaction | undefined {
		return this.reactions.find((r) => emojiEquals(r.emoji, emoji));
	}

	equals(other: Message): boolean {
		if (this === other) return true;
		if (this.instanceId !== other.instanceId) return false;
		if (this.id !== other.id) return false;
		if (this.channelId !== other.channelId) return false;
		if (this.guildId !== other.guildId) return false;
		if (this.type !== other.type) return false;
		if (this.flags !== other.flags) return false;
		if (this.pinned !== other.pinned) return false;
		if (this.mentionEveryone !== other.mentionEveryone) return false;
		if (this.tts !== other.tts) return false;
		if (this.content !== other.content) return false;
		if (this.state !== other.state) return false;
		if (this.nonce !== other.nonce) return false;
		if (this.blocked !== other.blocked) return false;
		if (this.webhookId !== other.webhookId) return false;
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
				const m1 = this.mentionChannels[i];
				const m2 = other.mentionChannels[i];
				if (m1.id !== m2.id || m1.type !== m2.type || m1.name !== m2.name) {
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
			if (s1.id !== s2.id || s1.name !== s2.name || s1.animated !== s2.animated) {
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
				if (!areMessageSnapshotsEqual(this.messageSnapshots[i], other.messageSnapshots[i])) {
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

	static hasRenderChanges(prev: Message | undefined, next: Message | undefined): boolean {
		if (!prev && !next) return false;
		if (!prev || !next) return true;
		return !prev.equals(next);
	}

	toJSON(): WireMessage {
		return {
			id: this.id,
			channel_id: this.channelId,
			guild_id: this.guildId,
			author: this.author.toJSON(),
			webhook_id: this.webhookId,
			type: this.type,
			flags: this.flags,
			pinned: this.pinned,
			mention_everyone: this.mentionEveryone,
			tts: this.tts,
			content: this.content,
			timestamp: this.timestamp.toISOString(),
			edited_timestamp: this.editedTimestamp?.toISOString(),
			mentions: this.mentions.map((user) => user.toJSON() as MessageMention),
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
			_allowedMentions: this._allowedMentions,
			_favoriteMemeId: this._favoriteMemeId,
		};
	}
}

export const messageMentionsCurrentUser = (message: WireMessage): boolean => {
	const channel = Channels.getChannel(message.channel_id);
	if (!channel) return false;
	if (message.mention_everyone && !UserGuildSettings.isEveryoneMentionSuppressed(channel.guildId ?? null)) return true;
	if (message.mentions?.some((user) => user.id === Authentication.currentUserId)) {
		return true;
	}
	if (!channel.guildId) return false;
	if (UserGuildSettings.isRoleMentionSuppressed(channel.guildId)) return false;
	const guild = Guilds.getGuild(channel.guildId);
	if (!guild) return false;
	const guildMember = GuildMembers.getMember(guild.id, Authentication.currentUserId);
	if (!guildMember) return false;
	return message.mention_roles?.some((roleId) => guildMember.roles.has(roleId)) ?? false;
};
