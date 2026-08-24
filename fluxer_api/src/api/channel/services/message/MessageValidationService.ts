// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	isMessageTypeDeletable,
	MessageFlags,
	MessageTypes,
	Permissions,
	SENDABLE_MESSAGE_FLAGS,
	TEXT_BASED_CHANNEL_TYPES,
} from '@fluxer/constants/src/ChannelConstants';
import {
	ATTACHMENT_MAX_SIZE_NON_PREMIUM,
	MAX_ATTACHMENTS_PER_MESSAGE,
	MAX_EMBEDS_PER_MESSAGE,
	MAX_MESSAGE_LENGTH_NON_PREMIUM,
	MAX_MESSAGE_LENGTH_PREMIUM,
	MAX_VOICE_MESSAGE_DURATION,
} from '@fluxer/constants/src/LimitConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {CannotEditSystemMessageError} from '@fluxer/errors/src/domains/channel/CannotEditSystemMessageError';
import {CannotSendEmptyMessageError} from '@fluxer/errors/src/domains/channel/CannotSendEmptyMessageError';
import {CannotSendMessageToNonTextChannelError} from '@fluxer/errors/src/domains/channel/CannotSendMessageToNonTextChannelError';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import {type ChannelID, createChannelID, type MessageID, type UserID} from '../../../BrandedTypes';
import {contentModerationService} from '../../../infrastructure/ContentModerationService';
import type {LimitConfigService} from '../../../limits/LimitConfigService';
import {resolveLimitSafe} from '../../../limits/LimitConfigUtils';
import {createLimitMatchContext} from '../../../limits/LimitMatchContextBuilder';
import type {Channel} from '../../../models/Channel';
import type {Message} from '../../../models/Message';
import type {User} from '../../../models/User';
import {hasVisibleContent} from '../../../utils/StringUtils';
import type {MessageRequest, MessageUpdateRequest} from '../../MessageTypes';
import {assertAttachmentFileSizesWithinLimit, MESSAGE_NONCE_TTL} from './MessageHelpers';

export class MessageValidationService {
	constructor(
		private cacheService: ICacheService,
		private limitConfigService: LimitConfigService,
	) {}

	ensureTextChannel(channel: Channel): void {
		if (!TEXT_BASED_CHANNEL_TYPES.has(channel.type)) {
			throw new CannotSendMessageToNonTextChannelError();
		}
	}

	validateMessageContent(
		data: MessageRequest | MessageUpdateRequest,
		user: User | null,
		options?: {
			isUpdate?: boolean;
			guildFeatures?: Iterable<string> | null;
			messageAuthorType?: 'webhook';
		},
	): void {
		const isUpdate = options?.isUpdate ?? false;
		const hasContent = data.content != null && hasVisibleContent(data.content);
		const hasEmbeds = Boolean(data.embeds && data.embeds.length > 0);
		const hasAttachments = Boolean(data.attachments && data.attachments.length > 0);
		const hasFavoriteMeme = Boolean('favorite_meme_id' in data && data.favorite_meme_id != null);
		const hasStickers = Boolean('sticker_ids' in data && data.sticker_ids != null && data.sticker_ids.length > 0);
		const hasFlags = data.flags !== undefined && data.flags !== null;
		const guildFeatures = options?.guildFeatures ?? null;
		const hasVoiceMessageFlag = !!(data.flags && data.flags & MessageFlags.VOICE_MESSAGE);
		if (hasVoiceMessageFlag) {
			this.validateVoiceMessageConstraints(
				data,
				hasContent,
				hasEmbeds,
				hasStickers,
				hasFavoriteMeme,
				user,
				guildFeatures,
			);
		}
		if (!hasContent && !hasEmbeds && !hasAttachments && !hasFavoriteMeme && !hasStickers && (!isUpdate || !hasFlags)) {
			throw new CannotSendEmptyMessageError();
		}
		this.validateContentLength(data.content, user, guildFeatures, options?.messageAuthorType);
		const modCtx = {
			userId: user ? user.id : null,
			guildId: null,
			channelId: null,
			messageId: null,
			surface: 'message_content' as const,
		};
		contentModerationService.scanText(data.content, modCtx);
		if (data.embeds) {
			for (const embed of data.embeds) {
				contentModerationService.scanText(embed.title ?? null, modCtx);
				contentModerationService.scanText(embed.description ?? null, modCtx);
				if (embed.footer) contentModerationService.scanText(embed.footer.text ?? null, modCtx);
				if (embed.author) contentModerationService.scanText(embed.author.name ?? null, modCtx);
				if (embed.fields) {
					for (const field of embed.fields) {
						contentModerationService.scanText(field.name ?? null, modCtx);
						contentModerationService.scanText(field.value ?? null, modCtx);
					}
				}
			}
		}
		const ctx = createLimitMatchContext({user, guildFeatures});
		const evaluationContext = guildFeatures ? 'guild' : 'user';
		const snapshot = this.limitConfigService.getConfigSnapshot();
		const maxEmbeds = Math.floor(
			resolveLimitSafe(snapshot, ctx, 'max_embeds_per_message', MAX_EMBEDS_PER_MESSAGE, evaluationContext),
		);
		const maxAttachments = Math.floor(
			resolveLimitSafe(snapshot, ctx, 'max_attachments_per_message', MAX_ATTACHMENTS_PER_MESSAGE, evaluationContext),
		);
		const totalEmbeds = data.embeds?.length ?? 0;
		if (totalEmbeds > maxEmbeds) {
			throw InputValidationError.fromCode('embeds', ValidationErrorCodes.TOO_MANY_EMBEDS, {maxEmbeds});
		}
		const totalAttachments = data.attachments?.length ?? 0;
		if (totalAttachments > maxAttachments) {
			throw InputValidationError.fromCode('attachments', ValidationErrorCodes.TOO_MANY_FILES, {
				maxFiles: maxAttachments,
			});
		}
	}

	validateContentLength(
		content: string | null | undefined,
		user: User | null,
		guildFeatures?: Iterable<string> | null,
		messageAuthorType?: 'webhook',
	): void {
		if (content == null) return;
		const ctx = createLimitMatchContext({user, guildFeatures});
		const evaluationContext = guildFeatures ? 'guild' : 'user';
		const resolvedMaxLength = Math.floor(
			resolveLimitSafe(
				this.limitConfigService.getConfigSnapshot(),
				ctx,
				'max_message_length',
				MAX_MESSAGE_LENGTH_NON_PREMIUM,
				evaluationContext,
			),
		);
		const maxLength =
			user?.isBot || messageAuthorType === 'webhook'
				? Math.max(resolvedMaxLength, MAX_MESSAGE_LENGTH_PREMIUM)
				: resolvedMaxLength;
		if (content.length > maxLength) {
			throw InputValidationError.fromCode('content', ValidationErrorCodes.CONTENT_EXCEEDS_MAX_LENGTH, {
				maxLength,
			});
		}
	}

	validateMessageEditable(message: Message): void {
		const editableTypes: ReadonlySet<Message['type']> = new Set([MessageTypes.DEFAULT, MessageTypes.REPLY]);
		if (!editableTypes.has(message.type)) {
			throw new CannotEditSystemMessageError();
		}
	}

	calculateMessageFlags(data: {flags?: number}): number {
		return data.flags ? data.flags & SENDABLE_MESSAGE_FLAGS : 0;
	}

	validateTotalAttachmentSize(
		attachments: Array<{
			size: number | bigint;
		}>,
		user: User,
		guildFeatures?: Iterable<string> | null,
	): void {
		const ctx = createLimitMatchContext({user, guildFeatures});
		const evaluationContext = guildFeatures ? 'guild' : 'user';
		const fallbackMaxSize = ATTACHMENT_MAX_SIZE_NON_PREMIUM;
		const maxFileSize = Math.floor(
			resolveLimitSafe(
				this.limitConfigService.getConfigSnapshot(),
				ctx,
				'max_attachment_file_size',
				fallbackMaxSize,
				evaluationContext,
			),
		);
		assertAttachmentFileSizesWithinLimit(
			attachments.map(({size}) => size),
			maxFileSize,
		);
	}

	async findExistingMessage({
		userId,
		nonce,
		expectedChannelId,
	}: {
		userId: UserID;
		nonce?: string;
		expectedChannelId: ChannelID;
	}): Promise<Message | null> {
		if (!nonce) return null;
		const existingNonce = await this.cacheService.get<{
			channel_id: string;
			message_id: string;
		}>(`message-nonce:${userId}:${nonce}`);
		if (!existingNonce) return null;
		const cachedChannelId = createChannelID(BigInt(existingNonce.channel_id));
		if (cachedChannelId !== expectedChannelId) {
			throw new UnknownMessageError();
		}
		return null;
	}

	async cacheMessageNonce({
		userId,
		nonce,
		channelId,
		messageId,
	}: {
		userId: UserID;
		nonce: string;
		channelId: ChannelID;
		messageId: MessageID;
	}): Promise<void> {
		await this.cacheService.set(
			`message-nonce:${userId}:${nonce}`,
			{
				channel_id: channelId.toString(),
				message_id: messageId.toString(),
			},
			MESSAGE_NONCE_TTL,
		);
	}

	async canDeleteMessage({
		message,
		userId,
		guild,
		hasPermission,
	}: {
		message: Message;
		userId: UserID;
		guild: GuildResponse | null;
		hasPermission: (permission: bigint) => Promise<boolean>;
	}): Promise<boolean> {
		if (!isMessageTypeDeletable(message.type)) {
			return false;
		}
		const isAuthor = message.authorId === userId;
		if (!guild) return isAuthor;
		const canManageMessages =
			(await hasPermission(Permissions.SEND_MESSAGES)) && (await hasPermission(Permissions.MANAGE_MESSAGES));
		return isAuthor || canManageMessages;
	}

	private validateVoiceMessageConstraints(
		data: MessageRequest,
		hasContent: boolean,
		hasEmbeds: boolean,
		hasStickers: boolean,
		hasFavoriteMeme: boolean,
		user: User | null,
		guildFeatures: Iterable<string> | null | undefined,
	): void {
		if (hasContent) {
			throw InputValidationError.fromCode('content', ValidationErrorCodes.VOICE_MESSAGES_CANNOT_HAVE_CONTENT);
		}
		if (hasEmbeds) {
			throw InputValidationError.fromCode('embeds', ValidationErrorCodes.VOICE_MESSAGES_CANNOT_HAVE_EMBEDS);
		}
		if (hasFavoriteMeme) {
			throw InputValidationError.fromCode(
				'favorite_meme_id',
				ValidationErrorCodes.VOICE_MESSAGES_CANNOT_HAVE_FAVORITE_MEMES,
			);
		}
		if (hasStickers) {
			throw InputValidationError.fromCode('sticker_ids', ValidationErrorCodes.VOICE_MESSAGES_CANNOT_HAVE_STICKERS);
		}
		const attachments = data.attachments ?? [];
		if (attachments.length !== 1) {
			throw InputValidationError.fromCode('attachments', ValidationErrorCodes.VOICE_MESSAGES_REQUIRE_SINGLE_ATTACHMENT);
		}
		const attachment = attachments[0];
		if (!('waveform' in attachment) || !attachment.waveform) {
			throw InputValidationError.fromCode(
				'attachments.0.waveform',
				ValidationErrorCodes.VOICE_MESSAGES_ATTACHMENT_WAVEFORM_REQUIRED,
			);
		}
		if (!('duration' in attachment) || attachment.duration == null) {
			throw InputValidationError.fromCode(
				'attachments.0.duration',
				ValidationErrorCodes.VOICE_MESSAGES_ATTACHMENT_DURATION_REQUIRED,
			);
		}
		const duration = attachment.duration;
		const ctx = createLimitMatchContext({user, guildFeatures});
		const evaluationContext = guildFeatures ? 'guild' : 'user';
		const maxDuration = Math.floor(
			resolveLimitSafe(
				this.limitConfigService.getConfigSnapshot(),
				ctx,
				'max_voice_message_duration',
				MAX_VOICE_MESSAGE_DURATION,
				evaluationContext,
			),
		);
		if (duration > maxDuration) {
			throw InputValidationError.fromCode(
				'attachments.0.duration',
				ValidationErrorCodes.VOICE_MESSAGES_DURATION_EXCEEDS_LIMIT,
				{maxDuration},
			);
		}
	}
}
