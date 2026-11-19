// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {MessageRequest, MessageUpdateRequest} from '@app/api/channel/MessageTypes';
import {assertAttachmentFileSizesWithinLimit} from '@app/api/channel/services/message/MessageHelpers';
import {contentModerationService} from '@app/api/infrastructure/ContentModerationService';
import {resolveAttachmentMaxFileSize} from '@app/api/limits/AttachmentFileSizeLimit';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import {resolveLimitSafe} from '@app/api/limits/LimitConfigUtils';
import {createLimitMatchContext} from '@app/api/limits/LimitMatchContextBuilder';
import type {Message} from '@app/api/models/Message';
import type {User} from '@app/api/models/User';
import {
	isMessageTypeDeletable,
	MessageFlags,
	MessageTypes,
	Permissions,
	SENDABLE_MESSAGE_FLAGS,
} from '@fluxer/constants/src/ChannelConstants';
import {
	MAX_ATTACHMENTS_PER_MESSAGE,
	MAX_EMBEDS_PER_MESSAGE,
	MAX_MESSAGE_LENGTH_NON_PREMIUM,
	MAX_MESSAGE_LENGTH_PREMIUM,
	MAX_POLL_ANSWER_LENGTH,
	MAX_POLL_ANSWERS,
	MAX_POLL_DURATION_HOURS,
	MAX_POLL_QUESTION_LENGTH,
	MAX_VOICE_MESSAGE_DURATION,
} from '@fluxer/constants/src/LimitConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {CannotEditPollError} from '@fluxer/errors/src/domains/channel/CannotEditPollError';
import {CannotEditSystemMessageError} from '@fluxer/errors/src/domains/channel/CannotEditSystemMessageError';
import {CannotSendEmptyMessageError} from '@fluxer/errors/src/domains/channel/CannotSendEmptyMessageError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {MessagePollRequest} from '@fluxer/schema/src/domains/message/PollSchemas';
import {hasVisibleContent} from '@fluxer/schema/src/primitives/MessageValidators';

export class MessageValidationService {
	constructor(private limitConfigService: LimitConfigService) {}

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
		const hasPoll = Boolean('poll' in data && data.poll);
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
				hasPoll,
				hasStickers,
				hasFavoriteMeme,
				user,
				guildFeatures,
			);
		}
		if (
			!hasContent &&
			!hasEmbeds &&
			!hasAttachments &&
			!hasFavoriteMeme &&
			!hasStickers &&
			(!isUpdate || !hasFlags) &&
			!hasPoll
		) {
			throw new CannotSendEmptyMessageError();
		}
		this.validateContentLength(data.content, user, guildFeatures, options?.messageAuthorType);
		if ('poll' in data && data.poll) this.validatePollContentLength(data.poll, user, guildFeatures);
		const modCtx = {
			userId: user ? user.id : null,
			guildId: null,
			channelId: null,
			messageId: null,
			surface: 'message_content' as const,
		};
		contentModerationService.scanText(data.content, modCtx);
		if ('poll' in data && data.poll) {
			contentModerationService.scanText(data.poll.question?.text ?? null, modCtx);
			if (data.poll.answers) {
				for (const answer of data.poll.answers)
					contentModerationService.scanText(answer.poll_media?.text ?? null, modCtx);
			}
		}
		if (data.embeds) {
			for (const embed of data.embeds) {
				if (embed.url) contentModerationService.scanUrl(embed.url, modCtx);
				contentModerationService.scanText(embed.title ?? null, modCtx);
				contentModerationService.scanText(embed.description ?? null, modCtx);
				if (embed.footer) {
					contentModerationService.scanText(embed.footer.text ?? null, modCtx);
					if (embed.footer.icon_url) contentModerationService.scanUrl(embed.footer.icon_url, modCtx);
				}
				if (embed.author) {
					contentModerationService.scanText(embed.author.name ?? null, modCtx);
					if (embed.author.url) contentModerationService.scanUrl(embed.author.url, modCtx);
					if (embed.author.icon_url) contentModerationService.scanUrl(embed.author.icon_url, modCtx);
				}
				for (const media of [embed.image, embed.thumbnail]) {
					if (!media) continue;
					contentModerationService.scanUrl(media.url, modCtx);
					contentModerationService.scanText(media.description ?? null, modCtx);
				}
				if (embed.fields) {
					for (const field of embed.fields) {
						contentModerationService.scanText(field.name ?? null, modCtx);
						contentModerationService.scanText(field.value ?? null, modCtx);
					}
				}
			}
		}
		if (data.attachments) {
			for (const attachment of data.attachments) {
				contentModerationService.scanText(attachment.title ?? null, modCtx);
				contentModerationService.scanText(attachment.description ?? null, modCtx);
			}
		}
		if ('message_snapshots' in data && data.message_snapshots) {
			for (const snapshot of data.message_snapshots) {
				for (const attachment of snapshot.attachments ?? []) {
					contentModerationService.scanText(attachment.title ?? null, modCtx);
					contentModerationService.scanText(attachment.description ?? null, modCtx);
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

	private validateContentLength(
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

	validatePollContentLength(
		poll: MessagePollRequest,
		user: User | null,
		guildFeatures?: Iterable<string> | null,
	): void {
		const ctx = createLimitMatchContext({user, guildFeatures});
		const evaluationContext = guildFeatures ? 'guild' : 'user';
		const configSnapshot = this.limitConfigService.getConfigSnapshot();

		const maxQuestionLength = Math.floor(
			resolveLimitSafe(configSnapshot, ctx, 'max_poll_question_length', MAX_POLL_QUESTION_LENGTH, evaluationContext),
		);
		const maxPollAnswers = Math.floor(
			resolveLimitSafe(configSnapshot, ctx, 'max_poll_answers', MAX_POLL_ANSWERS, evaluationContext),
		);
		const maxPollAnswerLength = Math.floor(
			resolveLimitSafe(configSnapshot, ctx, 'max_poll_answer_length', MAX_POLL_ANSWER_LENGTH, evaluationContext),
		);
		const maxPollDurationHours = Math.floor(
			resolveLimitSafe(configSnapshot, ctx, 'max_poll_duration_hours', MAX_POLL_DURATION_HOURS, evaluationContext),
		);

		const questionLength = poll.question?.text?.length ?? 0;
		if (questionLength === 0) throw new CannotSendEmptyMessageError();
		if (questionLength > maxQuestionLength) {
			throw InputValidationError.fromCode('poll.question.text', ValidationErrorCodes.CONTENT_EXCEEDS_MAX_LENGTH, {
				maxLength: maxQuestionLength,
			});
		}

		const answers = poll.answers ?? [];
		if (answers.length === 0) throw new CannotSendEmptyMessageError();
		if (answers.length > maxPollAnswers) {
			throw InputValidationError.fromCode('poll.answers', ValidationErrorCodes.TOO_MANY_POLL_ANSWERS, {
				maxPollAnswers,
			});
		}

		for (let i = 0; i < answers.length; i++) {
			const answerTextLength = answers[i].poll_media?.text?.length ?? 0;
			if (answerTextLength === 0) throw new CannotSendEmptyMessageError();
			if (answerTextLength > maxPollAnswerLength) {
				throw InputValidationError.fromCode(
					`poll.answers[${i}].poll_media.text`,
					ValidationErrorCodes.CONTENT_EXCEEDS_MAX_LENGTH,
					{
						maxLength: maxPollAnswerLength,
					},
				);
			}
		}

		const duration = poll.duration ?? 0;
		if (duration < 1) throw InputValidationError.fromCode(`poll.duration`, ValidationErrorCodes.POLL_DURATION_TOO_LOW);
		if (duration > maxPollDurationHours) {
			throw InputValidationError.fromCode(`poll.duration`, ValidationErrorCodes.POLL_DURATION_TOO_HIGH, {
				maxPollDurationHours,
			});
		}
	}

	validateMessageEditable(message: Message): void {
		const editableTypes: ReadonlySet<Message['type']> = new Set([MessageTypes.DEFAULT, MessageTypes.REPLY]);
		if (!editableTypes.has(message.type)) {
			throw new CannotEditSystemMessageError();
		}
		if (message.poll) {
			throw new CannotEditPollError();
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
		const maxFileSize = resolveAttachmentMaxFileSize(this.limitConfigService.getConfigSnapshot(), {
			user,
			guildFeatures,
		});
		assertAttachmentFileSizesWithinLimit(
			attachments.map(({size}) => size),
			maxFileSize,
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
		if (isAuthor) return true;
		return hasPermission(Permissions.MANAGE_MESSAGES);
	}

	private validateVoiceMessageConstraints(
		data: MessageRequest,
		hasContent: boolean,
		hasEmbeds: boolean,
		hasPoll: boolean,
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
		if (hasPoll) {
			throw InputValidationError.fromCode('poll', ValidationErrorCodes.VOICE_MESSAGES_CANNOT_HAVE_A_POLL);
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
