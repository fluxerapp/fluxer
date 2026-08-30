// SPDX-License-Identifier: AGPL-3.0-or-later

import {EmbedMediaFlags} from '@fluxer/constants/src/ChannelConstants';
import {MAX_EMBEDS_PER_MESSAGE} from '@fluxer/constants/src/LimitConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import type {MessageEmbedChildResponse, MessageEmbedResponse} from '@fluxer/schema/src/domains/message/EmbedSchemas';
import type {
	RichEmbedAuthorRequest,
	RichEmbedFooterRequest,
	RichEmbedMediaRequest,
	RichEmbedRequest,
} from '@fluxer/schema/src/domains/message/MessageRequestSchemas';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import type {ChannelID, MessageID} from '../BrandedTypes';
import type {RichEmbedMediaWithMetadata} from '../channel/EmbedTypes';
import type {IChannelRepository} from '../channel/IChannelRepository';
import {nextVersion} from '../database/CassandraTypes';
import type {MessageEmbed, MessageEmbedChild} from '../database/types/MessageTypes';
import {Logger} from '../Logger';
import {Embed} from '../models/Embed';
import {EmbedAuthor} from '../models/EmbedAuthor';
import {EmbedField} from '../models/EmbedField';
import {EmbedFooter} from '../models/EmbedFooter';
import {EmbedMedia} from '../models/EmbedMedia';
import * as UnfurlerUtils from '../utils/UnfurlerUtils';
import type {WorkerTaskName} from '../worker/WorkerLaneConfig';
import {WorkerQueueOverflowError} from '../worker/WorkerQueueOverflowError';
import {
	type IMediaService,
	type MediaProxyMetadataResponse,
	type MediaProxyNsfwMode,
	mediaProxyMetadataPolicy,
} from './IMediaService';
import type {IUnfurlerService, UnfurlOptions} from './IUnfurlerService';

interface CreateEmbedsParams {
	channelId: ChannelID;
	messageId: MessageID;
	content: string | null;
	customEmbeds?: Array<RichEmbedRequest>;
	guildId: bigint | null;
	nsfwMode: MediaProxyNsfwMode;
}

interface ProcessedUrlEmbeds {
	embeds: Array<Embed>;
	cacheTtlSeconds: number | null;
}

interface InitialUrlEmbedResult {
	embeds: Array<MessageEmbed>;
	hasUncachedUrls: boolean;
}

export class EmbedService {
	private readonly MAX_EMBED_CHARACTERS = 6000;
	private readonly MAX_EMBED_CHARACTERS_BUG_HUNTER = 12000;

	constructor(
		private channelRepository: IChannelRepository,
		private unfurlerService: IUnfurlerService,
		private mediaService: IMediaService,
		private workerService: IWorkerService<WorkerTaskName>,
	) {}

	async createAndSaveEmbeds(params: CreateEmbedsParams): Promise<Array<MessageEmbed> | null> {
		if (params.customEmbeds?.length) {
			return await this.processCustomEmbeds(params);
		} else {
			return await this.processUrlEmbeds(params);
		}
	}

	async getInitialEmbeds(params: {
		content: string | null;
		customEmbeds?: Array<RichEmbedRequest>;
		nsfwMode: MediaProxyNsfwMode;
		isBugHunterBot?: boolean;
	}): Promise<{
		embeds: Array<MessageEmbed> | null;
		hasUncachedUrls: boolean;
	}> {
		if (params.customEmbeds?.length) {
			this.validateEmbedSize(params.customEmbeds, params.isBugHunterBot ?? false);
			const embeds = await Promise.all(params.customEmbeds.map((embed) => this.createEmbed(embed, params.nsfwMode)));
			return {embeds: embeds.map((embed) => embed.toMessageEmbed()), hasUncachedUrls: false};
		}
		if (!params.content) {
			return {embeds: null, hasUncachedUrls: false};
		}
		const urls = UnfurlerUtils.extractURLs(params.content);
		if (!urls.length) {
			return {embeds: null, hasUncachedUrls: false};
		}
		return this.getInitialUrlEmbeds(urls, params.nsfwMode);
	}

	async enqueueUrlEmbedExtraction(
		channelId: ChannelID,
		messageId: MessageID,
		guildId: bigint | null,
		nsfwMode: MediaProxyNsfwMode,
		options: {content?: string | null} = {},
	): Promise<void> {
		await this.enqueue(channelId, messageId, guildId, nsfwMode, options);
	}

	async processUrl(
		url: string,
		nsfwMode: MediaProxyNsfwMode = 'block',
		options: UnfurlOptions = {},
	): Promise<Array<Embed>> {
		return (await this.processUrlWithCachePolicy(url, nsfwMode, options)).embeds;
	}

	async processUrlWithCachePolicy(
		url: string,
		nsfwMode: MediaProxyNsfwMode = 'block',
		options: UnfurlOptions = {},
	): Promise<ProcessedUrlEmbeds> {
		const result = await this.unfurlerService.unfurlWithCachePolicy(url, nsfwMode, options);
		return {
			embeds: result.embeds.map((embedData) => new Embed(this.mapResponseEmbed(embedData))),
			cacheTtlSeconds: result.cacheTtlSeconds,
		};
	}

	async cacheEmbeds(_url: string, _embeds: Array<Embed>, _cacheTtlSeconds?: number | null | undefined): Promise<void> {}

	private async getInitialUrlEmbeds(
		urls: Array<string>,
		nsfwMode: MediaProxyNsfwMode,
	): Promise<{
		embeds: Array<MessageEmbed> | null;
		hasUncachedUrls: boolean;
	}> {
		const resolved = await Promise.all(urls.map((url) => this.getCachedUrlEmbeds(url, nsfwMode)));
		const orderedEmbeds: Array<MessageEmbed> = [];
		let hasUncachedUrls = false;
		for (const result of resolved) {
			if (result.hasUncachedUrls) {
				hasUncachedUrls = true;
			}
			orderedEmbeds.push(...result.embeds);
			if (orderedEmbeds.length >= MAX_EMBEDS_PER_MESSAGE) {
				return {
					embeds: orderedEmbeds.slice(0, MAX_EMBEDS_PER_MESSAGE),
					hasUncachedUrls,
				};
			}
		}
		return {
			embeds: orderedEmbeds.length > 0 ? orderedEmbeds : null,
			hasUncachedUrls,
		};
	}

	private async getCachedUrlEmbeds(url: string, nsfwMode: MediaProxyNsfwMode): Promise<InitialUrlEmbedResult> {
		try {
			const {embeds} = await this.processUrlWithCachePolicy(url, nsfwMode, {cacheOnly: true});
			if (embeds.length === 0) {
				return {embeds: [], hasUncachedUrls: true};
			}
			const messageEmbeds = embeds.map((embed) => embed.toMessageEmbed());
			const safeEmbeds =
				nsfwMode === 'allow' ? messageEmbeds : messageEmbeds.filter((embed) => !this.isEmbedNsfw(embed));
			return {embeds: safeEmbeds, hasUncachedUrls: false};
		} catch (error) {
			Logger.warn({error, url}, 'Failed to read cached URL embeds during message creation');
			return {embeds: [], hasUncachedUrls: true};
		}
	}

	private isEmbedNsfw(embed: MessageEmbed): boolean {
		if (embed.nsfw) return true;
		for (const child of embed.children ?? []) {
			if (child.nsfw) return true;
		}
		return false;
	}

	private async processCustomEmbeds({
		channelId,
		messageId,
		customEmbeds,
		nsfwMode,
	}: CreateEmbedsParams): Promise<Array<MessageEmbed> | null> {
		if (!customEmbeds?.length) return null;
		this.validateEmbedSize(customEmbeds);
		const embeds = await Promise.all(customEmbeds.map((embed) => this.createEmbed(embed, nsfwMode)));
		await this.updateMessageEmbeds(channelId, messageId, embeds);
		return embeds.map((embed) => embed.toMessageEmbed());
	}

	private async processUrlEmbeds({
		channelId,
		messageId,
		content,
		guildId,
		nsfwMode,
	}: CreateEmbedsParams): Promise<Array<MessageEmbed> | null> {
		if (!content) {
			await this.updateMessageEmbeds(channelId, messageId, []);
			return null;
		}
		const urls = UnfurlerUtils.extractURLs(content);
		if (!urls.length) {
			await this.updateMessageEmbeds(channelId, messageId, []);
			return null;
		}
		await this.enqueue(channelId, messageId, guildId, nsfwMode, {content});
		return null;
	}

	private mapResponseEmbed(embed: MessageEmbedResponse): MessageEmbed {
		return {
			...this.mapResponseEmbedChild(embed),
			children:
				embed.children && embed.children.length > 0
					? embed.children.map((child) => this.mapResponseEmbedChild(child))
					: null,
		};
	}

	private mapResponseEmbedChild(embed: MessageEmbedChildResponse): MessageEmbedChild {
		return {
			type: embed.type ?? null,
			title: embed.title ?? null,
			description: embed.description ?? null,
			url: embed.url ?? null,
			timestamp: embed.timestamp ? new Date(embed.timestamp) : null,
			color: embed.color ?? null,
			author: embed.author
				? {
						name: embed.author.name ?? null,
						url: embed.author.url ?? null,
						icon_url: embed.author.icon_url ?? null,
					}
				: null,
			provider: embed.provider
				? {
						name: embed.provider.name ?? null,
						url: embed.provider.url ?? null,
					}
				: null,
			thumbnail: this.mapResponseMedia(embed.thumbnail),
			image: this.mapResponseMedia(embed.image),
			video: this.mapResponseMedia(embed.video),
			audio: this.mapResponseMedia(embed.audio),
			footer: embed.footer
				? {
						text: embed.footer.text ?? null,
						icon_url: embed.footer.icon_url ?? null,
					}
				: null,
			fields:
				embed.fields && embed.fields.length > 0
					? embed.fields.map((field) => ({
							name: field.name ?? null,
							value: field.value ?? null,
							inline: field.inline ?? false,
						}))
					: null,
			html: embed.html ?? null,
			html_width: embed.html_width ?? null,
			html_height: embed.html_height ?? null,
			nsfw: embed.nsfw ?? null,
		};
	}

	private mapResponseMedia(media?: MessageEmbedResponse['image']): MessageEmbed['image'] {
		if (!media) return null;
		return {
			url: media.url,
			content_type: media.content_type ?? null,
			content_hash: media.content_hash ?? null,
			width: media.width ?? null,
			height: media.height ?? null,
			description: media.description ?? null,
			placeholder: media.placeholder ?? null,
			duration: media.duration ?? null,
			flags: media.flags,
		};
	}

	private validateEmbedSize(embeds: Array<RichEmbedRequest>, isBugHunterBot: boolean = false): void {
		const totalChars = embeds.reduce<number>((sum, embed) => {
			return (
				sum +
				(embed.title?.length || 0) +
				(embed.description?.length || 0) +
				(embed.footer?.text?.length || 0) +
				(embed.author?.name?.length || 0) +
				(embed.fields?.reduce((fieldSum, field) => fieldSum + field.name.length + field.value.length, 0) || 0)
			);
		}, 0);
		const maxCharacters = isBugHunterBot ? this.MAX_EMBED_CHARACTERS_BUG_HUNTER : this.MAX_EMBED_CHARACTERS;
		if (totalChars > maxCharacters) {
			throw InputValidationError.fromCode('embeds', ValidationErrorCodes.EMBEDS_EXCEED_MAX_CHARACTERS, {
				maxCharacters,
			});
		}
	}

	private async createEmbed(
		embed: RichEmbedRequest & {
			image?: RichEmbedMediaWithMetadata | null;
			thumbnail?: RichEmbedMediaWithMetadata | null;
		},
		nsfwMode: MediaProxyNsfwMode,
	): Promise<Embed> {
		const [author, footer, imageResult, thumbnailResult] = await Promise.all([
			this.processAuthor(embed.author ?? undefined, nsfwMode),
			this.processFooter(embed.footer ?? undefined, nsfwMode),
			this.processMedia(embed.image ?? undefined, nsfwMode),
			this.processMedia(embed.thumbnail ?? undefined, nsfwMode),
		]);
		let nsfw: boolean | null = null;
		const hasNSFWImage = imageResult?.nsfw ?? false;
		const hasNSFWThumbnail = thumbnailResult?.nsfw ?? false;
		if (hasNSFWImage || hasNSFWThumbnail) {
			nsfw = true;
		}
		return new Embed({
			type: 'rich',
			title: embed.title ?? null,
			description: embed.description ?? null,
			url: embed.url ?? null,
			timestamp: embed.timestamp ?? null,
			color: embed.color ?? 0,
			footer: footer?.toMessageEmbedFooter() ?? null,
			image: imageResult?.media?.toMessageEmbedMedia() ?? null,
			thumbnail: thumbnailResult?.media?.toMessageEmbedMedia() ?? null,
			video: null,
			provider: null,
			author: author?.toMessageEmbedAuthor() ?? null,
			fields:
				embed.fields?.map((field) =>
					new EmbedField({
						name: field.name,
						value: field.value,
						inline: field.inline ?? false,
					}).toMessageEmbedField(),
				) ?? null,
			children: null,
			nsfw,
		});
	}

	private async processMedia(
		request?: RichEmbedMediaRequest | RichEmbedMediaWithMetadata,
		nsfwMode: MediaProxyNsfwMode = 'block',
	): Promise<{
		media: EmbedMedia;
		nsfw: boolean;
	} | null> {
		if (!request?.url) return null;
		if (request.url.startsWith('attachment://')) {
			throw InputValidationError.fromCode('embeds', ValidationErrorCodes.UNRESOLVED_ATTACHMENT_URL);
		}
		const attachmentMetadata = (request as RichEmbedMediaWithMetadata)._attachmentMetadata;
		if (attachmentMetadata) {
			return {
				media: new EmbedMedia({
					url: request.url,
					width: attachmentMetadata.width,
					height: attachmentMetadata.height,
					description: request.description ?? null,
					content_type: attachmentMetadata.content_type,
					content_hash: attachmentMetadata.content_hash,
					placeholder: attachmentMetadata.placeholder,
					flags: attachmentMetadata.flags,
					duration: attachmentMetadata.duration,
				}),
				nsfw: attachmentMetadata.nsfw ?? false,
			};
		}
		const {url, metadata} = await this.resolveExternalMedia(request.url, nsfwMode);
		if (!metadata) {
			return {
				media: new EmbedMedia({
					url,
					width: null,
					height: null,
					description: request.description ?? null,
					content_type: null,
					content_hash: null,
					placeholder: null,
					flags: 0,
					duration: null,
				}),
				nsfw: false,
			};
		}
		return {
			media: new EmbedMedia({
				url,
				width: metadata.width ?? null,
				height: metadata.height ?? null,
				description: request.description ?? null,
				content_type: metadata.content_type ?? null,
				content_hash: metadata.content_hash ?? null,
				placeholder: metadata.placeholder ?? null,
				flags:
					(metadata.animated ? EmbedMediaFlags.IS_ANIMATED : 0) |
					(metadata.nsfw ? EmbedMediaFlags.CONTAINS_EXPLICIT_MEDIA : 0),
				duration: metadata.duration ?? null,
			}),
			nsfw: metadata.nsfw,
		};
	}

	private async resolveExternalMedia(
		url: string,
		nsfwMode: MediaProxyNsfwMode,
	): Promise<{
		url: string;
		metadata: MediaProxyMetadataResponse | null;
	}> {
		const directMetadata = await this.mediaService.getMetadata({
			type: 'external',
			url,
			...mediaProxyMetadataPolicy(nsfwMode),
		});
		if (this.isRenderableMediaType(directMetadata?.content_type)) {
			return {url, metadata: directMetadata};
		}
		const unfurled = await this.unfurlerService.unfurl(url, nsfwMode);
		for (const embed of unfurled) {
			const candidates = [embed.video?.url, embed.audio?.url, embed.image?.url, embed.thumbnail?.url];
			for (const candidate of candidates) {
				if (!candidate || candidate === url) continue;
				const candidateMetadata = await this.mediaService.getMetadata({
					type: 'external',
					url: candidate,
					...mediaProxyMetadataPolicy(nsfwMode),
				});
				if (this.isRenderableMediaType(candidateMetadata?.content_type)) {
					return {url: candidate, metadata: candidateMetadata};
				}
			}
		}
		return {url, metadata: directMetadata};
	}

	private isRenderableMediaType(contentType: string | null | undefined): boolean {
		if (!contentType) return false;
		return contentType.startsWith('image/') || contentType.startsWith('video/');
	}

	private async processAuthor(
		author?: RichEmbedAuthorRequest,
		nsfwMode: MediaProxyNsfwMode = 'block',
	): Promise<EmbedAuthor | null> {
		if (!author) return null;
		let iconUrl: string | null = null;
		if (author.icon_url) {
			const metadata = await this.mediaService.getMetadata({
				type: 'external',
				url: author.icon_url,
				...mediaProxyMetadataPolicy(nsfwMode),
			});
			if (metadata) iconUrl = author.icon_url;
		}
		return new EmbedAuthor({
			name: author.name,
			url: author.url ?? null,
			icon_url: iconUrl,
		});
	}

	private async processFooter(
		footer?: RichEmbedFooterRequest,
		nsfwMode: MediaProxyNsfwMode = 'block',
	): Promise<EmbedFooter | null> {
		if (!footer) return null;
		let iconUrl: string | null = null;
		if (footer.icon_url) {
			const metadata = await this.mediaService.getMetadata({
				type: 'external',
				url: footer.icon_url,
				...mediaProxyMetadataPolicy(nsfwMode),
			});
			if (metadata) iconUrl = footer.icon_url;
		}
		return new EmbedFooter({
			text: footer.text,
			icon_url: iconUrl,
		});
	}

	private async enqueue(
		channelId: ChannelID,
		messageId: MessageID,
		guildId: bigint | null,
		nsfwMode: MediaProxyNsfwMode,
		options: {content?: string | null} = {},
	): Promise<void> {
		const expectedContentHash =
			options.content !== undefined ? UnfurlerUtils.hashUnfurlContent(options.content) : undefined;
		try {
			await this.workerService.addJob(
				'extractEmbeds',
				{
					guildId: guildId ? guildId.toString() : null,
					channelId: channelId.toString(),
					messageId: messageId.toString(),
					nsfwMode,
					...(expectedContentHash ? {expectedContentHash} : {}),
				},
				{
					jobKey: expectedContentHash ? `${messageId.toString()}:${expectedContentHash}` : messageId.toString(),
					skipLedger: true,
				},
			);
		} catch (error) {
			if (!(error instanceof WorkerQueueOverflowError)) {
				throw error;
			}
			Logger.warn(
				{channelId: channelId.toString(), messageId: messageId.toString()},
				'Dropped url embed extraction, jobs stream is at its limit',
			);
		}
	}

	private async updateMessageEmbeds(channelId: ChannelID, messageId: MessageID, embeds: Array<Embed>): Promise<void> {
		const currentMessage = await this.channelRepository.getMessage(channelId, messageId);
		if (!currentMessage) return;
		const currentRow = currentMessage.toRow();
		const updatedData = {
			...currentRow,
			embeds: embeds.length > 0 ? embeds.map((embed) => embed.toMessageEmbed()) : null,
			version: nextVersion(currentRow.version),
		};
		await this.channelRepository.upsertMessage(updatedData, currentRow);
	}
}
