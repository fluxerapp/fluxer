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

import type {ChannelID, MessageID} from '~/BrandedTypes';
import {MessageAttachmentFlags} from '~/Constants';
import type {
	RichEmbedAuthorRequest,
	RichEmbedFooterRequest,
	RichEmbedMediaRequest,
	RichEmbedMediaWithMetadata,
	RichEmbedRequest,
} from '~/channel/ChannelModel';
import type {MessageEmbedResponse} from '~/channel/EmbedTypes';
import type {IChannelRepository} from '~/channel/IChannelRepository';
import type {MessageEmbed} from '~/database/CassandraTypes';
import {InputValidationError} from '~/Errors';
import type {ICacheService} from '~/infrastructure/ICacheService';
import {Embed, EmbedAuthor, EmbedField, EmbedFooter, EmbedMedia} from '~/Models';
import * as UnfurlerUtils from '~/utils/UnfurlerUtils';
import type {IWorkerService} from '~/worker/IWorkerService';
import type {IMediaService} from './IMediaService';
import type {IUnfurlerService} from './IUnfurlerService';

interface CreateEmbedsParams {
	channelId: ChannelID;
	messageId: MessageID;
	content: string | null;
	customEmbeds?: Array<RichEmbedRequest>;
	guildId: bigint | null;
	isNSFWAllowed: boolean;
}

export class EmbedService {
	private readonly MAX_EMBED_CHARACTERS = 6000;
	private readonly CACHE_DURATION_SECONDS = 30 * 60;

	constructor(
		private channelRepository: IChannelRepository,
		private cacheService: ICacheService,
		private unfurlerService: IUnfurlerService,
		private mediaService: IMediaService,
		private workerService: IWorkerService,
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
		isNSFWAllowed?: boolean;
	}): Promise<{embeds: Array<MessageEmbed> | null; hasUncachedUrls: boolean}> {
		if (params.customEmbeds?.length) {
			this.validateEmbedSize(params.customEmbeds);
			const embeds = await Promise.all(
				params.customEmbeds.map((embed) => this.createEmbed(embed, params.isNSFWAllowed ?? false)),
			);
			return {embeds: embeds.map((embed) => embed.toMessageEmbed()), hasUncachedUrls: false};
		}

		if (!params.content) {
			return {embeds: null, hasUncachedUrls: false};
		}

		const urls = UnfurlerUtils.extractURLs(params.content);
		if (!urls.length) {
			return {embeds: null, hasUncachedUrls: false};
		}

		const {cachedEmbeds, uncachedUrls} = await this.getCachedEmbeds(urls);
		return {
			embeds: cachedEmbeds.length > 0 ? cachedEmbeds.map((embed) => embed.toMessageEmbed()) : null,
			hasUncachedUrls: uncachedUrls.length > 0,
		};
	}

	async enqueueUrlEmbedExtraction(
		channelId: ChannelID,
		messageId: MessageID,
		guildId: bigint | null,
		isNSFWAllowed: boolean,
	): Promise<void> {
		await this.enqueue(channelId, messageId, guildId, isNSFWAllowed);
	}

	async processUrl(url: string, isNSFWAllowed: boolean = false): Promise<Array<Embed>> {
		const embedsData = await this.unfurlerService.unfurl(url, isNSFWAllowed);
		return embedsData.map((embedData) => new Embed(this.mapResponseEmbed(embedData)));
	}

	async cacheEmbeds(url: string, embeds: Array<Embed>): Promise<void> {
		if (!embeds.length) return;

		const cacheKey = `url-embed:${url}`;
		await this.cacheService.set(
			cacheKey,
			embeds.map((embed) => embed.toMessageEmbed()),
			this.CACHE_DURATION_SECONDS,
		);
	}

	private async processCustomEmbeds({
		channelId,
		messageId,
		customEmbeds,
		isNSFWAllowed,
	}: CreateEmbedsParams): Promise<Array<MessageEmbed> | null> {
		if (!customEmbeds?.length) return null;

		this.validateEmbedSize(customEmbeds);
		const embeds = await Promise.all(customEmbeds.map((embed) => this.createEmbed(embed, isNSFWAllowed)));
		await this.updateMessageEmbeds(channelId, messageId, embeds);
		return embeds.map((embed) => embed.toMessageEmbed());
	}

	private async processUrlEmbeds({
		channelId,
		messageId,
		content,
		guildId,
		isNSFWAllowed,
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

		const {cachedEmbeds, uncachedUrls} = await this.getCachedEmbeds(urls);
		if (cachedEmbeds.length) {
			await this.updateMessageEmbeds(channelId, messageId, cachedEmbeds);
		}
		if (uncachedUrls.length) {
			await this.enqueue(channelId, messageId, guildId, isNSFWAllowed);
		}

		return cachedEmbeds.length > 0 ? cachedEmbeds.map((embed) => embed.toMessageEmbed()) : null;
	}

	private mapResponseEmbed(embed: MessageEmbedResponse): MessageEmbed {
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

	private validateEmbedSize(embeds: Array<RichEmbedRequest>): void {
		const totalChars = embeds.reduce((sum, embed) => {
			return (
				sum +
				(embed.title?.length || 0) +
				(embed.description?.length || 0) +
				(embed.footer?.text?.length || 0) +
				(embed.author?.name?.length || 0) +
				(embed.fields?.reduce((sum, field) => sum + field.name.length + field.value.length, 0) || 0)
			);
		}, 0);

		if (totalChars > this.MAX_EMBED_CHARACTERS) {
			throw InputValidationError.create(
				'embeds',
				`Embeds must not exceed ${this.MAX_EMBED_CHARACTERS} characters in total`,
			);
		}
	}

	private async createEmbed(
		embed: RichEmbedRequest & {
			image?: RichEmbedMediaWithMetadata | null;
			thumbnail?: RichEmbedMediaWithMetadata | null;
		},
		isNSFWAllowed: boolean,
	): Promise<Embed> {
		const [author, footer, imageResult, thumbnailResult] = await Promise.all([
			this.processAuthor(embed.author ?? undefined, isNSFWAllowed),
			this.processFooter(embed.footer ?? undefined, isNSFWAllowed),
			this.processMedia(embed.image ?? undefined, isNSFWAllowed),
			this.processMedia(embed.thumbnail ?? undefined, isNSFWAllowed),
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
				embed.fields?.map(({name, value, inline}) =>
					new EmbedField({
						name: name || null,
						value: value || null,
						inline: inline ?? false,
					}).toMessageEmbedField(),
				) ?? null,
			nsfw,
		});
	}

	private async processMedia(
		request?: RichEmbedMediaRequest | RichEmbedMediaWithMetadata,
		isNSFWAllowed?: boolean,
	): Promise<{media: EmbedMedia; nsfw: boolean} | null> {
		if (!request?.url) return null;

		if (request.url.startsWith('attachment://')) {
			throw InputValidationError.create(
				'embeds',
				'Unresolved attachment:// URL detected. This should have been resolved before reaching embed processing.',
			);
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

		const metadata = await this.mediaService.getMetadata({
			type: 'external',
			url: request.url,
			isNSFWAllowed: isNSFWAllowed ?? false,
		});
		if (!metadata) {
			return {
				media: new EmbedMedia({
					url: request.url,
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
				url: request.url,
				width: metadata.width ?? null,
				height: metadata.height ?? null,
				description: request.description ?? null,
				content_type: metadata.content_type ?? null,
				content_hash: metadata.content_hash ?? null,
				placeholder: metadata.placeholder ?? null,
				flags:
					(metadata.animated ? MessageAttachmentFlags.IS_ANIMATED : 0) |
					(metadata.nsfw ? MessageAttachmentFlags.CONTAINS_EXPLICIT_MEDIA : 0),
				duration: metadata.duration ?? null,
			}),
			nsfw: metadata.nsfw,
		};
	}

	private async processAuthor(author?: RichEmbedAuthorRequest, isNSFWAllowed?: boolean): Promise<EmbedAuthor | null> {
		if (!author) return null;

		let iconUrl: string | null = null;
		if (author.icon_url) {
			const metadata = await this.mediaService.getMetadata({
				type: 'external',
				url: author.icon_url,
				isNSFWAllowed: isNSFWAllowed ?? false,
			});
			if (metadata) iconUrl = author.icon_url;
		}

		return new EmbedAuthor({
			name: author.name,
			url: author.url ?? null,
			icon_url: iconUrl,
		});
	}

	private async processFooter(footer?: RichEmbedFooterRequest, isNSFWAllowed?: boolean): Promise<EmbedFooter | null> {
		if (!footer) return null;

		let iconUrl: string | null = null;
		if (footer.icon_url) {
			const metadata = await this.mediaService.getMetadata({
				type: 'external',
				url: footer.icon_url,
				isNSFWAllowed: isNSFWAllowed ?? false,
			});
			if (metadata) iconUrl = footer.icon_url;
		}

		return new EmbedFooter({
			text: footer.text,
			icon_url: iconUrl,
		});
	}

	private async getCachedEmbeds(
		urls: Array<string>,
	): Promise<{cachedEmbeds: Array<Embed>; uncachedUrls: Array<string>}> {
		const cachedEmbeds: Array<Embed> = [];
		const uncachedUrls: Array<string> = [];

		for (const url of urls) {
			const cacheKey = `url-embed:${url}`;
			const cached = await this.cacheService.get<Array<MessageEmbed>>(cacheKey);
			if (cached && cached.length > 0) {
				for (const embed of cached) {
					cachedEmbeds.push(new Embed(embed));
				}
			} else {
				uncachedUrls.push(url);
			}
		}

		return {cachedEmbeds, uncachedUrls};
	}

	private async enqueue(
		channelId: ChannelID,
		messageId: MessageID,
		guildId: bigint | null,
		isNSFWAllowed: boolean,
	): Promise<void> {
		await this.workerService.addJob('extractEmbeds', {
			guildId: guildId ? guildId.toString() : null,
			channelId: channelId.toString(),
			messageId: messageId.toString(),
			isNSFWAllowed,
		});
	}

	private async updateMessageEmbeds(channelId: ChannelID, messageId: MessageID, embeds: Array<Embed>): Promise<void> {
		const currentMessage = await this.channelRepository.getMessage(channelId, messageId);
		if (!currentMessage) return;

		const currentRow = currentMessage.toRow();
		const updatedData = {
			...currentRow,
			embeds: embeds.length > 0 ? embeds.map((embed) => embed.toMessageEmbed()) : null,
			version: (currentRow.version ?? 0) + 1,
		};

		await this.channelRepository.upsertMessage(updatedData, currentRow);
	}
}
