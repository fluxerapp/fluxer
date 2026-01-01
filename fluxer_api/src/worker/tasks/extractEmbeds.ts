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

import type {Task} from 'graphile-worker';
import {createChannelID, createGuildID, createMessageID} from '~/BrandedTypes';
import {MessageFlags} from '~/Constants';
import {mapMessageToResponse} from '~/channel/ChannelModel';
import type {MessageEmbed} from '~/database/CassandraTypes';
import {Logger} from '~/Logger';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import * as UnfurlerUtils from '~/utils/UnfurlerUtils';
import {ChannelEventDispatcher} from '../services/ChannelEventDispatcher';
import {CommonFields, validatePayload} from '../utils/TaskPayloadValidator';
import {getWorkerDependencies} from '../WorkerContext';

interface ExtractEmbedsPayload {
	guildId?: string | null;
	channelId: string;
	messageId: string;
	isNSFWAllowed?: boolean;
}

const payloadSchema = {
	channelId: CommonFields.channelId(),
	messageId: CommonFields.messageId(),
	guildId: CommonFields.guildId('optional'),
	isNSFWAllowed: CommonFields.boolean(),
};

const extractEmbeds: Task = async (payload, helpers) => {
	const validated = validatePayload<ExtractEmbedsPayload>(payload, payloadSchema);
	helpers.logger.debug('Processing extractEmbeds task', {payload: validated});

	const {channelRepository, userCacheService, mediaService, gatewayService, embedService, cacheService} =
		getWorkerDependencies();

	const requestCache: RequestCache = {
		userPartials: new Map(),
		clear() {
			this.userPartials.clear();
		},
	};

	const messageId = createMessageID(BigInt(validated.messageId));
	const channelId = createChannelID(BigInt(validated.channelId));

	const message = await channelRepository.getMessage(channelId, messageId);
	if (!message || !message.content) {
		Logger.debug({messageId}, 'Skipping extractEmbeds: message not found or no content');
		return;
	}

	const channel = await channelRepository.findUnique(channelId);
	if (!channel) {
		Logger.debug({channelId}, 'Skipping extractEmbeds: channel not found');
		return;
	}

	const guildId =
		validated.guildId && validated.guildId !== 'null' ? createGuildID(BigInt(validated.guildId)) : channel.guildId;

	const urls = UnfurlerUtils.extractURLs(message.content);
	if (urls.length === 0) {
		Logger.debug({messageId}, 'Skipping extractEmbeds: no URLs found');
		return;
	}

	const urlsToUnfurl: Array<string> = [];
	const cachedEmbedsByUrl = new Map<string, Array<MessageEmbed>>();

	for (const url of urls) {
		const cacheKey = `url-embed:${url}`;
		const cached = await cacheService.get<Array<MessageEmbed>>(cacheKey);
		if (cached && cached.length > 0) {
			cachedEmbedsByUrl.set(url, cached);
			Logger.debug({url, embedCount: cached.length}, 'Using cached embed(s) for URL');
		} else {
			urlsToUnfurl.push(url);
		}
	}

	if (urlsToUnfurl.length === 0) {
		Logger.debug({messageId}, 'Skipping extractEmbeds: all URLs already cached');
		return;
	}

	try {
		const unfurledEmbedsByUrl = new Map<string, Array<MessageEmbed>>();
		const isNSFWAllowed = validated.isNSFWAllowed ?? false;

		await Promise.all(
			urlsToUnfurl.map(async (url) => {
				try {
					const embeds = await embedService.processUrl(url, isNSFWAllowed);
					if (embeds.length > 0) {
						unfurledEmbedsByUrl.set(
							url,
							embeds.map((e) => e.toMessageEmbed()),
						);
						await embedService.cacheEmbeds(url, embeds);
					}
				} catch (error) {
					Logger.error({error, url}, 'Failed to unfurl URL');
				}
			}),
		);

		if (unfurledEmbedsByUrl.size > 0) {
			const latestMessage = await channelRepository.getMessage(channelId, messageId);
			if (!latestMessage) {
				Logger.debug({messageId}, 'Message no longer exists, skipping embed update');
				return;
			}

			const allEmbedsByUrl = new Map([...cachedEmbedsByUrl, ...unfurledEmbedsByUrl]);
			const orderedEmbeds: Array<MessageEmbed> = [];

			for (const url of urls) {
				if (allEmbedsByUrl.has(url)) {
					orderedEmbeds.push(...allEmbedsByUrl.get(url)!);
				}
			}

			const existingEmbeds = latestMessage.embeds ?? [];
			const existingEmbedUrls = existingEmbeds
				.map((e) => e.url)
				.filter(Boolean)
				.sort();
			const newEmbedUrls = orderedEmbeds
				.map((e) => e.url)
				.filter(Boolean)
				.sort();

			if (JSON.stringify(existingEmbedUrls) === JSON.stringify(newEmbedUrls)) {
				Logger.debug({messageId}, 'Embeds unchanged, skipping update');
				return;
			}

			await channelRepository.upsertMessage(
				{...latestMessage.toRow(), embeds: orderedEmbeds.length > 0 ? orderedEmbeds : null},
				latestMessage.toRow(),
			);
		}

		if (!(message.flags & MessageFlags.SUPPRESS_EMBEDS)) {
			const updatedMessage = await channelRepository.getMessage(channelId, messageId);
			if (updatedMessage) {
				const messageData = await mapMessageToResponse({
					message: updatedMessage,
					userCacheService,
					requestCache,
					mediaService,
				});

				const eventDispatcher = new ChannelEventDispatcher({gatewayService});

				if (guildId && !channel.guildId) {
					await gatewayService.dispatchGuild({
						guildId,
						event: 'MESSAGE_UPDATE',
						data: messageData,
					});
				} else {
					await eventDispatcher.dispatchMessageUpdate(channel, messageData);
				}

				Logger.debug({messageId: messageId.toString()}, 'Dispatched MESSAGE_UPDATE after embed processing');
			}
		} else {
			Logger.debug({messageId: messageId.toString()}, 'Skipping MESSAGE_UPDATE dispatch due to SUPPRESS_EMBEDS flag');
		}

		Logger.debug(
			{messageId: messageId.toString(), embedCount: unfurledEmbedsByUrl.size},
			'Handled extractEmbeds successfully',
		);
	} catch (error) {
		Logger.error({error, messageId: messageId.toString()}, 'Failed to process embeds');
		throw error;
	}
};

export default extractEmbeds;
