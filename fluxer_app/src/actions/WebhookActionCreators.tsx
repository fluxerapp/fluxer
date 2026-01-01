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

import {Endpoints} from '~/Endpoints';
import http from '~/lib/HttpClient';
import {Logger} from '~/lib/Logger';
import type {Webhook} from '~/records/WebhookRecord';
import WebhookStore from '~/stores/WebhookStore';

const logger = new Logger('WebhookActionCreators');

export interface CreateWebhookParams {
	channelId: string;
	name: string;
	avatar?: string | null;
}

export interface UpdateWebhookParams {
	webhookId: string;
	name?: string;
	avatar?: string | null;
}

export const fetchGuildWebhooks = async (guildId: string): Promise<Array<Webhook>> => {
	WebhookStore.handleGuildWebhooksFetchPending(guildId);

	try {
		const response = await http.get<Array<Webhook>>(Endpoints.GUILD_WEBHOOKS(guildId));
		const data = response.body;

		WebhookStore.handleGuildWebhooksFetchSuccess(guildId, data);

		return data;
	} catch (error) {
		logger.error(`Failed to fetch webhooks for guild ${guildId}:`, error);
		WebhookStore.handleGuildWebhooksFetchError(guildId);
		throw error;
	}
};

export const fetchChannelWebhooks = async ({
	guildId,
	channelId,
}: {
	guildId: string;
	channelId: string;
}): Promise<Array<Webhook>> => {
	WebhookStore.handleChannelWebhooksFetchPending(channelId);

	try {
		const response = await http.get<Array<Webhook>>(Endpoints.CHANNEL_WEBHOOKS(channelId));
		const data = response.body;

		WebhookStore.handleChannelWebhooksFetchSuccess(channelId, guildId, data);

		return data;
	} catch (error) {
		logger.error(`Failed to fetch webhooks for channel ${channelId}:`, error);
		WebhookStore.handleChannelWebhooksFetchError(channelId);
		throw error;
	}
};

export const createWebhook = async ({channelId, name, avatar}: CreateWebhookParams): Promise<Webhook> => {
	try {
		const response = await http.post<Webhook>(Endpoints.CHANNEL_WEBHOOKS(channelId), {name, avatar: avatar ?? null});
		const data = response.body;

		WebhookStore.handleWebhookCreate(data);

		return data;
	} catch (error) {
		logger.error(`Failed to create webhook for channel ${channelId}:`, error);
		throw error;
	}
};

export const deleteWebhook = async (webhookId: string): Promise<void> => {
	const existing = WebhookStore.getWebhook(webhookId);

	try {
		await http.delete({
			url: Endpoints.WEBHOOK(webhookId),
		});

		WebhookStore.handleWebhookDelete(webhookId, existing?.channelId ?? null, existing?.guildId ?? null);
	} catch (error) {
		logger.error(`Failed to delete webhook ${webhookId}:`, error);
		throw error;
	}
};

export const moveWebhook = async (webhookId: string, newChannelId: string): Promise<Webhook> => {
	const existing = WebhookStore.getWebhook(webhookId);
	if (!existing) {
		throw new Error(`Webhook ${webhookId} not found`);
	}

	try {
		const response = await http.patch<Webhook>(Endpoints.WEBHOOK(webhookId), {channel_id: newChannelId});
		const data = response.body;

		WebhookStore.handleWebhooksUpdate(existing.guildId, existing.channelId);
		WebhookStore.handleWebhookCreate(data);

		return data;
	} catch (error) {
		logger.error(`Failed to move webhook ${webhookId} to channel ${newChannelId}:`, error);
		throw error;
	}
};

const updateWebhook = async ({webhookId, name, avatar}: UpdateWebhookParams): Promise<Webhook> => {
	try {
		const response = await http.patch<Webhook>(Endpoints.WEBHOOK(webhookId), {name, avatar: avatar ?? null});
		const data = response.body;

		WebhookStore.handleWebhookCreate(data);

		return data;
	} catch (error) {
		logger.error(`Failed to update webhook ${webhookId}:`, error);
		throw error;
	}
};

export const updateWebhooks = async (updates: Array<UpdateWebhookParams>): Promise<Array<Webhook>> => {
	const results: Array<Webhook> = [];

	for (const update of updates) {
		try {
			const result = await updateWebhook(update);
			results.push(result);
		} catch (error) {
			logger.error(`Failed to update webhook ${update.webhookId}:`, error);
		}
	}

	return results;
};
