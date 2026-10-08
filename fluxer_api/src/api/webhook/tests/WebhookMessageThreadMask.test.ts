// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createChannelID,
	createGuildID,
	createMessageID,
	type WebhookID,
	type WebhookToken,
} from '@app/api/BrandedTypes';
import type {IChannelRepository} from '@app/api/channel/IChannelRepository';
import {createMessageResponseDataService} from '@app/api/channel/services/message/MessageResponseDataService';
import {syncChannelThreadsConfig} from '@app/api/experiment/ChannelThreadsGate';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {Message} from '@app/api/models/Message';
import type {SweegoWebhookService} from '@app/api/webhook/SweegoWebhookService';
import {WebhookRequestService} from '@app/api/webhook/WebhookRequestService';
import type {WebhookService} from '@app/api/webhook/WebhookService';
import {MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {ServerMessageFlags} from '@fluxer/constants/src/ThreadConstants';
import {ChannelThreadsConfigSchema} from '@fluxer/schema/src/domains/admin/ChannelThreadsSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterEach, describe, expect, it, vi} from 'vitest';

const GUILD = createGuildID(900n);
const CHANNEL = createChannelID(100n);

function setConfig(config: Record<string, unknown> | null): void {
	syncChannelThreadsConfig(config === null ? null : JSON.stringify(config), (raw) =>
		ChannelThreadsConfigSchema.parse(raw ? JSON.parse(raw) : {}),
	);
}

async function getMessage(): Promise<MessageResponse> {
	const message = {id: createMessageID(41n), channelId: CHANNEL} as unknown as Message;
	const response = {
		id: '41',
		channel_id: '100',
		type: MessageTypes.DEFAULT,
		flags: ServerMessageFlags.HAS_THREAD | 4,
	} as MessageResponse;
	vi.spyOn(createMessageResponseDataService(), 'buildMessage').mockResolvedValue(response);
	const service = new WebhookRequestService(
		{getWebhookMessage: async () => message} as unknown as WebhookService,
		{findUnique: async () => ({id: CHANNEL, guildId: GUILD})} as unknown as IChannelRepository,
		{} as UserCacheService,
		null,
		{} as SweegoWebhookService,
		{} as IGatewayService,
	);
	return service.getWebhookMessage({
		webhookId: 1n as WebhookID,
		token: 'token' as WebhookToken,
		messageId: message.id,
		requestCache: {} as RequestCache,
	});
}

describe('webhook message thread masking', () => {
	afterEach(() => {
		setConfig(null);
		vi.restoreAllMocks();
	});

	it('masks the thread flag once the guild is killed', async () => {
		setConfig({enabled: false, ever_enabled: true});
		expect((await getMessage()).flags).toBe(4);
	});

	it('keeps the thread flag while the guild is active', async () => {
		setConfig({enabled: true, ever_enabled: true, enabled_guild_ids: ['900']});
		expect((await getMessage()).flags).toBe(ServerMessageFlags.HAS_THREAD | 4);
	});
});
