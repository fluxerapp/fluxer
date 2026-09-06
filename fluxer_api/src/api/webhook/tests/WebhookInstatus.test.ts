// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterEach, beforeEach, describe, expect, it} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {createGuild} from '../../guild/tests/GuildTestUtils';
import {getMessages} from '../../message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilderWithoutAuth} from '../../test/TestRequestBuilder';
import {createWebhook, deleteWebhook} from './WebhookTestUtils';

function createInstatusPage() {
	return {
		id: 'pg_123',
		status_indicator: 'HASISSUES',
		status_description: 'Partially degraded service',
		url: 'https://status.fluxer.app',
	};
}

function createInstatusMeta() {
	// `documentation` seems to always be an empty string.
	// If only we had documentation about it.
	return {unsubscribe: 'https://status.fluxer.app/unsubscribe?id=1&token=abc', documentation: ''};
}

function createInstatusIncident(overrides: Record<string, unknown> = {}) {
	return {
		id: 'inc_1',
		name: 'Elevated API latency',
		url: 'https://status.fluxer.app/incident/inc_1',
		status: 'Investigating',
		backfilled: false,
		resolved_at: null,
		created_at: '2026-07-06T10:00:00.000Z',
		updated_at: '2026-07-06T10:00:00.000Z',
		incident_updates: [
			{
				id: 'u_1',
				incident_id: 'inc_1',
				markdown: 'We are currently investigating this incident.',
				status: 'Investigating',
				created_at: '2026-07-06T10:00:00.000Z',
				updated_at: '2026-07-06T10:00:00.000Z',
			},
		],
		affected_components: [],
		...overrides,
	};
}

async function countWebhookMessages(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
	webhookId: string,
): Promise<number> {
	const messages = await getMessages(harness, token, channelId);
	return messages.filter((message) => message.webhook_id === webhookId).length;
}

describe('Webhook Instatus integration', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	describe('POST /webhooks/:webhook_id/:token/instatus', () => {
		it('accepts an incident notification', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Instatus Incident Guild');
			const channelId = guild.system_channel_id!;
			const webhook = await createWebhook(harness, channelId, owner.token, 'Instatus Webhook');
			await createBuilderWithoutAuth(harness)
				.post(`/webhooks/${webhook.id}/${webhook.token}/instatus`)
				.body({meta: createInstatusMeta(), page: createInstatusPage(), incident: createInstatusIncident()})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			await deleteWebhook(harness, webhook.id, owner.token);
		});
		it('suppresses a repeated incident callback within the deduplication window', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Instatus Duplicate Guild');
			const channelId = guild.system_channel_id!;
			const webhook = await createWebhook(harness, channelId, owner.token, 'Instatus Duplicate Webhook');
			const body = {meta: createInstatusMeta(), page: createInstatusPage(), incident: createInstatusIncident()};
			for (let attempt = 0; attempt < 2; attempt++) {
				await createBuilderWithoutAuth(harness)
					.post(`/webhooks/${webhook.id}/${webhook.token}/instatus`)
					.body(body)
					.expect(HTTP_STATUS.NO_CONTENT)
					.execute();
			}
			expect(await countWebhookMessages(harness, owner.token, channelId, webhook.id)).toBe(1);
			await deleteWebhook(harness, webhook.id, owner.token);
		});
		it('processes a callback carrying no identifier every time', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Instatus Unidentified Guild');
			const channelId = guild.system_channel_id!;
			const webhook = await createWebhook(harness, channelId, owner.token, 'Instatus Unidentified Webhook');
			const body = {
				meta: createInstatusMeta(),
				page: createInstatusPage(),
				incident: createInstatusIncident({id: null}),
			};
			for (let attempt = 0; attempt < 2; attempt++) {
				await createBuilderWithoutAuth(harness)
					.post(`/webhooks/${webhook.id}/${webhook.token}/instatus`)
					.body(body)
					.expect(HTTP_STATUS.NO_CONTENT)
					.execute();
			}
			expect(await countWebhookMessages(harness, owner.token, channelId, webhook.id)).toBe(2);
			await deleteWebhook(harness, webhook.id, owner.token);
		});
		it('accepts a component status update', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Instatus Component Guild');
			const channelId = guild.system_channel_id!;
			const webhook = await createWebhook(harness, channelId, owner.token, 'Instatus Component Webhook');
			await createBuilderWithoutAuth(harness)
				.post(`/webhooks/${webhook.id}/${webhook.token}/instatus`)
				.body({
					meta: createInstatusMeta(),
					page: {...createInstatusPage(), status_indicator: 'UP', status_description: 'All Systems Operational'},
					component_update: {created_at: '2026-07-06T11:00:00.000Z', new_status: 'Major outage', component_id: 'c_1'},
					component: {id: 'c_1', name: 'API', status: 'Major outage', created_at: '2026-01-01T00:00:00.000Z'},
				})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			await deleteWebhook(harness, webhook.id, owner.token);
		});
		it('accepts an empty selected payload without sending', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Instatus Empty Guild');
			const channelId = guild.system_channel_id!;
			const webhook = await createWebhook(harness, channelId, owner.token, 'Instatus Empty Webhook');
			await createBuilderWithoutAuth(harness)
				.post(`/webhooks/${webhook.id}/${webhook.token}/instatus`)
				.body({meta: createInstatusMeta(), page: createInstatusPage()})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			await deleteWebhook(harness, webhook.id, owner.token);
		});
		it('rejects an instatus webhook with an invalid token', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Instatus Invalid Token Guild');
			const channelId = guild.system_channel_id!;
			const webhook = await createWebhook(harness, channelId, owner.token, 'Instatus Invalid Webhook');
			await createBuilderWithoutAuth(harness)
				.post(`/webhooks/${webhook.id}/invalid_token/instatus`)
				.body({
					meta: createInstatusMeta(),
					page: createInstatusPage(),
					incident: {name: 'Outage', status: 'INVESTIGATING'},
				})
				.expect(HTTP_STATUS.NOT_FOUND)
				.execute();
			await deleteWebhook(harness, webhook.id, owner.token);
		});
	});
});
