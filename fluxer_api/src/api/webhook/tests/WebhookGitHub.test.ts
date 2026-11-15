// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {createWebhook, deleteWebhook} from '@app/api/webhook/tests/WebhookTestUtils';
import {afterEach, beforeEach, describe, it} from 'vitest';

function createGitHubSender() {
	return {
		id: 123456,
		login: 'testauthor',
		html_url: 'https://github.com/testauthor',
		avatar_url: 'https://avatars.githubusercontent.com/u/123456',
	};
}

function createGitHubRepository() {
	return {
		id: 789,
		full_name: 'test/repo',
		name: 'repo',
		html_url: 'https://github.com/test/repo',
	};
}

describe('Webhook GitHub integration', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	describe('POST /webhooks/:webhook_id/:token/github', () => {
		it('accepts valid github push event', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'GitHub Webhook Guild');
			const channelId = guild.system_channel_id!;
			const webhook = await createWebhook(harness, channelId, owner.token, 'GitHub Webhook');
			const githubPayload = {
				ref: 'refs/heads/main',
				repository: createGitHubRepository(),
				commits: [
					{
						id: 'abc123def456789012345678901234567890abcd',
						message: 'Test commit',
						url: 'https://github.com/test/repo/commit/abc123',
						author: {
							name: 'Test Author',
							username: 'testauthor',
						},
					},
				],
				head_commit: {
					id: 'abc123def456789012345678901234567890abcd',
					message: 'Test commit',
					url: 'https://github.com/test/repo/commit/abc123',
					author: {
						name: 'Test Author',
						username: 'testauthor',
					},
				},
				compare: 'https://github.com/test/repo/compare/before...after',
				sender: createGitHubSender(),
			};
			await createBuilderWithoutAuth(harness)
				.post(`/webhooks/${webhook.id}/${webhook.token}/github`)
				.header('X-GitHub-Event', 'push')
				.header('X-GitHub-Delivery', 'test-delivery-id-123')
				.body(githubPayload)
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			await deleteWebhook(harness, webhook.id, owner.token);
		});
		it('rejects github webhook with invalid token', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'GitHub Invalid Token Guild');
			const channelId = guild.system_channel_id!;
			const webhook = await createWebhook(harness, channelId, owner.token, 'GitHub Invalid Webhook');
			await createBuilderWithoutAuth(harness)
				.post(`/webhooks/${webhook.id}/invalid_token/github`)
				.header('X-GitHub-Event', 'push')
				.header('X-GitHub-Delivery', 'test-invalid-delivery')
				.body({
					ref: 'refs/heads/main',
					repository: createGitHubRepository(),
					sender: createGitHubSender(),
				})
				.expect(HTTP_STATUS.NOT_FOUND)
				.execute();
			await deleteWebhook(harness, webhook.id, owner.token);
		});
		it('handles github webhook with missing event header', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'GitHub No Header Guild');
			const channelId = guild.system_channel_id!;
			const webhook = await createWebhook(harness, channelId, owner.token, 'GitHub No Header Webhook');
			await createBuilderWithoutAuth(harness)
				.post(`/webhooks/${webhook.id}/${webhook.token}/github`)
				.body({
					ref: 'refs/heads/main',
					repository: createGitHubRepository(),
					sender: createGitHubSender(),
				})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			await deleteWebhook(harness, webhook.id, owner.token);
		});
		it('handles unknown github event type gracefully', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'GitHub Unknown Event Guild');
			const channelId = guild.system_channel_id!;
			const webhook = await createWebhook(harness, channelId, owner.token, 'GitHub Unknown Event Webhook');
			await createBuilderWithoutAuth(harness)
				.post(`/webhooks/${webhook.id}/${webhook.token}/github`)
				.header('X-GitHub-Event', 'unknown_event_type')
				.header('X-GitHub-Delivery', 'test-unknown-delivery')
				.body({
					repository: createGitHubRepository(),
					sender: createGitHubSender(),
				})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			await deleteWebhook(harness, webhook.id, owner.token);
		});
	});
});
