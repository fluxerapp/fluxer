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

import {createTestAccount} from '@fluxer/api/src/auth/tests/AuthTestUtils';
import {
	createOAuth2Application,
	createUniqueApplicationName,
	deleteOAuth2Application,
	getOAuth2Application,
	listOAuth2Applications,
} from '@fluxer/api/src/oauth/tests/OAuth2TestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@fluxer/api/src/test/ApiTestHarness';
import {HTTP_STATUS} from '@fluxer/api/src/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@fluxer/api/src/test/TestRequestBuilder';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {beforeEach, describe, expect, test} from 'vitest';

describe('OAuth2 Application Delete', () => {
	let harness: ApiTestHarness;

	beforeEach(async () => {
		harness = await createApiTestHarness();
	});

	test('deletes application and invalidates bot token', async () => {
		const account = await createTestAccount(harness);
		const appName = createUniqueApplicationName();

		const createResult = await createOAuth2Application(harness, account.token, {
			name: appName,
			redirect_uris: ['https://example.com/callback'],
		});

		await createBuilder(harness, `Bot ${createResult.botToken}`).get('/users/@me').expect(HTTP_STATUS.OK).execute();

		await deleteOAuth2Application(harness, account.token, createResult.application.id, account.password);

		await createBuilder(harness, account.token)
			.get(`/oauth2/applications/${createResult.application.id}`)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();

		const applications = await listOAuth2Applications(harness, account.token);
		const found = applications.find((app) => app.id === createResult.application.id);
		expect(found).toBeUndefined();

		await createBuilder(harness, `Bot ${createResult.botToken}`)
			.get('/users/@me')
			.expect(HTTP_STATUS.UNAUTHORIZED)
			.execute();

		const botUser = await createBuilder<{
			id: string;
			username: string;
			discriminator: string;
			avatar: string | null;
		}>(harness, account.token)
			.get(`/users/${createResult.botUserId}`)
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(botUser.username).toBe('DeletedUser');
		expect(botUser.discriminator).toBe('0000');
		expect(botUser.avatar).toBeNull();
	});

	test('keeps bot-authored messages readable after deleting the application', async () => {
		const account = await createTestAccount(harness);
		const createResult = await createOAuth2Application(harness, account.token, {
			name: createUniqueApplicationName(),
		});

		const guild = await createBuilder<{id: string; system_channel_id?: string}>(harness, account.token)
			.post('/guilds')
			.body({name: `Bot Message Retention ${Date.now()}`})
			.expect(HTTP_STATUS.OK)
			.execute();
		const channelId = guild.system_channel_id;
		if (!channelId) {
			throw new Error('Guild response missing system channel');
		}

		const seeded = await createBuilder<{messages: Array<{message_id: string}>}>(harness, '')
			.post('/test/messages/seed')
			.body({
				channel_id: channelId,
				author_id: createResult.botUserId,
				clear_existing: true,
				messages: [{content: 'bot message before deletion'}],
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		const botMessageId = seeded.messages[0]?.message_id;
		if (!botMessageId) {
			throw new Error('Seeded message response missing message id');
		}

		await deleteOAuth2Application(harness, account.token, createResult.application.id, account.password);

		const messages = await createBuilder<Array<MessageResponse>>(harness, account.token)
			.get(`/channels/${channelId}/messages`)
			.expect(HTTP_STATUS.OK)
			.execute();

		const foundMessage = messages.find((message) => message.id === botMessageId);
		expect(foundMessage).toBeDefined();
		expect(foundMessage?.author.id).not.toBe(createResult.botUserId);
		expect(foundMessage?.author.username).toBe('DeletedUser');
		expect(foundMessage?.author.discriminator).toBe('0000');

		const previousAuthorCount = await createBuilderWithoutAuth<{count: number}>(harness)
			.get(`/test/users/${createResult.botUserId}/messages/count`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(previousAuthorCount.count).toBe(0);

		const replacementAuthorId = foundMessage?.author.id;
		expect(replacementAuthorId).toBeTruthy();

		const replacementAuthorCount = await createBuilderWithoutAuth<{count: number}>(harness)
			.get(`/test/users/${replacementAuthorId}/messages/count`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(replacementAuthorCount.count).toBe(1);
	});

	test('returns 404 for non-existent application', async () => {
		const account = await createTestAccount(harness);

		await createBuilder(harness, account.token)
			.delete('/oauth2/applications/999999999999999999')
			.body({password: account.password})
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});

	test('enforces access control', async () => {
		const owner = await createTestAccount(harness);
		const otherUser = await createTestAccount(harness);

		const createResult = await createOAuth2Application(harness, owner.token, {
			name: createUniqueApplicationName(),
		});

		await createBuilder(harness, otherUser.token)
			.delete(`/oauth2/applications/${createResult.application.id}`)
			.body({password: otherUser.password})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();

		const application = await getOAuth2Application(harness, owner.token, createResult.application.id);
		expect(application.id).toBe(createResult.application.id);
	});

	test('requires sudo verification', async () => {
		const account = await createTestAccount(harness);

		const createResult = await createOAuth2Application(harness, account.token, {
			name: createUniqueApplicationName(),
		});

		await createBuilder(harness, account.token)
			.delete(`/oauth2/applications/${createResult.application.id}`)
			.body({})
			.expect(HTTP_STATUS.FORBIDDEN, 'SUDO_MODE_REQUIRED')
			.execute();

		const application = await getOAuth2Application(harness, account.token, createResult.application.id);
		expect(application.id).toBe(createResult.application.id);
	});

	test('rejects wrong password', async () => {
		const account = await createTestAccount(harness);

		const createResult = await createOAuth2Application(harness, account.token, {
			name: createUniqueApplicationName(),
		});

		await createBuilder(harness, account.token)
			.delete(`/oauth2/applications/${createResult.application.id}`)
			.body({password: 'wrong-password'})
			.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
			.execute();
	});

	test('is idempotent', async () => {
		const account = await createTestAccount(harness);

		const createResult = await createOAuth2Application(harness, account.token, {
			name: createUniqueApplicationName(),
		});

		await deleteOAuth2Application(harness, account.token, createResult.application.id, account.password);

		await createBuilder(harness, account.token)
			.delete(`/oauth2/applications/${createResult.application.id}`)
			.body({password: account.password})
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});

	test('requires authentication', async () => {
		await createBuilderWithoutAuth(harness)
			.delete('/oauth2/applications/123')
			.body({password: 'test'})
			.expect(HTTP_STATUS.UNAUTHORIZED)
			.execute();
	});
});
