// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {getConfig} from '@app/api/Config';
import {createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {createWebhook} from '@app/api/webhook/tests/WebhookTestUtils';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {afterEach, beforeEach, describe, it} from 'vitest';

const TOKEN_ROUTE_SUFFIXES = ['', '/github', '/slack', '/instatus'];

describe('Webhook token routes and the web app origin', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});

	for (const suffix of TOKEN_ROUTE_SUFFIXES) {
		it(`refuses POST /webhooks/:webhook_id/:token${suffix} from the web app origin`, async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'App Origin Guild');
			const webhook = await createWebhook(harness, guild.system_channel_id!, owner.token, 'App Origin Webhook');
			await createBuilderWithoutAuth(harness)
				.post(`/webhooks/${webhook.id}/${webhook.token}${suffix}`)
				.header('origin', getConfig().endpoints.webAppOrigins[0]!)
				.body({content: 'hello'})
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.INVALID_API_ORIGIN)
				.execute();
		});
	}
});
