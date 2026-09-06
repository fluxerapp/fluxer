// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {ListApplicationsResponse} from '@fluxer/schema/src/domains/admin/AdminApplicationSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount, setUserACLs} from '../../auth/tests/AuthTestUtils';
import {createGuild} from '../../channel/tests/ChannelTestUtils';
import {createOAuth2Application, createUniqueApplicationName} from '../../oauth/tests/OAuth2TestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';

describe('Admin guild applications', () => {
	let harness: ApiTestHarness;

	beforeEach(async () => {
		harness = await createApiTestHarness();
	});

	afterEach(async () => {
		await harness.shutdown();
	});

	test('lists bot applications installed in a guild', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Admin Guild Apps');
		const app = await createOAuth2Application(harness, owner.token, {
			name: createUniqueApplicationName('Installed Guild App'),
			redirect_uris: ['https://example.test/callback'],
			bot_public: true,
		});
		await createBuilder(harness, owner.token)
			.post('/oauth2/authorize/consent')
			.body({
				client_id: app.application.id,
				scope: 'bot',
				guild_id: guild.id,
				permissions: Permissions.SEND_MESSAGES.toString(),
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		await setUserACLs(harness, owner, [AdminACLs.AUTHENTICATE, AdminACLs.APPLICATION_LOOKUP]);

		const response = await createBuilder<ListApplicationsResponse>(harness, `${owner.token}`)
			.get(`/admin/applications?guild_id=${guild.id}`)
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(response.applications).toHaveLength(1);
		expect(response.applications[0]).toMatchObject({
			id: app.application.id,
			bot_user_id: app.botUserId,
			owner_user_id: owner.userId,
		});
	});
});
