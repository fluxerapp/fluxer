// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	createOAuth2Application,
	createUniqueApplicationName,
	listOAuth2Applications,
} from '@app/api/oauth/tests/OAuth2TestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {beforeEach, describe, expect, test} from 'vitest';

describe('OAuth2 Application List', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	test('returns only applications owned by user', async () => {
		const owner1 = await createTestAccount(harness);
		const owner2 = await createTestAccount(harness);
		const app1 = await createOAuth2Application(harness, owner1.token, {
			name: createUniqueApplicationName(),
		});
		const app2 = await createOAuth2Application(harness, owner2.token, {
			name: createUniqueApplicationName(),
		});
		const owner1Apps = await listOAuth2Applications(harness, owner1.token);
		const owner2Apps = await listOAuth2Applications(harness, owner2.token);
		expect(owner1Apps.length).toBe(1);
		expect(owner1Apps[0]?.id).toBe(app1.application.id);
		expect(owner2Apps.length).toBe(1);
		expect(owner2Apps[0]?.id).toBe(app2.application.id);
	});
});
