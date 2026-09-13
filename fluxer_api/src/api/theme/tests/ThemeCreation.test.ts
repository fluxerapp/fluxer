// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface ThemeCreateResponse {
	id: string;
}

describe('Theme creation', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	it('successfully creates a theme with valid CSS', async () => {
		const user = await createTestAccount(harness);
		const theme = await createBuilder<ThemeCreateResponse>(harness, user.token)
			.post('/users/@me/themes')
			.body({css: '.test { color: red; }'})
			.expect(HTTP_STATUS.CREATED)
			.execute();
		expect(theme.id).toBeDefined();
		expect(typeof theme.id).toBe('string');
		expect(theme.id.length).toBe(16);
	});
	it('successfully creates a theme with complex CSS', async () => {
		const user = await createTestAccount(harness);
		const complexCss = `
			:root {
				--background-primary: #36393f;
				--background-secondary: #2f3136;
				--text-normal: #dcddde;
			}

			.container {
				display: flex;
				flex-direction: column;
				gap: 1rem;
			}

			@media (max-width: 768px) {
				.container {
					flex-direction: row;
				}
			}
		`;
		const theme = await createBuilder<ThemeCreateResponse>(harness, user.token)
			.post('/users/@me/themes')
			.body({css: complexCss})
			.expect(HTTP_STATUS.CREATED)
			.execute();
		expect(theme.id).toBeDefined();
	});
	it('successfully creates a theme with minimal CSS (1 character)', async () => {
		const user = await createTestAccount(harness);
		const theme = await createBuilder<ThemeCreateResponse>(harness, user.token)
			.post('/users/@me/themes')
			.body({css: 'a'})
			.expect(HTTP_STATUS.CREATED)
			.execute();
		expect(theme.id).toBeDefined();
	});
	it('successfully creates a theme with unicode CSS content', async () => {
		const user = await createTestAccount(harness);
		const unicodeCss = '.test::before { content: "\u2764\ufe0f"; }';
		const theme = await createBuilder<ThemeCreateResponse>(harness, user.token)
			.post('/users/@me/themes')
			.body({css: unicodeCss})
			.expect(HTTP_STATUS.CREATED)
			.execute();
		expect(theme.id).toBeDefined();
	});
});
