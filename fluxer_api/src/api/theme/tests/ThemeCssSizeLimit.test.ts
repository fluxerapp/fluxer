// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {getConfig} from '../../Config';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';
import {resolveThemeCssMaxBytes, setThemeCssMaxBytesForTesting, THEME_CSS_MAX_BYTES} from '../ThemeService';

interface ThemeCreateResponse {
	id: string;
}

const TEST_MAX_CSS_BYTES = 4096;

describe('Theme CSS size limits', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		setThemeCssMaxBytesForTesting(TEST_MAX_CSS_BYTES);
	});
	afterAll(async () => {
		setThemeCssMaxBytesForTesting(undefined);
		await harness?.shutdown();
	});
	it('enforces the 8MB limit in production and only honours the override in test mode', () => {
		const config = getConfig();
		expect(THEME_CSS_MAX_BYTES).toBe(8 * 1024 * 1024);
		expect(resolveThemeCssMaxBytes()).toBe(TEST_MAX_CSS_BYTES);
		config.dev.testModeEnabled = false;
		try {
			expect(resolveThemeCssMaxBytes()).toBe(THEME_CSS_MAX_BYTES);
		} finally {
			config.dev.testModeEnabled = true;
		}
		setThemeCssMaxBytesForTesting(undefined);
		try {
			expect(resolveThemeCssMaxBytes()).toBe(THEME_CSS_MAX_BYTES);
		} finally {
			setThemeCssMaxBytesForTesting(TEST_MAX_CSS_BYTES);
		}
	});
	it('accepts CSS at exactly the real 8MB limit through the full HTTP stack', async () => {
		setThemeCssMaxBytesForTesting(undefined);
		const user = await createTestAccount(harness);
		const maxCss = 'a'.repeat(THEME_CSS_MAX_BYTES);
		const theme = await createBuilder<ThemeCreateResponse>(harness, user.token)
			.post('/users/@me/themes')
			.body({css: maxCss})
			.expect(HTTP_STATUS.CREATED)
			.execute();
		expect(theme.id).toBeDefined();
	});
	it('rejects CSS that exceeds the real 8MB limit through the full HTTP stack', async () => {
		setThemeCssMaxBytesForTesting(undefined);
		const user = await createTestAccount(harness);
		const oversizedCss = 'a'.repeat(THEME_CSS_MAX_BYTES + 1);
		await createBuilder(harness, user.token)
			.post('/users/@me/themes')
			.body({css: oversizedCss})
			.expect(HTTP_STATUS.BAD_REQUEST, 'FILE_SIZE_TOO_LARGE')
			.execute();
	});
	it('rejects CSS that exceeds the limit', async () => {
		const user = await createTestAccount(harness);
		const oversizedCss = 'a'.repeat(TEST_MAX_CSS_BYTES + 1);
		await createBuilder(harness, user.token)
			.post('/users/@me/themes')
			.body({css: oversizedCss})
			.expect(HTTP_STATUS.BAD_REQUEST, 'FILE_SIZE_TOO_LARGE')
			.execute();
	});
	it('accepts CSS at exactly the limit', async () => {
		const user = await createTestAccount(harness);
		const maxCss = 'a'.repeat(TEST_MAX_CSS_BYTES);
		const theme = await createBuilder<ThemeCreateResponse>(harness, user.token)
			.post('/users/@me/themes')
			.body({css: maxCss})
			.expect(HTTP_STATUS.CREATED)
			.execute();
		expect(theme.id).toBeDefined();
	});
	it('accepts CSS just under the limit', async () => {
		const user = await createTestAccount(harness);
		const nearMaxCss = 'a'.repeat(TEST_MAX_CSS_BYTES - 1);
		const theme = await createBuilder<ThemeCreateResponse>(harness, user.token)
			.post('/users/@me/themes')
			.body({css: nearMaxCss})
			.expect(HTTP_STATUS.CREATED)
			.execute();
		expect(theme.id).toBeDefined();
	});
	it('rejects CSS whose byte length exceeds the limit even when the character count does not', async () => {
		const user = await createTestAccount(harness);
		const unicodeChar = '\u{1F600}';
		const bytesPerChar = Buffer.from(unicodeChar, 'utf-8').length;
		const charsNeeded = Math.ceil((TEST_MAX_CSS_BYTES + 1) / bytesPerChar);
		const oversizedUnicodeCss = unicodeChar.repeat(charsNeeded);
		expect(oversizedUnicodeCss.length).toBeLessThanOrEqual(TEST_MAX_CSS_BYTES);
		expect(Buffer.byteLength(oversizedUnicodeCss, 'utf-8')).toBeGreaterThan(TEST_MAX_CSS_BYTES);
		await createBuilder(harness, user.token)
			.post('/users/@me/themes')
			.body({css: oversizedUnicodeCss})
			.expect(HTTP_STATUS.BAD_REQUEST, 'FILE_SIZE_TOO_LARGE')
			.execute();
	});
});
