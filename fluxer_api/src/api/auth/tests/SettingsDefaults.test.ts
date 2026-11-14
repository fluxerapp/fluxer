// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createAuthHarness,
	createTestAccount,
	createUniqueEmail,
	createUniqueUsername,
	fetchSettings,
	registerUser,
} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('User settings defaults', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	it('defaults incoming calls to friends-only (adult and minor)', async () => {
		const incomingCallFriendsOnly = 8;
		const adult = await createTestAccount(harness, {dateOfBirth: '2000-01-01'});
		const adultSettings = await fetchSettings(harness, adult.token);
		expect(adultSettings.response.status).toBe(200);
		expect(
			(
				adultSettings.json as {
					incoming_call_flags: number;
				}
			).incoming_call_flags,
		).toBe(incomingCallFriendsOnly);
		const minorReg = await registerUser(harness, {
			email: createUniqueEmail(),
			username: createUniqueUsername(),
			global_name: 'Minor Settings',
			password: 'a-strong-password',
			date_of_birth: '2012-01-01',
			consent: true,
		});
		const minorSettings = await fetchSettings(harness, minorReg.token);
		expect(minorSettings.response.status).toBe(200);
		expect(
			(
				minorSettings.json as {
					incoming_call_flags: number;
				}
			).incoming_call_flags,
		).toBe(incomingCallFriendsOnly);
	});
});
