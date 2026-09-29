// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUniqueEmail, createUniqueUsername, registerUser} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {TEST_CREDENTIALS} from '@app/api/test/TestConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

describe('suspicious flags compare and set', () => {
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

	test('applies to a new account whose flags were never written, and refuses a stale read', async () => {
		const {user_id} = await registerUser(harness, {
			email: createUniqueEmail('cas'),
			username: createUniqueUsername('cas'),
			global_name: 'Cas',
			password: TEST_CREDENTIALS.STRONG_PASSWORD,
			date_of_birth: '2000-01-01',
			consent: true,
		});
		const users = getUserRepository();
		const id = createUserID(BigInt(user_id));
		const fresh = (await users.findUnique(id))!;
		expect(fresh.suspiciousActivityFlags).toBe(0);
		const updated = await users.compareAndSetSuspiciousFlags(fresh, 1);
		expect(updated?.suspiciousActivityFlags).toBe(1);
		expect((await users.findUnique(id))!.suspiciousActivityFlags).toBe(1);
		expect(await users.compareAndSetSuspiciousFlags(fresh, 2)).toBeNull();
		expect((await users.findUnique(id))!.suspiciousActivityFlags).toBe(1);
	});
});
