// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAuthHarness, createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface DataExistsResponse {
	user_exists: boolean;
	pending_deletion_at: string | null;
	has_self_deleted_flag: boolean;
	has_deleted_flag: boolean;
}

describe('Auth login self deleted recovery', () => {
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
	it('covers auto-recovery for self-deleted accounts with pending deletion', async () => {
		const account = await createTestAccount(harness);
		const oneHourAgo = new Date(Date.now() - 60 * 60 * 1000);
		await createBuilderWithoutAuth(harness)
			.post(`/test/users/${account.userId}/set-pending-deletion`)
			.body({
				pending_deletion_at: oneHourAgo.toISOString(),
				set_self_deleted_flag: true,
			})
			.expect(200)
			.execute();
		await createBuilderWithoutAuth(harness)
			.post('/auth/login')
			.body({
				email: account.email,
				password: account.password,
			})
			.expect(200)
			.execute();
		const payload = await createBuilder<DataExistsResponse>(harness, account.token)
			.get(`/test/users/${account.userId}/data-exists`)
			.execute();
		expect(payload.pending_deletion_at).toBeNull();
		expect(payload.has_self_deleted_flag).toBe(false);
		expect(payload.has_deleted_flag).toBe(false);
	});
});
