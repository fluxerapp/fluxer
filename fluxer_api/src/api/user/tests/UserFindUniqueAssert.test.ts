// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {createUserID} from '../../BrandedTypes';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {UserRepository} from '../repositories/UserRepository';

const MISSING_USER_ID = createUserID(999999999999999999n);
const SYSTEM_USER_ID = createUserID(0n);

describe('UserRepository.findUniqueAssert', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	afterAll(async () => {
		await harness.shutdown();
	});
	beforeEach(async () => {
		await harness.resetData();
	});
	test('returns the user when the row exists', async () => {
		const account = await createTestAccount(harness);
		const user = await new UserRepository().findUniqueAssert(createUserID(BigInt(account.userId)));
		expect(user.id.toString()).toBe(account.userId);
	});
	test('resolves the system account without a stored row', async () => {
		const user = await new UserRepository().findUniqueAssert(SYSTEM_USER_ID);
		expect(user.id.toString()).toBe('0');
		expect(user.isSystem).toBe(true);
		expect(user.username).toBe('Fluxer');
	});
	test('throws UnknownUserError when the row is gone', async () => {
		const repository = new UserRepository();
		const error = await repository.findUniqueAssert(MISSING_USER_ID).then(
			() => null,
			(caught: unknown) => caught,
		);
		expect(error).toBeInstanceOf(UnknownUserError);
		expect((error as UnknownUserError).status).toBe(404);
		expect((error as UnknownUserError).code).toBe(APIErrorCodes.UNKNOWN_USER);
	});
});
