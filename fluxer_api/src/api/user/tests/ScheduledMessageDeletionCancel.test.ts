// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

interface Me {
	id: string;
	flags?: string | number;
	pending_bulk_message_deletion: {scheduled_at: string} | null;
}

describe('Cancelling a scheduled message deletion', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('a flagged account cannot cancel it until the flag is cleared', async () => {
		const account = await createTestAccount(harness);
		const me = () => createBuilder<Me>(harness, account.token).get('/users/@me').expect(HTTP_STATUS.OK).execute();
		const setFlags = (flags: bigint) =>
			createBuilder<unknown>(harness, account.token)
				.patch(`/test/users/${account.userId}/flags`)
				.body({flags: flags.toString()})
				.execute();
		const cancel = () =>
			createBuilder<{success: boolean}>(harness, account.token)
				.delete('/users/@me/messages/delete')
				.expect(HTTP_STATUS.OK)
				.execute();
		await createBuilder<void>(harness, account.token)
			.post('/users/@me/messages/delete')
			.body({password: account.password})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const scheduled = (await me()).pending_bulk_message_deletion;
		expect(scheduled).not.toBeNull();
		const flags = BigInt((await me()).flags ?? 0);
		await setFlags(flags | UserFlags.SPAMMER);
		await cancel();
		expect((await me()).pending_bulk_message_deletion?.scheduled_at).toBe(scheduled?.scheduled_at);
		await setFlags(flags);
		await cancel();
		expect((await me()).pending_bulk_message_deletion).toBeNull();
	}, 60000);
});
