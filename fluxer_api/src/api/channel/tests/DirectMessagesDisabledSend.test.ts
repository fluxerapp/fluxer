// SPDX-License-Identifier: AGPL-3.0-or-later

import type {TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {
	createDmChannel,
	createFriendship,
	createGroupDmChannel,
	sendChannelMessage,
} from '@app/api/channel/tests/ChannelTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {afterAll, beforeAll, beforeEach, describe, it} from 'vitest';

describe('sending in existing private channels while direct messages are off', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	const setDirectMessagesDisabled = async (disabled: boolean): Promise<void> => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_CONFIG_VIEW,
			AdminACLs.INSTANCE_CONFIG_UPDATE,
		]);
		await createBuilder(harness, admin.token)
			.patch('/admin/instance/config')
			.body({policy: {direct_messages_disabled: disabled}})
			.execute();
	};

	const expectRefused = async (account: TestAccount, channelId: string): Promise<void> => {
		await createBuilder(harness, account.token)
			.post(`/channels/${channelId}/messages`)
			.body({content: 'still here?'})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.DIRECT_MESSAGES_DISABLED)
			.execute();
	};

	it('refuses messages in an existing DM from both sides', async () => {
		const alice = await createTestAccount(harness);
		const bob = await createTestAccount(harness);
		await createFriendship(harness, alice, bob);
		const dm = await createDmChannel(harness, alice.token, bob.userId);
		await sendChannelMessage(harness, alice.token, dm.id, 'before');
		await sendChannelMessage(harness, bob.token, dm.id, 'before');

		await setDirectMessagesDisabled(true);

		await expectRefused(alice, dm.id);
		await expectRefused(bob, dm.id);
	});

	it('refuses messages in an existing group DM', async () => {
		const alice = await createTestAccount(harness);
		const bob = await createTestAccount(harness);
		const carol = await createTestAccount(harness);
		await createFriendship(harness, alice, bob);
		await createFriendship(harness, alice, carol);
		const group = await createGroupDmChannel(harness, alice.token, [bob.userId, carol.userId]);
		await sendChannelMessage(harness, alice.token, group.id, 'before');

		await setDirectMessagesDisabled(true);

		await expectRefused(alice, group.id);
		await expectRefused(carol, group.id);
	});

	it('keeps personal notes working', async () => {
		const alice = await createTestAccount(harness);

		await setDirectMessagesDisabled(true);

		await sendChannelMessage(harness, alice.token, alice.userId, 'a note to self');
	});
});
