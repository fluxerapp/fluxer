// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createFriendship, createGroupDmChannel} from '@app/api/channel/tests/ChannelTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, it} from 'vitest';

describe('Group DM Security Boundaries', () => {
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
	it('non-member cannot read messages from group DM', async () => {
		const creator = await createTestAccount(harness);
		const recipient = await createTestAccount(harness);
		const attacker = await createTestAccount(harness);
		await createFriendship(harness, creator, recipient);
		const groupDm = await createGroupDmChannel(harness, creator.token, [recipient.userId]);
		await createBuilder(harness, attacker.token)
			.get(`/channels/${groupDm.id}/messages`)
			.expect(HTTP_STATUS.NOT_FOUND, 'UNKNOWN_CHANNEL')
			.execute();
	});
	it('non-member cannot send messages to group DM', async () => {
		const creator = await createTestAccount(harness);
		const recipient = await createTestAccount(harness);
		const attacker = await createTestAccount(harness);
		await createFriendship(harness, creator, recipient);
		const groupDm = await createGroupDmChannel(harness, creator.token, [recipient.userId]);
		await createBuilder(harness, attacker.token)
			.post(`/channels/${groupDm.id}/messages`)
			.body({content: 'Unauthorized message'})
			.expect(HTTP_STATUS.NOT_FOUND, 'UNKNOWN_CHANNEL')
			.execute();
	});
	it('non-member cannot add themselves to group DM', async () => {
		const creator = await createTestAccount(harness);
		const recipient = await createTestAccount(harness);
		const attacker = await createTestAccount(harness);
		await createFriendship(harness, creator, recipient);
		const groupDm = await createGroupDmChannel(harness, creator.token, [recipient.userId]);
		await createBuilder(harness, attacker.token)
			.put(`/channels/${groupDm.id}/recipients/${attacker.userId}`)
			.body(null)
			.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_ACCESS')
			.execute();
	});
	it('recipient can leave group DM', async () => {
		const creator = await createTestAccount(harness);
		const recipient = await createTestAccount(harness);
		await createFriendship(harness, creator, recipient);
		const groupDm = await createGroupDmChannel(harness, creator.token, [recipient.userId]);
		await createBuilder(harness, recipient.token)
			.delete(`/channels/${groupDm.id}/recipients/${recipient.userId}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
	});
	it('after leaving, former member cannot read messages', async () => {
		const creator = await createTestAccount(harness);
		const recipient = await createTestAccount(harness);
		await createFriendship(harness, creator, recipient);
		const groupDm = await createGroupDmChannel(harness, creator.token, [recipient.userId]);
		await createBuilder(harness, recipient.token)
			.delete(`/channels/${groupDm.id}/recipients/${recipient.userId}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		await createBuilder(harness, recipient.token)
			.get(`/channels/${groupDm.id}/messages`)
			.expect(HTTP_STATUS.NOT_FOUND, 'UNKNOWN_CHANNEL')
			.execute();
	});
	it('after leaving, former member cannot send messages', async () => {
		const creator = await createTestAccount(harness);
		const recipient = await createTestAccount(harness);
		await createFriendship(harness, creator, recipient);
		const groupDm = await createGroupDmChannel(harness, creator.token, [recipient.userId]);
		await createBuilder(harness, recipient.token)
			.delete(`/channels/${groupDm.id}/recipients/${recipient.userId}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		await createBuilder(harness, recipient.token)
			.post(`/channels/${groupDm.id}/messages`)
			.body({content: 'Message after leaving'})
			.expect(HTTP_STATUS.NOT_FOUND, 'UNKNOWN_CHANNEL')
			.execute();
	});
});
