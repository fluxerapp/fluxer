// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {
	createDmChannel,
	createFriendship,
	createGroupDmChannel,
	deleteChannel,
	removeRecipientFromGroupDm,
} from '@app/api/channel/tests/ChannelTestUtils';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

async function pinnedDmIds(account: TestAccount): Promise<Array<string>> {
	const ids = await new UserRepository().getPinnedDms(createUserID(BigInt(account.userId)));
	return ids.map(String);
}

async function pin(harness: ApiTestHarness, account: TestAccount, channelId: string): Promise<void> {
	await createBuilder(harness, account.token).put(`/users/@me/channels/${channelId}/pin`).expect(204).execute();
}

describe('Pinned DMs follow the private channel list', () => {
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

	async function accounts(count: number): Promise<Array<TestAccount>> {
		const created: Array<TestAccount> = [];
		for (let i = 0; i < count; i++) {
			const account = await createTestAccount(harness);
			await ensureSessionStarted(harness, account.token);
			created.push(account);
		}
		return created;
	}

	it('closing a pinned DM unpins it', async () => {
		const [alice, bob] = await accounts(2);
		await createFriendship(harness, alice, bob);
		const dm = await createDmChannel(harness, alice.token, bob.userId);
		await pin(harness, alice, dm.id);
		expect(await pinnedDmIds(alice)).toEqual([dm.id]);
		await deleteChannel(harness, alice.token, dm.id);
		expect(await pinnedDmIds(alice)).toEqual([]);
	});

	it('leaving a pinned group DM unpins it and keeps other members pins', async () => {
		const [owner, member, other] = await accounts(3);
		await createFriendship(harness, owner, member);
		await createFriendship(harness, owner, other);
		const group = await createGroupDmChannel(harness, owner.token, [member.userId, other.userId]);
		await pin(harness, member, group.id);
		await pin(harness, owner, group.id);
		await removeRecipientFromGroupDm(harness, member.token, group.id, member.userId);
		expect(await pinnedDmIds(member)).toEqual([]);
		expect(await pinnedDmIds(owner)).toEqual([group.id]);
	});

	it('being removed from a pinned group DM unpins it', async () => {
		const [owner, member, other] = await accounts(3);
		await createFriendship(harness, owner, member);
		await createFriendship(harness, owner, other);
		const group = await createGroupDmChannel(harness, owner.token, [member.userId, other.userId]);
		await pin(harness, member, group.id);
		await removeRecipientFromGroupDm(harness, owner.token, group.id, member.userId);
		expect(await pinnedDmIds(member)).toEqual([]);
	});

	it('the last member leaving a pinned group DM unpins it', async () => {
		const [owner, member, other] = await accounts(3);
		await createFriendship(harness, owner, member);
		await createFriendship(harness, owner, other);
		const group = await createGroupDmChannel(harness, owner.token, [member.userId, other.userId]);
		await pin(harness, owner, group.id);
		await removeRecipientFromGroupDm(harness, member.token, group.id, member.userId);
		await removeRecipientFromGroupDm(harness, other.token, group.id, other.userId);
		await removeRecipientFromGroupDm(harness, owner.token, group.id, owner.userId);
		expect(await pinnedDmIds(owner)).toEqual([]);
	});
});
