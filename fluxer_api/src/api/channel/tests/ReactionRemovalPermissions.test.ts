// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	createDmChannel,
	createFriendship,
	createPermissionOverwrite,
	sendChannelMessage,
	setupTestGuildWithMembers,
} from '@app/api/channel/tests/ChannelTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const EMOJI = encodeURIComponent('👍');

interface ReactionUsersPage {
	items: Array<{id: string}>;
}

describe('Reaction removal permissions', () => {
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

	async function react(account: TestAccount, channelId: string, messageId: string): Promise<void> {
		await createBuilder(harness, account.token)
			.put(`/channels/${channelId}/messages/${messageId}/reactions/${EMOJI}/@me`)
			.body(null)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
	}

	async function listReactorIds(account: TestAccount, channelId: string, messageId: string): Promise<Array<string>> {
		const page = await createBuilder<ReactionUsersPage>(harness, account.token)
			.get(`/channels/${channelId}/messages/${messageId}/reactions/${EMOJI}/users`)
			.execute();
		return page.items.map((user) => user.id);
	}

	async function expectRemovalsRejected(
		account: TestAccount,
		channelId: string,
		messageId: string,
		targetId: string,
	): Promise<void> {
		const base = `/channels/${channelId}/messages/${messageId}/reactions`;
		for (const route of [`${base}/${EMOJI}/${targetId}`, `${base}/${EMOJI}`, base]) {
			await createBuilder(harness, account.token)
				.delete(route)
				.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_PERMISSIONS')
				.execute();
		}
	}

	it('requires MANAGE_MESSAGES for the author to remove reactions from their own guild message', async () => {
		const {members, systemChannel} = await setupTestGuildWithMembers(harness, 2);
		const [author, reactor] = members as [TestAccount, TestAccount];
		const message = await sendChannelMessage(harness, author.token, systemChannel.id, 'react to me');
		await react(reactor, systemChannel.id, message.id);

		await expectRemovalsRejected(author, systemChannel.id, message.id, reactor.userId);

		expect(await listReactorIds(author, systemChannel.id, message.id)).toEqual([reactor.userId]);
	});

	it('refuses the author removing reactions from their own direct message', async () => {
		const author = await createTestAccount(harness);
		const reactor = await createTestAccount(harness);
		await createFriendship(harness, author, reactor);
		const dm = await createDmChannel(harness, author.token, reactor.userId);
		const message = await sendChannelMessage(harness, author.token, dm.id, 'react to me');
		await react(reactor, dm.id, message.id);

		await expectRemovalsRejected(author, dm.id, message.id, reactor.userId);

		expect(await listReactorIds(author, dm.id, message.id)).toEqual([reactor.userId]);
	});

	it('lets a member with MANAGE_MESSAGES remove reactions from another member message', async () => {
		const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 3);
		const [author, reactor, moderator] = members as [TestAccount, TestAccount, TestAccount];
		await createPermissionOverwrite(harness, owner.token, systemChannel.id, moderator.userId, {
			type: 1,
			allow: Permissions.MANAGE_MESSAGES.toString(),
			deny: '0',
		});
		const message = await sendChannelMessage(harness, author.token, systemChannel.id, 'react to me');
		const base = `/channels/${systemChannel.id}/messages/${message.id}/reactions`;

		await react(reactor, systemChannel.id, message.id);
		await createBuilder(harness, moderator.token)
			.delete(`${base}/${EMOJI}/${reactor.userId}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		expect(await listReactorIds(moderator, systemChannel.id, message.id)).toEqual([]);

		await react(reactor, systemChannel.id, message.id);
		await createBuilder(harness, moderator.token).delete(`${base}/${EMOJI}`).expect(HTTP_STATUS.NO_CONTENT).execute();
		expect(await listReactorIds(moderator, systemChannel.id, message.id)).toEqual([]);

		await react(reactor, systemChannel.id, message.id);
		await createBuilder(harness, moderator.token).delete(base).expect(HTTP_STATUS.NO_CONTENT).execute();
		expect(await listReactorIds(moderator, systemChannel.id, message.id)).toEqual([]);
	});

	it('lets a member without MANAGE_MESSAGES remove their own reaction by user ID', async () => {
		const {members, systemChannel} = await setupTestGuildWithMembers(harness, 2);
		const [author, reactor] = members as [TestAccount, TestAccount];
		const message = await sendChannelMessage(harness, author.token, systemChannel.id, 'react to me');
		await react(reactor, systemChannel.id, message.id);

		await createBuilder(harness, reactor.token)
			.delete(`/channels/${systemChannel.id}/messages/${message.id}/reactions/${EMOJI}/${reactor.userId}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();

		expect(await listReactorIds(author, systemChannel.id, message.id)).toEqual([]);
	});
});
