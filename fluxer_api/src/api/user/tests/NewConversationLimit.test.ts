// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {updateUserSettings} from '@app/api/channel/tests/ChannelTestUtils';
import {
	acceptInvite,
	createChannelInvite,
	createDMChannel,
	createFriendship,
	createGuild,
	ensureSessionStarted,
	sendMessage,
} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, type TestRequestBuilder} from '@app/api/test/TestRequestBuilder';
import {clearNewConversationLimit, setNewConversationLimit} from '@app/api/user/NewConversationLimit';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

const HOUR_MS = 60 * 60 * 1000;

interface ErrorBody {
	code: string;
	message: string;
}

describe('New conversation limit', () => {
	let harness: ApiTestHarness;

	beforeEach(async () => {
		harness = await createApiTestHarness();
	});

	afterEach(async () => {
		await harness?.shutdown();
	});

	async function members(count: number): Promise<Array<TestAccount>> {
		const accounts: Array<TestAccount> = [];
		for (let i = 0; i < count; i++) {
			const account = await createTestAccount(harness);
			await ensureSessionStarted(harness, account.token);
			await updateUserSettings(harness, account.token, {default_guilds_restricted: false});
			accounts.push(account);
		}
		const guild = await createGuild(harness, accounts[0]!.token, 'New conversation limit');
		const invite = await createChannelInvite(harness, accounts[0]!.token, guild.system_channel_id!);
		for (const account of accounts.slice(1)) {
			await acceptInvite(harness, account.token, invite.code);
		}
		return accounts;
	}

	async function limit(account: TestAccount, hours = 24): Promise<void> {
		await setNewConversationLimit(createUserID(BigInt(account.userId)), Date.now() + hours * HOUR_MS);
	}

	async function refusedDm(from: TestAccount, to: TestAccount): Promise<ErrorBody> {
		const {json} = await createBuilder<ErrorBody>(harness, from.token)
			.post('/users/@me/channels')
			.body({recipient_id: to.userId})
			.expect(403)
			.executeWithResponse();
		return json;
	}

	test('a limited account cannot open a DM with someone it has no conversation with', async () => {
		const [limited, other] = await members(2);
		await limit(limited!);
		const error = await refusedDm(limited!, other!);
		expect(error.code).toBe(APIErrorCodes.NEW_CONVERSATIONS_LIMITED);
		expect(error.message).toBe("You can't start new conversations right now. Please try again later.");
	});

	test('a limited account cannot send the first message into an empty DM', async () => {
		const [limited, other] = await members(2);
		const channel = await createDMChannel(harness, limited!.token, other!.userId);
		await limit(limited!);
		const {json} = await createBuilder<ErrorBody>(harness, limited!.token)
			.post(`/channels/${channel.id}/messages`)
			.body({content: 'hello'})
			.expect(403)
			.executeWithResponse();
		expect(json.code).toBe(APIErrorCodes.NEW_CONVERSATIONS_LIMITED);
	});

	test('friends, replies and existing conversations keep working', async () => {
		const [limited, friend, opener] = await members(3);
		await createFriendship(harness, limited!, friend!);
		const theirs = await createDMChannel(harness, opener!.token, limited!.userId);
		await sendMessage(harness, opener!.token, theirs.id, 'hi there');
		await limit(limited!);
		const withFriend = await createDMChannel(harness, limited!.token, friend!.userId);
		await sendMessage(harness, limited!.token, withFriend.id, 'hi friend');
		await sendMessage(harness, limited!.token, theirs.id, 'hello back');
		const reopened = await createDMChannel(harness, limited!.token, opener!.userId);
		expect(reopened.id).toBe(theirs.id);
	});

	test('outgoing friend requests are refused while accepting incoming requests still works', async () => {
		const [limited, other, requester] = await members(3);
		await limit(limited!);
		const {json} = await createBuilder<ErrorBody>(harness, limited!.token)
			.post(`/users/@me/relationships/${other!.userId}`)
			.body({})
			.expect(403)
			.executeWithResponse();
		expect(json.code).toBe(APIErrorCodes.NEW_CONVERSATIONS_LIMITED);
		await createFriendship(harness, requester!, limited!);
	});

	test('a cleared or expired limit lets the account start conversations again', async () => {
		const [limited, first, second] = await members(3);
		await limit(limited!);
		await refusedDm(limited!, first!);
		await clearNewConversationLimit(createUserID(BigInt(limited!.userId)));
		await createDMChannel(harness, limited!.token, first!.userId);
		await setNewConversationLimit(createUserID(BigInt(limited!.userId)), Date.now() - 1);
		await createDMChannel(harness, limited!.token, second!.userId);
	});

	test('staff and trusted accounts are never limited', async () => {
		for (const flag of [UserFlags.STAFF, UserFlags.NOT_SUSPICIOUS]) {
			const [account, other] = await members(2);
			const me = await createBuilder<{flags?: string | number}>(harness, account!.token).get('/users/@me').execute();
			await createBuilder<unknown>(harness, account!.token)
				.patch(`/test/users/${account!.userId}/flags`)
				.body({flags: (BigInt(me.flags ?? 0) | flag).toString()})
				.execute();
			await limit(account!);
			await createDMChannel(harness, account!.token, other!.userId);
		}
	});

	test('actions in a direct message the other account has not written in are refused', async () => {
		const [limited, other] = await members(2);
		const channel = await createDMChannel(harness, limited!.token, other!.userId);
		const own = await sendMessage(harness, limited!.token, channel.id, 'hey');
		await limit(limited!);
		const actions: Array<[string, () => TestRequestBuilder<ErrorBody>]> = [
			[
				'send',
				() =>
					createBuilder<ErrorBody>(harness, limited!.token)
						.post(`/channels/${channel.id}/messages`)
						.body({content: 'hello'}),
			],
			[
				'edit',
				() =>
					createBuilder<ErrorBody>(harness, limited!.token)
						.patch(`/channels/${channel.id}/messages/${own.id}`)
						.body({content: 'hello there'}),
			],
			['pin', () => createBuilder<ErrorBody>(harness, limited!.token).put(`/channels/${channel.id}/pins/${own.id}`)],
			[
				'react',
				() =>
					createBuilder<ErrorBody>(harness, limited!.token).put(
						`/channels/${channel.id}/messages/${own.id}/reactions/%F0%9F%91%8D/@me`,
					),
			],
			[
				'ring',
				() =>
					createBuilder<ErrorBody>(harness, limited!.token)
						.post(`/channels/${channel.id}/call/ring`)
						.body({recipients: [other!.userId]}),
			],
		];
		for (const [name, request] of actions) {
			const {json} = await request().expect(403).executeWithResponse();
			expect(json.code, name).toBe(APIErrorCodes.NEW_CONVERSATIONS_LIMITED);
		}
	});

	test('bots can still be messaged while limited', async () => {
		const [limited, bot] = await members(2);
		await createBuilder<unknown>(harness, bot!.token)
			.post(`/test/users/${bot!.userId}/set-bot-flag`)
			.body({is_bot: true})
			.execute();
		await limit(limited!);
		const channel = await createDMChannel(harness, limited!.token, bot!.userId);
		await sendMessage(harness, limited!.token, channel.id, 'help');
	});
});
