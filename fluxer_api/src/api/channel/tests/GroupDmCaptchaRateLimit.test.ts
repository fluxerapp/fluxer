// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	createFriendship,
	createGroupDmChannel,
	type GroupDmChannelResponse,
} from '@app/api/channel/tests/ChannelTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {CAPTCHA_TEST_HEADER, issueSolvedCaptchaToken, useCheapCaptcha} from '@app/api/test/CaptchaTestUtils';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const HTTP_TOO_MANY_REQUESTS = 429;
const RATE_LIMIT_TEST_HEADER = 'x-fluxer-test-enable-rate-limits';

async function createGroupDmWithCaptcha(
	harness: ApiTestHarness,
	owner: TestAccount,
	recipientIds: Array<string>,
): Promise<GroupDmChannelResponse> {
	return await createBuilder<GroupDmChannelResponse>(harness, owner.token)
		.post('/users/@me/channels')
		.header(CAPTCHA_TEST_HEADER, 'true')
		.header('x-captcha-token', await issueSolvedCaptchaToken(harness))
		.body({recipients: recipientIds})
		.expect(HTTP_STATUS.OK)
		.execute();
}

describe('Group DM captcha and rate limits', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		await useCheapCaptcha();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	it('requires captcha for group DM creation but not opening a 1:1 DM', async () => {
		const owner = await createTestAccount(harness);
		const dmRecipient = await createTestAccount(harness);
		const groupDmRecipient = await createTestAccount(harness);
		await createFriendship(harness, owner, dmRecipient);
		await createFriendship(harness, owner, groupDmRecipient);
		const dm = await createBuilder<{type: number}>(harness, owner.token)
			.post('/users/@me/channels')
			.body({recipient_id: dmRecipient.userId})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(dm.type).toBe(ChannelTypes.DM);
		await createBuilder(harness, owner.token)
			.post('/users/@me/channels')
			.header(CAPTCHA_TEST_HEADER, 'true')
			.body({recipients: [groupDmRecipient.userId]})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.CAPTCHA_REQUIRED)
			.execute();
		const groupDm = await createGroupDmWithCaptcha(harness, owner, [groupDmRecipient.userId]);
		expect(groupDm.type).toBe(ChannelTypes.GROUP_DM);
	});

	it('requires captcha when adding a recipient to an existing group DM', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const newMember = await createTestAccount(harness);
		await createFriendship(harness, owner, member);
		await createFriendship(harness, owner, newMember);
		const groupDm = await createGroupDmChannel(harness, owner.token, [member.userId]);
		await createBuilder(harness, owner.token)
			.put(`/channels/${groupDm.id}/recipients/${newMember.userId}`)
			.header(CAPTCHA_TEST_HEADER, 'true')
			.body(null)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.CAPTCHA_REQUIRED)
			.execute();
		await createBuilder(harness, owner.token)
			.put(`/channels/${groupDm.id}/recipients/${newMember.userId}`)
			.header(CAPTCHA_TEST_HEADER, 'true')
			.header('x-captcha-token', await issueSolvedCaptchaToken(harness))
			.body(null)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
	});

	it('limits group DM creation to 10 per user per hour', async () => {
		const owner = await createTestAccount(harness);
		const recipient = await createTestAccount(harness);
		await createFriendship(harness, owner, recipient);
		for (let index = 0; index < 10; index++) {
			await createBuilder(harness, owner.token)
				.post('/users/@me/channels')
				.header(RATE_LIMIT_TEST_HEADER, 'true')
				.header(CAPTCHA_TEST_HEADER, 'true')
				.header('x-captcha-token', await issueSolvedCaptchaToken(harness))
				.body({recipients: [recipient.userId]})
				.expect(HTTP_STATUS.OK)
				.execute();
		}
		await createBuilder(harness, owner.token)
			.post('/users/@me/channels')
			.header(RATE_LIMIT_TEST_HEADER, 'true')
			.header(CAPTCHA_TEST_HEADER, 'true')
			.header('x-captcha-token', await issueSolvedCaptchaToken(harness))
			.body({recipients: [recipient.userId]})
			.expect(HTTP_TOO_MANY_REQUESTS, APIErrorCodes.RATE_LIMITED)
			.execute();
	});

	it('limits group DM recipient additions to 10 per user per hour', async () => {
		const owner = await createTestAccount(harness);
		const initialMember = await createTestAccount(harness);
		const newMembers: Array<TestAccount> = [];
		await createFriendship(harness, owner, initialMember);
		for (let index = 0; index < 11; index++) {
			const member = await createTestAccount(harness);
			await createFriendship(harness, owner, member);
			newMembers.push(member);
		}
		const groupDm = await createGroupDmChannel(harness, owner.token, [initialMember.userId]);
		for (let index = 0; index < 10; index++) {
			await createBuilder(harness, owner.token)
				.put(`/channels/${groupDm.id}/recipients/${newMembers[index].userId}`)
				.header(RATE_LIMIT_TEST_HEADER, 'true')
				.header(CAPTCHA_TEST_HEADER, 'true')
				.header('x-captcha-token', await issueSolvedCaptchaToken(harness))
				.body(null)
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
		}
		await createBuilder(harness, owner.token)
			.put(`/channels/${groupDm.id}/recipients/${newMembers[10].userId}`)
			.header(RATE_LIMIT_TEST_HEADER, 'true')
			.header(CAPTCHA_TEST_HEADER, 'true')
			.header('x-captcha-token', await issueSolvedCaptchaToken(harness))
			.body(null)
			.expect(HTTP_TOO_MANY_REQUESTS, APIErrorCodes.RATE_LIMITED)
			.execute();
	});
});
