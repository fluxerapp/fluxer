// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelInvite, createFriendship, createGroupDmChannel} from '@app/api/channel/tests/ChannelTestUtils';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('group dm mature content', () => {
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

	async function setup() {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const other = await createTestAccount(harness);
		await createFriendship(harness, owner, member);
		await createFriendship(harness, owner, other);
		await ensureSessionStarted(harness, owner.token);
		const group = await createGroupDmChannel(harness, owner.token, [member.userId, other.userId]);
		return {owner, member, other, groupId: group.id};
	}

	async function joinAsMinor(groupId: string, owner: TestAccount, status: number, code?: string) {
		const minor = await createTestAccount(harness, {dateOfBirth: '2012-01-01'});
		const invite = await createChannelInvite(harness, owner.token, groupId);
		await createBuilder(harness, minor.token).post(`/invites/${invite.code}`).body({}).expect(status, code).execute();
	}

	function setNsfw(account: TestAccount, channelId: string, nsfw: boolean, status = 200, code?: string) {
		return createBuilder<ChannelResponse>(harness, account.token)
			.patch(`/channels/${channelId}`)
			.body({type: ChannelTypes.GROUP_DM, nsfw})
			.expect(status, code)
			.execute();
	}

	it('lets the owner mark and clear the group', async () => {
		const s = await setup();
		const updated = await setNsfw(s.owner, s.groupId, true);
		expect(updated.nsfw).toBe(true);
		const cleared = await setNsfw(s.owner, s.groupId, false);
		expect(cleared.nsfw).toBe(false);
	});

	it('rejects the toggle from a member who is not the owner', async () => {
		const s = await setup();
		await setNsfw(s.member, s.groupId, true, 403);
	});

	it('refuses to mark a group that includes a minor', async () => {
		const s = await setup();
		await joinAsMinor(s.groupId, s.owner, 200);
		await setNsfw(s.owner, s.groupId, true, 400, APIErrorCodes.GROUP_DM_MATURE_CONTENT_INELIGIBLE);
	});

	it('refuses to let a minor join a marked group', async () => {
		const s = await setup();
		await setNsfw(s.owner, s.groupId, true);
		await joinAsMinor(s.groupId, s.owner, 403, APIErrorCodes.NSFW_CONTENT_AGE_RESTRICTED);
	});
});
