// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	acceptInvite,
	addMemberRole,
	createChannel,
	createChannelInvite,
	createGuild,
	createRole,
	deleteChannel,
	updateChannel,
} from '@app/api/channel/tests/ChannelTestUtils';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {afterAll, beforeAll, beforeEach, describe, it} from 'vitest';

describe('deleting an nsfw channel as a minor', () => {
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

	it('lets a minor with manage channels delete it without reading it', async () => {
		const owner = await createTestAccount(harness);
		const minor = await createTestAccount(harness, {dateOfBirth: '2012-01-01'});
		const guild = await createGuild(harness, owner.token, 'nsfw delete');
		const sfw = await createChannel(harness, owner.token, guild.id, 'sfw');
		const nsfw = await createChannel(harness, owner.token, guild.id, 'nsfw');
		await updateChannel(harness, owner.token, nsfw.id, {nsfw: true});
		const invite = await createChannelInvite(harness, owner.token, sfw.id);
		await acceptInvite(harness, minor.token, invite.code);
		await ensureSessionStarted(harness, minor.token);
		const role = await createRole(harness, owner.token, guild.id, {
			name: 'Moderator',
			permissions: Permissions.MANAGE_CHANNELS.toString(),
		});
		await addMemberRole(harness, owner.token, guild.id, minor.userId, role.id);

		await createBuilder(harness, minor.token)
			.get(`/channels/${nsfw.id}/messages`)
			.expect(403, APIErrorCodes.NSFW_CONTENT_AGE_RESTRICTED)
			.execute();
		await deleteChannel(harness, minor.token, nsfw.id);
	});
});
