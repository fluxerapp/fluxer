// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	acceptInvite,
	createChannel,
	createChannelInvite,
	createGuild,
	sendChannelMessage,
	updateChannel,
} from '@app/api/channel/tests/ChannelTestUtils';
import {getMessages} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

describe('Announcement channel type', () => {
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

	test('applies the age gate to an age-restricted announcement channel', async () => {
		const owner = await createTestAccount(harness, {dateOfBirth: '2000-01-01'});
		const minor = await createTestAccount(harness, {dateOfBirth: '2012-01-01'});
		const guild = await createGuild(harness, owner.token, 'Age Gate Guild');
		const invite = await createChannelInvite(harness, owner.token, guild.system_channel_id!);
		await acceptInvite(harness, minor.token, invite.code);
		const announcement = await createChannel(harness, owner.token, guild.id, 'news', ChannelTypes.GUILD_ANNOUNCEMENT);
		await sendChannelMessage(harness, owner.token, announcement.id, 'before the gate');
		const minorView = await getMessages(harness, minor.token, announcement.id);
		expect(minorView).toHaveLength(1);
		const restricted = await updateChannel(harness, owner.token, announcement.id, {nsfw: true});
		expect(restricted.nsfw).toBe(true);
		await createBuilder(harness, minor.token)
			.get(`/channels/${announcement.id}/messages`)
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.NSFW_CONTENT_AGE_RESTRICTED)
			.execute();
		const adultView = await getMessages(harness, owner.token, announcement.id);
		expect(adultView).toHaveLength(1);
	});
});
