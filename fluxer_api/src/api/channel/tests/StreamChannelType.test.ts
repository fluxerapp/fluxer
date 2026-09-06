// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {afterAll, beforeAll, beforeEach, describe, it} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';
import {createChannel, createDmChannel, createFriendship, createGuild, getChannel} from './ChannelTestUtils';

const CONNECTION_ID = 'conn-channel-type';

describe('stream key channel type gate', () => {
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

	async function createGuildChannels(): Promise<{
		token: string;
		textKey: string;
		voiceKey: string;
	}> {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Stream Type Guild');
		const textChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const voiceChannel = await createChannel(harness, owner.token, guild.id, 'stream-voice', ChannelTypes.GUILD_VOICE);
		return {
			token: owner.token,
			textKey: `${guild.id}:${textChannel.id}:${CONNECTION_ID}`,
			voiceKey: `${guild.id}:${voiceChannel.id}:${CONNECTION_ID}`,
		};
	}

	it('rejects a region update whose key names a guild text channel', async () => {
		const {token, textKey} = await createGuildChannels();
		await createBuilder(harness, token)
			.patch(`/streams/${textKey}/stream`)
			.body({region: 'us-east'})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_CHANNEL_TYPE)
			.execute();
	});

	it('rejects a preview read whose key names a guild text channel', async () => {
		const {token, textKey} = await createGuildChannels();
		await createBuilder(harness, token)
			.get(`/streams/${textKey}/preview`)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_CHANNEL_TYPE)
			.execute();
	});

	it('lets a guild voice key past the type gate and fail on the missing voice state', async () => {
		const {token, voiceKey} = await createGuildChannels();
		await createBuilder(harness, token)
			.patch(`/streams/${voiceKey}/stream`)
			.body({region: 'us-east'})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.ACCESS_DENIED)
			.execute();
	});

	it('lets a guild voice key past the type gate and read an absent preview', async () => {
		const {token, voiceKey} = await createGuildChannels();
		await createBuilder(harness, token).get(`/streams/${voiceKey}/preview`).expect(HTTP_STATUS.NOT_FOUND).execute();
	});

	it('lets a dm key past the type gate and read an absent preview', async () => {
		const owner = await createTestAccount(harness);
		const recipient = await createTestAccount(harness);
		await createFriendship(harness, owner, recipient);
		const dm = await createDmChannel(harness, owner.token, recipient.userId);
		await createBuilder(harness, owner.token)
			.get(`/streams/dm:${dm.id}:${CONNECTION_ID}/preview`)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});
});
