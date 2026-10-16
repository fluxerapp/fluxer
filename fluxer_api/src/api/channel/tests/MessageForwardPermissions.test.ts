// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createChannel,
	createPermissionOverwrite,
	sendChannelMessage,
	setupTestGuildWithMembers,
	updateGuild,
} from '@app/api/channel/tests/ChannelTestUtils';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {MessageReferenceTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {extractTimestamp} from '@fluxer/snowflake/src/SnowflakeUtils';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Message forward permissions', () => {
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

	it('allows forwarding an accessible source message without Read Message History', async () => {
		const {owner, members, guild, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		const destinationChannel = await createChannel(harness, owner.token, guild.id, 'forward-destination');
		await updateGuild(harness, owner.token, guild.id, {
			message_history_cutoff: new Date(extractTimestamp(guild.id)).toISOString(),
		});
		await createPermissionOverwrite(harness, owner.token, systemChannel.id, member.userId, {
			type: 1,
			allow: Permissions.VIEW_CHANNEL.toString(),
			deny: Permissions.READ_MESSAGE_HISTORY.toString(),
		});
		await createPermissionOverwrite(harness, owner.token, destinationChannel.id, member.userId, {
			type: 1,
			allow: (Permissions.VIEW_CHANNEL | Permissions.SEND_MESSAGES).toString(),
			deny: Permissions.READ_MESSAGE_HISTORY.toString(),
		});
		await ensureSessionStarted(harness, member.token);
		const sourceMessage = await sendChannelMessage(harness, owner.token, systemChannel.id, 'forward me');

		const forwardedMessage = await createBuilder<MessageResponse>(harness, member.token)
			.post(`/channels/${destinationChannel.id}/messages`)
			.body({
				message_reference: {
					message_id: sourceMessage.id,
					channel_id: systemChannel.id,
					guild_id: guild.id,
					type: MessageReferenceTypes.FORWARD,
				},
			})
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(forwardedMessage.message_snapshots?.[0]?.content).toBe(sourceMessage.content);
	});

	it('allows forwarding source history without Read Message History', async () => {
		const {owner, members, guild, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		const destinationChannel = await createChannel(harness, owner.token, guild.id, 'forward-destination');
		const sourceMessage = await sendChannelMessage(harness, owner.token, systemChannel.id, 'hidden history');
		await createPermissionOverwrite(harness, owner.token, systemChannel.id, member.userId, {
			type: 1,
			allow: Permissions.VIEW_CHANNEL.toString(),
			deny: Permissions.READ_MESSAGE_HISTORY.toString(),
		});
		await createPermissionOverwrite(harness, owner.token, destinationChannel.id, member.userId, {
			type: 1,
			allow: (Permissions.VIEW_CHANNEL | Permissions.SEND_MESSAGES).toString(),
			deny: Permissions.READ_MESSAGE_HISTORY.toString(),
		});
		await ensureSessionStarted(harness, member.token);

		const forwardedMessage = await createBuilder<MessageResponse>(harness, member.token)
			.post(`/channels/${destinationChannel.id}/messages`)
			.body({
				message_reference: {
					message_id: sourceMessage.id,
					channel_id: systemChannel.id,
					guild_id: guild.id,
					type: MessageReferenceTypes.FORWARD,
				},
			})
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(forwardedMessage.message_snapshots?.[0]?.content).toBe(sourceMessage.content);
	});
});
