// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createDmChannel, sendChannelMessage} from '@app/api/channel/tests/ChannelTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {listRelationships, sendFriendRequest} from '@app/api/user/tests/RelationshipTestUtils';
import {RelationshipTypes, UserFlags} from '@fluxer/constants/src/UserConstants';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

async function setUserFlags(harness: ApiTestHarness, userId: string, flags: bigint): Promise<void> {
	await createBuilder(harness, '')
		.patch(`/test/users/${userId}/flags`)
		.body({flags: flags.toString()})
		.expect(HTTP_STATUS.OK)
		.execute();
}

describe('Direct message spammer shadow behavior', () => {
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

	it('opens and sends one-to-one DMs only for the flagged sender', async () => {
		const sender = await createTestAccount(harness);
		const target = await createTestAccount(harness);
		await setUserFlags(harness, sender.userId, UserFlags.SPAMMER | UserFlags.HAS_SESSION_STARTED);
		const dm = await createDmChannel(harness, sender.token, target.userId);
		const senderChannels = await createBuilder<Array<ChannelResponse>>(harness, sender.token)
			.get('/users/@me/channels')
			.execute();
		expect(senderChannels.some((channel) => channel.id === dm.id)).toBe(true);
		let targetChannels = await createBuilder<Array<ChannelResponse>>(harness, target.token)
			.get('/users/@me/channels')
			.execute();
		expect(targetChannels.some((channel) => channel.id === dm.id)).toBe(false);
		await sendChannelMessage(harness, sender.token, dm.id, 'local only');
		targetChannels = await createBuilder<Array<ChannelResponse>>(harness, target.token)
			.get('/users/@me/channels')
			.execute();
		expect(targetChannels.some((channel) => channel.id === dm.id)).toBe(false);
		await createBuilder(harness, target.token)
			.get(`/channels/${dm.id}/messages`)
			.expect(HTTP_STATUS.NOT_FOUND, 'UNKNOWN_CHANNEL')
			.execute();
	});

	it('creates only a local outgoing friend request for flagged senders', async () => {
		const sender = await createTestAccount(harness);
		const target = await createTestAccount(harness);
		await setUserFlags(harness, sender.userId, UserFlags.SPAMMER | UserFlags.HAS_SESSION_STARTED);
		const {json: relationship} = await sendFriendRequest(harness, sender.token, target.userId);
		expect(relationship).toMatchObject({
			id: target.userId,
			type: RelationshipTypes.OUTGOING_REQUEST,
		});
		const {json: senderRelationships} = await listRelationships(harness, sender.token);
		expect(senderRelationships).toContainEqual(
			expect.objectContaining({id: target.userId, type: RelationshipTypes.OUTGOING_REQUEST}),
		);
		const {json: targetRelationships} = await listRelationships(harness, target.token);
		expect(targetRelationships.some((rel) => rel.id === sender.userId)).toBe(false);
	});
});
