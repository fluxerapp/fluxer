// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuild, sendChannelMessage} from '@app/api/channel/tests/ChannelTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Bulk Delete Messages', () => {
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
	it('removes every message when the bulk delete spans more than one batch chunk', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Bulk Delete Chunk Test Guild');
		if (!guild.system_channel_id) {
			throw new Error('Guild should have a system channel');
		}
		const deletedIds: Array<string> = [];
		for (let i = 0; i < 16; i++) {
			const message = await sendChannelMessage(harness, owner.token, guild.system_channel_id, `chunk target ${i}`);
			deletedIds.push(message.id);
		}
		const survivor = await sendChannelMessage(harness, owner.token, guild.system_channel_id, 'chunk survivor');
		await createBuilder(harness, owner.token)
			.post(`/channels/${guild.system_channel_id}/messages/bulk-delete`)
			.body({message_ids: deletedIds})
			.expect(204)
			.execute();
		const messages = await createBuilder<Array<MessageResponse>>(harness, owner.token)
			.get(`/channels/${guild.system_channel_id}/messages`)
			.execute();
		const remainingIds = messages.map((message) => message.id);
		for (const deletedId of deletedIds) {
			expect(remainingIds).not.toContain(deletedId);
		}
		expect(remainingIds).toContain(survivor.id);
	});
});
