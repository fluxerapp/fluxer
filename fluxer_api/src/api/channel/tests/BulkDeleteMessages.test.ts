// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {createBuilder} from '../../test/TestRequestBuilder';
import {createGuild, sendChannelMessage} from './ChannelTestUtils';

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
	it('rejects empty message_ids array', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Validation Test Guild');
		if (!guild.system_channel_id) {
			throw new Error('Guild should have a system channel');
		}
		await createBuilder(harness, owner.token)
			.post(`/channels/${guild.system_channel_id}/messages/bulk-delete`)
			.body({message_ids: []})
			.expect(400)
			.execute();
	});
	it('rejects more than 100 messages', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Validation Test Guild');
		if (!guild.system_channel_id) {
			throw new Error('Guild should have a system channel');
		}
		const tooManyIds: Array<string> = [];
		for (let i = 0; i < 101; i++) {
			tooManyIds.push(`${1000000000000000000n + BigInt(i)}`);
		}
		await createBuilder(harness, owner.token)
			.post(`/channels/${guild.system_channel_id}/messages/bulk-delete`)
			.body({message_ids: tooManyIds})
			.expect(400)
			.execute();
	});
	it('rejects bulk delete without MANAGE_MESSAGES permission when channel does not exist', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Permissions Test Guild');
		if (!guild.system_channel_id) {
			throw new Error('Guild should have a system channel');
		}
		const messageIds: Array<string> = [];
		for (let i = 0; i < 3; i++) {
			messageIds.push(`${1000000000000000000n + BigInt(i)}`);
		}
		await createBuilder(harness, member.token)
			.post(`/channels/${guild.system_channel_id}/messages/bulk-delete`)
			.body({message_ids: messageIds})
			.expect(403)
			.execute();
	});
	it('accepts bulk delete request with valid message IDs (does not require messages to exist)', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Bulk Delete Test Guild');
		if (!guild.system_channel_id) {
			throw new Error('Guild should have a system channel');
		}
		const messageIds: Array<string> = [];
		for (let i = 0; i < 5; i++) {
			messageIds.push(`${1000000000000000000n + BigInt(i)}`);
		}
		await createBuilder(harness, owner.token)
			.post(`/channels/${guild.system_channel_id}/messages/bulk-delete`)
			.body({message_ids: messageIds})
			.expect(204)
			.execute();
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
	it('accepts bulk delete request with the compat messages alias', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Bulk Delete Alias Test Guild');
		if (!guild.system_channel_id) {
			throw new Error('Guild should have a system channel');
		}
		const messageIds: Array<string> = [];
		for (let i = 0; i < 3; i++) {
			messageIds.push(`${1000000000000000000n + BigInt(i)}`);
		}
		await createBuilder(harness, owner.token)
			.post(`/channels/${guild.system_channel_id}/messages/bulk-delete`)
			.body({messages: messageIds})
			.expect(204)
			.execute();
	});
});
