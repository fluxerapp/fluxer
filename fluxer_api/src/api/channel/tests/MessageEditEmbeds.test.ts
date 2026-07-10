// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {ensureSessionStarted} from '../../message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {createBuilder} from '../../test/TestRequestBuilder';
import {createGuild} from './ChannelTestUtils';

describe('Message embed edits', () => {
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

	it('preserves embeds when omitted from an edit and removes them when explicitly empty', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Message embed edit');
		const channelId = guild.system_channel_id!;
		await ensureSessionStarted(harness, account.token);

		const createdMessage = await createBuilder<MessageResponse>(harness, account.token)
			.post(`/channels/${channelId}/messages`)
			.body({
				content: "I swear if someone steals my embed I'm gonna",
				embeds: [{title: 'My saucy embed', description: 'Keep me close. Hold me. Never let me go.'}],
			})
			.execute();

		const contentEditedMessage = await createBuilder<MessageResponse>(harness, account.token)
			.patch(`/channels/${channelId}/messages/${createdMessage.id}`)
			.body({content: 'MY EMBED IS STILL HERE OMG'})
			.execute();

		expect(contentEditedMessage.content).toBe('MY EMBED IS STILL HERE OMG');
		expect(contentEditedMessage.embeds).toHaveLength(1);
		expect(contentEditedMessage.embeds?.[0]?.title).toBe('My saucy embed');
		expect(contentEditedMessage.embeds?.[0]?.description).toBe('Keep me close. Hold me. Never let me go.');

		const embedsRemovedMessage = await createBuilder<MessageResponse>(harness, account.token)
			.patch(`/channels/${channelId}/messages/${createdMessage.id}`)
			.body({content: 'I got nothing left', embeds: []})
			.execute();

		expect(embedsRemovedMessage.content).toBe('I got nothing left');
		expect(embedsRemovedMessage.embeds ?? []).toHaveLength(0);
	});
});
