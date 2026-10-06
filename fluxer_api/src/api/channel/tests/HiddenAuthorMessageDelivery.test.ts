// SPDX-License-Identifier: AGPL-3.0-or-later

import type {TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {setupTestGuildWithMembers} from '@app/api/guild/tests/GuildTestUtils';
import {sendMessage} from '@app/api/message/tests/MessageTestUtils';
import {getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

describe('messages from an author inside a hide window', () => {
	let harness: ApiTestHarness;
	let author: TestAccount;
	let channelId: string;

	beforeEach(async () => {
		harness = await createApiTestHarness();
		const setup = await setupTestGuildWithMembers(harness, 1);
		author = setup.members[0]!;
		channelId = setup.channels[0]!.id;
		vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
		vi.spyOn(NoopGatewayService.prototype, 'dispatchPresence');
	});

	afterEach(async () => {
		vi.restoreAllMocks();
		await harness?.shutdown();
	});

	async function setHiddenSince(since: Date | null): Promise<void> {
		await getUserRepository().patchUpsert(createUserID(BigInt(author.userId)), {content_hidden_since: since});
	}

	function createEvents(messageId: string) {
		const guild = vi
			.mocked(NoopGatewayService.prototype.dispatchGuild)
			.mock.calls.filter(([call]) => call.event === 'MESSAGE_CREATE' && (call.data as {id: string}).id === messageId);
		const presence = vi
			.mocked(NoopGatewayService.prototype.dispatchPresence)
			.mock.calls.filter(([call]) => call.event === 'MESSAGE_CREATE' && (call.data as {id: string}).id === messageId);
		return {guild, presence: presence.map(([call]) => call.userId.toString())};
	}

	test('reach only the author and fan out again once the window is cleared', async () => {
		await setHiddenSince(new Date(Date.now() - 60_000));
		const hidden = await sendMessage(harness, author.token, channelId, 'inside the window');
		expect(hidden.content).toBe('inside the window');
		expect(createEvents(hidden.id)).toEqual({guild: [], presence: [author.userId]});

		await setHiddenSince(null);
		const shown = await sendMessage(harness, author.token, channelId, 'after restore');
		const events = createEvents(shown.id);
		expect(events.guild).toHaveLength(1);
		expect(events.presence).toEqual([]);
	});

	test('a window that starts later leaves current messages alone', async () => {
		await setHiddenSince(new Date(Date.now() + 3_600_000));
		const message = await sendMessage(harness, author.token, channelId, 'before the window');
		expect(createEvents(message.id).guild).toHaveLength(1);
	});
});
