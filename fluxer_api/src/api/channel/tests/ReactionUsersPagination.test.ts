// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';
import {sendChannelMessage, setupTestGuildWithMembers} from './ChannelTestUtils';

const EMOJI = encodeURIComponent('👍');

interface ReactionUsersPage {
	items: Array<{id: string}>;
	has_more: boolean;
	next_after: string | null;
}

describe('Reaction users pagination', () => {
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

	async function setupReactedMessage(): Promise<{token: string; channelId: string; messageId: string}> {
		const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 2);
		const message = await sendChannelMessage(harness, owner.token, systemChannel.id, 'react to me');
		for (const account of [owner, ...members]) {
			await createBuilder(harness, account.token)
				.put(`/channels/${systemChannel.id}/messages/${message.id}/reactions/${EMOJI}/@me`)
				.body(null)
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
		}
		return {token: owner.token, channelId: systemChannel.id, messageId: message.id};
	}

	it('carries the pagination signal of the page in headers', async () => {
		const {token, channelId, messageId} = await setupReactedMessage();

		const legacy = await createBuilder<Array<{id: string}>>(harness, token)
			.get(`/channels/${channelId}/messages/${messageId}/reactions/${EMOJI}?limit=2`)
			.executeWithResponse();
		const page = await createBuilder<ReactionUsersPage>(harness, token)
			.get(`/channels/${channelId}/messages/${messageId}/reactions/${EMOJI}/users?limit=2`)
			.execute();

		expect(legacy.json.map((user) => user.id)).toEqual(page.items.map((user) => user.id));
		expect(page.has_more).toBe(true);
		expect(legacy.response.headers.get('X-Has-More')).toBe('true');
		expect(legacy.response.headers.get('X-Next-After')).toBe(page.next_after);
	});

	it('omits the cursor header on the final page', async () => {
		const {token, channelId, messageId} = await setupReactedMessage();

		const legacy = await createBuilder<Array<{id: string}>>(harness, token)
			.get(`/channels/${channelId}/messages/${messageId}/reactions/${EMOJI}?limit=3`)
			.executeWithResponse();

		expect(legacy.json).toHaveLength(3);
		expect(legacy.response.headers.get('X-Has-More')).toBe('false');
		expect(legacy.response.headers.get('X-Next-After')).toBeNull();
	});
});
