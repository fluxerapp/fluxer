// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import {ensureSessionStarted, getMessages} from '../../message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {createBuilder} from '../../test/TestRequestBuilder';
import {createChannel, sendChannelMessage, setupTestGuildWithMembers, updateChannel} from './ChannelTestUtils';

describe('Slowmode Enforcement', () => {
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
	it('does not consume slowmode when message send fails validation', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		await ensureSessionStarted(harness, member.token);
		const channel = await createChannel(harness, owner.token, guild.id, 'slowmode-channel');
		await updateChannel(harness, owner.token, channel.id, {
			rate_limit_per_user: 30,
		});
		await createBuilder(harness, member.token)
			.post(`/channels/${channel.id}/messages`)
			.body({})
			.expect(400, APIErrorCodes.CANNOT_SEND_EMPTY_MESSAGE)
			.execute();
		const msg = await sendChannelMessage(harness, member.token, channel.id, 'this should succeed');
		expect(msg.id).toBeDefined();
	});
	it('enforces slowmode after a successful message send', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		await ensureSessionStarted(harness, member.token);
		const channel = await createChannel(harness, owner.token, guild.id, 'slowmode-channel');
		await updateChannel(harness, owner.token, channel.id, {
			rate_limit_per_user: 30,
		});
		const firstMessage = await sendChannelMessage(harness, member.token, channel.id, 'first message');
		await createBuilder(harness, member.token)
			.post(`/channels/${channel.id}/messages`)
			.body({content: 'second message'})
			.expect(400, APIErrorCodes.SLOWMODE_RATE_LIMITED)
			.execute();
		const messages = await getMessages(harness, member.token, channel.id);
		expect(messages).toHaveLength(1);
		expect(messages[0]?.id).toBe(firstMessage.id);
	});
	it('reports the slowmode retry window in seconds on the Retry-After header', async () => {
		const rateLimitPerUser = 5;
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		await ensureSessionStarted(harness, member.token);
		const channel = await createChannel(harness, owner.token, guild.id, 'slowmode-channel');
		await updateChannel(harness, owner.token, channel.id, {
			rate_limit_per_user: rateLimitPerUser,
		});
		await sendChannelMessage(harness, member.token, channel.id, 'first message');
		const {response, json} = await createBuilder<{code: string; retry_after: number}>(harness, member.token)
			.post(`/channels/${channel.id}/messages`)
			.body({content: 'second message'})
			.expect(400, APIErrorCodes.SLOWMODE_RATE_LIMITED)
			.executeWithResponse();
		const headerRetryAfter = Number(response.headers.get('Retry-After'));
		expect(Number.isInteger(headerRetryAfter)).toBe(true);
		expect(headerRetryAfter).toBeGreaterThan(0);
		expect(headerRetryAfter).toBeLessThanOrEqual(rateLimitPerUser);
		expect(json.retry_after).toBeGreaterThan(0);
		expect(json.retry_after).toBeLessThanOrEqual(rateLimitPerUser);
		expect(headerRetryAfter - json.retry_after).toBeLessThan(1);
	});
});
