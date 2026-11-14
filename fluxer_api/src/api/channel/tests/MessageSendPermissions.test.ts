// SPDX-License-Identifier: AGPL-3.0-or-later

import {createPermissionOverwrite, setupTestGuildWithMembers} from '@app/api/channel/tests/ChannelTestUtils';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Message send permissions', () => {
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

	it('returns the created message when the sender cannot read history or add reactions', async () => {
		const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		await createPermissionOverwrite(harness, owner.token, systemChannel.id, member.userId, {
			type: 1,
			allow: (Permissions.VIEW_CHANNEL | Permissions.SEND_MESSAGES).toString(),
			deny: (Permissions.READ_MESSAGE_HISTORY | Permissions.ADD_REACTIONS).toString(),
		});
		await ensureSessionStarted(harness, member.token);

		const sentMessage = await createBuilder<MessageResponse>(harness, member.token)
			.post(`/channels/${systemChannel.id}/messages`)
			.body({content: 'no history send'})
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(sentMessage.content).toBe('no history send');
		expect(sentMessage.author.id).toBe(member.userId);
	});

	it('rejects sending and editing when VIEW_CHANNEL is denied', async () => {
		const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		await ensureSessionStarted(harness, member.token);
		const sentMessage = await createBuilder<MessageResponse>(harness, member.token)
			.post(`/channels/${systemChannel.id}/messages`)
			.body({content: 'before the lockout'})
			.execute();
		await createPermissionOverwrite(harness, owner.token, systemChannel.id, member.userId, {
			type: 1,
			allow: '0',
			deny: Permissions.VIEW_CHANNEL.toString(),
		});

		await createBuilder(harness, member.token)
			.post(`/channels/${systemChannel.id}/messages`)
			.body({content: 'after the lockout'})
			.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_PERMISSIONS')
			.execute();
		await createBuilder(harness, member.token)
			.patch(`/channels/${systemChannel.id}/messages/${sentMessage.id}`)
			.body({content: 'after the lockout'})
			.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_PERMISSIONS')
			.execute();
	});

	it('hides the edited message when the editor cannot read history', async () => {
		const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		await ensureSessionStarted(harness, member.token);
		const sentMessage = await createBuilder<MessageResponse>(harness, member.token)
			.post(`/channels/${systemChannel.id}/messages`)
			.body({content: 'history is a luxury'})
			.execute();
		await createPermissionOverwrite(harness, owner.token, systemChannel.id, member.userId, {
			type: 1,
			allow: (Permissions.VIEW_CHANNEL | Permissions.SEND_MESSAGES).toString(),
			deny: Permissions.READ_MESSAGE_HISTORY.toString(),
		});

		await createBuilder(harness, member.token)
			.patch(`/channels/${systemChannel.id}/messages/${sentMessage.id}`)
			.body({content: 'history is still a luxury'})
			.expect(HTTP_STATUS.NOT_FOUND, 'UNKNOWN_MESSAGE')
			.execute();
	});
});
