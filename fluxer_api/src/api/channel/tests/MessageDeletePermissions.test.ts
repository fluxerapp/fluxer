// SPDX-License-Identifier: AGPL-3.0-or-later

import type {TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	createPermissionOverwrite,
	sendChannelMessage,
	setupTestGuildWithMembers,
} from '@app/api/channel/tests/ChannelTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {afterAll, beforeAll, beforeEach, describe, it} from 'vitest';

describe('Message delete permissions', () => {
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

	it('lets a member with MANAGE_MESSAGES but without SEND_MESSAGES delete another member message', async () => {
		const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 2);
		const [author, moderator] = members as [TestAccount, TestAccount];
		const message = await sendChannelMessage(harness, author.token, systemChannel.id, 'delete me');
		await createPermissionOverwrite(harness, owner.token, systemChannel.id, moderator.userId, {
			type: 1,
			allow: Permissions.MANAGE_MESSAGES.toString(),
			deny: Permissions.SEND_MESSAGES.toString(),
		});

		await createBuilder(harness, moderator.token)
			.delete(`/channels/${systemChannel.id}/messages/${message.id}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();

		await createBuilder(harness, author.token)
			.get(`/channels/${systemChannel.id}/messages/${message.id}`)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});

	it('refuses a member without MANAGE_MESSAGES deleting another member message', async () => {
		const {members, systemChannel} = await setupTestGuildWithMembers(harness, 2);
		const [author, other] = members as [TestAccount, TestAccount];
		const message = await sendChannelMessage(harness, author.token, systemChannel.id, 'keep me');

		await createBuilder(harness, other.token)
			.delete(`/channels/${systemChannel.id}/messages/${message.id}`)
			.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_PERMISSIONS')
			.execute();
	});
});
