// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {createChannelID, createMessageID, createRoleID, createUserID} from '@app/api/BrandedTypes';
import {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {createPermissionOverwrite, setupTestGuildWithMembers} from '@app/api/channel/tests/ChannelTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {AllowedMentionsRequest} from '@fluxer/schema/src/domains/message/SharedMessageSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import type {z} from 'zod';

type AllowedMentionsInput = z.input<typeof AllowedMentionsRequest>;

describe('Message allowed mentions', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	async function setup() {
		const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 2);
		const [first, second] = members;
		assert(first && second, 'Expected two guild members');
		return {owner, first, second, systemChannel};
	}

	async function expectMentionState(
		message: MessageResponse,
		expected: {users: Array<string>; roles: Array<string>; everyone: boolean},
	) {
		expect(message.mentions.map((user) => user.id)).toEqual(expected.users);
		expect(message.mention_roles).toEqual(expected.roles);
		expect(message.mention_everyone).toBe(expected.everyone);
		const stored = await new ChannelRepository().messages.getMessage(
			createChannelID(BigInt(message.channel_id)),
			createMessageID(BigInt(message.id)),
		);
		expect(stored?.mentionedUserIds).toEqual(new Set(expected.users.map((id) => createUserID(BigInt(id)))));
		expect(stored?.mentionedRoleIds).toEqual(new Set(expected.roles.map((id) => createRoleID(BigInt(id)))));
		expect(stored?.mentionEveryone).toBe(expected.everyone);
	}

	it.each(['@everyone', '@here'])('respects the permission boundary for %s', async (mention) => {
		const world = await setup();
		const content = `${mention} <@${world.second.userId}>`;
		const allowedMentions = {parse: ['users', 'everyone']} satisfies AllowedMentionsInput;
		const permitted = await createBuilder<MessageResponse>(harness, world.owner.token)
			.post(`/channels/${world.systemChannel.id}/messages`)
			.body({content, allowed_mentions: allowedMentions})
			.execute();
		await expectMentionState(permitted, {users: [world.second.userId], roles: [], everyone: true});
		await createPermissionOverwrite(harness, world.owner.token, world.systemChannel.id, world.first.userId, {
			type: 1,
			allow: Permissions.VIEW_CHANNEL.toString(),
			deny: Permissions.MENTION_EVERYONE.toString(),
		});
		const message = await createBuilder<MessageResponse>(harness, world.first.token)
			.post(`/channels/${world.systemChannel.id}/messages`)
			.body({content, allowed_mentions: allowedMentions})
			.execute();

		await expectMentionState(message, {users: [world.second.userId], roles: [], everyone: false});
	});
});
