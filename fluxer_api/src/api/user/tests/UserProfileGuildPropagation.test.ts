// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createTestGuild, getPngDataUrl} from '@app/api/emoji/tests/EmojiTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import {updateAvatar} from '@app/api/user/tests/UserTestUtils';
import type {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, type MockInstance, vi} from 'vitest';

type DispatchGuildSpy = MockInstance<NoopGatewayService['dispatchGuild']>;

interface MemberUpdateDispatch {
	guildId: {toString(): string};
	event: string;
	data: GuildMemberResponse;
}

function memberUpdatesFor(dispatchGuild: DispatchGuildSpy, guildId: string): Array<MemberUpdateDispatch> {
	return dispatchGuild.mock.calls
		.map(([params]) => params as unknown as MemberUpdateDispatch)
		.filter((params) => params.event === 'GUILD_MEMBER_UPDATE' && params.guildId.toString() === guildId);
}

describe('User Profile Guild Propagation', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterEach(() => {
		vi.restoreAllMocks();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	it('pushes the new avatar to every guild the user is in', async () => {
		const account = await createTestAccount(harness);
		const guild = await createTestGuild(harness, account.token);
		const dispatchGuild = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');

		const updated = await updateAvatar(harness, account.token, getPngDataUrl());

		expect(updated.avatar).toBeTruthy();
		const updates = memberUpdatesFor(dispatchGuild, guild.id);
		expect(updates).toHaveLength(1);
		expect(updates[0]!.data.user.id).toBe(account.userId);
		expect(updates[0]!.data.user.avatar).toBe(updated.avatar);
	});

	it('does not push a member update when no partial field changed', async () => {
		const account = await createTestAccount(harness);
		const guild = await createTestGuild(harness, account.token);
		const dispatchGuild = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');

		await updateAvatar(harness, account.token, null);

		expect(memberUpdatesFor(dispatchGuild, guild.id)).toHaveLength(0);
	});
});
