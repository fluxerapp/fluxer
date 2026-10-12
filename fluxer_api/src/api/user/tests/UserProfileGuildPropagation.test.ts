// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {createTestGuild, getPngDataUrl} from '@app/api/emoji/tests/EmojiTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
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

	it('still updates guild members after the committed profile cannot publish its private update', async () => {
		const account = await createTestAccount(harness);
		const guild = await createTestGuild(harness, account.token);
		const dispatchGuild = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
		vi.spyOn(NoopGatewayService.prototype, 'dispatchPresence').mockImplementation(async ({event}) => {
			if (event === 'USER_UPDATE') throw new Error('gateway publish failed');
		});

		await createBuilder(harness, account.token)
			.patch('/users/@me')
			.body({avatar: getPngDataUrl()})
			.expect(500)
			.execute();

		const stored = await new UserRepository().findUnique(createUserID(BigInt(account.userId)));
		expect(stored?.avatarHash).toBeTruthy();
		const updates = memberUpdatesFor(dispatchGuild, guild.id);
		expect(updates).toHaveLength(1);
		expect(updates[0]!.data.user.avatar).toBe(stored?.avatarHash);
	});
});
