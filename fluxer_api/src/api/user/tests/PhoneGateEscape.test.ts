// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAuthHarness, createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import {acceptInvite, createChannelInvite, createGuild} from '@app/api/message/tests/MessageTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import {
	DEFERRED_PHONE_ON_COMMUNITY_JOIN,
	PHONE_GATE_PROMOTED_FROM_DEFERRAL,
	SuspiciousActivityFlags,
} from '@fluxer/constants/src/UserConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

const ESCAPE_PATH = '/users/@me/required-actions/phone-gate-escape';
const PROMOTED = SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE | PHONE_GATE_PROMOTED_FROM_DEFERRAL;

interface EscapePreviewResponse {
	available: boolean;
	guilds: Array<{id: string; name: string}>;
	owned_guilds: Array<{id: string; name: string}>;
}

interface PrivateUser {
	required_actions: Array<string> | null;
}

describe('Phone gate escape', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	async function setFlags(account: TestAccount, flags: number): Promise<void> {
		await createBuilderWithoutAuth(harness)
			.post(`/test/users/${account.userId}/security-flags`)
			.body({suspicious_activity_flags: flags})
			.execute();
	}

	async function readFlags(account: TestAccount): Promise<number> {
		const user = await new UserRepository().findUnique(createUserID(BigInt(account.userId)));
		return user?.suspiciousActivityFlags ?? 0;
	}

	async function guildIds(account: TestAccount): Promise<Array<string>> {
		const guilds = await new GuildRepository().listUserGuilds(createUserID(BigInt(account.userId)));
		return guilds.map((guild) => guild.id.toString()).sort();
	}

	async function joinedAccount(): Promise<{account: TestAccount; guildIds: Array<string>}> {
		const owner = await createTestAccount(harness);
		const account = await createTestAccount(harness);
		const joined = await createGuild(harness, owner.token, 'Joined');
		const invite = await createChannelInvite(harness, owner.token, joined.system_channel_id!);
		await acceptInvite(harness, account.token, invite.code);
		const owned = await createGuild(harness, account.token, 'Owned');
		return {account, guildIds: [joined.id, owned.id].sort()};
	}

	function preview(account: TestAccount) {
		return createBuilder<EscapePreviewResponse>(harness, account.token).get(ESCAPE_PATH).expect(200).execute();
	}

	function execute(account: TestAccount) {
		return createBuilder<PrivateUser>(harness, account.token).post(ESCAPE_PATH).body({}).expect(200).execute();
	}

	function refused(account: TestAccount) {
		return createBuilder(harness, account.token)
			.post(ESCAPE_PATH)
			.body({})
			.expect(400, 'PHONE_GATE_ESCAPE_UNAVAILABLE')
			.execute();
	}

	it('lifts a promoted requirement without leaving any guild', async () => {
		const {account, guildIds: before} = await joinedAccount();
		await setFlags(account, PROMOTED);
		await createBuilder(harness, account.token)
			.get('/users/@me/guilds')
			.expect(403, 'ACCOUNT_SUSPICIOUS_ACTIVITY')
			.execute();

		expect(await preview(account)).toEqual({available: true, guilds: [], owned_guilds: []});

		const dispatch = vi.spyOn(NoopGatewayService.prototype, 'dispatchPresence');
		try {
			const updated = await execute(account);
			expect(updated.required_actions ?? []).toEqual([]);
			const userUpdates = dispatch.mock.calls.filter(
				([params]) => params.event === 'USER_UPDATE' && params.userId.toString() === account.userId,
			);
			expect(userUpdates).toHaveLength(1);
		} finally {
			dispatch.mockRestore();
		}

		expect(await guildIds(account)).toEqual(before);
		const flags = await readFlags(account);
		expect(flags & DEFERRED_PHONE_ON_COMMUNITY_JOIN).not.toBe(0);
		expect(flags & PHONE_GATE_PROMOTED_FROM_DEFERRAL).toBe(0);
		expect(flags & SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE).not.toBe(0);
		await createBuilder(harness, account.token).get('/users/@me/guilds').expect(200).execute();
	});

	it('closes itself after a successful escape', async () => {
		const account = await createTestAccount(harness);
		await setFlags(account, PROMOTED);
		await execute(account);
		expect(await preview(account)).toEqual({available: false, guilds: [], owned_guilds: []});
		await refused(account);
	});

	it('leaves a remaining email requirement in place', async () => {
		const account = await createTestAccount(harness);
		await setFlags(account, PROMOTED | SuspiciousActivityFlags.REQUIRE_REVERIFIED_EMAIL);
		const updated = await execute(account);
		expect(updated.required_actions).toEqual(['REQUIRE_REVERIFIED_EMAIL']);
	});

	it('refuses a requirement that was never deferred', async () => {
		const account = await createTestAccount(harness);
		await setFlags(account, SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE);
		expect((await preview(account)).available).toBe(false);
		await refused(account);
		expect(await readFlags(account)).toBe(SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE);
	});

	it('refuses a promoted account that also owes inbound verification', async () => {
		const account = await createTestAccount(harness);
		const flags = PROMOTED | SuspiciousActivityFlags.REQUIRE_INBOUND_PHONE_VERIFICATION;
		await setFlags(account, flags);
		expect((await preview(account)).available).toBe(false);
		await refused(account);
		expect(await readFlags(account)).toBe(flags);
	});
});
