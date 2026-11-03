// SPDX-License-Identifier: AGPL-3.0-or-later

import {registerControllers} from '@app/api/app/ControllerRegistry';
import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {createChannelInvite, createGuild} from '@app/api/message/tests/MessageTestUtils';
import {DATE_OF_BIRTH_ALLOWLIST, isDateOfBirthAllowedRoute} from '@app/api/middleware/DateOfBirthGateMiddleware';
import {getInstanceConfigRepository, getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import {DateOfBirthPolicies} from '@fluxer/constants/src/AccountConfigConstants';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {SensitiveMediaFilterLevel, UserFlags} from '@fluxer/constants/src/UserConstants';
import {Hono} from 'hono';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface RegisterBody {
	token: string;
	user_id: string;
	entry?: {guild_id: string; mode: string} | null;
}

interface DateOfBirthBody {
	user: {date_of_birth_required: boolean; nsfw_allowed: boolean};
	entry: {guild_id: string; mode: string} | null;
}

function yearsAgo(years: number): string {
	const date = new Date();
	date.setUTCFullYear(date.getUTCFullYear() - years);
	date.setUTCDate(date.getUTCDate() - 1);
	return date.toISOString().slice(0, 10);
}

describe('Date of birth step', () => {
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

	async function registerWithoutBirthday(body: Record<string, unknown> = {}): Promise<RegisterBody> {
		return await createBuilderWithoutAuth<RegisterBody>(harness)
			.post('/auth/register')
			.body({username: 'birthdayless', consent: true, ...body})
			.execute();
	}

	function setBirthday(token: string, dateOfBirth: string) {
		return createBuilder<DateOfBirthBody>(harness, token)
			.put('/users/@me/date-of-birth')
			.body({date_of_birth: dateOfBirth});
	}

	it('blocks everything but the allowed routes until a date of birth is set', async () => {
		const account = await registerWithoutBirthday();
		await createBuilder(harness, account.token)
			.get('/users/@me/guilds')
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.DATE_OF_BIRTH_REQUIRED)
			.execute();
		await createBuilder(harness, account.token)
			.patch('/users/@me')
			.body({global_name: 'Too Early'})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.DATE_OF_BIRTH_REQUIRED)
			.execute();
		await createBuilder(harness, account.token).get('/users/@me').expect(HTTP_STATUS.OK).execute();
		await createBuilder(harness, account.token).get('/users/@me/settings').expect(HTTP_STATUS.OK).execute();
		await createBuilder(harness, account.token).get('/experiments').expect(HTTP_STATUS.OK).execute();
	});

	it('sets an adult date once, upgrades untouched defaults and opens the app', async () => {
		const account = await registerWithoutBirthday();
		const result = await setBirthday(account.token, yearsAgo(30)).execute();
		expect(result.user).toMatchObject({date_of_birth_required: false, nsfw_allowed: true});
		const settings = await createBuilder<{sensitive_content_friend_dm_filter: number}>(harness, account.token)
			.get('/users/@me/settings')
			.execute();
		expect(settings.sensitive_content_friend_dm_filter).toBe(SensitiveMediaFilterLevel.SHOW);
		await createBuilder(harness, account.token).get('/users/@me/guilds').expect(HTTP_STATUS.OK).execute();
		await setBirthday(account.token, yearsAgo(31))
			.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.DATE_OF_BIRTH_ALREADY_SET)
			.execute();
	});

	it('keeps teen defaults for a teen', async () => {
		const account = await registerWithoutBirthday();
		const result = await setBirthday(account.token, yearsAgo(15)).execute();
		expect(result.user).toMatchObject({date_of_birth_required: false, nsfw_allowed: false});
		const settings = await createBuilder<{sensitive_content_friend_dm_filter: number}>(harness, account.token)
			.get('/users/@me/settings')
			.execute();
		expect(settings.sensitive_content_friend_dm_filter).toBe(SensitiveMediaFilterLevel.BLUR);
	});

	it('clamps adult content settings when an account without a date turns out to be a teen', async () => {
		const legacy = await createTestAccount(harness);
		const legacyUser = await getUserRepository().findUniqueAssert(createUserID(BigInt(legacy.userId)));
		await getUserRepository().patchUpsert(legacyUser.id, {date_of_birth: null}, legacyUser.toRow());
		await setBirthday(legacy.token, yearsAgo(15)).execute();
		const settings = await createBuilder<{
			sensitive_content_friend_dm_filter: number;
			sensitive_content_non_friend_dm_filter: number;
		}>(harness, legacy.token)
			.get('/users/@me/settings')
			.execute();
		expect(settings.sensitive_content_friend_dm_filter).toBe(SensitiveMediaFilterLevel.BLUR);
		expect(settings.sensitive_content_non_friend_dm_filter).toBe(SensitiveMediaFilterLevel.BLOCK);
	});

	it('refuses dates that are malformed, impossible, today, in the future or more than 120 years back', async () => {
		const account = await registerWithoutBirthday();
		const today = new Date().toISOString().slice(0, 10);
		for (const value of [
			'2000-02-30',
			'1800-01-01',
			yearsAgo(142),
			yearsAgo(120),
			today,
			`${new Date().getUTCFullYear() + 1}-01-01`,
		]) {
			await setBirthday(account.token, value).expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY').execute();
		}
	});

	it('accepts someone who turns 120 today', async () => {
		const account = await registerWithoutBirthday();
		const date = new Date();
		date.setUTCFullYear(date.getUTCFullYear() - 120);
		await setBirthday(account.token, date.toISOString().slice(0, 10)).expect(HTTP_STATUS.OK).execute();
	});

	it('refuses an implausible date at registration too', async () => {
		await createBuilderWithoutAuth(harness)
			.post('/auth/register')
			.body({username: 'ancient', consent: true, date_of_birth: yearsAgo(142)})
			.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
			.execute();
	});

	it('closes an account below the minimum age and keeps no date', async () => {
		const account = await registerWithoutBirthday();
		await setBirthday(account.token, yearsAgo(9))
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.ACCOUNT_BELOW_MINIMUM_AGE)
			.execute();
		const user = await getUserRepository().findUniqueAssert(createUserID(BigInt(account.user_id)));
		expect(user.dateOfBirth).toBeNull();
		expect(user.traits.has('below_minimum_age')).toBe(true);
		expect(user.username).not.toBe('birthdayless');
		expect(user.deletionReasonCode).toBe(DeletionReasons.BELOW_MINIMUM_AGE);
		const remainingMs = user.pendingDeletionAt!.getTime() - Date.now();
		expect(remainingMs).toBeGreaterThan(71 * 3600 * 1000);
		expect(remainingMs).toBeLessThanOrEqual(72 * 3600 * 1000);
		await createBuilder(harness, account.token).get('/users/@me').expect(HTTP_STATUS.UNAUTHORIZED).execute();
	});

	async function correctBirthday(userId: string): Promise<void> {
		const staff = await setUserACLs(harness, await createTestAccount(harness), [AdminACLs.WILDCARD]);
		await createBuilder(harness, staff.token)
			.patch(`/admin/users/${userId}/date-of-birth`)
			.body({date_of_birth: yearsAgo(30)})
			.execute();
	}

	it('a corrected date of birth cancels the under-age deletion', async () => {
		const account = await registerWithoutBirthday();
		await setBirthday(account.token, yearsAgo(9))
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.ACCOUNT_BELOW_MINIMUM_AGE)
			.execute();
		await correctBirthday(account.user_id);
		const user = await getUserRepository().findUniqueAssert(createUserID(BigInt(account.user_id)));
		expect(user.traits.has('below_minimum_age')).toBe(false);
		expect(user.flags & UserFlags.DELETED).toBe(0n);
		expect(user.pendingDeletionAt).toBeNull();
	});

	it('enforces only for accounts created without a date until every account is required', async () => {
		const legacy = await createTestAccount(harness);
		const legacyUser = await getUserRepository().findUniqueAssert(createUserID(BigInt(legacy.userId)));
		await getUserRepository().patchUpsert(legacyUser.id, {date_of_birth: null}, legacyUser.toRow());
		await createBuilder(harness, legacy.token).get('/users/@me/guilds').expect(HTTP_STATUS.OK).execute();
		const me = await createBuilder<{date_of_birth_required: boolean}>(harness, legacy.token)
			.get('/users/@me')
			.execute();
		expect(me.date_of_birth_required).toBe(true);
		await getInstanceConfigRepository().updateAccountsConfig({date_of_birth: DateOfBirthPolicies.REQUIRE_ALL});
		await createBuilder(harness, legacy.token)
			.get('/users/@me/guilds')
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.DATE_OF_BIRTH_REQUIRED)
			.execute();
		await getInstanceConfigRepository().updateAccountsConfig({date_of_birth: DateOfBirthPolicies.ASK});
		const fresh = await registerWithoutBirthday();
		await createBuilder(harness, fresh.token).get('/users/@me/guilds').expect(HTTP_STATUS.OK).execute();
		const freshMe = await createBuilder<{date_of_birth_required: boolean}>(harness, fresh.token)
			.get('/users/@me')
			.execute();
		expect(freshMe.date_of_birth_required).toBe(true);
	});

	it('starts the gateway session without guilds until the date is set', async () => {
		await getInstanceConfigRepository().updateAccountsConfig({date_of_birth: DateOfBirthPolicies.REQUIRE_ALL});
		const legacy = await createTestAccount(harness);
		await createGuild(harness, legacy.token, 'Before the step');
		const legacyUser = await getUserRepository().findUniqueAssert(createUserID(BigInt(legacy.userId)));
		await getUserRepository().patchUpsert(legacyUser.id, {date_of_birth: null}, legacyUser.toRow());
		const ready = await createBuilder<{data: {guilds: Array<unknown>}}>(harness, '')
			.post('/test/rpc-session-init')
			.body({type: 'session', token: legacy.token, version: 1, ip: '127.0.0.1'})
			.execute();
		expect(ready.data.guilds).toEqual([]);
		await setBirthday(legacy.token, yearsAgo(25)).execute();
		const after = await createBuilder<{data: {guilds: Array<unknown>}}>(harness, '')
			.post('/test/rpc-session-init')
			.body({type: 'session', token: legacy.token, version: 1, ip: '127.0.0.1'})
			.execute();
		expect(after.data.guilds).toHaveLength(1);
	});

	it('runs the invite from sign-up after the date of birth, once', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Welcome');
		const invite = await createChannelInvite(harness, owner.token, guild.system_channel_id!);
		const account = await registerWithoutBirthday({invite_code: invite.code});
		expect(account.entry ?? null).toBeNull();
		const result = await setBirthday(account.token, yearsAgo(20)).execute();
		expect(result.entry).toEqual({guild_id: guild.id, mode: 'member'});
		const guilds = await createBuilder<Array<{id: string}>>(harness, account.token).get('/users/@me/guilds').execute();
		expect(guilds.map((entry) => entry.id)).toContain(guild.id);
	});

	it('runs the invite at registration for older apps that send a date of birth', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Old app welcome');
		const invite = await createChannelInvite(harness, owner.token, guild.system_channel_id!);
		const account = await registerWithoutBirthday({invite_code: invite.code, date_of_birth: yearsAgo(22)});
		expect(account.entry).toEqual({guild_id: guild.id, mode: 'member'});
	});

	it('answers not found where the instance does not collect a date of birth', async () => {
		const account = await createTestAccount(harness);
		await getInstanceConfigRepository().updateAccountsConfig({date_of_birth: DateOfBirthPolicies.OFF});
		await setBirthday(account.token, yearsAgo(20)).expect(HTTP_STATUS.NOT_FOUND).execute();
	});
});

describe('Date of birth gate route coverage', () => {
	function registeredRoutes(): Array<{method: string; path: string}> {
		const app = new Hono<HonoEnv>();
		registerControllers(app, Config);
		const seen = new Set<string>();
		return app.routes
			.filter((route) => route.method !== 'ALL')
			.filter((route) => {
				const key = `${route.method} ${route.path}`;
				if (seen.has(key)) return false;
				seen.add(key);
				return true;
			})
			.map((route) => ({method: route.method, path: route.path}));
	}

	it('points every allowlist entry at a registered route', () => {
		const routes = registeredRoutes();
		for (const entry of DATE_OF_BIRTH_ALLOWLIST) {
			const matched = routes.some((route) => route.method === entry.method && entry.path.test(route.path));
			expect(matched, `${entry.method} ${entry.path}`).toBe(true);
		}
	});

	it('lets a person without a date of birth reach only the reviewed routes outside sign-in and tooling', () => {
		const reachable = registeredRoutes()
			.filter((route) => !/^\/(auth|admin|test)\//.test(route.path))
			.filter((route) => isDateOfBirthAllowedRoute(route.method, route.path.replace(/:[a-z_]+/g, 'x')))
			.map((route) => `${route.method} ${route.path}`)
			.sort();
		expect(reachable).toEqual(
			[
				'GET /experiments',
				'GET /gateway/bot',
				'GET /users/@me',
				'GET /users/@me/harvest/:harvestId',
				'GET /users/@me/harvest/:harvestId/download',
				'GET /users/@me/harvest/latest',
				'GET /users/@me/settings',
				'GET /users/@me/sudo/mfa-methods',
				'POST /users/@me/delete',
				'POST /users/@me/disable',
				'POST /users/@me/harvest',
				'POST /users/@me/harvest/filtered',
				'POST /users/@me/sudo/email-code',
				'POST /users/@me/sudo/webauthn/authentication-options',
				'POST /users/@me/terms-acceptance',
				'PUT /users/@me/date-of-birth',
			].sort(),
		);
	});
});
