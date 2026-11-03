// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestCustomBadge} from '@app/api/admin/tests/AdminTestUtils';
import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {acceptInvite, createChannelInvite, createGuild, getGuildChannels} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {PROFILE_BADGE_ASSETS} from '@app/api/user/profile_badges/ProfileBadgeAssets';
import {VISIONARY_NUMBER_TAGS} from '@app/api/user/profile_badges/VisionaryNumberTags';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {
	PremiumFlags,
	ProfilePrivacyLevels,
	PublicUserFlags,
	UserFlags,
	UserPremiumTypes,
} from '@fluxer/constants/src/UserConstants';
import type {UserAdminResponse} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';
import type {
	ProfileBadgePreviewResponse,
	ProfileBadgeResponse,
} from '@fluxer/schema/src/domains/user/ProfileBadgeSchemas';
import type {UserPrivateResponse, UserProfileFullResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

type ProfileWithBadges = UserProfileFullResponse & {profile_badges: Array<ProfileBadgeResponse>};

const PREMIUM_SINCE = '2026-10-15T12:00:00.000Z';
const PUBLIC_BADGE_FLAGS = PublicUserFlags.STAFF | PublicUserFlags.PARTNER | PublicUserFlags.BUG_HUNTER;

describe('profile badges', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	afterAll(async () => {
		await harness.shutdown();
	});
	beforeEach(async () => {
		await harness.resetData();
	});

	async function shareGuild(owner: TestAccount, member: TestAccount): Promise<void> {
		const guild = await createGuild(harness, owner.token, 'Shared');
		const channels = await getGuildChannels(harness, owner.token, guild.id);
		const invite = await createChannelInvite(harness, owner.token, channels[0]!.id);
		await acceptInvite(harness, member.token, invite.code);
	}

	async function setFlags(account: TestAccount, flags: bigint): Promise<void> {
		await createBuilder(harness, '')
			.patch(`/test/users/${account.userId}/flags`)
			.body({flags: flags.toString()})
			.execute();
	}

	async function setPremium(account: TestAccount, body: Record<string, unknown>): Promise<void> {
		await createBuilder(harness, '').post(`/test/users/${account.userId}/premium`).body(body).execute();
	}

	async function makeVisionary(account: TestAccount): Promise<void> {
		await setPremium(account, {
			premium_type: UserPremiumTypes.LIFETIME,
			premium_since: PREMIUM_SINCE,
			premium_lifetime_sequence: 42,
		});
	}

	async function profile(
		viewer: TestAccount,
		target: TestAccount,
		acceptLanguage?: string,
	): Promise<ProfileWithBadges> {
		const builder = createBuilder<ProfileWithBadges>(harness, viewer.token).get(`/users/${target.userId}/profile`);
		if (acceptLanguage) builder.header('Accept-Language', acceptLanguage);
		return builder.expect(HTTP_STATUS.OK).execute();
	}

	async function preview(account: TestAccount, body: Record<string, unknown>): Promise<ProfileBadgePreviewResponse> {
		return createBuilder<ProfileBadgePreviewResponse>(harness, account.token)
			.post('/users/@me/profile-badges/preview')
			.body(body)
			.expect(HTTP_STATUS.OK)
			.execute();
	}

	const kinds = (response: {profile_badges: Array<{kind: string}>}) => response.profile_badges.map((b) => b.kind);

	test('profiles list the badges with assets and labels in the viewer locale', async () => {
		const target = await createTestAccount(harness);
		const viewer = await createTestAccount(harness);
		await shareGuild(target, viewer);
		await setFlags(target, UserFlags.STAFF | UserFlags.BUG_HUNTER);
		await makeVisionary(target);
		const english = await profile(viewer, target);
		expect(english.profile_badges).toEqual([
			{
				key: 'staff',
				kind: 'staff',
				label: 'Fluxer Staff',
				asset: PROFILE_BADGE_ASSETS.staff,
				asset_url: null,
				aspect_ratio: null,
				caption: null,
				href: expect.any(String),
			},
			{
				key: 'bug_hunter',
				kind: 'bug_hunter',
				label: 'Fluxer Bug Hunter',
				asset: PROFILE_BADGE_ASSETS.bug_hunter,
				asset_url: null,
				aspect_ratio: null,
				caption: null,
				href: expect.any(String),
			},
			{
				key: 'visionary',
				kind: 'visionary',
				label: 'Fluxer Visionary since October 15, 2026',
				asset: PROFILE_BADGE_ASSETS.visionary,
				asset_url: null,
				aspect_ratio: null,
				caption: null,
				href: expect.stringMatching(/\/help\/visionary$/),
			},
			{
				key: 'visionary_number',
				kind: 'visionary_number',
				label: 'Visionary ID #42',
				asset: VISIONARY_NUMBER_TAGS['42']!.asset,
				asset_url: null,
				aspect_ratio: 226 / 128,
				caption: '#42',
				href: expect.stringMatching(/\/help\/visionary$/),
			},
		]);
	});

	test('a stored sequence of 0 shows #0 and an unset one shows no number', async () => {
		const account = await createTestAccount(harness);
		await makeVisionary(account);
		await setPremium(account, {premium_lifetime_sequence: 0});
		const [, number] = (await profile(account, account)).profile_badges;
		expect(number).toMatchObject({kind: 'visionary_number', caption: '#0', asset: VISIONARY_NUMBER_TAGS['0']!.asset});
		await setPremium(account, {premium_lifetime_sequence: null});
		expect(kinds(await profile(account, account))).toEqual(['visionary']);
	});

	test('the stored viewer locale beats the Accept-Language header', async () => {
		const target = await createTestAccount(harness);
		const viewer = await createTestAccount(harness);
		await shareGuild(target, viewer);
		await makeVisionary(target);
		const visionaryLabel = async (acceptLanguage: string) =>
			(await profile(viewer, target, acceptLanguage)).profile_badges.find((badge) => badge.kind === 'visionary')?.label;
		await createBuilder(harness, viewer.token).patch('/users/@me/settings').body({locale: 'de'}).execute();
		expect(await visionaryLabel('ja')).toContain('Oktober 2026');
		await createBuilder(harness, viewer.token).patch('/users/@me/settings').body({locale: 'ja'}).execute();
		expect(await visionaryLabel('de')).toContain('2026年10月');
	});

	test('legacy premium fields keep their current shape', async () => {
		const target = await createTestAccount(harness);
		const viewer = await createTestAccount(harness);
		await shareGuild(target, viewer);
		await makeVisionary(target);
		const response = await profile(viewer, target);
		expect(response).toMatchObject({
			premium_type: UserPremiumTypes.LIFETIME,
			premium_since: PREMIUM_SINCE,
			premium_lifetime_sequence: 42,
		});
		await createBuilder(harness, '')
			.patch(`/test/users/${target.userId}/premium-flags`)
			.body({premium_flags: PremiumFlags.BADGE_MASKED})
			.execute();
		const masked = await profile(viewer, target);
		expect(masked.premium_type).toBe(UserPremiumTypes.SUBSCRIPTION);
		expect(kinds(masked)).toEqual(['plutonium']);
	});

	test('a restricted viewer only sees the flag badges', async () => {
		const target = await createTestAccount(harness);
		const viewer = await createTestAccount(harness);
		await shareGuild(target, viewer);
		await setFlags(target, UserFlags.PARTNER);
		await makeVisionary(target);
		await createBuilder(harness, target.token)
			.patch('/users/@me/settings')
			.body({profile_privacy: ProfilePrivacyLevels.FRIENDS_ONLY})
			.execute();
		const response = await profile(viewer, target);
		expect(response.profile_limited).toBe(true);
		expect(kinds(response)).toEqual(['partner']);
		expect(kinds(await profile(target, target))).toEqual(['partner', 'visionary', 'visionary_number']);
	});

	test('hidden accounts show no badges', async () => {
		const target = await createTestAccount(harness);
		const viewer = await createTestAccount(harness);
		await shareGuild(target, viewer);
		await setFlags(target, UserFlags.STAFF | UserFlags.PROFILE_HIDDEN);
		await makeVisionary(target);
		expect((await profile(viewer, target)).profile_badges).toEqual([]);
	});

	async function hideBadges(account: TestAccount, body: Record<string, unknown>): Promise<UserPrivateResponse> {
		return createBuilder<UserPrivateResponse>(harness, account.token)
			.patch('/users/@me')
			.body(body)
			.expect(HTTP_STATUS.OK)
			.execute();
	}

	test('the preview reflects each setting without saving it', async () => {
		const account = await createTestAccount(harness);
		await makeVisionary(account);
		expect(kinds(await preview(account, {}))).toEqual(['visionary', 'visionary_number']);
		expect(kinds(await preview(account, {hidden_profile_badges: ['visionary']}))).toEqual([]);
		expect(kinds(await preview(account, {premium_badge_masked: true}))).toEqual(['plutonium']);
		expect(kinds(await preview(account, {hidden_profile_badges: ['visionary_number']}))).toEqual(['visionary']);
		const dateless = await preview(account, {premium_badge_timestamp_hidden: true});
		expect(dateless.profile_badges[0]?.label).toBe('Fluxer Visionary');
		const masked = await preview(account, {premium_badge_masked: true, hidden_profile_badges: ['visionary_number']});
		expect(masked.own_badges.map(({key, hidden}) => [key, hidden])).toEqual([
			['visionary', false],
			['visionary_number', true],
		]);
		const saved = await createBuilder<UserPrivateResponse>(harness, account.token).get('/users/@me').execute();
		expect(saved.hidden_profile_badges).toEqual([]);
		expect(kinds(await profile(account, account))).toEqual(['visionary', 'visionary_number']);
	});

	test('hidden badges leave other people and the public flags but stay listed for you and staff', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [AdminACLs.WILDCARD]);
		const target = await createTestAccount(harness);
		const viewer = await createTestAccount(harness);
		await shareGuild(target, viewer);
		await setFlags(target, UserFlags.STAFF | UserFlags.PARTNER | UserFlags.BUG_HUNTER);
		await makeVisionary(target);
		const custom = await createTestCustomBadge(harness, admin.token);
		await createBuilder(harness, admin.token)
			.patch(`/admin/users/${target.userId}/badges`)
			.body({badge_ids: [custom.id]})
			.expect(HTTP_STATUS.OK)
			.execute();
		const hidden = ['staff', 'bug_hunter', 'visionary_number', `custom:${custom.id}`];
		const saved = await hideBadges(target, {hidden_profile_badges: hidden});
		expect(saved.hidden_profile_badges).toEqual([...hidden].sort());

		expect(kinds(await profile(viewer, target))).toEqual(['partner', 'visionary']);
		const partial = await createBuilder<{flags: number}>(harness, viewer.token)
			.get(`/users/${target.userId}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(partial.flags & PUBLIC_BADGE_FLAGS).toBe(PublicUserFlags.PARTNER);

		const own = await preview(target, {});
		expect(own.own_badges.map(({key, hidden}) => [key, hidden])).toEqual([
			['staff', true],
			['partner', false],
			['bug_hunter', true],
			['visionary', false],
			['visionary_number', true],
			[`custom:${custom.id}`, true],
		]);

		const {users} = await createBuilder<{users: Array<UserAdminResponse>}>(harness, admin.token)
			.get(`/admin/users/${target.userId}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(users[0]!.profile_badges.map(({key, suppressed}) => [key, suppressed])).toEqual([
			['staff', true],
			['partner', false],
			['bug_hunter', true],
			['visionary', false],
			['visionary_number', true],
			[`custom:${custom.id}`, true],
		]);
	});

	test('the deprecated hide fields edit the list, and the list wins when both are sent', async () => {
		const account = await createTestAccount(harness);
		await makeVisionary(account);
		const hidden = await hideBadges(account, {premium_badge_hidden: true, operator_pass_badge_hidden: true});
		expect(hidden.hidden_profile_badges).toEqual(['operator_pass', 'plutonium', 'premium', 'visionary']);
		expect(hidden).toMatchObject({premium_badge_hidden: true, premium_badge_sequence_hidden: false});
		expect(kinds(await profile(account, account))).toEqual([]);

		await hideBadges(account, {premium_badge_sequence_hidden: true});
		const shown = await hideBadges(account, {premium_badge_hidden: false});
		expect(shown.hidden_profile_badges).toEqual(['operator_pass', 'visionary_number']);
		expect(shown).toMatchObject({premium_badge_hidden: false, premium_badge_sequence_hidden: true});

		const listed = await hideBadges(account, {premium_badge_hidden: true, hidden_profile_badges: ['staff']});
		expect(listed.hidden_profile_badges).toEqual(['staff']);
		expect(listed.premium_badge_hidden).toBe(false);

		await createBuilder(harness, account.token)
			.patch('/users/@me')
			.body({hidden_profile_badges: ['not_a_badge']})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
});
