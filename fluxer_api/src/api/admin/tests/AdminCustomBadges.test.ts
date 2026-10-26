// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestCustomBadge} from '@app/api/admin/tests/AdminTestUtils';
import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {getPngDataUrl} from '@app/api/emoji/tests/EmojiTestUtils';
import {createGuild} from '@app/api/message/tests/MessageTestUtils';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import type {AdminCustomBadgeResponse} from '@fluxer/schema/src/domains/admin/AdminBadgeSchemas';
import type {ProfileBadgeResponse} from '@fluxer/schema/src/domains/user/ProfileBadgeSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

describe('custom badges', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	afterAll(async () => {
		await harness.shutdown();
	});
	beforeEach(async () => {
		await harness.resetData();
		admin = await setUserACLs(harness, await createTestAccount(harness), [AdminACLs.WILDCARD]);
	});

	async function profileBadges(viewer: TestAccount, target: TestAccount): Promise<Array<ProfileBadgeResponse>> {
		const profile = await createBuilder<{profile_badges: Array<ProfileBadgeResponse>}>(harness, viewer.token)
			.get(`/users/${target.userId}/profile`)
			.expect(HTTP_STATUS.OK)
			.execute();
		return profile.profile_badges;
	}

	async function assignUserBadges(target: TestAccount, badgeIds: Array<string>, status: number, code?: string) {
		await createBuilder(harness, admin.token)
			.patch(`/admin/users/${target.userId}/badges`)
			.body({badge_ids: badgeIds})
			.expect(status, code)
			.execute();
	}

	test('stores an uploaded image reference and serves it as a profile badge until deleted', async () => {
		const badge = await createTestCustomBadge(harness, admin.token, 'user', 'Translator');
		const [stored] = await getInstanceConfigRepository().getCustomBadges();
		expect(stored?.asset).toMatch(new RegExp(`^custom-badges/${badge.id}/[0-9a-f]{8}\\.png$`));
		expect(badge.asset_url).toMatch(new RegExp(`/custom-badges/${badge.id}/[0-9a-f]{8}\\.webp$`));

		const updated = await createBuilder<AdminCustomBadgeResponse>(harness, admin.token)
			.patch(`/admin/badges/${badge.id}`)
			.body({name: 'Translator', description: 'Helps translate', href: 'https://example.com/translate'})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(updated.asset_url).toBe(badge.asset_url);

		const target = await createTestAccount(harness);
		await assignUserBadges(target, [badge.id], HTTP_STATUS.OK);
		expect(await profileBadges(target, target)).toContainEqual(
			expect.objectContaining({
				key: `custom:${badge.id}`,
				kind: 'custom',
				label: 'Translator',
				asset: null,
				asset_url: badge.asset_url,
				href: 'https://example.com/translate',
				details: {title: 'Translator', lines: ['Helps translate']},
			}),
		);

		await createBuilder(harness, admin.token)
			.delete(`/admin/badges/${badge.id}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		expect((await profileBadges(target, target)).some((entry) => entry.kind === 'custom')).toBe(false);
	});

	test('refuses a guild badge on a user and a user badge on a guild', async () => {
		const guildBadge = await createTestCustomBadge(harness, admin.token, 'guild');
		const userBadge = await createTestCustomBadge(harness, admin.token, 'user');
		const target = await createTestAccount(harness);
		await assignUserBadges(target, [guildBadge.id], HTTP_STATUS.NOT_FOUND, 'UNKNOWN_BADGE');
		const guild = await createGuild(harness, admin.token, 'Badge Guild');
		await createBuilder(harness, admin.token)
			.patch(`/admin/guilds/${guild.id}`)
			.body({badge_ids: [userBadge.id]})
			.expect(HTTP_STATUS.NOT_FOUND, 'UNKNOWN_BADGE')
			.execute();
	});

	test('needs the matching ACL on every route', async () => {
		const badge = await createTestCustomBadge(harness, admin.token);
		const target = await createTestAccount(harness);
		const limited = await setUserACLs(harness, await createTestAccount(harness), [AdminACLs.AUTHENTICATE]);
		const requests = [
			createBuilder(harness, limited.token).get('/admin/badges'),
			createBuilder(harness, limited.token)
				.post('/admin/badges')
				.body({type: 'user', name: 'Denied', image: getPngDataUrl()}),
			createBuilder(harness, limited.token)
				.patch(`/admin/badges/${badge.id}`)
				.body({name: 'Denied', description: null, href: null}),
			createBuilder(harness, limited.token).delete(`/admin/badges/${badge.id}`),
			createBuilder(harness, limited.token)
				.patch(`/admin/users/${target.userId}/badges`)
				.body({badge_ids: [badge.id]}),
		];
		for (const request of requests) {
			await request.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_ACL').execute();
		}
	});
});
