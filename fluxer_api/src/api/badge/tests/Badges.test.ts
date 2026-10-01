// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createTestBadge, TEST_BADGE_ICON} from '@app/api/badge/tests/BadgeTestUtils';
import {createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {getGatewayService} from '@app/api/middleware/ServiceRegistry';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {BadgeTypes, BuiltinBadges} from '@fluxer/constants/src/BadgeConstants';
import type {BadgeMutationResponse} from '@fluxer/schema/src/domains/admin/AdminBadgeSchemas';
import type {BadgesResponse, BuiltinBadgeIconsResponse} from '@fluxer/schema/src/domains/badge/BadgeSchemas';
import type {UserProfileFullResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

async function listPublicBadges(harness: ApiTestHarness): Promise<BadgesResponse> {
	return createBuilderWithoutAuth<BadgesResponse>(harness).get('/badges').expect(HTTP_STATUS.OK).execute();
}

describe('Badges', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		admin = await setUserACLs(harness, await createTestAccount(harness), [AdminACLs.WILDCARD]);
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	it('creates a badge and broadcasts it to the gateway', async () => {
		const broadcast = vi.spyOn(getGatewayService(), 'broadcastDispatch');
		const badge = await createTestBadge(harness, admin.token, {
			icon: '<svg viewBox="0 0 16 16"><rect width="16" height="16"/></svg>',
			url: 'https://www.fluxer.app/badge',
		});
		expect(badge).toMatchObject({
			type: BadgeTypes.USER,
			icon: `<svg viewBox="0 0 16 16"><rect width="16" height="16"/></svg>`,
			url: 'https://www.fluxer.app/badge',
			position: 0,
		});
		const badges = await listPublicBadges(harness);
		expect(badges.badges).toEqual([badge]);
		expect(broadcast).toHaveBeenCalledWith({
			event: 'BADGES_UPDATE',
			data: {version: badges.version, badges: [badge]},
		});
		broadcast.mockRestore();
	});

	it('updates and deletes badges and incrementing the version', async () => {
		const badge = await createTestBadge(harness, admin.token);
		const {version} = await listPublicBadges(harness);
		const {badge: updated} = await createBuilder<BadgeMutationResponse>(harness, admin.token)
			.patch(`/admin/badges/${badge.id}`)
			.body({tooltip: 'Updated tooltip', url: null})
			.execute();
		expect(updated).toEqual({...badge, tooltip: 'Updated tooltip', url: null});
		await createBuilder(harness, admin.token).delete(`/admin/badges/${badge.id}`).execute();
		const badges = await listPublicBadges(harness);
		expect(badges.badges).toEqual([]);
		expect(badges.version).toBe(version + 2);
		await createBuilder(harness, admin.token)
			.delete(`/admin/badges/${badge.id}`)
			.expect(HTTP_STATUS.NOT_FOUND, 'UNKNOWN_BADGE')
			.execute();
	});

	it('serves the badge list', async () => {
		await createTestBadge(harness, admin.token);
		const {response} = await createBuilderWithoutAuth(harness).get('/badges').executeWithResponse();
		const etag = response.headers.get('etag');
		expect(etag).toBeTruthy();
		const {response: notModified} = await createBuilderWithoutAuth(harness)
			.get('/badges')
			.header('If-None-Match', etag!)
			.expect(304)
			.executeWithResponse();
		expect(notModified.status).toBe(304);
	});

	it('overrides and restores built-in badge icons', async () => {
		const icons = await createBuilder<BuiltinBadgeIconsResponse>(harness, admin.token)
			.put(`/admin/badges/builtin/${BuiltinBadges.VERIFIED}`)
			.body({icon: TEST_BADGE_ICON})
			.execute();
		expect(icons).toEqual({staff: null, premium: null, verified: TEST_BADGE_ICON, partnered: null, discoverable: null});
		expect((await listPublicBadges(harness)).builtin_icons.verified).toBe(TEST_BADGE_ICON);
		await createBuilder(harness, admin.token)
			.put(`/admin/badges/builtin/${BuiltinBadges.VERIFIED}`)
			.body({icon: null})
			.execute();
		expect((await listPublicBadges(harness)).builtin_icons.verified).toBeNull();
	});

	it('assigns user badges shown on the profile', async () => {
		const target = await createTestAccount(harness);
		const badge = await createTestBadge(harness, admin.token);
		await createBuilder(harness, admin.token)
			.patch(`/admin/users/${target.userId}/badges`)
			.body({badge_ids: [badge.id]})
			.execute();
		const profile = await createBuilder<UserProfileFullResponse>(harness, target.token)
			.get(`/users/${target.userId}/profile`)
			.execute();
		expect(profile.badges).toEqual([badge.id]);
	});

	it('rejects assigning a badge defined for another entity type', async () => {
		const target = await createTestAccount(harness);
		const guildBadge = await createTestBadge(harness, admin.token, {type: BadgeTypes.GUILD});
		await createBuilder(harness, admin.token)
			.patch(`/admin/users/${target.userId}/badges`)
			.body({badge_ids: [guildBadge.id]})
			.expect(HTTP_STATUS.NOT_FOUND, 'UNKNOWN_BADGE')
			.execute();
	});

	it('assigns guild badges', async () => {
		const guild = await createGuild(harness, admin.token, 'Badge Guild');
		const badge = await createTestBadge(harness, admin.token, {type: BadgeTypes.GUILD});
		await createBuilder(harness, admin.token)
			.patch(`/admin/guilds/${guild.id}`)
			.body({badge_ids: [badge.id]})
			.execute();
		const {guild: lookup} = await createBuilder<{guild: {badge_ids: Array<string>}}>(harness, admin.token)
			.get(`/admin/guilds/${guild.id}`)
			.execute();
		expect(lookup.badge_ids).toEqual([badge.id]);
	});

	it('requires the badge ACLs', async () => {
		const moderator = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.BADGE_LIST,
		]);
		await createBuilder(harness, moderator.token).get('/admin/badges').execute();
		await createBuilder(harness, moderator.token)
			.post('/admin/badges')
			.body({type: BadgeTypes.USER, name: 'Nope', tooltip: 'Nope', icon: TEST_BADGE_ICON})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
});
