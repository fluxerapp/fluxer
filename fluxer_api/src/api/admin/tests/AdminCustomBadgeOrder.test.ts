// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestCustomBadge} from '@app/api/admin/tests/AdminTestUtils';
import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import type {AdminCustomBadgeListResponse} from '@fluxer/schema/src/domains/admin/AdminBadgeSchemas';
import type {ProfileBadgeResponse} from '@fluxer/schema/src/domains/user/ProfileBadgeSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

describe('custom badge order', () => {
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

	async function listedIds(): Promise<Array<string>> {
		const response = await createBuilder<AdminCustomBadgeListResponse>(harness, admin.token)
			.get('/admin/badges')
			.expect(HTTP_STATUS.OK)
			.execute();
		return response.badges.map((badge) => badge.id);
	}

	async function createBadges(): Promise<Array<string>> {
		const first = await createTestCustomBadge(harness, admin.token, 'user', 'First');
		const second = await createTestCustomBadge(harness, admin.token, 'guild', 'Second');
		const third = await createTestCustomBadge(harness, admin.token, 'user', 'Third');
		return [first.id, second.id, third.id];
	}

	async function expectRejectedOrder(badgeIds: Array<string>, status: number, code: string) {
		const {json} = await createBuilder<{errors?: Array<{path: string}>}>(harness, admin.token)
			.put('/admin/badges/order')
			.body({badge_ids: badgeIds})
			.expect(status, code)
			.executeWithResponse();
		return json;
	}

	test('saves the new order and shows it in the list and on profiles', async () => {
		const [first, second, third] = await createBadges();
		const response = await createBuilder<AdminCustomBadgeListResponse>(harness, admin.token)
			.put('/admin/badges/order')
			.body({badge_ids: [third, second, first]})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(response.badges.map((badge) => badge.id)).toEqual([third, second, first]);
		expect(await listedIds()).toEqual([third, second, first]);

		const target = await createTestAccount(harness);
		await createBuilder(harness, admin.token)
			.patch(`/admin/users/${target.userId}/badges`)
			.body({badge_ids: [first, third]})
			.expect(HTTP_STATUS.OK)
			.execute();
		const profile = await createBuilder<{profile_badges: Array<ProfileBadgeResponse>}>(harness, target.token)
			.get(`/users/${target.userId}/profile`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(profile.profile_badges.filter((badge) => badge.kind === 'custom').map((badge) => badge.key)).toEqual([
			`custom:${third}`,
			`custom:${first}`,
		]);
	});

	test('rejects an unknown ID and keeps the order', async () => {
		const [first, second, third] = await createBadges();
		await expectRejectedOrder([third, second, first, '1'], HTTP_STATUS.NOT_FOUND, 'UNKNOWN_BADGE');
		expect(await listedIds()).toEqual([first, second, third]);
	});

	test('rejects a missing ID and keeps the order', async () => {
		const [first, second, third] = await createBadges();
		const json = await expectRejectedOrder([third, first], HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY');
		expect(json.errors).toEqual([expect.objectContaining({path: 'badge_ids'})]);
		expect(await listedIds()).toEqual([first, second, third]);
	});

	test('rejects a duplicate ID and keeps the order', async () => {
		const [first, second, third] = await createBadges();
		const json = await expectRejectedOrder([third, second, first, third], HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY');
		expect(json.errors).toEqual([expect.objectContaining({path: 'badge_ids'})]);
		expect(await listedIds()).toEqual([first, second, third]);
	});

	test('needs the badge update ACL', async () => {
		const [first, second, third] = await createBadges();
		const reader = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.BADGE_LIST,
		]);
		await createBuilder(harness, reader.token)
			.put('/admin/badges/order')
			.body({badge_ids: [third, second, first]})
			.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_ACL')
			.execute();
		expect(await listedIds()).toEqual([first, second, third]);
	});
});
