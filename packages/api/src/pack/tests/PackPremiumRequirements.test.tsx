/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {createTestAccount} from '@fluxer/api/src/auth/tests/AuthTestUtils';
import {
	createPack,
	grantPremium,
	grantStaffAccess,
	installPack,
	listPacks,
	revokePremium,
	setupNonPremiumPackTestAccount,
	setupPackTestAccount,
} from '@fluxer/api/src/pack/tests/PackTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@fluxer/api/src/test/ApiTestHarness';
import {HTTP_STATUS} from '@fluxer/api/src/test/TestConstants';
import {createBuilder} from '@fluxer/api/src/test/TestRequestBuilder';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

describe('Pack Premium Requirements', () => {
	let harness: ApiTestHarness;

	beforeEach(async () => {
		harness = await createApiTestHarness();
	});

	afterEach(async () => {
		await harness?.shutdown();
	});

	test('user without staff flag cannot list packs', async () => {
		const account = await createTestAccount(harness);

		await createBuilder(harness, account.token).get('/packs').expect(HTTP_STATUS.FORBIDDEN).execute();
	});

	test('user with staff flag but no premium cannot create pack', async () => {
		const {account} = await setupNonPremiumPackTestAccount(harness);

		await createBuilder(harness, account.token)
			.post('/packs/emoji')
			.body({name: 'Test Pack'})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});

	test('premium user with staff flag can create emoji pack', async () => {
		const {account} = await setupPackTestAccount(harness);

		const pack = await createPack(harness, account.token, 'emoji', {name: 'My Emoji Pack'});

		expect(pack.id).toBeTruthy();
		expect(pack.name).toBe('My Emoji Pack');
		expect(pack.type).toBe('emoji');
	});

	test('premium user with staff flag can create sticker pack', async () => {
		const {account} = await setupPackTestAccount(harness);

		const pack = await createPack(harness, account.token, 'sticker', {name: 'My Sticker Pack'});

		expect(pack.id).toBeTruthy();
		expect(pack.name).toBe('My Sticker Pack');
		expect(pack.type).toBe('sticker');
	});

	test('non-premium user cannot create pack emoji', async () => {
		const {account} = await setupPackTestAccount(harness);
		const pack = await createPack(harness, account.token, 'emoji', {name: 'Emoji Pack'});

		await revokePremium(harness, account.userId);

		await createBuilder(harness, account.token)
			.post(`/packs/emojis/${pack.id}`)
			.body({name: 'emoji1', image: 'data:image/png;base64,iVBORw0KGgo='})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});

	test('non-premium user cannot install pack', async () => {
		const {account: owner} = await setupPackTestAccount(harness);
		const pack = await createPack(harness, owner.token, 'emoji', {name: 'Shared Pack'});

		const {account: installer} = await setupNonPremiumPackTestAccount(harness);

		await createBuilder(harness, installer.token)
			.post(`/packs/${pack.id}/install`)
			.body({})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});

	test('premium user can install pack', async () => {
		const {account: owner} = await setupPackTestAccount(harness);
		const pack = await createPack(harness, owner.token, 'emoji', {name: 'Installable Pack'});

		const {account: installer} = await setupPackTestAccount(harness);

		await installPack(harness, installer.token, pack.id);

		const dashboard = await listPacks(harness, installer.token);
		const installed = dashboard.emoji.installed.find((p) => p.id === pack.id);
		expect(installed).toBeTruthy();
	});

	test('user without staff flag cannot access pack endpoints', async () => {
		const account = await createTestAccount(harness);

		await createBuilder(harness, account.token).get('/packs').expect(HTTP_STATUS.FORBIDDEN).execute();

		await createBuilder(harness, account.token)
			.post('/packs/emoji')
			.body({name: 'Unauthorized Pack'})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});

	test('user gains staff flag and can access expression packs', async () => {
		const owner = await createTestAccount(harness);
		await grantStaffAccess(harness, owner.userId);
		await grantPremium(harness, owner.userId);

		const dashboard = await listPacks(harness, owner.token);

		expect(dashboard.emoji).toBeTruthy();
		expect(dashboard.sticker).toBeTruthy();
	});

	test('pack limits show correct values for premium user', async () => {
		const {account} = await setupPackTestAccount(harness);

		const dashboard = await listPacks(harness, account.token);

		expect(dashboard.emoji.created_limit).toBeGreaterThan(0);
		expect(dashboard.emoji.installed_limit).toBeGreaterThan(0);
		expect(dashboard.sticker.created_limit).toBeGreaterThan(0);
		expect(dashboard.sticker.installed_limit).toBeGreaterThan(0);
	});

	test('pack limits show zero for non-premium user with staff flag', async () => {
		const {account} = await setupNonPremiumPackTestAccount(harness);

		const dashboard = await listPacks(harness, account.token);

		expect(dashboard.emoji.created_limit).toBe(0);
		expect(dashboard.emoji.installed_limit).toBe(0);
	});
});
