// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import {createTestAccount, setUserACLs} from '../../auth/tests/AuthTestUtils';
import {upsertOne} from '../../database/CassandraQueryExecution';
import {BannedIps} from '../../Tables';
import type {ApiTestHarness} from '../../test/ApiTestHarness';
import {createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';
import {AdminRepository} from '../AdminRepository';

describe('AdminRepository IP ban canonicalization', () => {
	let harness: ApiTestHarness;
	let repository: AdminRepository;

	beforeAll(async () => {
		harness = await createApiTestHarness();
		repository = new AdminRepository();
	});

	beforeEach(async () => {
		await harness.reset();
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	async function storedIps(): Promise<Array<string>> {
		return Array.from(await repository.loadAllBannedIps()).sort();
	}

	async function insertLegacyBannedIp(ip: string): Promise<void> {
		await upsertOne(
			BannedIps.insert({
				ip,
				ban_kind: 'permanent',
				reason: 'legacy_test_row',
				expires_at: null,
				created_at: new Date(),
			}),
		);
	}

	it('stores new IPv6 bans under the canonical key', async () => {
		await repository.banIp('[2001:DB8::1]');

		expect(await storedIps()).toEqual(['2001:0db8:0000:0000:0000:0000:0000:0001']);
	});

	it('stores new IPv4-mapped IPv6 bans using the IPv4 canonical key', async () => {
		await repository.banIp('::ffff:192.0.2.1');

		expect(await storedIps()).toEqual(['192.0.2.1']);
	});

	it('stores new CIDR bans under their normalized network key', async () => {
		await repository.banIp('192.168.1.100/24');

		expect(await storedIps()).toEqual(['192.168.1.0/24']);
	});

	it('removes legacy non-canonical rows when unbanning by canonical equivalent', async () => {
		await insertLegacyBannedIp('[2001:DB8::1]');
		await insertLegacyBannedIp('2001:0db8:0000:0000:0000:0000:0000:0001');

		await repository.unbanIp('2001:db8::1');

		expect(await storedIps()).toEqual([]);
	});

	it('still removes a legacy row when unbanning by its exact stored spelling', async () => {
		await insertLegacyBannedIp('[2001:DB8::1]');

		await repository.unbanIp('[2001:DB8::1]');

		expect(await storedIps()).toEqual([]);
	});
});

describe('Admin blocklist entry body parsing', () => {
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

	it('rejects a blocklist entry body that is not valid JSON', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.BAN_IP_ADD]);
		const {json} = await createBuilder<{
			code: string;
			errors: Array<{
				path: string;
				code: string;
			}>;
		}>(harness, `${admin.token}`)
			.post('/admin/blocklists/ip/entries')
			.body('{not json')
			.expect(HTTP_STATUS.BAD_REQUEST)
			.executeWithResponse();
		expect(json.code).toBe(APIErrorCodes.INVALID_FORM_BODY);
		const bodyError = json.errors.find((entry) => entry.path === 'body');
		expect(bodyError).toBeDefined();
		expect(bodyError?.code).toBe(ValidationErrorCodes.INVALID_FORMAT);
	});
});
