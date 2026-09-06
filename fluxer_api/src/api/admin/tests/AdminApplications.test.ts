// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {ADMIN_OAUTH2_APPLICATION_ID} from '@fluxer/constants/src/Core';
import type {
	ApplicationUpdateResponse,
	ListApplicationsResponse,
	LookupApplicationResponse,
} from '@fluxer/schema/src/domains/admin/AdminApplicationSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount, setUserACLs} from '../../auth/tests/AuthTestUtils';
import {createOAuth2Application, createUniqueApplicationName} from '../../oauth/tests/OAuth2TestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';

interface AuditLogsResponse {
	logs: Array<{
		target_type: string;
		target_id: string;
		action: string;
	}>;
}

describe('Admin applications', () => {
	let harness: ApiTestHarness;

	beforeEach(async () => {
		harness = await createApiTestHarness();
	});

	afterEach(async () => {
		await harness.shutdown();
	});

	test('gets an application by id', async () => {
		const owner = await createTestAccount(harness);
		const app = await createOAuth2Application(harness, owner.token, {
			name: createUniqueApplicationName('Lookup App'),
			redirect_uris: ['https://example.test/callback'],
		});
		await setUserACLs(harness, owner, [AdminACLs.AUTHENTICATE, AdminACLs.APPLICATION_LOOKUP]);

		const response = await createBuilder<LookupApplicationResponse>(harness, `${owner.token}`)
			.get(`/admin/applications/${app.application.id}`)
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(response.application).toMatchObject({
			id: app.application.id,
			owner_user_id: owner.userId,
		});
	});

	test('lists applications owned by a user', async () => {
		const owner = await createTestAccount(harness);
		const app = await createOAuth2Application(harness, owner.token, {
			name: createUniqueApplicationName('Owned App'),
			redirect_uris: ['https://example.test/callback'],
		});
		await setUserACLs(harness, owner, [AdminACLs.AUTHENTICATE, AdminACLs.APPLICATION_LIST_BY_OWNER]);

		const response = await createBuilder<ListApplicationsResponse>(harness, `${owner.token}`)
			.get(`/admin/applications?owner_id=${owner.userId}`)
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(response.applications).toHaveLength(1);
		expect(response.applications[0]).toMatchObject({id: app.application.id});
	});

	test('rejects a list by owner from a lookup-only admin', async () => {
		const owner = await createTestAccount(harness);
		await createOAuth2Application(harness, owner.token, {
			name: createUniqueApplicationName('Owned App'),
			redirect_uris: ['https://example.test/callback'],
		});
		await setUserACLs(harness, owner, [AdminACLs.AUTHENTICATE, AdminACLs.APPLICATION_LOOKUP]);

		await createBuilder(harness, `${owner.token}`)
			.get(`/admin/applications?owner_id=${owner.userId}`)
			.expect(HTTP_STATUS.FORBIDDEN)
			.executeWithResponse();
	});

	test('rejects a list without owner_id or guild_id', async () => {
		const owner = await createTestAccount(harness);
		await setUserACLs(harness, owner, [AdminACLs.AUTHENTICATE, AdminACLs.APPLICATION_LOOKUP]);

		await createBuilder(harness, `${owner.token}`)
			.get('/admin/applications')
			.expect(HTTP_STATUS.BAD_REQUEST)
			.executeWithResponse();
	});

	test('transfers application ownership', async () => {
		const owner = await createTestAccount(harness);
		const newOwner = await createTestAccount(harness);
		const app = await createOAuth2Application(harness, owner.token, {
			name: createUniqueApplicationName('Transferred App'),
			redirect_uris: ['https://example.test/callback'],
		});
		await setUserACLs(harness, owner, [AdminACLs.AUTHENTICATE, AdminACLs.APPLICATION_TRANSFER_OWNERSHIP]);

		const response = await createBuilder<ApplicationUpdateResponse>(harness, `${owner.token}`)
			.patch(`/admin/applications/${app.application.id}`)
			.body({new_owner_id: newOwner.userId})
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(response.application.owner_user_id).toBe(newOwner.userId);
	});

	test('refuses to transfer the built-in admin application', async () => {
		const owner = await createTestAccount(harness);
		const newOwner = await createTestAccount(harness);
		await setUserACLs(harness, owner, [
			AdminACLs.AUTHENTICATE,
			AdminACLs.APPLICATION_TRANSFER_OWNERSHIP,
			AdminACLs.AUDIT_LOG_VIEW,
		]);

		await createBuilder(harness, `${owner.token}`)
			.patch(`/admin/applications/${ADMIN_OAUTH2_APPLICATION_ID}`)
			.body({new_owner_id: newOwner.userId})
			.expect(HTTP_STATUS.FORBIDDEN, 'FORBIDDEN')
			.execute();

		const auditLogs = await createBuilder<AuditLogsResponse>(harness, `${owner.token}`)
			.get(`/admin/audit-logs?target_type=application&target_id=${ADMIN_OAUTH2_APPLICATION_ID}`)
			.execute();

		expect(auditLogs.logs).toEqual([]);
	});

	test('transfer requires the application update permission', async () => {
		const owner = await createTestAccount(harness);
		const newOwner = await createTestAccount(harness);
		const app = await createOAuth2Application(harness, owner.token, {
			name: createUniqueApplicationName('Guarded App'),
			redirect_uris: ['https://example.test/callback'],
		});
		await setUserACLs(harness, owner, [AdminACLs.AUTHENTICATE, AdminACLs.APPLICATION_LOOKUP]);

		await createBuilder(harness, `${owner.token}`)
			.patch(`/admin/applications/${app.application.id}`)
			.body({new_owner_id: newOwner.userId})
			.expect(HTTP_STATUS.FORBIDDEN)
			.executeWithResponse();
	});
});
