// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditCoverageCase} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';
import {
	createUniqueEmail,
	createUniqueUsername,
	registerUser,
	type TestAccount,
} from '@app/api/auth/tests/AuthTestUtils';
import {getPngDataUrl} from '@app/api/emoji/tests/EmojiTestUtils';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS, TEST_IDS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import type {CreateRegistrationUrlResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';

async function createRegistrationUrl(harness: ApiTestHarness, admin: TestAccount): Promise<string> {
	const created = await createBuilder<CreateRegistrationUrlResponse>(harness, admin.token)
		.post('/admin/instance/registration-urls')
		.body({approval_required: false})
		.expect(HTTP_STATUS.OK)
		.execute();
	return created.registration_url.id;
}

export const InstanceConfigAdminAuditCases: ReadonlyArray<AdminAuditCoverageCase> = [
	{
		method: 'GET',
		route: '/admin/instance/config',
		name: 'admin acl',
		async prepare({harness, admin}) {
			await createRegistrationUrl(harness, admin);
			return {
				request: {path: '/admin/instance/config'},
				expected: {
					action: 'get_instance_config',
					targetType: 'instance_config',
					targetId: '0',
					metadata: {registration_url_count: '1', pending_registration_count: '0'},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/instance/config',
		name: 'setup session',
		async prepare() {
			await getInstanceConfigRepository().setAppPublicConfig({setup: {configured: false}});
			return {
				request: {path: '/admin/instance/config'},
				expected: {
					action: 'get_instance_config',
					targetType: 'instance_config',
					targetId: '0',
					metadata: {registration_url_count: '0', pending_registration_count: '0'},
				},
			};
		},
	},
	{
		method: 'PATCH',
		route: '/admin/instance/config',
		async prepare() {
			return {
				request: {
					path: '/admin/instance/config',
					body: {
						registration: {mode: 'approval'},
						experiment_delivery: {poll_interval_seconds: 600},
						sso: null,
					},
				},
				expected: {
					action: 'update_instance_config',
					targetType: 'instance_config',
					targetId: '0',
					metadata: {sections: 'experiment_delivery,registration'},
				},
			};
		},
	},
	{
		method: 'POST',
		route: '/admin/instance/config/branding-assets',
		async prepare() {
			return {
				request: {path: '/admin/instance/config/branding-assets', body: {kind: 'icon', image: getPngDataUrl()}},
				expected: {
					action: 'upload_branding_asset',
					targetType: 'instance_config',
					targetId: '0',
					metadata: {kind: 'icon', cleared: 'false'},
				},
			};
		},
	},
	{
		method: 'POST',
		route: '/admin/instance/config/smtp-tests',
		async prepare() {
			return {
				request: {
					path: '/admin/instance/config/smtp-tests',
					body: {host: '127.0.0.1', port: 1, username: 'coverage', password: 'coverage-password', secure: false},
				},
				expected: {
					action: 'test_smtp_connection',
					targetType: 'instance_config',
					targetId: '0',
					metadata: {port: '1', secure: 'false', ok: 'false'},
				},
			};
		},
	},
	{
		method: 'POST',
		route: '/admin/instance/registration-urls',
		async prepare() {
			return {
				request: {
					path: '/admin/instance/registration-urls',
					body: {label: 'Coverage invite', max_uses: 5, approval_required: true},
				},
				expected: {
					action: 'create_registration_url',
					targetType: 'registration_url',
					targetId: '0',
					metadata: {approval_required: 'true', max_uses: '5'},
				},
			};
		},
	},
	{
		method: 'DELETE',
		route: '/admin/instance/registration-urls/:registration_url_id',
		async prepare({harness, admin}) {
			const registrationUrlId = await createRegistrationUrl(harness, admin);
			return {
				request: {path: `/admin/instance/registration-urls/${registrationUrlId}`},
				expected: {
					action: 'revoke_registration_url',
					targetType: 'registration_url',
					targetId: '0',
					metadata: {},
				},
			};
		},
	},
	{
		method: 'PATCH',
		route: '/admin/instance/pending-registrations/:user_id',
		async prepare({harness}) {
			await getInstanceConfigRepository().setRegistrationConfig({mode: 'approval'});
			const pending = await registerUser(harness, {
				email: createUniqueEmail('coverage-pending'),
				username: createUniqueUsername('coveragepending'),
				global_name: 'Coverage Pending',
				password: 'coverage-pending-password',
				date_of_birth: '2000-01-01',
				consent: true,
			});
			return {
				request: {path: `/admin/instance/pending-registrations/${pending.user_id}`, body: {status: 'approved'}},
				expected: {
					action: 'approve_registration',
					targetType: 'user',
					targetId: pending.user_id,
					metadata: {},
				},
			};
		},
	},
	{
		method: 'PATCH',
		route: '/admin/instance/pending-registrations/:user_id',
		name: 'missing account',
		async prepare() {
			await getInstanceConfigRepository().setRegistrationConfig({mode: 'approval'});
			return {
				request: {
					path: `/admin/instance/pending-registrations/${TEST_IDS.NONEXISTENT_USER}`,
					body: {status: 'rejected'},
				},
				expected: {
					action: 'reject_registration',
					targetType: 'user',
					targetId: TEST_IDS.NONEXISTENT_USER,
					metadata: {account_found: 'false'},
				},
			};
		},
	},
];
