// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditCoverageCase} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';
import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuild} from '@app/api/channel/tests/ChannelTestUtils';
import {createOAuth2Application, createUniqueApplicationName} from '@app/api/oauth/tests/OAuth2TestUtils';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';

const MISSING_APPLICATION_ID = '999999999999999999';

export const ApplicationAdminAuditCases: ReadonlyArray<AdminAuditCoverageCase> = [
	{
		method: 'GET',
		route: '/admin/applications',
		name: 'owner_id',
		async prepare({harness}) {
			const owner = await createTestAccount(harness);
			await createOAuth2Application(harness, owner.token, {name: createUniqueApplicationName('Audit Owned App')});
			return {
				request: {path: `/admin/applications?owner_id=${owner.userId}`},
				expected: {
					action: 'list_user_applications',
					targetType: 'user',
					targetId: owner.userId,
					metadata: {application_count: '1'},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/applications',
		name: 'guild_id',
		async prepare({harness}) {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Audit Application Guild');
			const app = await createOAuth2Application(harness, owner.token, {
				name: createUniqueApplicationName('Audit Guild App'),
				bot_public: true,
			});
			await createBuilder(harness, owner.token)
				.post('/oauth2/authorize/consent')
				.body({
					client_id: app.application.id,
					scope: 'bot',
					guild_id: guild.id,
					permissions: Permissions.SEND_MESSAGES.toString(),
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			return {
				request: {path: `/admin/applications?guild_id=${guild.id}`},
				expected: {
					action: 'list_guild_applications',
					targetType: 'guild',
					targetId: guild.id,
					metadata: {application_count: '1'},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/users/:user_id/applications',
		async prepare({harness}) {
			const owner = await createTestAccount(harness);
			await createOAuth2Application(harness, owner.token, {name: createUniqueApplicationName('Audit First App')});
			await createOAuth2Application(harness, owner.token, {name: createUniqueApplicationName('Audit Second App')});
			return {
				request: {path: `/admin/users/${owner.userId}/applications`},
				expected: {
					action: 'list_user_applications',
					targetType: 'user',
					targetId: owner.userId,
					metadata: {application_count: '2'},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/applications/:application_id',
		name: 'found',
		async prepare({harness}) {
			const owner = await createTestAccount(harness);
			const app = await createOAuth2Application(harness, owner.token, {
				name: createUniqueApplicationName('Audit Lookup App'),
			});
			return {
				request: {path: `/admin/applications/${app.application.id}`},
				expected: {
					action: 'get_application',
					targetType: 'application',
					targetId: app.application.id,
					metadata: {
						found: 'true',
						owner_user_id: owner.userId,
						bot_user_id: app.botUserId,
					},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/applications/:application_id',
		name: 'missing',
		async prepare() {
			return {
				request: {path: `/admin/applications/${MISSING_APPLICATION_ID}`},
				expected: {
					action: 'get_application',
					targetType: 'application',
					targetId: MISSING_APPLICATION_ID,
					metadata: {found: 'false'},
				},
			};
		},
	},
	{
		method: 'PATCH',
		route: '/admin/applications/:application_id',
		async prepare({harness}) {
			const owner = await createTestAccount(harness);
			const newOwner = await createTestAccount(harness);
			const app = await createOAuth2Application(harness, owner.token, {
				name: createUniqueApplicationName('Audit Transferred App'),
			});
			return {
				request: {
					path: `/admin/applications/${app.application.id}`,
					body: {new_owner_id: newOwner.userId},
				},
				expected: {
					action: 'transfer_ownership',
					targetType: 'application',
					targetId: app.application.id,
					metadata: {
						old_owner_id: owner.userId,
						new_owner_id: newOwner.userId,
					},
				},
			};
		},
	},
];
