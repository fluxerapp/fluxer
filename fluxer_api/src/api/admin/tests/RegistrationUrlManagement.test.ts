// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createTestAccount,
	createUniqueEmail,
	createUniqueUsername,
	setUserACLs,
	type TestAccount,
} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelInvite, setupTestGuildWithMembers} from '@app/api/guild/tests/GuildTestUtils';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import type {
	CreateRegistrationUrlResponse,
	InstanceConfigResponse,
} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('admin registration links', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_CONFIG_VIEW,
			AdminACLs.INSTANCE_CONFIG_UPDATE,
		]);
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	function createLink(body: Record<string, unknown>) {
		return createBuilder<CreateRegistrationUrlResponse>(harness, admin.token)
			.post('/admin/instance/registration-urls')
			.body({approval_required: false, ...body});
	}

	it('revokes a link and removes it from the list on the second delete', async () => {
		const {registration_url: link} = await createLink({}).execute();
		const revoked = await createBuilder<InstanceConfigResponse>(harness, admin.token)
			.delete(`/admin/instance/registration-urls/${link.id}`)
			.execute();
		expect(revoked.registration.urls.find((url) => url.id === link.id)?.revoked_at).not.toBeNull();
		const removed = await createBuilder<InstanceConfigResponse>(harness, admin.token)
			.delete(`/admin/instance/registration-urls/${link.id}`)
			.execute();
		expect(removed.registration.urls.some((url) => url.id === link.id)).toBe(false);
	});

	it('joins the community of the attached invite after sign-up', async () => {
		const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
		const invite = await createChannelInvite(harness, owner.token, guild.system_channel_id!);
		await getInstanceConfigRepository().setRegistrationConfig({mode: 'closed', admin_registration_urls_enabled: true});
		const {code, registration_url: link} = await createLink({invite_code: invite.code}).execute();
		expect(link.invite_code).toBe(invite.code);
		const registered = await createBuilderWithoutAuth<{user_id: string}>(harness)
			.post('/auth/register')
			.body({
				email: createUniqueEmail('linkinvite'),
				username: createUniqueUsername('linkinvite'),
				global_name: 'Invited by link',
				password: 'joined-through-a-link',
				date_of_birth: '2000-01-01',
				consent: true,
				registration_url_code: code,
			})
			.execute();
		await createBuilder(harness, owner.token)
			.get(`/guilds/${guild.id}/members/${registered.user_id}`)
			.expect(HTTP_STATUS.OK)
			.execute();
	});
});
