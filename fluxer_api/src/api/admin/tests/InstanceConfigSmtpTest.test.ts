// SPDX-License-Identifier: AGPL-3.0-or-later

import type {TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import type {
	InstanceConfigResponse,
	InstanceEmailSmtpTestResponse,
} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('instance SMTP settings', () => {
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

	async function patchSmtp(smtp: Record<string, unknown>) {
		const config = await createBuilder<InstanceConfigResponse>(harness, admin.token)
			.patch('/admin/instance/config')
			.body({integrations: {email: {smtp}}})
			.execute();
		return config.integrations.email.smtp;
	}

	async function runSmtpTest(body: Record<string, unknown>) {
		return await createBuilder<InstanceEmailSmtpTestResponse>(harness, admin.token)
			.post('/admin/instance/config/smtp-tests')
			.body(body)
			.execute();
	}

	it.each([{host: 'draft.example.com'}, {username: 'someone-else'}, {host: 'draft.example.com', password: null}])(
		'keeps the saved password away from the draft %j',
		async (draft) => {
			await patchSmtp({host: 'smtp.example.com', port: 465, username: 'mailer', password: 'saved-password'});
			await expect(runSmtpTest(draft)).resolves.toEqual({
				ok: false,
				error: 'Enter the SMTP password to test a different host or username',
			});
		},
	);
});
