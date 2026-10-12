// SPDX-License-Identifier: AGPL-3.0-or-later

import type {TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {getConfig} from '@app/api/Config';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import type {
	InstanceConfigResponse,
	InstanceEmailSmtpTestResponse,
} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import nodemailer, {type Transporter} from 'nodemailer';
import {afterAll, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

describe('instance SMTP settings', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;
	const verify = vi.fn();
	const createTransport = vi.spyOn(nodemailer, 'createTransport');

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		createTransport.mockReset();
		createTransport.mockReturnValue({verify} as unknown as Transporter);
		verify.mockReset();
		verify.mockResolvedValue(true);
		admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_CONFIG_VIEW,
			AdminACLs.INSTANCE_CONFIG_UPDATE,
		]);
	});

	afterAll(async () => {
		await harness.shutdown();
		createTransport.mockRestore();
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

	it('fills a draft for the saved host with the saved password', async () => {
		await patchSmtp({
			host: 'smtp.example.com',
			port: 465,
			username: 'mailer',
			password: 'saved-password',
			tls_mode: 'implicit',
		});
		await expect(runSmtpTest({host: 'SMTP.example.com', port: 2525, tls_mode: null})).resolves.toEqual({
			ok: true,
			error: null,
		});
		expect(createTransport).toHaveBeenCalledWith(
			expect.objectContaining({
				host: 'SMTP.example.com',
				port: 2525,
				secure: false,
				ignoreTLS: true,
				auth: {user: 'mailer', pass: 'saved-password'},
			}),
		);
	});

	it.each([{host: 'draft.example.com'}, {username: 'someone-else'}, {host: 'draft.example.com', password: null}])(
		'keeps the saved password away from the draft %j',
		async (draft) => {
			await patchSmtp({host: 'smtp.example.com', port: 465, username: 'mailer', password: 'saved-password'});
			await expect(runSmtpTest(draft)).resolves.toEqual({
				ok: false,
				error: 'Enter the SMTP password to test a different host or username',
			});
			expect(createTransport).not.toHaveBeenCalled();
		},
	);

	it('picks the TLS mode from the port until one is saved', async () => {
		const original = getConfig().email.smtp;
		getConfig().email.smtp = original ? {...original, tlsMode: null} : original;
		try {
			expect(await patchSmtp({host: 'smtp.example.com', port: 587})).toMatchObject({
				tls_mode: null,
				effective_tls_mode: 'starttls',
			});
			expect(await patchSmtp({port: 465})).toMatchObject({tls_mode: null, effective_tls_mode: 'implicit'});
			expect(await patchSmtp({tls_mode: 'none'})).toMatchObject({tls_mode: 'none', effective_tls_mode: 'none'});
		} finally {
			getConfig().email.smtp = original;
		}
	});

	it('reads the legacy secure flag and lets the mode replace it', async () => {
		expect(await patchSmtp({host: 'smtp.example.com', port: 587, secure: true})).toMatchObject({
			tls_mode: 'implicit',
		});
		expect(await patchSmtp({secure: false})).toMatchObject({tls_mode: 'opportunistic'});
		expect(await patchSmtp({tls_mode: 'starttls'})).toMatchObject({tls_mode: 'starttls'});
	});

	it('clears every saved field so the environment applies again', async () => {
		await patchSmtp({
			host: 'smtp.example.com',
			port: 465,
			username: 'mailer',
			password: 'saved-password',
			tls_mode: 'implicit',
		});
		const environment = getConfig().email.smtp;
		expect(await patchSmtp({host: null, port: null, username: null, password: null, tls_mode: null})).toMatchObject({
			host: environment?.host,
			port: environment?.port,
			username: environment?.username,
			tls_mode: null,
			effective_tls_mode: 'none',
		});
	});
});
