// SPDX-License-Identifier: AGPL-3.0-or-later

import type {TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createTestAccount, createUniqueEmail, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {getConfig} from '@app/api/Config';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {EmailDeliveryModes, VerificationBases} from '@fluxer/constants/src/AccountConfigConstants';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import type {InstanceConfigResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface ValidationErrorBody {
	errors: Array<{path: string; code: string}>;
}

describe('instance account settings', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;
	const config = getConfig();
	let originalSelfHosted: boolean;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		originalSelfHosted = config.instance.selfHosted;
		admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_CONFIG_VIEW,
			AdminACLs.INSTANCE_CONFIG_UPDATE,
		]);
	});
	afterEach(() => {
		config.instance.selfHosted = originalSelfHosted;
		getInstanceConfigRepository().clearCacheForTesting();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	function patch(body: Record<string, unknown>) {
		return createBuilder<InstanceConfigResponse>(harness, admin.token).patch('/admin/instance/config').body(body);
	}

	async function expectRejected(body: Record<string, unknown>, path: string, code: string): Promise<void> {
		const {json} = await createBuilder<ValidationErrorBody>(harness, admin.token)
			.patch('/admin/instance/config')
			.body(body)
			.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
			.executeWithResponse();
		expect(json.errors).toEqual([expect.objectContaining({path, code})]);
	}

	it('refuses to turn the captcha off while instant accounts are on', async () => {
		expect((await getInstanceConfigRepository().getAuthConfig()).instant_accounts.enabled).toBe(true);
		await expectRejected(
			{captcha: {provider: 'none'}},
			'captcha.provider',
			ValidationErrorCodes.CAPTCHA_REQUIRED_BY_INSTANT_ACCOUNTS,
		);
		await expectRejected(
			{captcha: {enabled: false}},
			'captcha.enabled',
			ValidationErrorCodes.CAPTCHA_REQUIRED_BY_INSTANT_ACCOUNTS,
		);
		expect((await getInstanceConfigRepository().getCaptchaConfig()).provider).toBe('altcha');
	});

	it('refuses to turn instant accounts on while the captcha is off', async () => {
		await patch({auth: {instant_accounts: {enabled: false}}, captcha: {provider: 'none'}}).execute();
		await expectRejected(
			{auth: {instant_accounts: {enabled: true}}},
			'auth.instant_accounts.enabled',
			ValidationErrorCodes.INSTANT_ACCOUNTS_REQUIRE_CAPTCHA,
		);
	});

	it('refuses a confirmed email as the verification basis while no email can be sent', async () => {
		await patch({integrations: {email: {delivery: EmailDeliveryModes.OFF}}}).execute();
		await expectRejected(
			{accounts: {verification_basis: VerificationBases.CONFIRMED_EMAIL}},
			'accounts.verification_basis',
			ValidationErrorCodes.VERIFICATION_BASIS_REQUIRES_EMAIL_DELIVERY,
		);
	});

	it('refuses to turn email delivery off while accounts need a confirmed email', async () => {
		const repository = getInstanceConfigRepository();
		if ((await repository.getEmailDeliveryState()) !== 'on') {
			await repository.setInstanceIntegrationsConfig({
				email: {
					delivery: EmailDeliveryModes.SMTP,
					from_email: 'noreply@example.com',
					smtp: {host: 'smtp.example.com', port: 587, username: 'user', password: 'secret'},
				},
			});
		}
		await patch({accounts: {verification_basis: VerificationBases.CONFIRMED_EMAIL}}).execute();
		await expectRejected(
			{integrations: {email: {delivery: EmailDeliveryModes.OFF}}},
			'integrations.email.delivery',
			ValidationErrorCodes.VERIFICATION_BASIS_REQUIRES_EMAIL_DELIVERY,
		);
	});

	it('refuses an email-only sign-up that could never sign in again while email delivery is off', async () => {
		await getInstanceConfigRepository().setInstanceIntegrationsConfig({email: {delivery: EmailDeliveryModes.OFF}});
		await createBuilderWithoutAuth(harness)
			.post('/auth/register')
			.body({email: createUniqueEmail('nodelivery'), global_name: 'Nobody', consent: true})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.REGISTRATION_CREDENTIAL_REQUIRED)
			.execute();
		await createBuilderWithoutAuth(harness)
			.post('/auth/register')
			.body({
				email: createUniqueEmail('nodelivery'),
				global_name: 'Somebody',
				password: 'a-strong-password-123',
				consent: true,
			})
			.execute();
	});
});
