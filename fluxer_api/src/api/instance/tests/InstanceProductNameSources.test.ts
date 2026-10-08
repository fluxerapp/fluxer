// SPDX-License-Identifier: AGPL-3.0-or-later

import {createApplicationID} from '@app/api/BrandedTypes';
import {getConfig} from '@app/api/Config';
import {setCachedProductName} from '@app/api/instance/ProductName';
import {createRuntimeEmailService, getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import {ApplicationRepository, resetAdminSecretHashForTesting} from '@app/api/oauth/repositories/ApplicationRepository';
import {ApnsPushServiceTestHooks} from '@app/api/push/ApnsPushService';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {ADMIN_OAUTH2_APPLICATION_ID} from '@fluxer/constants/src/Core';
import {EmailI18nService} from '@pkgs/email/src/EmailI18nService';
import type {IEmailService} from '@pkgs/email/src/IEmailService';
import {SmtpEmailProvider} from '@pkgs/email/src/SmtpEmailProvider';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

describe('instance product name in server-rendered text', () => {
	let harness: ApiTestHarness;
	const original = {
		productName: getConfig().instance.branding.productName,
		selfHosted: getConfig().instance.selfHosted,
		adminSecret: getConfig().admin.oauthClientSecret,
		emailEnabled: getConfig().email.enabled,
		emailFromName: getConfig().email.fromName,
	};

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		getInstanceConfigRepository().clearCacheForTesting();
		setCachedProductName(null);
		getConfig().email.enabled = false;
	});

	afterEach(() => {
		vi.restoreAllMocks();
		getConfig().instance.branding.productName = original.productName;
		getConfig().instance.selfHosted = original.selfHosted;
		getConfig().admin.oauthClientSecret = original.adminSecret;
		getConfig().email.enabled = original.emailEnabled;
		getConfig().email.fromName = original.emailFromName;
		resetAdminSecretHashForTesting();
		setCachedProductName(null);
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	function createRecordedEmailService(): {service: IEmailService; subjects: Array<string>; names: Array<string>} {
		const subjects: Array<string> = [];
		const names: Array<string> = [];
		const getTemplate = EmailI18nService.prototype.getTemplate;
		vi.spyOn(EmailI18nService.prototype, 'getTemplate').mockImplementation(function (
			this: EmailI18nService,
			key,
			locale,
			variables,
			productName,
		) {
			const result = getTemplate.call(this, key, locale, variables, productName);
			names.push(productName);
			if (result.ok) subjects.push(result.value.subject);
			return result;
		});
		return {service: createRuntimeEmailService({isEmailBounced: async () => false}), subjects, names};
	}

	it('sends hosted emails as Fluxer', async () => {
		getConfig().instance.branding.productName = '';
		const {service, subjects, names} = createRecordedEmailService();
		await expect(service.sendPasswordResetEmail('user@example.com', 'testuser', 'token', 'en-US')).resolves.toBe(true);
		expect(names).toEqual(['Fluxer']);
		expect(subjects).toEqual(['Reset your Fluxer password']);
	});

	it('sends self-hosted emails under the configured name', async () => {
		getConfig().instance.selfHosted = true;
		getConfig().instance.branding.productName = 'Configured Chat';
		const {service, subjects} = createRecordedEmailService();
		await service.sendPasswordResetEmail('user@example.com', 'testuser', 'token', 'en-US');
		await service.sendEmailVerification('user@example.com', 'testuser', 'token', 'de');
		expect(subjects[0]).toBe('Reset your Configured Chat password');
		expect(subjects[1]).toContain('Configured Chat');
		expect(subjects.join('\n')).not.toContain('Fluxer');
	});

	it('picks up a name saved in the dashboard on the next email without a restart', async () => {
		getConfig().instance.selfHosted = true;
		getConfig().instance.branding.productName = 'Configured Chat';
		const {service, subjects} = createRecordedEmailService();
		await service.sendPasswordResetEmail('user@example.com', 'testuser', 'token', 'en-US');
		await getInstanceConfigRepository().setAppPublicConfig({branding: {product_name: 'Renamed Chat'}});
		await service.sendPasswordResetEmail('user@example.com', 'testuser', 'token', 'en-US');
		setCachedProductName(null);
		await service.sendPasswordResetEmail('user@example.com', 'testuser', 'token', 'en-US');
		expect(subjects).toEqual([
			'Reset your Configured Chat password',
			'Reset your Renamed Chat password',
			'Reset your Renamed Chat password',
		]);
	});

	async function sendAndReadSenderName(): Promise<string | undefined> {
		getConfig().email.enabled = true;
		const sendEmail = vi.spyOn(SmtpEmailProvider.prototype, 'sendEmail').mockResolvedValue(true);
		const service = createRuntimeEmailService({isEmailBounced: async () => false});
		await expect(service.sendPasswordResetEmail('user@example.com', 'testuser', 'token', 'en-US')).resolves.toBe(true);
		return sendEmail.mock.calls.at(-1)?.[0].from.name;
	}

	it('sends hosted emails from Fluxer when no sender name is set', async () => {
		getConfig().instance.branding.productName = '';
		getConfig().email.fromName = '';
		await expect(sendAndReadSenderName()).resolves.toBe('Fluxer');
	});

	it('sends self-hosted emails from the product name when no sender name is set', async () => {
		getConfig().instance.selfHosted = true;
		getConfig().instance.branding.productName = 'Configured Chat';
		getConfig().email.fromName = '';
		await expect(sendAndReadSenderName()).resolves.toBe('Configured Chat');
		await getInstanceConfigRepository().setAppPublicConfig({branding: {product_name: 'Renamed Chat'}});
		await expect(sendAndReadSenderName()).resolves.toBe('Renamed Chat');
	});

	it('keeps a configured sender name over the product name', async () => {
		getConfig().instance.selfHosted = true;
		getConfig().instance.branding.productName = 'Configured Chat';
		getConfig().email.fromName = 'Configured Mailer';
		await expect(sendAndReadSenderName()).resolves.toBe('Configured Mailer');
		getConfig().instance.selfHosted = false;
		getConfig().instance.branding.productName = '';
		await expect(sendAndReadSenderName()).resolves.toBe('Configured Mailer');
	});

	it('titles an APNs alert that has no title with the instance name', () => {
		const titleOf = (): unknown => {
			const {aps} = ApnsPushServiceTestHooks.buildApnsPayload({notification: {body: 'Hello'}});
			return (aps as {alert: {title: string}}).alert.title;
		};
		getConfig().instance.branding.productName = '';
		expect(titleOf()).toBe('Fluxer');
		getConfig().instance.branding.productName = 'Configured Chat';
		expect(titleOf()).toBe('Configured Chat');
		setCachedProductName('Renamed Chat');
		expect(titleOf()).toBe('Renamed Chat');
	});

	it('names the built-in admin application after the instance', async () => {
		getConfig().admin.oauthClientSecret = 'product-name-test-secret';
		resetAdminSecretHashForTesting();
		const repository = new ApplicationRepository();
		const nameOf = async (): Promise<string | undefined> =>
			(await repository.getApplication(createApplicationID(ADMIN_OAUTH2_APPLICATION_ID)))?.name;
		getConfig().instance.branding.productName = '';
		await expect(nameOf()).resolves.toBe('Fluxer Admin');
		setCachedProductName('Renamed Chat');
		await expect(nameOf()).resolves.toBe('Renamed Chat Admin');
	});
});
