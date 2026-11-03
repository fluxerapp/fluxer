// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {setCassandraQueryExecutorForTesting} from '@app/api/database/CassandraQueryExecution';
import {InstanceConfigRepository} from '@app/api/instance/InstanceConfigRepository';
import {getCachedInstancePremiumMode, setCachedInstancePremiumMode} from '@app/api/limits/InstancePremiumModeCache';
import {
	getEffectiveBillingConfig,
	getStoredBillingConfig,
	setStoredBillingConfig,
} from '@app/api/stripe/BillingConfigCache';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {MockKVProvider} from '@pkgs/kv_client/src/testing/MockKVProvider';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

const INSTANCE_BILLING_CONFIG_KEY = 'instance_billing_config';

const GBP_PRICES = {
	monthly: 'price_GbpMonthly',
	yearly: 'price_GbpYearly',
	gift_1_month: 'price_GbpGiftMonth',
	gift_1_year: 'price_GbpGiftYear',
};

describe('InstanceConfigRepository billing and premium branding', () => {
	const repositories: Array<InstanceConfigRepository> = [];
	const originalSelfHosted = Config.instance.selfHosted;
	let originalStored = getStoredBillingConfig();
	let originalPremiumMode = getCachedInstancePremiumMode();
	let executor: InMemoryCassandraQueryExecutor;

	beforeEach(() => {
		originalStored = getStoredBillingConfig();
		originalPremiumMode = getCachedInstancePremiumMode();
		executor = new InMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		Config.instance.selfHosted = true;
	});

	afterEach(() => {
		for (const repository of repositories) {
			repository.shutdown();
		}
		repositories.length = 0;
		Config.instance.selfHosted = originalSelfHosted;
		setCachedInstancePremiumMode(originalPremiumMode);
		setStoredBillingConfig(originalStored);
	});

	function createRepository(kvProvider = new MockKVProvider()): InstanceConfigRepository {
		const repository = new InstanceConfigRepository(kvProvider);
		repositories.push(repository);
		return repository;
	}

	async function readRaw(repository: InstanceConfigRepository, key: string): Promise<unknown> {
		const raw = await repository.getConfig(key);
		return raw === null ? null : JSON.parse(raw);
	}

	it('merges patches, keeps secrets on undefined, clears them on null and replaces the catalog maps', async () => {
		const repository = createRepository();
		await repository.setInstanceBillingConfig({
			enabled: true,
			stripe_secret_key: 'sk_test_operator',
			stripe_webhook_secret: 'whsec_operator',
			default_currency: 'GBP',
			prices: {GBP: GBP_PRICES, CHF: {monthly: 'price_ChfMonthly', yearly: 'price_ChfYearly'}},
			country_currencies: {CH: 'CHF'},
			legacy_prices: {monthly_GBP: ['price_GbpOld']},
		});

		const merged = await repository.setInstanceBillingConfig({
			default_currency: 'CHF',
			prices: {GBP: GBP_PRICES},
			country_currencies: {LI: 'CHF'},
		});
		expect(merged).toEqual({
			enabled: true,
			stripe_secret_key: 'sk_test_operator',
			stripe_webhook_secret: 'whsec_operator',
			automatic_tax: null,
			tax_id_collection: null,
			terms_consent_required: null,
			billing_emails: null,
			default_currency: 'CHF',
			prices: {GBP: GBP_PRICES},
			country_currencies: {LI: 'CHF'},
			legacy_prices: {monthly_GBP: ['price_GbpOld']},
		});
		expect(getStoredBillingConfig()).toEqual(merged);
		expect(getEffectiveBillingConfig().catalogMode).toBe('operator');

		const cleared = await repository.setInstanceBillingConfig({
			stripe_webhook_secret: null,
			legacy_prices: null,
		});
		expect(cleared.stripe_secret_key).toBe('sk_test_operator');
		expect(cleared.stripe_webhook_secret).toBeNull();
		expect(cleared.legacy_prices).toBeNull();
		expect(await readRaw(repository, INSTANCE_BILLING_CONFIG_KEY)).toEqual(cleared);
	});

	it('reports env-sourced secrets as set but not stored', async () => {
		const repository = createRepository();
		const admin = await repository.getInstanceBillingAdminConfig();
		expect(admin.stripe_secret_key_set).toBe(Boolean(Config.stripe.secretKey));
		expect(admin.stripe_webhook_secret_set).toBe(Boolean(Config.stripe.webhookSecret));
		expect(admin.stripe_secret_key_stored).toBe(false);
		expect(admin.stripe_webhook_secret_stored).toBe(false);
		await repository.setInstanceBillingConfig({stripe_webhook_secret: 'whsec_stored_only'});
		const stored = await repository.getInstanceBillingAdminConfig();
		expect(stored.stripe_secret_key_stored).toBe(false);
		expect(stored.stripe_webhook_secret_stored).toBe(true);
	});

	it('ignores a stored billing config on hosted', async () => {
		Config.instance.selfHosted = false;
		const repository = createRepository();
		await repository.setInstanceBillingConfig({
			enabled: false,
			stripe_secret_key: 'sk_test_stored',
			prices: {GBP: GBP_PRICES},
			automatic_tax: false,
			tax_id_collection: false,
			terms_consent_required: false,
			billing_emails: false,
		});
		const effective = getEffectiveBillingConfig();
		expect(effective.catalogMode).toBe('env');
		expect(effective.enabled).toBe(Config.stripe.enabled);
		expect(effective.secretKey).toBe(Config.stripe.secretKey || null);
		const admin = await repository.getInstanceBillingAdminConfig();
		expect(admin.catalog_mode).toBe('env');
		expect(admin.effective_automatic_tax).toBe(true);
		expect(admin.effective_tax_id_collection).toBe(true);
		expect(admin.effective_terms_consent_required).toBe(true);
		expect(admin.effective_billing_emails).toBe(true);
	});
});
