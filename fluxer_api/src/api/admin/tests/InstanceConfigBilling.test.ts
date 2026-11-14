// SPDX-License-Identifier: AGPL-3.0-or-later

import type {TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {getConfig} from '@app/api/Config';
import {executeQuery} from '@app/api/database/CassandraQueryExecution';
import {getCachedInstancePremiumMode, setCachedInstancePremiumMode} from '@app/api/limits/InstancePremiumModeCache';
import {getAdminRepository, getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import {getStoredBillingConfig, setStoredBillingConfig} from '@app/api/stripe/BillingConfigCache';
import {InstanceConfiguration} from '@app/api/Tables';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {InstanceConfigResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const OPERATOR_SECRET_KEY = 'sk_test_operator_secret_value';
const OPERATOR_WEBHOOK_SECRET = 'whsec_operator_secret_value';

interface GlobalState {
	selfHosted: boolean;
	premiumMode: ReturnType<typeof getCachedInstancePremiumMode>;
	storedBilling: ReturnType<typeof getStoredBillingConfig>;
}

function captureGlobalState(): GlobalState {
	return {
		selfHosted: getConfig().instance.selfHosted,
		premiumMode: getCachedInstancePremiumMode(),
		storedBilling: getStoredBillingConfig(),
	};
}

function restoreGlobalState(state: GlobalState): void {
	getConfig().instance.selfHosted = state.selfHosted;
	setCachedInstancePremiumMode(state.premiumMode);
	setStoredBillingConfig(state.storedBilling);
}

function useInstanceHarness({selfHosted}: {selfHosted: boolean}) {
	let harness: ApiTestHarness;
	let original: GlobalState;
	beforeAll(async () => {
		original = captureGlobalState();
		getConfig().instance.selfHosted = selfHosted;
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		setStoredBillingConfig(null);
		setCachedInstancePremiumMode('everyone');
	});
	afterAll(async () => {
		await harness.shutdown();
		restoreGlobalState(original);
	});
	return {
		get harness(): ApiTestHarness {
			return harness;
		},
	};
}

async function createAdmin(harness: ApiTestHarness): Promise<TestAccount> {
	return await setUserACLs(harness, await createTestAccount(harness), [
		AdminACLs.AUTHENTICATE,
		AdminACLs.INSTANCE_CONFIG_VIEW,
		AdminACLs.INSTANCE_CONFIG_UPDATE,
	]);
}

function patchConfig(harness: ApiTestHarness, admin: TestAccount, body: Record<string, unknown>) {
	return createBuilder<InstanceConfigResponse>(harness, admin.token).patch('/admin/instance/config').body(body);
}

const OPERATOR_BILLING = {
	enabled: true,
	stripe_secret_key: OPERATOR_SECRET_KEY,
	stripe_webhook_secret: OPERATOR_WEBHOOK_SECRET,
	default_currency: 'GBP',
	prices: {
		GBP: {
			monthly: 'price_monthlygbp',
			yearly: 'price_yearlygbp',
			gift_1_month: 'price_gift1monthgbp',
			gift_1_year: 'price_gift1yeargbp',
		},
	},
	country_currencies: {GB: 'GBP'},
	legacy_prices: {monthly_GBP: ['price_oldmonthlygbp']},
};

describe('instance config billing on a self-hosted instance', () => {
	const context = useInstanceHarness({selfHosted: true});

	it('rejects enabling billing while the premium mode is everyone', async () => {
		const admin = await createAdmin(context.harness);
		await patchConfig(context.harness, admin, {billing: {enabled: true}})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_FORM_BODY)
			.execute();
		await patchConfig(context.harness, admin, {billing: {enabled: true}, policy: {premium_mode: 'everyone'}})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_FORM_BODY)
			.execute();
		const current = await createBuilder<InstanceConfigResponse>(context.harness, admin.token)
			.get('/admin/instance/config')
			.execute();
		expect(current.billing.enabled).toBeNull();
		expect(current.policy.premium_mode).toBe('everyone');
	});

	it('saves the environment fallback in everyone mode even when the environment enables billing', async () => {
		const admin = await createAdmin(context.harness);
		const originalEnabled = getConfig().stripe.enabled;
		getConfig().stripe.enabled = true;
		try {
			const updated = await patchConfig(context.harness, admin, {
				app_public: {branding: {premium_product_name: 'Gold'}},
				billing: {enabled: null},
			}).execute();
			expect(updated.billing.enabled).toBeNull();
			expect(updated.billing.billing_active).toBe(false);
			expect(updated.app_public.branding.premium_product_name).toBe('Gold');
			await patchConfig(context.harness, admin, {policy: {premium_mode: 'everyone'}}).execute();
		} finally {
			getConfig().stripe.enabled = originalEnabled;
		}
	});

	it('enables billing and mirror mode from one request and redacts the secrets', async () => {
		const admin = await createAdmin(context.harness);
		const updated = await patchConfig(context.harness, admin, {
			billing: OPERATOR_BILLING,
			policy: {premium_mode: 'mirror'},
		}).execute();
		expect(updated.policy.premium_mode).toBe('mirror');
		expect(updated.billing).toMatchObject({
			enabled: true,
			effective_enabled: true,
			stripe_secret_key_set: true,
			stripe_webhook_secret_set: true,
			default_currency: 'GBP',
			prices: OPERATOR_BILLING.prices,
			country_currencies: {GB: 'GBP'},
			legacy_prices: {monthly_GBP: ['price_oldmonthlygbp']},
			billing_active: true,
			catalog_mode: 'operator',
		});
		expect(updated.billing.webhook_url).toMatch(/\/stripe\/webhook$/);
		const {text} = await createBuilder(context.harness, admin.token).get('/admin/instance/config').executeRaw();
		expect(text).not.toContain(OPERATOR_SECRET_KEY);
		expect(text).not.toContain(OPERATOR_WEBHOOK_SECRET);
		expect(JSON.parse(text).billing).toMatchObject({stripe_secret_key_set: true, billing_active: true});
	});

	it('rejects switching the premium mode to everyone while billing is enabled', async () => {
		const admin = await createAdmin(context.harness);
		await patchConfig(context.harness, admin, {billing: OPERATOR_BILLING, policy: {premium_mode: 'mirror'}}).execute();
		await patchConfig(context.harness, admin, {policy: {premium_mode: 'everyone'}})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_FORM_BODY)
			.execute();
		const unchanged = await createBuilder<InstanceConfigResponse>(context.harness, admin.token)
			.get('/admin/instance/config')
			.execute();
		expect(unchanged.policy.premium_mode).toBe('mirror');
		const switched = await patchConfig(context.harness, admin, {
			billing: {enabled: false},
			policy: {premium_mode: 'everyone'},
		}).execute();
		expect(switched.policy.premium_mode).toBe('everyone');
		expect(switched.billing).toMatchObject({enabled: false, effective_enabled: false, billing_active: false});
	});

	it('clears secrets and the operator catalog with null', async () => {
		const admin = await createAdmin(context.harness);
		await patchConfig(context.harness, admin, {billing: OPERATOR_BILLING, policy: {premium_mode: 'mirror'}}).execute();
		const kept = await patchConfig(context.harness, admin, {billing: {default_currency: 'GBP'}}).execute();
		expect(kept.billing.prices).toEqual(OPERATOR_BILLING.prices);
		const cleared = await patchConfig(context.harness, admin, {
			billing: {enabled: true, stripe_webhook_secret: null, prices: null, legacy_prices: null},
		}).execute();
		expect(cleared.billing).toMatchObject({
			enabled: true,
			prices: null,
			legacy_prices: null,
			catalog_mode: 'env',
			stripe_secret_key_set: true,
		});
	});

	it('rejects malformed billing input', async () => {
		const admin = await createAdmin(context.harness);
		for (const billing of [
			{prices: {gbp: {monthly: 'price_x'}}},
			{prices: {GBP: {monthly: 'not_a_price'}}},
			{country_currencies: {GBR: 'GBP'}},
			{legacy_prices: {weekly_GBP: ['price_x']}},
			{default_currency: 'pounds'},
		]) {
			await patchConfig(context.harness, admin, {billing})
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_FORM_BODY)
				.execute();
		}
	});

	it('applies billing sections that only hold nulls', async () => {
		const admin = await createAdmin(context.harness);
		await patchConfig(context.harness, admin, {billing: OPERATOR_BILLING, policy: {premium_mode: 'mirror'}}).execute();
		const envCatalog = await patchConfig(context.harness, admin, {billing: {prices: null}}).execute();
		expect(envCatalog.billing).toMatchObject({prices: null, catalog_mode: 'env', enabled: true});
		const followsEnv = await patchConfig(context.harness, admin, {billing: {enabled: null}}).execute();
		expect(followsEnv.billing).toMatchObject({enabled: null, effective_enabled: getConfig().stripe.enabled});
	});

	it('applies the policy before billing so a failed policy change stores no billing', async () => {
		const admin = await createAdmin(context.harness);
		await patchConfig(context.harness, admin, {app_public: {setup: {configured: true}}}).execute();
		await patchConfig(context.harness, admin, {
			billing: {enabled: true},
			policy: {premium_mode: 'mirror', single_community_enabled: true},
		})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
		const current = await createBuilder<InstanceConfigResponse>(context.harness, admin.token)
			.get('/admin/instance/config')
			.execute();
		expect(current.policy.premium_mode).toBe('everyone');
		expect(current.billing.enabled).toBeNull();
	});

	it('re-checks the stored premium mode right before the billing write', async () => {
		const admin = await createAdmin(context.harness);
		await patchConfig(context.harness, admin, {policy: {premium_mode: 'mirror'}}).execute();
		const repository = getInstanceConfigRepository();
		await executeQuery(
			InstanceConfiguration.upsertAll({
				key: 'instance_policy_config',
				value: JSON.stringify({premium_mode: 'everyone'}),
				updated_at: new Date(),
			}),
		);
		expect((await repository.getInstancePolicyConfig()).premium_mode).toBe('mirror');
		expect((await repository.readStoredInstancePolicyConfig()).premium_mode).toBe('everyone');
		await patchConfig(context.harness, admin, {billing: {enabled: true}})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_FORM_BODY)
			.execute();
		const current = await createBuilder<InstanceConfigResponse>(context.harness, admin.token)
			.get('/admin/instance/config')
			.execute();
		expect(current.billing.enabled).toBeNull();
	});

	it('re-checks the stored billing right before the premium mode write', async () => {
		const admin = await createAdmin(context.harness);
		await patchConfig(context.harness, admin, {billing: OPERATOR_BILLING, policy: {premium_mode: 'mirror'}}).execute();
		const cached = getStoredBillingConfig();
		expect(cached?.enabled).toBe(true);
		setStoredBillingConfig(cached === null ? null : {...cached, enabled: false});
		await patchConfig(context.harness, admin, {policy: {premium_mode: 'everyone'}})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_FORM_BODY)
			.execute();
		const current = await createBuilder<InstanceConfigResponse>(context.harness, admin.token)
			.get('/admin/instance/config')
			.execute();
		expect(current.policy.premium_mode).toBe('mirror');
		expect(current.billing.enabled).toBe(true);
	});

	it('records the billing section in the audit log without secrets', async () => {
		const admin = await createAdmin(context.harness);
		await patchConfig(context.harness, admin, {billing: OPERATOR_BILLING, policy: {premium_mode: 'mirror'}}).execute();
		const logs = await getAdminRepository().listAllAuditLogsPaginated(100000);
		const update = logs.find((log) => log.action === 'update_instance_config');
		expect(update?.metadata.get('sections')).toBe('billing,policy');
		const serialized = JSON.stringify(logs.map((log) => [...log.metadata.entries()]));
		expect(serialized).not.toContain(OPERATOR_SECRET_KEY);
		expect(serialized).not.toContain(OPERATOR_WEBHOOK_SECRET);
	});
});

describe('instance config billing on a hosted instance', () => {
	const context = useInstanceHarness({selfHosted: false});

	it('keeps the Plutonium default and env billing', async () => {
		const admin = await createAdmin(context.harness);
		const current = await createBuilder<InstanceConfigResponse>(context.harness, admin.token)
			.get('/admin/instance/config')
			.execute();
		expect(current.app_public.branding.premium_product_name).toBe('Plutonium');
		expect(current.billing).toMatchObject({enabled: null, catalog_mode: 'env', stripe_secret_key_set: true});
		const updated = await patchConfig(context.harness, admin, {policy: {premium_mode: 'everyone'}}).execute();
		expect(updated.policy.premium_mode).toBe('everyone');
	});

	it('rejects the billing section and the premium branding fields', async () => {
		const admin = await createAdmin(context.harness);
		for (const body of [
			{billing: {enabled: true}},
			{billing: {enabled: null}},
			{billing: {prices: null}},
			{billing: OPERATOR_BILLING},
			{app_public: {branding: {premium_product_name: 'Gold'}}},
			{app_public: {branding: {premium_info_url: 'https://example.com/gold'}}},
			{app_public: {branding: {premium_product_name: null, premium_info_url: null}}},
			{app_public: {branding: {product_name: 'Fluxer', premium_product_name: 'Gold'}}},
		]) {
			await patchConfig(context.harness, admin, body)
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_FORM_BODY)
				.execute();
		}
		const current = await createBuilder<InstanceConfigResponse>(context.harness, admin.token)
			.get('/admin/instance/config')
			.execute();
		expect(current.billing).toMatchObject({enabled: null, catalog_mode: 'env', prices: null});
		expect(current.app_public.branding).toMatchObject({premium_product_name: 'Plutonium', premium_info_url: null});
		expect(getStoredBillingConfig()?.prices ?? null).toBeNull();
	});
});
