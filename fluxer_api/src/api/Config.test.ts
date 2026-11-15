// SPDX-License-Identifier: AGPL-3.0-or-later

import {buildAPIConfigFromMaster} from '@app/api/Config';
import {OPERATOR_PASS_EARLY_BIRD_PRICES_MINOR} from '@app/api/operator_pass/OperatorPassPrices';
import {loadConfig, resetConfig} from '@fluxer/config/src/ConfigLoader';
import type {MasterConfig} from '@fluxer/config/src/MasterConfig';
import {afterAll, afterEach, beforeAll, describe, expect, it, test, vi} from 'vitest';

afterEach(() => {
	vi.unstubAllEnvs();
	resetConfig();
});

afterAll(async () => {
	await loadConfig();
});

function withUploadRelaySecret(master: MasterConfig, secretBase64: string): MasterConfig {
	return {
		...master,
		services: {
			...master.services,
			media_proxy: {
				...master.services.media_proxy,
				upload_relay: {
					...master.services.media_proxy.upload_relay,
					secret_base64: secretBase64,
				},
			},
		},
	};
}

function withStripeLegacyPrices(
	master: MasterConfig,
	legacyPrices: Record<string, Array<string> | undefined> | undefined,
): MasterConfig {
	return {
		...master,
		integrations: {
			...master.integrations,
			stripe: {
				...master.integrations.stripe,
				legacy_prices: legacyPrices,
			},
		},
	};
}

describe('buildAPIConfigFromMaster upload relay secret', () => {
	let master: MasterConfig;
	beforeAll(async () => {
		master = await loadConfig();
	});

	it('refuses to build without FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64', () => {
		expect(() => buildAPIConfigFromMaster(withUploadRelaySecret(master, ''))).toThrow(
			/FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64/,
		);
	});

	it('refuses a secret that decodes to fewer than 32 bytes', () => {
		const secret = Buffer.alloc(16, 7).toString('base64');
		expect(() => buildAPIConfigFromMaster(withUploadRelaySecret(master, secret))).toThrow(/at least 32 bytes/);
	});

	it('accepts a secret that decodes to 32 bytes', () => {
		const secret = Buffer.alloc(32, 7).toString('base64');
		expect(
			buildAPIConfigFromMaster(withUploadRelaySecret(master, secret)).mediaProxy.uploadRelay.relaySecretBase64,
		).toBe(secret);
	});
});

describe('buildAPIConfigFromMaster stripe legacy prices', () => {
	let master: MasterConfig;
	beforeAll(async () => {
		master = await loadConfig();
	});

	it('copies the retired stripe price map from master config onto the api config', () => {
		const legacyPrices = {
			monthly_brl: ['price_retired_monthly_brl'],
			yearly_brl: ['price_retired_yearly_brl_a', 'price_retired_yearly_brl_b'],
			monthly_try: ['price_1TMYpdFPC94Os7FdZVRx98Up'],
		};
		expect(buildAPIConfigFromMaster(withStripeLegacyPrices(master, legacyPrices)).stripe.legacyPrices).toEqual(
			legacyPrices,
		);
	});

	it('copies the retired price map even when no live prices are configured', () => {
		const withoutPrices: MasterConfig = {
			...master,
			integrations: {
				...master.integrations,
				stripe: {
					...master.integrations.stripe,
					prices: undefined,
					legacy_prices: {monthly_try: ['price_1TMYpdFPC94Os7FdZVRx98Up']},
				},
			},
		};
		const config = buildAPIConfigFromMaster(withoutPrices);
		expect(config.stripe.prices).toBeUndefined();
		expect(config.stripe.legacyPrices).toEqual({monthly_try: ['price_1TMYpdFPC94Os7FdZVRx98Up']});
	});

	it('leaves the retired price map undefined when master config does not set one', () => {
		expect(buildAPIConfigFromMaster(withStripeLegacyPrices(master, undefined)).stripe.legacyPrices).toBeUndefined();
	});
});

function withSelfHostedInstance(master: MasterConfig): MasterConfig {
	return {
		...master,
		integrations: {
			...master.integrations,
			breached_password_check: {enabled: undefined},
		},
		instance: {
			...master.instance,
			self_hosted: true,
		},
	};
}

describe('buildAPIConfigFromMaster optional outbound lookups', () => {
	let master: MasterConfig;
	beforeAll(async () => {
		master = await loadConfig();
	});

	it('leaves the lookup off on a self-hosted instance', () => {
		const config = buildAPIConfigFromMaster(withSelfHostedInstance(master));
		expect(config.breachedPasswordCheck.enabled).toBe(false);
	});
});

function withAgePolicy(
	master: MasterConfig,
	selfHosted: boolean,
	agePolicy: MasterConfig['instance']['age_policy'],
): MasterConfig {
	return {...master, instance: {...master.instance, self_hosted: selfHosted, age_policy: agePolicy}};
}

describe('buildAPIConfigFromMaster regional age policy', () => {
	let master: MasterConfig;
	beforeAll(async () => {
		master = await loadConfig();
	});

	it('restricts the UK and Brazil on the hosted service by default', () => {
		expect(buildAPIConfigFromMaster(withAgePolicy(master, false, {})).instance.agePolicy).toEqual({
			restricted: [
				{countryCode: 'GB', regionCode: null},
				{countryCode: 'BR', regionCode: null},
			],
		});
	});

	it('applies no regional age policy on a self-hosted instance by default', () => {
		expect(buildAPIConfigFromMaster(withAgePolicy(master, true, {})).instance.agePolicy).toEqual({
			restricted: [],
		});
	});

	it('applies only the regions a self-hosted operator chooses', () => {
		expect(
			buildAPIConfigFromMaster(withAgePolicy(master, true, {restricted_regions: ['DE']})).instance.agePolicy,
		).toEqual({restricted: [{countryCode: 'DE', regionCode: null}]});
	});

	it('lets the hosted service switch the policy off', () => {
		expect(buildAPIConfigFromMaster(withAgePolicy(master, false, {enabled: false})).instance.agePolicy).toEqual({
			restricted: [],
		});
	});
});

function withHostedOperatorPassSale(
	master: MasterConfig,
	operatorPass: Partial<MasterConfig['operator_pass']>,
): MasterConfig {
	return {
		...master,
		env: 'production',
		instance: {...master.instance, self_hosted: false},
		operator_pass: {
			...master.operator_pass,
			price_rises_at: '2027-02-01T12:00:00Z',
			labs_guild_id: '1',
			labs_operator_role_id: '2',
			labs_visionary_role_id: '3',
			labs_operators_channel_id: '4',
			standard_prices_minor: Object.fromEntries(
				Object.entries(OPERATOR_PASS_EARLY_BIRD_PRICES_MINOR).map(([currency, amount]) => [
					currency.toLowerCase(),
					amount + 1,
				]),
			),
			...operatorPass,
		},
	};
}

describe('buildAPIConfigFromMaster Operator Pass sale', () => {
	let master: MasterConfig;
	beforeAll(async () => {
		master = await loadConfig();
	});

	test.each([
		['standard price', {standard_prices_minor: undefined}, /FLUXER_OPERATOR_PASS_STANDARD_PRICE_/],
		['Labs Visionary role', {labs_visionary_role_id: undefined}, /FLUXER_LABS_VISIONARY_ROLE_ID/],
		['Labs #operators channel', {labs_operators_channel_id: undefined}, /FLUXER_LABS_OPERATORS_CHANNEL_ID/],
	] as const)('refuses a hosted production sale without the %s', (_name, missing, message) => {
		expect(() => buildAPIConfigFromMaster(withHostedOperatorPassSale(master, missing))).toThrow(message);
	});
});

async function trustedCallersFromEnv(env: Record<string, string>) {
	for (const [key, value] of Object.entries(env)) {
		vi.stubEnv(key, value);
	}
	resetConfig();
	return buildAPIConfigFromMaster(await loadConfig()).internal.trustedCallers;
}

describe('buildAPIConfigFromMaster trusted callers', () => {
	const bugsKey = 'b'.repeat(32);
	const donationKey = 'd'.repeat(32);

	test('reads callers from FLUXER_API_TRUSTED_CALLERS', async () => {
		const callers = await trustedCallersFromEnv({
			FLUXER_API_TRUSTED_CALLERS: JSON.stringify([
				{name: 'bugs', key: bugsKey, buckets: ['oauth:token', 'oauth:revoke']},
			]),
		});
		expect(callers).toEqual([{name: 'bugs', key: bugsKey, buckets: ['oauth:token', 'oauth:revoke']}]);
	});

	test('turns FLUXER_API_DONATION_PROXY_KEY into a caller scoped to the donation and Operator Pass buckets', async () => {
		const callers = await trustedCallersFromEnv({FLUXER_API_DONATION_PROXY_KEY: donationKey});
		expect(callers).toEqual([
			{
				name: 'donation',
				key: donationKey,
				buckets: [
					'donation:request_link',
					'donation:manage',
					'donation:checkout',
					'operator_pass:state',
					'operator_pass:certificate_link',
					'operator_pass:waitlist_unsubscribe',
				],
			},
		]);
	});

	test('keeps both forms side by side', async () => {
		const callers = await trustedCallersFromEnv({
			FLUXER_API_DONATION_PROXY_KEY: donationKey,
			FLUXER_API_TRUSTED_CALLERS: JSON.stringify([{name: 'bugs', key: bugsKey, buckets: ['oauth:token']}]),
		});
		expect(callers.map((caller) => caller.name)).toEqual(['bugs', 'donation']);
	});

	test('tolerates bucket names and fields this build does not know', async () => {
		const callers = await trustedCallersFromEnv({
			FLUXER_API_TRUSTED_CALLERS: JSON.stringify([
				{name: 'bugs', key: bugsKey, buckets: ['oauth:token', 'future:bucket'], note: 'added later'},
			]),
		});
		expect(callers).toEqual([{name: 'bugs', key: bugsKey, buckets: ['oauth:token', 'future:bucket']}]);
	});

	test('fails at boot on a short key', async () => {
		await expect(
			trustedCallersFromEnv({
				FLUXER_API_TRUSTED_CALLERS: JSON.stringify([{name: 'bugs', key: 'short', buckets: ['oauth:token']}]),
			}),
		).rejects.toThrow('FLUXER_API_TRUSTED_CALLERS entry 1 key must be at least 32 characters');
	});

	test('fails at boot on an entry with no buckets', async () => {
		await expect(
			trustedCallersFromEnv({
				FLUXER_API_TRUSTED_CALLERS: JSON.stringify([{name: 'bugs', key: bugsKey, buckets: []}]),
			}),
		).rejects.toThrow('FLUXER_API_TRUSTED_CALLERS entry 1 buckets must be a non-empty list of bucket names');
	});

	test('fails at boot on a value that is not a JSON array', async () => {
		await expect(trustedCallersFromEnv({FLUXER_API_TRUSTED_CALLERS: '{"name":"bugs"}'})).rejects.toThrow(
			'FLUXER_API_TRUSTED_CALLERS must be a JSON array',
		);
	});
});

async function setupTokenFromEnv(env: Record<string, string>): Promise<string | null> {
	for (const [key, value] of Object.entries(env)) {
		vi.stubEnv(key, value);
	}
	resetConfig();
	return buildAPIConfigFromMaster(await loadConfig()).instance.setup.token;
}

describe('buildAPIConfigFromMaster setup token', () => {
	const setupToken = 's'.repeat(32);

	test('reads the token from FLUXER_SETUP_TOKEN', async () => {
		expect(await setupTokenFromEnv({FLUXER_SETUP_TOKEN: ` ${setupToken}\n`})).toBe(setupToken);
	});

	test.each(['', '  '])('leaves the token unset for %j', async (blank) => {
		expect(await setupTokenFromEnv({FLUXER_SETUP_TOKEN: blank})).toBeNull();
	});

	test('leaves the token unset when the variable is missing', async () => {
		expect(await setupTokenFromEnv({})).toBeNull();
	});

	test('accepts a token of 256 characters', async () => {
		expect(await setupTokenFromEnv({FLUXER_SETUP_TOKEN: 's'.repeat(256)})).toBe('s'.repeat(256));
	});

	test.each([
		['shorter than 32 characters', 's'.repeat(31)],
		['longer than 256 characters', 's'.repeat(257)],
		['holding a form feed', `${'s'.repeat(16)}\f${'s'.repeat(16)}`],
		['holding a text direction override', `${'s'.repeat(16)}\u202E${'s'.repeat(16)}`],
	])('fails at boot on a token %s', async (_case, token) => {
		await expect(setupTokenFromEnv({FLUXER_SETUP_TOKEN: token})).rejects.toThrow(
			'FLUXER_SETUP_TOKEN must be 32 to 256 printable characters',
		);
	});
});
