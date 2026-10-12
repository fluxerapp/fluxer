// SPDX-License-Identifier: AGPL-3.0-or-later

import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import type {LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';
import {DEFAULT_RESTRICTED_LIMITS, DEFAULT_STOCK_LIMITS} from '@fluxer/limits/src/LimitDefaults';
import {expandWireFormat} from '@fluxer/limits/src/LimitDiffer';
import {computeDefaultsHash} from '@fluxer/limits/src/LimitHashing';
import {resolveLimits} from '@fluxer/limits/src/LimitResolver';
import type {LimitConfigWireFormat, LimitMatchContext} from '@fluxer/limits/src/LimitTypes';
import {afterAll, beforeAll, describe, expect, test} from 'vitest';

const WIRE_COMPATIBILITY_LIMIT_KEYS: ReadonlySet<LimitKey> = new Set<LimitKey>([
	'max_guild_emojis',
	'max_guild_emojis_animated_more',
	'max_guild_emojis_animated',
	'max_guild_emojis_static_more',
	'max_guild_emojis_static',
	'max_guild_stickers_more',
	'max_guild_stickers',
]);

interface WellKnownResponse {
	limits: LimitConfigWireFormat;
}

describe('Limit Wire Format', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	test('rules have overrides field not limits field in well-known response', async () => {
		const response = (await createBuilderWithoutAuth(harness)
			.get('/.well-known/fluxer')
			.expect(HTTP_STATUS.OK)
			.execute()) as WellKnownResponse;
		expect(response.limits.rules.length).toBeGreaterThan(0);
		for (const rule of response.limits.rules) {
			expect(typeof rule.id).toBe('string');
			expect(rule.overrides).toBeDefined();
			expect(typeof rule.overrides).toBe('object');
			expect(Reflect.get(rule, 'limits')).toBeUndefined();
		}
	});
	test('defaultsHash matches computed hash', async () => {
		const response = (await createBuilderWithoutAuth(harness)
			.get('/.well-known/fluxer')
			.expect(HTTP_STATUS.OK)
			.execute()) as WellKnownResponse;
		const expectedHash = computeDefaultsHash();
		expect(response.limits.defaultsHash).toBe(expectedHash);
	});
	test('default rule keeps wire-compatible expression limit overrides', async () => {
		const response = (await createBuilderWithoutAuth(harness)
			.get('/.well-known/fluxer')
			.expect(HTTP_STATUS.OK)
			.execute()) as WellKnownResponse;
		const defaultRule = response.limits.rules.find((rule) => rule.id === 'default');
		if (!defaultRule) {
			return;
		}
		expect(defaultRule.overrides).toBeDefined();
		expect(new Set(Object.keys(defaultRule.overrides) as Array<LimitKey>)).toEqual(WIRE_COMPATIBILITY_LIMIT_KEYS);
		for (const key of WIRE_COMPATIBILITY_LIMIT_KEYS) {
			expect(defaultRule.overrides[key]).toBe(DEFAULT_RESTRICTED_LIMITS[key]);
		}
	});
	test('wire format roundtrip preserves limit resolution behavior', async () => {
		const response = (await createBuilderWithoutAuth(harness)
			.get('/.well-known/fluxer')
			.expect(HTTP_STATUS.OK)
			.execute()) as WellKnownResponse;
		const expanded = expandWireFormat(response.limits);
		expect(response.limits.version).toBe(2);
		const freeContext: LimitMatchContext = {
			traits: new Set(),
			guildFeatures: new Set(),
		};
		const premiumContext: LimitMatchContext = {
			traits: new Set(['premium']),
			guildFeatures: new Set(),
		};
		const expandedFree = resolveLimits(expanded, freeContext);
		const expandedPremium = resolveLimits(expanded, premiumContext);
		for (const key of Object.keys(DEFAULT_RESTRICTED_LIMITS) as Array<LimitKey>) {
			expect(expandedFree.limits[key]).toBe(DEFAULT_RESTRICTED_LIMITS[key]);
		}
		for (const key of Object.keys(DEFAULT_STOCK_LIMITS) as Array<LimitKey>) {
			expect(expandedPremium.limits[key]).toBe(DEFAULT_STOCK_LIMITS[key]);
		}
	});
});
