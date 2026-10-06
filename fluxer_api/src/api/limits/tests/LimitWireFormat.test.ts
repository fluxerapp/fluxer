// SPDX-License-Identifier: AGPL-3.0-or-later

import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import type {LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';
import {DEFAULT_FREE_LIMITS, DEFAULT_PREMIUM_LIMITS} from '@fluxer/limits/src/LimitDefaults';
import {expandWireFormat} from '@fluxer/limits/src/LimitDiffer';
import {computeDefaultsHash} from '@fluxer/limits/src/LimitHashing';
import {resolveLimits} from '@fluxer/limits/src/LimitResolver';
import type {LimitConfigWireFormat, LimitMatchContext} from '@fluxer/limits/src/LimitTypes';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

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
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('well-known endpoint returns version 2 format', async () => {
		const response = (await createBuilderWithoutAuth(harness)
			.get('/.well-known/fluxer')
			.expect(HTTP_STATUS.OK)
			.execute()) as WellKnownResponse;
		expect(response.limits).toBeDefined();
		expect(response.limits.version).toBe(2);
		expect(response.limits.traitDefinitions).toBeDefined();
		expect(Array.isArray(response.limits.traitDefinitions)).toBe(true);
		expect(response.limits.rules).toBeDefined();
		expect(Array.isArray(response.limits.rules)).toBe(true);
		expect(response.limits.defaultsHash).toBeDefined();
		expect(typeof response.limits.defaultsHash).toBe('string');
	});
	test('rules have overrides field not limits field in well-known response', async () => {
		const response = (await createBuilderWithoutAuth(harness)
			.get('/.well-known/fluxer')
			.expect(HTTP_STATUS.OK)
			.execute()) as WellKnownResponse;
		expect(response.limits.rules.length).toBeGreaterThan(0);
		for (const rule of response.limits.rules) {
			expect(rule.id).toBeDefined();
			expect(rule.overrides).toBeDefined();
			expect(typeof rule.overrides).toBe('object');
			expect(Reflect.get(rule, 'limits')).toBeUndefined();
		}
	});
	test('wire format already has overrides field', async () => {
		const response = (await createBuilderWithoutAuth(harness)
			.get('/.well-known/fluxer')
			.expect(HTTP_STATUS.OK)
			.execute()) as WellKnownResponse;
		expect(response.limits.version).toBe(2);
		expect(response.limits.defaultsHash).toBeDefined();
		expect(typeof response.limits.defaultsHash).toBe('string');
		expect(response.limits.defaultsHash.length).toBeGreaterThan(0);
		for (const rule of response.limits.rules) {
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
	test('wire format can be expanded back to full format', async () => {
		const response = (await createBuilderWithoutAuth(harness)
			.get('/.well-known/fluxer')
			.expect(HTTP_STATUS.OK)
			.execute()) as WellKnownResponse;
		const expanded = expandWireFormat(response.limits);
		expect(expanded.traitDefinitions).toEqual(response.limits.traitDefinitions);
		expect(expanded.rules.length).toBe(response.limits.rules.length);
		for (let i = 0; i < expanded.rules.length; i++) {
			const expandedRule = expanded.rules[i];
			const wireRule = response.limits.rules[i];
			expect(expandedRule.id).toBe(wireRule.id);
			expect(expandedRule.filters).toEqual(wireRule.filters);
			expect(expandedRule.limits).toBeDefined();
			expect(typeof expandedRule.limits).toBe('object');
			for (const key of Object.keys(DEFAULT_FREE_LIMITS) as Array<LimitKey>) {
				expect(expandedRule.limits[key]).toBeDefined();
			}
		}
	});
	test('premium rules have correct overrides compared to free defaults', async () => {
		const response = (await createBuilderWithoutAuth(harness)
			.get('/.well-known/fluxer')
			.expect(HTTP_STATUS.OK)
			.execute()) as WellKnownResponse;
		const premiumRule = response.limits.rules.find((rule) => rule.id === 'premium');
		if (!premiumRule) {
			return;
		}
		expect(premiumRule.overrides).toBeDefined();
		const overrideKeys = Object.keys(premiumRule.overrides) as Array<LimitKey>;
		expect(overrideKeys.length).toBeGreaterThan(0);
		for (const key of overrideKeys) {
			const overrideValue = premiumRule.overrides[key];
			const freeValue = DEFAULT_FREE_LIMITS[key];
			expect(DEFAULT_PREMIUM_LIMITS[key]).toBe(overrideValue);
			if (!WIRE_COMPATIBILITY_LIMIT_KEYS.has(key)) {
				expect(overrideValue).not.toBe(freeValue);
			}
		}
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
			expect(defaultRule.overrides[key]).toBe(DEFAULT_FREE_LIMITS[key]);
		}
	});
	test('wire format roundtrip preserves limit resolution behavior', async () => {
		const response = (await createBuilderWithoutAuth(harness)
			.get('/.well-known/fluxer')
			.expect(HTTP_STATUS.OK)
			.execute()) as WellKnownResponse;
		const expanded = expandWireFormat(response.limits);
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
		for (const key of Object.keys(DEFAULT_FREE_LIMITS) as Array<LimitKey>) {
			expect(expandedFree.limits[key]).toBe(DEFAULT_FREE_LIMITS[key]);
		}
		for (const key of Object.keys(DEFAULT_PREMIUM_LIMITS) as Array<LimitKey>) {
			expect(expandedPremium.limits[key]).toBe(DEFAULT_PREMIUM_LIMITS[key]);
		}
	});
	test('wire format correctly identifies premium feature overrides', async () => {
		const response = (await createBuilderWithoutAuth(harness)
			.get('/.well-known/fluxer')
			.expect(HTTP_STATUS.OK)
			.execute()) as WellKnownResponse;
		const premiumRule = response.limits.rules.find((rule) => rule.id === 'premium');
		if (!premiumRule) {
			return;
		}
		const featureKeys = Object.keys(premiumRule.overrides).filter((key) =>
			key.startsWith('feature_'),
		) as Array<LimitKey>;
		for (const key of featureKeys) {
			expect(DEFAULT_FREE_LIMITS[key]).toBe(0);
			expect(DEFAULT_PREMIUM_LIMITS[key]).toBe(1);
			expect(premiumRule.overrides[key]).toBe(1);
		}
	});
	test('wire format preserves trait definitions', async () => {
		const response = (await createBuilderWithoutAuth(harness)
			.get('/.well-known/fluxer')
			.expect(HTTP_STATUS.OK)
			.execute()) as WellKnownResponse;
		expect(response.limits.traitDefinitions).toBeDefined();
		expect(Array.isArray(response.limits.traitDefinitions)).toBe(true);
	});
	test('wire format preserves rule filters', async () => {
		const response = (await createBuilderWithoutAuth(harness)
			.get('/.well-known/fluxer')
			.expect(HTTP_STATUS.OK)
			.execute()) as WellKnownResponse;
		for (const rule of response.limits.rules) {
			if (rule.filters) {
				expect(typeof rule.filters).toBe('object');
			}
		}
	});
	test('defaultsHash is stable across conversions', async () => {
		const hash1 = computeDefaultsHash();
		const hash2 = computeDefaultsHash();
		expect(hash1).toBe(hash2);
		expect(typeof hash1).toBe('string');
		expect(hash1.length).toBeGreaterThan(0);
	});
	test('wire format type definition matches runtime structure', async () => {
		const response = (await createBuilderWithoutAuth(harness)
			.get('/.well-known/fluxer')
			.expect(HTTP_STATUS.OK)
			.execute()) as WellKnownResponse;
		const wireFormat: LimitConfigWireFormat = response.limits;
		expect(wireFormat.version).toBe(2);
		expect(typeof wireFormat.defaultsHash).toBe('string');
		expect(Array.isArray(wireFormat.traitDefinitions)).toBe(true);
		expect(Array.isArray(wireFormat.rules)).toBe(true);
		for (const rule of wireFormat.rules) {
			expect(typeof rule.id).toBe('string');
			expect(typeof rule.overrides).toBe('object');
			if (rule.filters) {
				expect(typeof rule.filters).toBe('object');
			}
		}
	});
});
