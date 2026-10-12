// SPDX-License-Identifier: AGPL-3.0-or-later

import {DEFAULT_RESTRICTED_LIMITS, DEFAULT_STOCK_LIMITS} from '@fluxer/limits/src/LimitDefaults';
import {computeOverrides, computeWireFormat, expandWireFormat} from '@fluxer/limits/src/LimitDiffer';
import type {LimitConfigSnapshot} from '@fluxer/limits/src/LimitTypes';
import {describe, expect, test} from 'vitest';

const WIRE_COMPATIBILITY_LIMIT_KEYS = [
	'max_guild_emojis',
	'max_guild_emojis_animated_more',
	'max_guild_emojis_animated',
	'max_guild_emojis_static_more',
	'max_guild_emojis_static',
	'max_guild_stickers_more',
	'max_guild_stickers',
] as const;

function expectWireCompatibilityOverrides(
	overrides: Partial<typeof DEFAULT_RESTRICTED_LIMITS>,
	limits: typeof DEFAULT_RESTRICTED_LIMITS,
): void {
	expect(Object.keys(overrides).sort()).toEqual([...WIRE_COMPATIBILITY_LIMIT_KEYS].sort());
	for (const key of WIRE_COMPATIBILITY_LIMIT_KEYS) {
		expect(overrides[key]).toBe(limits[key]);
	}
}

describe('LimitDiffer', () => {
	test('computeOverrides returns empty object when limits match defaults', () => {
		const overrides = computeOverrides(DEFAULT_RESTRICTED_LIMITS, DEFAULT_RESTRICTED_LIMITS);
		expect(overrides).toEqual({});
	});
	test('computeOverrides extracts differences from defaults', () => {
		const customLimits = {
			...DEFAULT_RESTRICTED_LIMITS,
			max_guilds: 150,
			max_message_length: 3000,
		};
		const overrides = computeOverrides(customLimits, DEFAULT_RESTRICTED_LIMITS);
		expect(overrides).toEqual({
			max_guilds: 150,
			max_message_length: 3000,
		});
	});
	test('computeWireFormat creates wire format with overrides', () => {
		const config: LimitConfigSnapshot = {
			traitDefinitions: ['premium'],
			rules: [
				{
					id: 'default',
					limits: {...DEFAULT_RESTRICTED_LIMITS},
				},
				{
					id: 'premium',
					filters: {traits: ['premium']},
					limits: {...DEFAULT_STOCK_LIMITS},
				},
			],
		};
		const wireFormat = computeWireFormat(config);
		expect(wireFormat.version).toBe(2);
		expect(wireFormat.traitDefinitions).toEqual(['premium']);
		expect(wireFormat.rules).toHaveLength(2);
		expect(wireFormat.defaultsHash).toBeTruthy();
		expect(wireFormat.rules[0].id).toBe('default');
		expectWireCompatibilityOverrides(wireFormat.rules[0].overrides, DEFAULT_RESTRICTED_LIMITS);
		expect(wireFormat.rules[1].id).toBe('premium');
		expect(Object.keys(wireFormat.rules[1].overrides).length).toBeGreaterThan(0);
		expect(wireFormat.rules[1].overrides.max_guilds).toBe(200);
	});
	test('expandWireFormat reconstructs full config from overrides', () => {
		const config: LimitConfigSnapshot = {
			traitDefinitions: ['premium'],
			rules: [
				{
					id: 'default',
					limits: {...DEFAULT_RESTRICTED_LIMITS},
				},
				{
					id: 'premium',
					filters: {traits: ['premium']},
					limits: {...DEFAULT_STOCK_LIMITS},
				},
			],
		};
		const wireFormat = computeWireFormat(config);
		const expanded = expandWireFormat(wireFormat);
		expect(expanded.version).toBe(2);
		expect(expanded.traitDefinitions).toEqual(['premium']);
		expect(expanded.rules).toHaveLength(2);
		expect(expanded.rules[0].limits.max_guilds).toBe(100);
		expect(expanded.rules[1].limits.max_guilds).toBe(200);
		expect(expanded.rules[1].limits.max_message_length).toBe(4000);
	});
	test('roundtrip: expand(compute(config)) equals config', () => {
		const config: LimitConfigSnapshot = {
			traitDefinitions: ['premium'],
			rules: [
				{
					id: 'default',
					limits: {...DEFAULT_RESTRICTED_LIMITS},
				},
				{
					id: 'premium',
					filters: {traits: ['premium']},
					limits: {
						...DEFAULT_STOCK_LIMITS,
						max_guilds: 250,
					},
				},
			],
		};
		const wireFormat = computeWireFormat(config);
		const expanded = expandWireFormat(wireFormat);
		expect(expanded.rules[0].limits).toEqual(config.rules[0].limits);
		expect(expanded.rules[1].limits.max_guilds).toBe(250);
		expect(expanded.rules[1].limits.max_message_length).toBe(4000);
	});
});
