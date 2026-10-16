// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	buildStoredLimitConfig,
	createDefaultLimitConfig,
	mergeWithCurrentDefaults,
	readStoredLimitOverrides,
	sanitizeLimitConfigForInstance,
} from '@app/api/constants/LimitConfig';
import {LIMIT_KEYS} from '@fluxer/constants/src/LimitConfigMetadata';
import {DEFAULT_RESTRICTED_LIMITS, DEFAULT_STOCK_LIMITS} from '@fluxer/limits/src/LimitDefaults';
import type {LimitConfigSnapshot, LimitRule} from '@fluxer/limits/src/LimitTypes';
import {describe, expect, test} from 'vitest';

describe('Limit config storage', () => {
	const mirror = {selfHosted: true, premiumMode: 'mirror'} as const;
	const everyone = {selfHosted: true, premiumMode: 'everyone'} as const;
	const hosted = {selfHosted: false, premiumMode: 'everyone'} as const;
	const modeKeys = LIMIT_KEYS.filter((key) => DEFAULT_STOCK_LIMITS[key] !== DEFAULT_RESTRICTED_LIMITS[key]);

	function defaultRuleConfig(rule: Omit<LimitRule, 'id'>): LimitConfigSnapshot {
		return {traitDefinitions: [], rules: [{id: 'default', ...rule}]};
	}

	function fullConfig(options: {selfHosted: boolean; premiumMode: 'mirror' | 'everyone'}): LimitConfigSnapshot {
		const {traitDefinitions, rules} = createDefaultLimitConfig(options);
		return {traitDefinitions, rules};
	}

	function withLimits(config: LimitConfigSnapshot, ruleId: string, limits: LimitRule['limits']): LimitConfigSnapshot {
		return {
			traitDefinitions: config.traitDefinitions,
			rules: config.rules.map((rule) => (rule.id === ruleId ? {...rule, limits: {...rule.limits, ...limits}} : rule)),
		};
	}

	function load(stored: LimitConfigSnapshot, options: typeof mirror | typeof everyone): LimitConfigSnapshot {
		const overrides = readStoredLimitOverrides(structuredClone(stored), options);
		return mergeWithCurrentDefaults(sanitizeLimitConfigForInstance(overrides, options), options);
	}

	function limitsOf(config: LimitConfigSnapshot, ruleId: string): LimitRule['limits'] | undefined {
		return config.rules.find((rule) => rule.id === ruleId)?.limits;
	}

	test('built-in rules keep only values that differ from the defaults', () => {
		const stored = buildStoredLimitConfig(
			{
				traitDefinitions: ['premium', 'vip'],
				rules: [
					{id: 'premium', filters: {traits: ['premium']}, limits: {...DEFAULT_STOCK_LIMITS, max_guilds: 250}},
					{id: 'default', limits: {...DEFAULT_RESTRICTED_LIMITS}, modifiedFields: ['max_guilds']},
					{id: 'vip', filters: {traits: ['vip']}, limits: {max_guilds: 100}},
				],
			},
			null,
			mirror,
		);
		expect(stored.rules).toEqual([
			{id: 'premium', filters: {traits: ['premium']}, limits: {max_guilds: 250}},
			{id: 'default', limits: {}},
			{id: 'vip', filters: {traits: ['vip']}, limits: {max_guilds: 100}},
		]);
	});

	test('hosted instances keep storing whole rules', () => {
		const submitted = withLimits(fullConfig(hosted), 'default', {max_guilds: 150});
		const stored = buildStoredLimitConfig(submitted, null, hosted);
		expect(stored).toEqual(mergeWithCurrentDefaults(submitted, hosted));
		expect(limitsOf(stored, 'default')).toEqual({...DEFAULT_RESTRICTED_LIMITS, max_guilds: 150});
		expect(readStoredLimitOverrides(stored, hosted)).toBe(stored);
	});

	test('a whole default rule saved under the other premium model keeps only its edits', () => {
		const stored = defaultRuleConfig({
			limits: {...DEFAULT_RESTRICTED_LIMITS, max_guilds: 150, max_bio_length: 300},
			modifiedFields: ['max_guilds', 'max_bio_length'],
		});
		expect(readStoredLimitOverrides(stored, everyone).rules).toEqual([
			{id: 'default', limits: {max_guilds: 150, max_bio_length: 300}},
		]);
	});

	test('a whole default rule saved under the current premium model keeps only its edits', () => {
		const stored = defaultRuleConfig({
			limits: {...DEFAULT_RESTRICTED_LIMITS, feature_custom_discriminator: 1},
			modifiedFields: ['feature_custom_discriminator'],
		});
		expect(readStoredLimitOverrides(stored, mirror).rules).toEqual([
			{id: 'default', limits: {feature_custom_discriminator: 1}},
		]);
	});

	test('free limits saved again after a switch to everyone are dropped', () => {
		const savedInMirror = mergeWithCurrentDefaults(fullConfig(mirror), mirror);
		const shownInEveryone = mergeWithCurrentDefaults(savedInMirror, everyone);
		expect(limitsOf(shownInEveryone, 'default')).toEqual(DEFAULT_RESTRICTED_LIMITS);
		const savedAgain = mergeWithCurrentDefaults(shownInEveryone, everyone);
		expect(savedAgain.rules.find((rule) => rule.id === 'default')?.modifiedFields).toEqual(modeKeys);
		expect(limitsOf(load(savedAgain, everyone), 'default')).toEqual(DEFAULT_STOCK_LIMITS);
		expect(limitsOf(load(savedAgain, mirror), 'default')).toEqual(DEFAULT_RESTRICTED_LIMITS);
	});

	test('free limits saved again with an edit after a switch to everyone keep only the edit', () => {
		const savedInMirror = mergeWithCurrentDefaults(
			withLimits(fullConfig(mirror), 'default', {max_guilds: 150, max_bio_length: 300}),
			mirror,
		);
		const savedAgain = mergeWithCurrentDefaults(mergeWithCurrentDefaults(savedInMirror, everyone), everyone);
		expect(savedAgain.rules.find((rule) => rule.id === 'default')?.modifiedFields).toEqual(
			LIMIT_KEYS.filter((key) => modeKeys.includes(key) || key === 'max_bio_length'),
		);
		expect(readStoredLimitOverrides(savedAgain, everyone).rules.find((rule) => rule.id === 'default')).toEqual({
			id: 'default',
			limits: {max_bio_length: 300, max_guilds: 150},
		});
	});

	test('a default rule fixed by hand after a switch is left as saved', () => {
		const stored = defaultRuleConfig({
			limits: {...DEFAULT_RESTRICTED_LIMITS, feature_custom_discriminator: 1},
			modifiedFields: modeKeys.filter((key) => key !== 'feature_custom_discriminator'),
		});
		expect(limitsOf(load(stored, everyone), 'default')).toEqual({
			...DEFAULT_RESTRICTED_LIMITS,
			feature_custom_discriminator: 1,
		});
	});

	test('premium limits saved again after a switch to mirror are dropped', () => {
		const savedInEveryone = mergeWithCurrentDefaults(fullConfig(everyone), everyone);
		const shownInMirror = mergeWithCurrentDefaults(savedInEveryone, mirror);
		expect(limitsOf(shownInMirror, 'default')).toEqual(DEFAULT_STOCK_LIMITS);
		const savedAgain = mergeWithCurrentDefaults(shownInMirror, mirror);
		const loaded = load(savedAgain, mirror);
		expect(limitsOf(loaded, 'default')).toEqual(DEFAULT_RESTRICTED_LIMITS);
		expect(limitsOf(loaded, 'premium')).toEqual(DEFAULT_STOCK_LIMITS);
		expect(loaded.traitDefinitions).toEqual(['premium']);
	});

	test('overrides saved in the current format are never reinterpreted', () => {
		const raised = buildStoredLimitConfig(
			withLimits(fullConfig(mirror), 'default', DEFAULT_STOCK_LIMITS),
			null,
			mirror,
		);
		expect(Object.keys(limitsOf(raised, 'default') ?? {})).toEqual(modeKeys);
		expect(readStoredLimitOverrides(raised, mirror)).toBe(raised);
		expect(readStoredLimitOverrides(raised, everyone)).toBe(raised);
		expect(limitsOf(load(raised, mirror), 'default')).toEqual(DEFAULT_STOCK_LIMITS);
	});

	test('an edit that matches the other premium model survives a save there', () => {
		const edits = {feature_custom_discriminator: 1, max_guilds: DEFAULT_STOCK_LIMITS.max_guilds};
		const savedInMirror = buildStoredLimitConfig(withLimits(fullConfig(mirror), 'default', edits), null, mirror);
		const savedInEveryone = buildStoredLimitConfig(
			withLimits(load(savedInMirror, everyone), 'default', {max_bio_length: 300}),
			savedInMirror,
			everyone,
		);
		expect(limitsOf(savedInEveryone, 'default')).toEqual({...edits, max_bio_length: 300});
		expect(limitsOf(load(savedInEveryone, mirror), 'default')).toEqual({
			...DEFAULT_RESTRICTED_LIMITS,
			...edits,
			max_bio_length: 300,
		});
	});

	test('a value moved back to the default stops being stored', () => {
		const savedInMirror = buildStoredLimitConfig(
			withLimits(fullConfig(mirror), 'default', {max_guilds: 150}),
			null,
			mirror,
		);
		const reverted = buildStoredLimitConfig(fullConfig(mirror), savedInMirror, mirror);
		expect(limitsOf(reverted, 'default')).toEqual({});
	});

	test('premium rule edits survive a save while everyone hides the rule', () => {
		const edits = {max_guilds: 999, feature_custom_discriminator: 0};
		const savedInMirror = buildStoredLimitConfig(withLimits(fullConfig(mirror), 'premium', edits), null, mirror);
		expect(limitsOf(load(savedInMirror, everyone), 'premium')).toBeUndefined();
		const savedInEveryone = buildStoredLimitConfig(fullConfig(everyone), savedInMirror, everyone);
		expect(savedInEveryone.traitDefinitions).toEqual(['premium']);
		expect(limitsOf(savedInEveryone, 'premium')).toEqual(edits);
		expect(limitsOf(load(savedInEveryone, mirror), 'premium')).toEqual({...DEFAULT_STOCK_LIMITS, ...edits});
	});
});
