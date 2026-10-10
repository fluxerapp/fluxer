// SPDX-License-Identifier: AGPL-3.0-or-later

import {rebaseLimitsSavedUnderOtherPremiumMode, stripCurrentDefaults} from '@app/api/constants/LimitConfig';
import {LIMIT_KEYS} from '@fluxer/constants/src/LimitConfigMetadata';
import {DEFAULT_RESTRICTED_LIMITS, DEFAULT_STOCK_LIMITS} from '@fluxer/limits/src/LimitDefaults';
import type {LimitConfigSnapshot, LimitRule} from '@fluxer/limits/src/LimitTypes';
import {describe, expect, test} from 'vitest';

describe('Limit config storage', () => {
	const mirror = {selfHosted: true, premiumMode: 'mirror'} as const;
	const everyone = {selfHosted: true, premiumMode: 'everyone'} as const;

	function defaultRuleConfig(rule: Omit<LimitRule, 'id'>): LimitConfigSnapshot {
		return {traitDefinitions: [], rules: [{id: 'default', ...rule}]};
	}

	test('built-in rules keep only values that differ from the defaults', () => {
		const stripped = stripCurrentDefaults(
			{
				traitDefinitions: ['premium', 'vip'],
				rules: [
					{id: 'premium', filters: {traits: ['premium']}, limits: {...DEFAULT_STOCK_LIMITS, max_guilds: 250}},
					{id: 'default', limits: {...DEFAULT_RESTRICTED_LIMITS}, modifiedFields: ['max_guilds']},
					{id: 'vip', filters: {traits: ['vip']}, limits: {max_guilds: 100}},
				],
			},
			mirror,
		);
		expect(stripped.rules).toEqual([
			{id: 'premium', filters: {traits: ['premium']}, limits: {max_guilds: 250}},
			{id: 'default', limits: {}},
			{id: 'vip', filters: {traits: ['vip']}, limits: {max_guilds: 100}},
		]);
	});

	test('a full default rule saved under the other premium mode keeps only its edits', () => {
		const stored = defaultRuleConfig({
			limits: {...DEFAULT_RESTRICTED_LIMITS, max_guilds: 150, max_bio_length: 300},
			modifiedFields: ['max_guilds', 'max_bio_length'],
		});
		expect(rebaseLimitsSavedUnderOtherPremiumMode(stored, everyone).rules).toEqual([
			{id: 'default', limits: {max_guilds: 150, max_bio_length: 300}},
		]);
	});

	test('a full default rule saved under the current premium mode is left alone', () => {
		const stored = defaultRuleConfig({
			limits: {...DEFAULT_RESTRICTED_LIMITS, feature_custom_discriminator: 1},
			modifiedFields: ['feature_custom_discriminator'],
		});
		expect(rebaseLimitsSavedUnderOtherPremiumMode(stored, mirror)).toBe(stored);
	});

	test('a default rule deliberately raised to the other tier is left alone', () => {
		const full = defaultRuleConfig({
			limits: {...DEFAULT_STOCK_LIMITS},
			modifiedFields: LIMIT_KEYS.filter((key) => DEFAULT_STOCK_LIMITS[key] !== DEFAULT_RESTRICTED_LIMITS[key]),
		});
		expect(rebaseLimitsSavedUnderOtherPremiumMode(full, mirror)).toBe(full);
		const sparse = stripCurrentDefaults(full, mirror);
		expect(rebaseLimitsSavedUnderOtherPremiumMode(sparse, mirror)).toBe(sparse);
	});

	test('hosted instances never rebase', () => {
		const stored = defaultRuleConfig({limits: {...DEFAULT_STOCK_LIMITS}});
		expect(rebaseLimitsSavedUnderOtherPremiumMode(stored, {selfHosted: false, premiumMode: 'everyone'})).toBe(stored);
	});
});
