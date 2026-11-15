// SPDX-License-Identifier: AGPL-3.0-or-later

import {resolveLimits} from '@fluxer/limits/src/LimitResolver';
import {ruleMatches} from '@fluxer/limits/src/LimitRuleRuntime';
import {describe, expect, test} from 'vitest';

const NO_GUILD = new Set<string>();

describe('trait filters', () => {
	test.each([
		{rule: ['Premium'], user: ['premium']},
		{rule: ['premium'], user: ['Premium']},
		{rule: ['VIP', 'beta'], user: ['vip', 'BETA']},
	])('match $rule for traits $user without regard to case', ({rule, user}) => {
		expect(ruleMatches({traits: rule}, {traits: new Set(user), guildFeatures: NO_GUILD})).toBe(true);
	});

	test('still need every trait in the filter', () => {
		expect(ruleMatches({traits: ['vip', 'beta']}, {traits: new Set(['VIP']), guildFeatures: NO_GUILD})).toBe(false);
	});

	test('keep guild features exact', () => {
		expect(
			ruleMatches(
				{guildFeatures: ['VERY_LARGE_GUILD']},
				{traits: new Set(), guildFeatures: new Set(['very_large_guild'])},
			),
		).toBe(false);
	});

	test('apply a rule written with a capital letter to the lowercase trait', () => {
		const {limits} = resolveLimits(
			{
				traitDefinitions: ['Premium'],
				rules: [{id: 'premium', filters: {traits: ['Premium']}, limits: {max_guilds: 321}}],
			},
			{traits: new Set(['premium']), guildFeatures: NO_GUILD},
		);
		expect(limits.max_guilds).toBe(321);
	});
});
