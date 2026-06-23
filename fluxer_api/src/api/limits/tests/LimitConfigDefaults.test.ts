// SPDX-License-Identifier: AGPL-3.0-or-later

import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {MAX_GUILD_MEMBERS_VERY_LARGE_GUILD} from '@fluxer/constants/src/LimitConstants';
import type {LimitConfigSnapshot, LimitRule} from '@fluxer/limits/src/LimitTypes';
import {describe, expect, test} from 'vitest';
import {createDefaultLimitConfig, mergeWithCurrentDefaults} from '../../constants/LimitConfig';

interface LegacyLimitRule extends LimitRule {
	unlockedFeatures?: Array<string>;
}

interface LegacyLimitConfigSnapshot extends Omit<LimitConfigSnapshot, 'rules'> {
	rules: Array<LegacyLimitRule>;
}

describe('Limit config defaults', () => {
	test('hosted defaults include premium, default, and very large guild limit rules', () => {
		const config = createDefaultLimitConfig({selfHosted: false});
		const premiumRule = config.rules.find((rule) => rule.id === 'premium');
		const defaultRule = config.rules.find((rule) => rule.id === 'default');
		const veryLargeGuildRule = config.rules.find((rule) => rule.id === 'very_large_guild');
		expect(premiumRule).toBeDefined();
		expect(defaultRule).toBeDefined();
		expect(veryLargeGuildRule).toMatchObject({
			filters: {guildFeatures: [GuildFeatures.VERY_LARGE_GUILD]},
			limits: {max_guild_members: MAX_GUILD_MEMBERS_VERY_LARGE_GUILD},
		});
		expect(config.rules.map((rule) => rule.id)).toEqual(['premium', 'default', 'very_large_guild']);
	});
	test('self-hosted defaults include default and very large guild limit rules', () => {
		const config = createDefaultLimitConfig({selfHosted: true});
		const veryLargeGuildRule = config.rules.find((rule) => rule.id === 'very_large_guild');
		expect(veryLargeGuildRule).toMatchObject({
			filters: {guildFeatures: [GuildFeatures.VERY_LARGE_GUILD]},
			limits: {max_guild_members: MAX_GUILD_MEMBERS_VERY_LARGE_GUILD},
		});
		expect(config.rules.map((rule) => rule.id)).toEqual(['default', 'very_large_guild']);
	});
});

describe('Limit config default merge', () => {
	test('legacy unlocked features on known rules are dropped during merge', () => {
		const legacyConfig: LegacyLimitConfigSnapshot = {
			traitDefinitions: ['premium'],
			rules: [
				{
					id: 'premium',
					filters: {traits: ['premium']},
					limits: {},
					unlockedFeatures: ['MORE_EMOJI', 'UNLIMITED_EMOJI'],
				},
				{
					id: 'default',
					limits: {},
				},
			],
		};
		const merged = mergeWithCurrentDefaults(legacyConfig, {selfHosted: false});
		const premiumRule = merged.rules.find((rule) => rule.id === 'premium') as Record<string, unknown> | undefined;
		expect(premiumRule?.unlockedFeatures).toBeUndefined();
	});
});
