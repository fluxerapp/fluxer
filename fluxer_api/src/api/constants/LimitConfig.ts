// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import type {LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';
import {LIMIT_KEYS} from '@fluxer/constants/src/LimitConfigMetadata';
import {MAX_GUILD_MEMBERS_VERY_LARGE_GUILD} from '@fluxer/constants/src/LimitConstants';
import {DEFAULT_RESTRICTED_LIMITS, DEFAULT_STOCK_LIMITS} from '@fluxer/limits/src/LimitDefaults';
import type {LimitConfigSnapshot, LimitRule} from '@fluxer/limits/src/LimitTypes';

const LIMIT_RULE_IDS = {
	DEFAULT: 'default',
	HOSTED_UPGRADE: 'premium',
	VERY_LARGE_GUILD: 'very_large_guild',
} as const;

export function getLegacyLimitConfigKvKey(selfHosted: boolean): string {
	return `limit_config:${selfHosted ? 'self_hosted' : 'saas'}`;
}

export const LIMIT_CONFIG_REFRESH_CHANNEL = 'limit-config-refresh';

function cloneLimitConfigSnapshot(config: LimitConfigSnapshot): LimitConfigSnapshot {
	return structuredClone(config);
}

export function sanitizeLimitConfigForInstance(
	config: LimitConfigSnapshot,
	options?: {
		selfHosted?: boolean;
		premiumMode?: 'mirror' | 'everyone';
	},
): LimitConfigSnapshot {
	const selfHosted = options?.selfHosted ?? false;
	const premiumMode = options?.premiumMode ?? 'everyone';
	const normalized: LimitConfigSnapshot = {
		traitDefinitions: Array.isArray(config.traitDefinitions) ? config.traitDefinitions : [],
		rules: Array.isArray(config.rules) ? config.rules : [],
	};
	if (!selfHosted || premiumMode === 'mirror') {
		return normalized;
	}
	const traitDefinitions = normalized.traitDefinitions.filter((t) => t !== 'premium');
	const rules = normalized.rules.filter((rule) => {
		const traits = rule.filters?.traits ?? [];
		return !traits.includes('premium');
	});
	return {
		traitDefinitions,
		rules,
	};
}

export function createDefaultLimitConfig(options?: {
	selfHosted?: boolean;
	premiumMode?: 'mirror' | 'everyone';
}): LimitConfigSnapshot {
	const selfHosted = options?.selfHosted ?? false;
	const premiumMode = options?.premiumMode ?? 'everyone';
	const useTiers = !selfHosted || premiumMode === 'mirror';
	const hostedDefault: LimitConfigSnapshot = {
		traitDefinitions: useTiers ? ['premium'] : [],
		rules: useTiers
			? [
					{
						id: LIMIT_RULE_IDS.HOSTED_UPGRADE,
						filters: {traits: ['premium']},
						limits: {...DEFAULT_STOCK_LIMITS},
					},
					{
						id: LIMIT_RULE_IDS.DEFAULT,
						limits: {...DEFAULT_RESTRICTED_LIMITS},
					},
					{
						id: LIMIT_RULE_IDS.VERY_LARGE_GUILD,
						filters: {guildFeatures: [GuildFeatures.VERY_LARGE_GUILD]},
						limits: {max_guild_members: MAX_GUILD_MEMBERS_VERY_LARGE_GUILD},
					},
				]
			: [
					{
						id: LIMIT_RULE_IDS.DEFAULT,
						limits: {...DEFAULT_STOCK_LIMITS},
					},
					{
						id: LIMIT_RULE_IDS.VERY_LARGE_GUILD,
						filters: {guildFeatures: [GuildFeatures.VERY_LARGE_GUILD]},
						limits: {max_guild_members: MAX_GUILD_MEMBERS_VERY_LARGE_GUILD},
					},
				],
	};
	return sanitizeLimitConfigForInstance(cloneLimitConfigSnapshot(hostedDefault), {selfHosted, premiumMode});
}

export function mergeWithCurrentDefaults(
	stored: LimitConfigSnapshot,
	options?: {
		selfHosted?: boolean;
		premiumMode?: 'mirror' | 'everyone';
	},
): LimitConfigSnapshot {
	const selfHosted = options?.selfHosted ?? false;
	const premiumMode = options?.premiumMode ?? 'everyone';
	const newDefaults = createDefaultLimitConfig({selfHosted, premiumMode});
	const defaultBaseRule = newDefaults.rules.find((rule) => rule.id === LIMIT_RULE_IDS.DEFAULT);
	assert(defaultBaseRule, 'Limit configuration defaults must include a default rule');
	const mergedRules: Array<LimitRule> = [];
	const existingRulesMap = new Map<string, LimitRule>();
	for (const rule of stored.rules) {
		existingRulesMap.set(rule.id, rule);
	}
	for (const defaultRule of newDefaults.rules) {
		const existingRule = existingRulesMap.get(defaultRule.id);
		if (!existingRule) {
			mergedRules.push({...defaultRule});
			continue;
		}
		const mergedLimits: Partial<Record<LimitKey, number>> = {...defaultRule.limits};
		const modifiedFields = findModifiedLimits(existingRule.limits, defaultRule.limits);
		for (const key of modifiedFields) {
			mergedLimits[key] = existingRule.limits[key];
		}
		mergedRules.push({
			id: existingRule.id,
			filters: existingRule.filters ?? defaultRule.filters,
			limits: mergedLimits,
			modifiedFields: modifiedFields.length > 0 ? modifiedFields : undefined,
		});
		existingRulesMap.delete(defaultRule.id);
	}
	for (const [, rule] of existingRulesMap) {
		const modifiedFields = findModifiedLimits(rule.limits, defaultBaseRule.limits);
		mergedRules.push({
			id: rule.id,
			filters: rule.filters,
			limits: rule.limits,
			modifiedFields: modifiedFields.length > 0 ? modifiedFields : undefined,
		});
	}
	return {
		traitDefinitions: stored.traitDefinitions,
		rules: mergedRules,
	};
}

function findModifiedLimits(
	currentLimits: Partial<Record<LimitKey, number>>,
	defaultLimits: Partial<Record<LimitKey, number>>,
): Array<LimitKey> {
	return LIMIT_KEYS.filter((key) => {
		const currentValue = currentLimits[key];
		return currentValue !== undefined && currentValue !== defaultLimits[key];
	});
}
