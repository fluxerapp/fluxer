// SPDX-License-Identifier: AGPL-3.0-or-later

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

export interface CachedLimitConfig {
	config: LimitConfigSnapshot;
	defaultsHash: string;
}

export function getLimitConfigKvKey(selfHosted: boolean): string {
	return `limit_config:${selfHosted ? 'self_hosted' : 'saas'}`;
}

export const LIMIT_CONFIG_REFRESH_CHANNEL = 'limit-config-refresh';
export const LIMIT_CONFIG_REFRESH_LOCK_KEY = 'limit-config-refresh-lock';

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
		const modifiedFields: Array<LimitKey> = [];
		for (const key of LIMIT_KEYS) {
			const existingValue = existingRule.limits[key];
			const defaultValue = defaultRule.limits[key];
			if (existingValue !== undefined && existingValue !== defaultValue) {
				mergedLimits[key] = existingValue;
				modifiedFields.push(key);
			}
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
		mergedRules.push({
			id: rule.id,
			filters: rule.filters,
			limits: rule.limits,
			modifiedFields: rule.modifiedFields ?? (Object.keys(rule.limits) as Array<LimitKey>),
		});
	}
	return {
		traitDefinitions: stored.traitDefinitions,
		rules: mergedRules,
	};
}
