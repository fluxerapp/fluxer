// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import type {LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';
import {LIMIT_KEY_METADATA, LIMIT_KEYS} from '@fluxer/constants/src/LimitConfigMetadata';
import {MAX_GUILD_MEMBERS_VERY_LARGE_GUILD} from '@fluxer/constants/src/LimitConstants';
import {DEFAULT_RESTRICTED_LIMITS, DEFAULT_STOCK_LIMITS} from '@fluxer/limits/src/LimitDefaults';
import type {LimitConfigSnapshot, LimitRule} from '@fluxer/limits/src/LimitTypes';

const LIMIT_RULE_IDS = {
	DEFAULT: 'default',
	HOSTED_UPGRADE: 'premium',
	VERY_LARGE_GUILD: 'very_large_guild',
} as const;

export const LIMIT_CONFIG_REFRESH_CHANNEL = 'limit-config-refresh';

export interface LimitBuildOptions {
	selfHosted?: boolean;
	premiumMode?: 'mirror' | 'everyone';
}

export interface StoredLimitConfig extends LimitConfigSnapshot {
	overridesOnly?: boolean;
}

function cloneLimitConfigSnapshot(config: LimitConfigSnapshot): LimitConfigSnapshot {
	return structuredClone(config);
}

export function sanitizeLimitConfigForInstance(
	config: LimitConfigSnapshot,
	options?: LimitBuildOptions,
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

export function createDefaultLimitConfig(options?: LimitBuildOptions): LimitConfigSnapshot {
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
	options?: LimitBuildOptions,
): LimitConfigSnapshot {
	const selfHosted = options?.selfHosted ?? false;
	const premiumMode = options?.premiumMode ?? 'everyone';
	const newDefaults = createDefaultLimitConfig({selfHosted, premiumMode});
	const defaultBaseRule = newDefaults.rules.find((rule) => rule.id === LIMIT_RULE_IDS.DEFAULT);
	assert(defaultBaseRule, 'Limit configuration defaults must include a default rule');
	const mergedRules: Array<LimitRule> = [];
	const existingRulesMap = new Map<string, LimitRule>();
	for (const rule of stored.rules) {
		existingRulesMap.set(rule.id, {...rule, limits: clampToAllowedRange(rule.limits)});
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
		traitDefinitions: [...new Set([...newDefaults.traitDefinitions, ...stored.traitDefinitions])],
		rules: mergedRules,
	};
}

export function readStoredLimitOverrides(
	stored: StoredLimitConfig,
	options: Required<LimitBuildOptions>,
): StoredLimitConfig {
	if (!options.selfHosted || stored.overridesOnly) {
		return stored;
	}
	const currentDefaults = builtInRuleLimits(options);
	const otherDefaults = builtInRuleLimits({
		selfHosted: true,
		premiumMode: options.premiumMode === 'mirror' ? 'everyone' : 'mirror',
	});
	return {
		overridesOnly: true,
		traitDefinitions: stored.traitDefinitions,
		rules: stored.rules.map((rule) => {
			const current = currentDefaults.get(rule.id);
			const other = otherDefaults.get(rule.id);
			const savedUnder = current && !(other && wasSavedUnderOtherDefaults(rule, current, other)) ? current : other;
			return {
				id: rule.id,
				filters: rule.filters,
				limits: savedUnder ? pickModifiedLimits(rule.limits, savedUnder) : rule.limits,
			};
		}),
	};
}

export function buildStoredLimitConfig(
	submitted: LimitConfigSnapshot,
	previous: StoredLimitConfig | null,
	options: Required<LimitBuildOptions>,
): StoredLimitConfig {
	const visible = sanitizeLimitConfigForInstance(submitted, options);
	if (!options.selfHosted) {
		return mergeWithCurrentDefaults(visible, options);
	}
	const kept = previous === null ? null : readStoredLimitOverrides(previous, options);
	const keptRules = new Map((kept?.rules ?? []).map((rule) => [rule.id, rule]));
	const defaults = builtInRuleLimits(options);
	const rules: Array<LimitRule> = visible.rules.map((rule) => {
		const ruleDefaults = defaults.get(rule.id);
		const keptLimits = keptRules.get(rule.id)?.limits ?? {};
		return {
			id: rule.id,
			filters: rule.filters,
			limits: ruleDefaults
				? pickLimits(rule.limits, (key, value) => value !== ruleDefaults[key] || value === keptLimits[key])
				: rule.limits,
		};
	});
	const hidden = kept === null ? null : hiddenFromInstance(kept, options);
	const visibleRuleIds = new Set(rules.map((rule) => rule.id));
	return {
		overridesOnly: true,
		traitDefinitions: [
			...(hidden?.traitDefinitions ?? []).filter((trait) => !visible.traitDefinitions.includes(trait)),
			...visible.traitDefinitions,
		],
		rules: [...(hidden?.rules ?? []).filter((rule) => !visibleRuleIds.has(rule.id)), ...rules],
	};
}

function hiddenFromInstance(config: LimitConfigSnapshot, options: Required<LimitBuildOptions>): LimitConfigSnapshot {
	const visible = sanitizeLimitConfigForInstance(config, options);
	return {
		traitDefinitions: config.traitDefinitions.filter((trait) => !visible.traitDefinitions.includes(trait)),
		rules: config.rules.filter((rule) => !visible.rules.includes(rule)),
	};
}

function builtInRuleLimits(options?: LimitBuildOptions): Map<string, Partial<Record<LimitKey, number>>> {
	return new Map(createDefaultLimitConfig(options).rules.map((rule) => [rule.id, rule.limits]));
}

function wasSavedUnderOtherDefaults(
	rule: LimitRule,
	currentDefaults: Partial<Record<LimitKey, number>>,
	otherDefaults: Partial<Record<LimitKey, number>>,
): boolean {
	const modeKeys = LIMIT_KEYS.filter((key) => currentDefaults[key] !== otherDefaults[key]);
	const edited = new Set<string>(rule.modifiedFields ?? []);
	const untouchedModeKeys = modeKeys.filter((key) => !edited.has(key));
	if (untouchedModeKeys.length > 0) {
		return untouchedModeKeys.every((key) => rule.limits[key] === otherDefaults[key]);
	}
	return (
		modeKeys.some((key) => rule.limits[key] === otherDefaults[key]) &&
		modeKeys.every((key) => rule.limits[key] !== undefined && rule.limits[key] !== currentDefaults[key])
	);
}

function clampToAllowedRange(limits: Partial<Record<LimitKey, number>>): Partial<Record<LimitKey, number>> {
	const clamped = {...limits};
	for (const key of LIMIT_KEYS) {
		const value = clamped[key];
		const {min = 0, max} = LIMIT_KEY_METADATA[key];
		if (value !== undefined && max !== undefined) {
			clamped[key] = Math.min(Math.max(value, min), max);
		}
	}
	return clamped;
}

function pickModifiedLimits(
	limits: Partial<Record<LimitKey, number>>,
	defaultLimits: Partial<Record<LimitKey, number>>,
): Partial<Record<LimitKey, number>> {
	return pickLimits(limits, (key, value) => value !== defaultLimits[key]);
}

function pickLimits(
	limits: Partial<Record<LimitKey, number>>,
	keep: (key: LimitKey, value: number) => boolean,
): Partial<Record<LimitKey, number>> {
	const picked: Partial<Record<LimitKey, number>> = {};
	for (const key of LIMIT_KEYS) {
		const value = limits[key];
		if (value !== undefined && keep(key, value)) {
			picked[key] = value;
		}
	}
	return picked;
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
