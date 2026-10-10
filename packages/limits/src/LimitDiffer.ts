// SPDX-License-Identifier: AGPL-3.0-or-later

import {LIMIT_KEYS, type LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';
import {DEFAULT_FREE_LIMITS} from '@fluxer/limits/src/LimitDefaults';
import {computeDefaultsHash} from '@fluxer/limits/src/LimitHashing';
import type {LimitConfigSnapshot, LimitConfigWireFormat} from '@fluxer/limits/src/LimitTypes';

const WIRE_COMPATIBILITY_LIMIT_KEYS: ReadonlySet<LimitKey> = new Set<LimitKey>([
	'max_guild_emojis',
	'max_guild_emojis_animated_more',
	'max_guild_emojis_animated',
	'max_guild_emojis_static_more',
	'max_guild_emojis_static',
	'max_guild_stickers_more',
	'max_guild_stickers',
]);

export function computeOverrides(
	fullLimits: Partial<Record<LimitKey, number>>,
	defaults: Record<LimitKey, number>,
): Partial<Record<LimitKey, number>> {
	const overrides: Partial<Record<LimitKey, number>> = {};
	for (const key of LIMIT_KEYS) {
		const value = fullLimits[key];
		if (value === undefined) {
			continue;
		}
		if (value !== defaults[key]) {
			overrides[key] = value;
		}
	}
	return overrides;
}

export function computeWireFormat(config: LimitConfigSnapshot): LimitConfigWireFormat {
	const defaultsHash = computeDefaultsHash();
	const rules = config.rules.map((rule) => {
		const overrides = computeOverrides(rule.limits, DEFAULT_FREE_LIMITS);
		for (const key of WIRE_COMPATIBILITY_LIMIT_KEYS) {
			const value = rule.limits[key];
			if (value !== undefined) {
				overrides[key] = value;
			}
		}
		return {
			id: rule.id,
			filters: rule.filters,
			overrides,
		};
	});
	return {
		version: 2,
		traitDefinitions: config.traitDefinitions,
		rules,
		defaultsHash,
	};
}

export function expandWireFormat(wireFormat: LimitConfigWireFormat): LimitConfigSnapshot {
	const rules = wireFormat.rules.map((rule) => {
		const limits: Partial<Record<LimitKey, number>> = {
			...DEFAULT_FREE_LIMITS,
			...rule.overrides,
		};
		return {
			id: rule.id,
			filters: rule.filters,
			limits,
		};
	});
	return {
		version: wireFormat.version,
		traitDefinitions: wireFormat.traitDefinitions,
		rules,
	};
}
