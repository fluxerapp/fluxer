// SPDX-License-Identifier: AGPL-3.0-or-later

import {z} from 'zod';

const FNV_OFFSET_BASIS_32 = 0x811c9dc5;
const FNV_PRIME_32 = 0x01000193;

export const EXPERIMENT_BUCKET_RESOLUTION = 10000;
const EXPERIMENT_MAX_ROLLOUT_COUNTRY_CODES = 250;

export const ExperimentRolloutCountryCodesSchema = z
	.array(z.string().regex(/^[A-Z]{2}$/u))
	.max(EXPERIMENT_MAX_ROLLOUT_COUNTRY_CODES)
	.refine((countryCodes) => new Set(countryCodes).size === countryCodes.length, {
		message: 'Country codes must be unique',
	});

export function experimentBucket(userId: string, salt: string): number {
	let hash = FNV_OFFSET_BASIS_32;
	const input = `${salt}:${userId}`;
	for (let index = 0; index < input.length; index++) {
		hash ^= input.charCodeAt(index) & 0xff;
		hash = Math.imul(hash, FNV_PRIME_32) >>> 0;
	}
	return hash % EXPERIMENT_BUCKET_RESOLUTION;
}

export interface ExperimentTargeting {
	readonly memberGuildIds: ReadonlySet<string>;
	readonly premium: boolean;
	readonly countryCode: string | null;
}

interface ExperimentAudienceRules {
	readonly included_guild_ids: ReadonlyArray<string>;
	readonly include_premium_users: boolean;
}

export function experimentAudienceIncludes(rules: ExperimentAudienceRules, targeting: ExperimentTargeting): boolean {
	if (rules.include_premium_users && targeting.premium) return true;
	return rules.included_guild_ids.some((guildId) => targeting.memberGuildIds.has(guildId));
}

interface ExperimentRolloutCountryRules {
	readonly rollout_country_codes: ReadonlyArray<string>;
}

export function experimentRolloutCountryIncludes(
	rules: ExperimentRolloutCountryRules,
	targeting: ExperimentTargeting,
): boolean {
	if (rules.rollout_country_codes.length === 0) return true;
	return targeting.countryCode !== null && rules.rollout_country_codes.includes(targeting.countryCode);
}
