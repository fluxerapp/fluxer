import type {StoredLimitConfig} from '@app/api/constants/LimitConfig';
import {isJsonRecord, isStringArray} from '@app/api/utils/JsonBoundaryUtils';
import type {LimitRule} from '@fluxer/limits/src/LimitTypes';

function isLimitRuleSnapshot(value: unknown): value is LimitRule {
	if (!isJsonRecord(value) || typeof value.id !== 'string' || !isJsonRecord(value.limits)) return false;
	const filters = value.filters;
	return (
		(filters === undefined ||
			(isJsonRecord(filters) &&
				(filters.traits === undefined || isStringArray(filters.traits)) &&
				(filters.guildFeatures === undefined || isStringArray(filters.guildFeatures)))) &&
		(value.modifiedFields === undefined || isStringArray(value.modifiedFields))
	);
}

export function isStoredLimitConfig(value: unknown): value is StoredLimitConfig {
	return (
		isJsonRecord(value) &&
		(value.version === undefined || typeof value.version === 'number') &&
		(value.overridesOnly === undefined || typeof value.overridesOnly === 'boolean') &&
		isStringArray(value.traitDefinitions) &&
		Array.isArray(value.rules) &&
		value.rules.every(isLimitRuleSnapshot)
	);
}
