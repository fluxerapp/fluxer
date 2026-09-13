import {isJsonRecord} from '@app/api/utils/JsonBoundaryUtils';
import type {LimitConfigSnapshot, LimitRule} from '@fluxer/limits/src/LimitTypes';

function isStringArray(value: unknown): value is Array<string> {
	return Array.isArray(value) && value.every((entry) => typeof entry === 'string');
}

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

export function isLimitConfigSnapshot(value: unknown): value is LimitConfigSnapshot {
	return (
		isJsonRecord(value) &&
		(value.version === undefined || typeof value.version === 'number') &&
		isStringArray(value.traitDefinitions) &&
		Array.isArray(value.rules) &&
		value.rules.every(isLimitRuleSnapshot)
	);
}
