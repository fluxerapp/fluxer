// SPDX-License-Identifier: AGPL-3.0-or-later

export type MeilisearchFilter = string;

type ScalarFilterValue = string | number | boolean;

interface MeilisearchRangeOptions {
	gte?: number | string;
	lte?: number | string;
	gt?: number | string;
	lt?: number | string;
}

function quoteIdentifier(field: string): string {
	return field
		.split('.')
		.map((part) => {
			if (/^[A-Za-z_][A-Za-z0-9_]*$/u.test(part)) {
				return part;
			}
			return JSON.stringify(part);
		})
		.join('.');
}

function formatValue(value: ScalarFilterValue): string {
	return typeof value === 'string' ? JSON.stringify(value) : String(value);
}

export function meiliTermFilter(field: string, value: ScalarFilterValue): MeilisearchFilter {
	return `${quoteIdentifier(field)} = ${formatValue(value)}`;
}

export function meiliTermsFilter(field: string, values: Array<ScalarFilterValue>): MeilisearchFilter | undefined {
	if (values.length === 0) {
		return undefined;
	}
	return `(${values.map((value) => meiliTermFilter(field, value)).join(' OR ')})`;
}

export function meiliRangeFilter(field: string, options: MeilisearchRangeOptions): MeilisearchFilter | undefined {
	const fieldName = quoteIdentifier(field);
	const clauses: Array<string> = [];
	if (options.gte !== undefined) clauses.push(`${fieldName} >= ${formatValue(options.gte)}`);
	if (options.lte !== undefined) clauses.push(`${fieldName} <= ${formatValue(options.lte)}`);
	if (options.gt !== undefined) clauses.push(`${fieldName} > ${formatValue(options.gt)}`);
	if (options.lt !== undefined) clauses.push(`${fieldName} < ${formatValue(options.lt)}`);
	if (clauses.length === 0) {
		return undefined;
	}
	return clauses.length === 1 ? clauses[0]! : `(${clauses.join(' AND ')})`;
}

export function meiliExistsFilter(field: string): MeilisearchFilter {
	const fieldName = quoteIdentifier(field);
	return `(${fieldName} EXISTS AND ${fieldName} IS NOT NULL)`;
}

export function meiliNotExistsFilter(field: string): MeilisearchFilter {
	const fieldName = quoteIdentifier(field);
	return `(${fieldName} NOT EXISTS OR ${fieldName} IS NULL)`;
}

export function meiliAndTerms(field: string, values: Array<ScalarFilterValue>): Array<MeilisearchFilter> {
	return values.map((value) => meiliTermFilter(field, value));
}

export function meiliExcludeAny(field: string, values: Array<ScalarFilterValue>): Array<MeilisearchFilter> {
	return values.map((value) => `NOT ${meiliTermFilter(field, value)}`);
}

export function compactMeiliFilters(filters: Array<MeilisearchFilter | undefined>): Array<MeilisearchFilter> {
	return filters.filter((filter): filter is MeilisearchFilter => filter !== undefined);
}

export function joinMeiliFilters(filters: Array<MeilisearchFilter | undefined>): string | undefined {
	const compacted = compactMeiliFilters(filters);
	if (compacted.length === 0) {
		return undefined;
	}
	return compacted.map((filter) => `(${filter})`).join(' AND ');
}
