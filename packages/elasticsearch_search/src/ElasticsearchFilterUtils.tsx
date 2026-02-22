/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

export type ElasticsearchFilter = Record<string, unknown>;

export interface ElasticsearchRangeOptions {
	gte?: number;
	lte?: number;
	gt?: number;
	lt?: number;
}

export function esTermFilter(field: string, value: string | number | boolean): ElasticsearchFilter {
	return {term: {[field]: value}};
}

export function esTermsFilter(field: string, values: Array<string | number | boolean>): ElasticsearchFilter {
	return {terms: {[field]: values}};
}

export function esRangeFilter(field: string, opts: ElasticsearchRangeOptions): ElasticsearchFilter {
	return {range: {[field]: opts}};
}

export function esExistsFilter(field: string): ElasticsearchFilter {
	return {exists: {field}};
}

export function esNotExistsFilter(field: string): ElasticsearchFilter {
	return {bool: {must_not: [{exists: {field}}]}};
}

export function esMustNotTerm(field: string, value: string | number | boolean): ElasticsearchFilter {
	return {bool: {must_not: [{term: {[field]: value}}]}};
}

export function esMustNotTerms(field: string, values: Array<string | number | boolean>): ElasticsearchFilter {
	return {bool: {must_not: [{terms: {[field]: values}}]}};
}

export function esAndTerms(field: string, values: Array<string | number | boolean>): Array<ElasticsearchFilter> {
	return values.map((v) => esTermFilter(field, v));
}

export function esExcludeAny(field: string, values: Array<string | number | boolean>): Array<ElasticsearchFilter> {
	return values.map((v) => esMustNotTerm(field, v));
}

export function compactFilters(filters: Array<ElasticsearchFilter | undefined>): Array<ElasticsearchFilter> {
	return filters.filter((f): f is ElasticsearchFilter => f !== undefined);
}
