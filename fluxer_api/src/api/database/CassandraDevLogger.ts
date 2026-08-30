// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '../Logger';
import {getIsDev} from './CassandraMetaRegistry';
import {getStatementMeta} from './CassandraTypes';

const colors = {
	reset: '\x1b[0m',
	dim: '\x1b[2m',
	bold: '\x1b[1m',
	cyan: '\x1b[36m',
	yellow: '\x1b[33m',
	green: '\x1b[32m',
	magenta: '\x1b[35m',
	blue: '\x1b[34m',
	gray: '\x1b[90m',
	white: '\x1b[37m',
} as const;

function formatValue(value: unknown): string {
	if (value === null) return `${colors.dim}null${colors.reset}`;
	if (value === undefined) return `${colors.dim}undefined${colors.reset}`;
	if (typeof value === 'string') {
		const truncated = value.length > 50 ? `${value.slice(0, 50)}...` : value;
		return `${colors.green}"${truncated}"${colors.reset}`;
	}
	if (typeof value === 'number' || typeof value === 'bigint') {
		return `${colors.yellow}${value}${colors.reset}`;
	}
	if (typeof value === 'boolean') {
		return `${colors.magenta}${value}${colors.reset}`;
	}
	if (value instanceof Date) {
		return `${colors.cyan}Date(${value.toISOString()})${colors.reset}`;
	}
	if (value instanceof Buffer) {
		return `${colors.dim}Buffer(${value.length} bytes)${colors.reset}`;
	}
	if (value instanceof Set) {
		if (value.size === 0) return `${colors.dim}Set{}${colors.reset}`;
		if (value.size > 3) return `${colors.dim}Set{${value.size} items}${colors.reset}`;
		const items = [...value].map((v) => formatValue(v)).join(', ');
		return `Set{${items}}`;
	}
	if (value instanceof Map) {
		if (value.size === 0) return `${colors.dim}Map{}${colors.reset}`;
		return `${colors.dim}Map{${value.size} entries}${colors.reset}`;
	}
	if (Array.isArray(value)) {
		if (value.length === 0) return `${colors.dim}[]${colors.reset}`;
		if (value.length > 5) return `${colors.dim}[${value.length} items]${colors.reset}`;
		const items = value.map((v) => formatValue(v)).join(', ');
		return `[${items}]`;
	}
	if (typeof value === 'object') {
		const entries = Object.entries(value as Record<string, unknown>);
		if (entries.length === 0) return `${colors.dim}{}${colors.reset}`;
		if (entries.length > 5) return `${colors.dim}{${entries.length} keys}${colors.reset}`;
		const formatted = entries.map(([k, v]) => `${k}: ${formatValue(v)}`).join(', ');
		return `{${formatted}}`;
	}
	return String(value);
}

function formatParams(params: Record<string, unknown>): string {
	const entries = Object.entries(params);
	if (entries.length === 0) return `${colors.dim}(no params)${colors.reset}`;
	return entries.map(([k, v]) => `  ${colors.blue}:${k}${colors.reset} = ${formatValue(v)}`).join('\n');
}

function formatCql(cql: string): string {
	return cql
		.replace(/\s+/g, ' ')
		.replace(/\s*;\s*$/, '')
		.trim();
}

export function logQuery(
	queryType: string,
	cql: string,
	params: Record<string, unknown>,
	durationMs: number,
	rowCount?: number,
): void {
	if (!getIsDev()) return;
	const typeColors: Record<string, string> = {
		SELECT: colors.cyan,
		INSERT: colors.green,
		UPDATE: colors.yellow,
		DELETE: colors.magenta,
		BATCH: colors.blue,
		QUERY: colors.white,
	};
	const typeColor = typeColors[queryType] || colors.white;
	const durationColor = durationMs > 100 ? colors.yellow : durationMs > 50 ? colors.dim : colors.green;
	const lines = [
		`${colors.dim}\u250c\u2500\u2500${colors.reset} ${typeColor}${colors.bold}${queryType}${colors.reset} ${colors.dim}\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500${colors.reset}`,
		`${colors.dim}\u2502${colors.reset} ${formatCql(cql)}`,
		`${colors.dim}\u2502${colors.reset}`,
		...formatParams(params)
			.split('\n')
			.map((line) => `${colors.dim}\u2502${colors.reset}${line}`),
		`${colors.dim}\u2502${colors.reset}`,
		`${colors.dim}\u2514\u2500\u2500${colors.reset} ${durationColor}${durationMs.toFixed(2)}ms${colors.reset}${rowCount !== undefined ? ` ${colors.dim}(${rowCount} rows)${colors.reset}` : ''}`,
	];
	Logger.debug(lines.join('\n'));
}

export function logBatch(
	queries: Array<{
		query: string;
		params: object;
	}>,
	durationMs: number,
): void {
	if (!getIsDev()) return;
	const lines = [
		`${colors.dim}\u250c\u2500\u2500${colors.reset} ${colors.blue}${colors.bold}BATCH${colors.reset} ${colors.dim}(${queries.length} queries) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500${colors.reset}`,
	];
	for (let i = 0; i < queries.length; i++) {
		const {query, params} = queries[i];
		const queryType = getStatementMeta(query).type;
		const typeColors: Record<string, string> = {
			SELECT: colors.cyan,
			INSERT: colors.green,
			UPDATE: colors.yellow,
			DELETE: colors.magenta,
		};
		const typeColor = typeColors[queryType] || colors.white;
		lines.push(
			`${colors.dim}\u2502${colors.reset} ${colors.dim}[${i + 1}]${colors.reset} ${typeColor}${queryType}${colors.reset} ${formatCql(query)}`,
		);
		const paramEntries = Object.entries(params as Record<string, unknown>);
		if (paramEntries.length > 0 && paramEntries.length <= 4) {
			const paramStr = paramEntries.map(([k, v]) => `${colors.blue}:${k}${colors.reset}=${formatValue(v)}`).join(' ');
			lines.push(`${colors.dim}\u2502${colors.reset}     ${paramStr}`);
		} else if (paramEntries.length > 4) {
			lines.push(`${colors.dim}\u2502${colors.reset}     ${colors.dim}(${paramEntries.length} params)${colors.reset}`);
		}
	}
	const durationColor = durationMs > 100 ? colors.yellow : durationMs > 50 ? colors.dim : colors.green;
	lines.push(
		`${colors.dim}\u2514\u2500\u2500${colors.reset} ${durationColor}${durationMs.toFixed(2)}ms${colors.reset}`,
	);
	Logger.debug(lines.join('\n'));
}
