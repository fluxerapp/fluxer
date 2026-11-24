// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RowState, TableLayout} from '@app/api/capture/CaptureTables';
import type cassandra from 'cassandra-driver';

export type JsonValue = null | boolean | number | bigint | string | Array<JsonValue> | {[key: string]: JsonValue};

export interface CdcColumn {
	name: string;
	cqlType: string;
	chType: string;
	materialized?: string;
}

export interface RowExpiry {
	expiresAtMs: number;
	rereadAtMs: number;
}

const CQL_TO_CH: Readonly<Record<string, string>> = {
	text: 'String',
	varchar: 'String',
	ascii: 'String',
	bigint: 'Int64',
	int: 'Int32',
	smallint: 'Int16',
	tinyint: 'Int8',
	varint: 'String',
	counter: 'Int64',
	float: 'Float32',
	double: 'Float64',
	decimal: 'String',
	boolean: 'UInt8',
	timestamp: 'Nullable(DateTime64(3))',
	date: 'Nullable(Date32)',
	time: 'Int64',
	uuid: 'String',
	timeuuid: 'String',
	inet: 'String',
	blob: 'String',
	duration: 'String',
};

const MIN_TIMESTAMP_MS = -62_135_596_800_000;
const MAX_TIMESTAMP_MS = 253_402_300_799_999;

export function clickHouseType(cqlType: string): string {
	const type = cqlType.trim();
	if (type.startsWith('frozen<')) return clickHouseType(type.slice(7, -1));
	return CQL_TO_CH[type] ?? 'String';
}

export function isCollectionType(cqlType: string): boolean {
	return clickHouseType(cqlType) === 'String' && !(cqlType.trim() in CQL_TO_CH);
}

export function timestampText(ms: number): string | null {
	if (!Number.isFinite(ms) || ms < MIN_TIMESTAMP_MS || ms > MAX_TIMESTAMP_MS) return null;
	return new Date(ms).toISOString().slice(0, 23).replace('T', ' ');
}

function isPlainObject(value: object): boolean {
	return Object.getPrototypeOf(value) === Object.prototype;
}

function nonFinite(value: number, spaced: boolean): string {
	if (Number.isNaN(value)) return spaced ? 'NaN' : '"nan"';
	if (value > 0) return spaced ? 'Infinity' : '"inf"';
	return spaced ? '-Infinity' : '"-inf"';
}

function jsonText(value: JsonValue, spaced: boolean): string {
	if (value === null) return 'null';
	if (typeof value === 'bigint') return value.toString();
	if (typeof value === 'number') return Number.isFinite(value) ? JSON.stringify(value) : nonFinite(value, spaced);
	if (typeof value !== 'object') return JSON.stringify(value);
	const comma = spaced ? ', ' : ',';
	if (Array.isArray(value)) return `[${value.map((entry) => jsonText(entry, spaced)).join(comma)}]`;
	const colon = spaced ? ': ' : ':';
	const entries = Object.entries(value).map(
		([key, entry]) => `${JSON.stringify(key)}${colon}${jsonText(entry, spaced)}`,
	);
	return `{${entries.join(comma)}}`;
}

export class UnmappableValue extends Error {
	override readonly name = 'UnmappableValue';
}

export class RowEncoder {
	private readonly types: typeof cassandra.types;

	constructor(types: typeof cassandra.types) {
		this.types = types;
	}

	nested(value: unknown): JsonValue {
		if (value === null || value === undefined) return null;
		if (typeof value === 'boolean') return value ? 1 : 0;
		if (typeof value === 'number' || typeof value === 'bigint' || typeof value === 'string') return value;
		if (typeof value !== 'object') throw new UnmappableValue(`cannot map a ${typeof value} value to ClickHouse`);
		if (Buffer.isBuffer(value)) return value.toString('hex');
		if (value instanceof Date) return timestampText(value.getTime());
		if (Array.isArray(value) || value instanceof Set) return [...value].map((entry) => this.nested(entry));
		if (value instanceof Map) {
			return Object.fromEntries([...value].map(([key, entry]) => [this.mapKey(key), this.nested(entry)]));
		}
		if (value instanceof this.types.Tuple) return value.elements.map((entry) => this.nested(entry));
		if (value instanceof this.types.LocalTime) return BigInt(value.getTotalNanoseconds().toString());
		if (isPlainObject(value)) {
			return Object.fromEntries(Object.entries(value).map(([key, entry]) => [key, this.nested(entry)]));
		}
		return String(value);
	}

	column(column: CdcColumn, value: unknown): JsonValue | undefined {
		if (value === null || value === undefined) return undefined;
		const mapped = this.nested(value);
		if (column.chType !== 'String' || typeof mapped === 'string') return mapped;
		return typeof mapped === 'bigint' || typeof mapped === 'number' ? String(mapped) : jsonText(mapped, true);
	}

	rowLine(
		columns: ReadonlyArray<CdcColumn>,
		row: RowState,
		version: number,
		expiry: RowExpiry,
		derived: Readonly<Record<string, JsonValue>> = {},
	): string {
		const values: Record<string, JsonValue> = {};
		for (const column of columns) {
			if (column.materialized !== undefined) continue;
			if (column.name in derived) {
				values[column.name] = derived[column.name];
				continue;
			}
			const value = this.column(column, column.name in row.key ? row.key[column.name] : row.cells[column.name]?.v);
			if (value !== undefined) values[column.name] = value;
		}
		values['_version'] = BigInt(version);
		values['_deleted'] = 0;
		if (expiry.expiresAtMs > 0) values['_expires_at'] = timestampText(expiry.expiresAtMs);
		if (expiry.rereadAtMs > 0) values['_reread_at'] = timestampText(expiry.rereadAtMs);
		return jsonText(values, false);
	}

	tombstoneLine(keyColumns: ReadonlyArray<CdcColumn>, key: Readonly<Record<string, unknown>>, version: number): string {
		const values: Record<string, JsonValue> = {};
		for (const column of keyColumns) {
			const value = this.column(column, key[column.name]);
			if (value !== undefined) values[column.name] = value;
		}
		values['_version'] = BigInt(version);
		values['_deleted'] = 1;
		return jsonText(values, false);
	}

	keyText(column: CdcColumn, value: unknown): string {
		const mapped = this.column(column, value);
		return mapped === undefined || mapped === null ? '' : String(mapped);
	}

	fromClickHouse(cqlType: string, value: unknown): unknown {
		const type = cqlType.replace(/^frozen<(.*)>$/, '$1');
		const text = String(value);
		switch (type) {
			case 'bigint':
			case 'counter':
			case 'varint':
				return BigInt(text);
			case 'int':
			case 'smallint':
			case 'tinyint':
			case 'float':
			case 'double':
				return Number(text);
			case 'boolean':
				return text === '1' || text === 'true';
			case 'timestamp':
				return new Date(`${text.replace(' ', 'T')}Z`);
			case 'date':
				return this.types.LocalDate.fromString(text);
			case 'timeuuid':
				return this.types.TimeUuid.fromString(text);
			case 'blob':
				return Buffer.from(text, 'hex');
			default:
				return text;
		}
	}

	private mapKey(key: unknown): string {
		const mapped = this.nested(key);
		return typeof mapped === 'string' ? mapped : jsonText(mapped, true);
	}
}

export function rowVersion(recordTs: number, row: RowState | null, readStartUs: number): number {
	let version = Math.max(recordTs, readStartUs);
	for (const cell of Object.values(row?.cells ?? {})) version = Math.max(version, cell.wt);
	return version;
}

export function rowExpiry(layout: TableLayout, row: RowState, readStartMs: number): RowExpiry {
	const cells = Object.entries(row.cells);
	if (cells.length === 0) {
		return {expiresAtMs: 0, rereadAtMs: layout.defaultTtl > 0 ? readStartMs + layout.defaultTtl * 1000 : 0};
	}
	const ttls = cells.flatMap(([, cell]) => (cell.ttl === null ? [] : [cell.ttl]));
	if (ttls.length === 0) return {expiresAtMs: 0, rereadAtMs: 0};
	const smallest = readStartMs + Math.min(...ttls) * 1000;
	const largest = readStartMs + Math.max(...ttls) * 1000;
	const partial =
		ttls.length < cells.length || cells.some(([name, cell]) => cell.ttl !== null && layout.multiCell.has(name));
	if (partial) return {expiresAtMs: 0, rereadAtMs: smallest};
	return {expiresAtMs: largest, rereadAtMs: smallest === largest ? 0 : smallest};
}
