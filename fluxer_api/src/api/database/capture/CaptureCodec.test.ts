// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFileSync} from 'node:fs';
import {CaptureCodec, type CaptureValue} from '@app/api/database/capture/CaptureCodec';
import cassandra from 'cassandra-driver';
import {describe, expect, it} from 'vitest';

interface Schema {
	user_types: Array<{name: string; fields: Array<{name: string; type: string}>}>;
	tables: Array<{columns: Array<{type: string}>}>;
}

const schema = JSON.parse(
	readFileSync(new URL('../../../../../tools/dev/cassandra_target_schema.json', import.meta.url), 'utf8'),
) as Schema;
const records = JSON.parse(readFileSync(new URL('./fixtures/records.json', import.meta.url), 'utf8')) as Array<{
	statements?: Array<{p: Record<string, CaptureValue>}>;
}>;

const codec = new CaptureCodec(cassandra.types);

function splitTypeArguments(inner: string): Array<string> {
	const parts: Array<string> = [];
	let depth = 0;
	let start = 0;
	for (const [index, char] of [...inner].entries()) {
		if (char === '<') depth++;
		if (char === '>') depth--;
		if (char === ',' && depth === 0) {
			parts.push(inner.slice(start, index).trim());
			start = index + 1;
		}
	}
	parts.push(inner.slice(start).trim());
	return parts;
}

function sample(type: string): unknown {
	const generic = /^(\w+)<(.+)>$/.exec(type);
	if (generic) {
		const [, name, inner] = generic as unknown as [string, string, string];
		const args = splitTypeArguments(inner);
		switch (name) {
			case 'frozen':
				return sample(inner);
			case 'list':
				return [sample(args[0] ?? ''), sample(args[0] ?? '')];
			case 'set':
				return new Set([sample(args[0] ?? '')]);
			case 'map':
				return new Map([[sample(args[0] ?? ''), sample(args[1] ?? '')]]);
			case 'tuple':
				return cassandra.types.Tuple.fromArray(args.map(sample));
		}
	}
	switch (type) {
		case 'text':
			return 'fluxer';
		case 'bigint':
		case 'varint':
			return 9_007_199_254_740_993n;
		case 'int':
			return -7;
		case 'double':
			return 0.1;
		case 'boolean':
			return true;
		case 'blob':
			return Buffer.from([0, 1, 255]);
		case 'timestamp':
			return new Date(1_763_750_000_123);
		case 'date':
			return cassandra.types.LocalDate.fromString('2001-02-03');
		case 'timeuuid':
			return cassandra.types.TimeUuid.now();
	}
	const udt = schema.user_types.find((candidate) => candidate.name === type);
	if (!udt) throw new Error(`No sample for ${type}`);
	return Object.fromEntries(udt.fields.map((field) => [field.name, sample(field.type)]));
}

function roundTrip(value: unknown): unknown {
	return codec.decode(JSON.parse(JSON.stringify(codec.encode(value))) as CaptureValue);
}

describe('capture codec', () => {
	it('round-trips a value of every column type in the schema', () => {
		const types = new Set(schema.tables.flatMap((table) => table.columns.map((column) => column.type)));
		for (const type of types) {
			const value = sample(type);
			expect(roundTrip(value), type).toEqual(value);
		}
	});

	it('records an array with an index getter as a tuple and replays it as one', () => {
		const item = Object.defineProperty(['si_1', 'price_1', 1, 1890n], 'get', {
			value(index: number) {
				return item[index];
			},
			enumerable: false,
		});
		const encoded = codec.encode([item]);
		expect(encoded).toEqual([{$t: 'tuple', v: ['si_1', 'price_1', 1, {$t: 'i64', v: '1890'}]}]);
		const decoded = codec.decode(encoded) as Array<cassandra.types.Tuple>;
		expect(decoded[0]).toBeInstanceOf(cassandra.types.Tuple);
		expect(decoded[0]?.get(3)).toBe(1890n);
	});

	it('gives decoded lists an index getter so tuple columns recorded as lists still replay', () => {
		const decoded = codec.decode([['si_1', 'price_1', 1, {$t: 'i64', v: '1890'}]]) as Array<{
			get(index: number): unknown;
		}>;
		expect(decoded[0]?.get(0)).toBe('si_1');
		expect(decoded[0]?.get(3)).toBe(1890n);
		expect(Object.keys(decoded[0] ?? {})).toEqual(['0', '1', '2', '3']);
		expect(codec.encode(decoded)).toEqual([['si_1', 'price_1', 1, {$t: 'i64', v: '1890'}]]);
	});

	it('round-trips floats JSON cannot represent', () => {
		for (const value of [Number.NaN, Number.POSITIVE_INFINITY, Number.NEGATIVE_INFINITY]) {
			expect(roundTrip(value)).toBe(value);
		}
		expect(Object.is(roundTrip(-0), -0)).toBe(true);
	});

	it('refuses values it cannot type', () => {
		expect(() => codec.encode(undefined)).toThrow();
		expect(() => codec.encode(new (class Opaque {})())).toThrow();
	});

	it('decodes and re-encodes the shared fixture unchanged', () => {
		for (const record of records) {
			for (const statement of record.statements ?? []) {
				expect(codec.encodeRecord(codec.decodeRecord(statement.p))).toEqual(statement.p);
			}
		}
	});
});
