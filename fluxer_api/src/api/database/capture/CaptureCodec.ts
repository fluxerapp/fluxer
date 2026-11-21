// SPDX-License-Identifier: AGPL-3.0-or-later

import type cassandra from 'cassandra-driver';

export type CaptureValue = null | string | number | boolean | Array<CaptureValue> | TaggedValue;

type TaggedValue =
	| {$t: 'i64' | 'blob' | 'timeuuid' | 'date' | 'f'; v: string}
	| {$t: 'ts'; v: number}
	| {$t: 'set' | 'tuple'; v: Array<CaptureValue>}
	| {$t: 'map'; v: Array<[CaptureValue, CaptureValue]>}
	| {$t: 'udt'; v: Record<string, CaptureValue>};

type DriverTypes = typeof cassandra.types;

function isPlainObject(value: object): boolean {
	return Object.getPrototypeOf(value) === Object.prototype;
}

function encodeNumber(value: number): CaptureValue {
	if (Number.isFinite(value) && !Object.is(value, -0)) return value;
	return {$t: 'f', v: Object.is(value, -0) ? '-0' : String(value)};
}

function decodeNumber(value: string): number {
	switch (value) {
		case '-0':
			return -0;
		case 'NaN':
			return Number.NaN;
		case 'Infinity':
			return Number.POSITIVE_INFINITY;
		case '-Infinity':
			return Number.NEGATIVE_INFINITY;
		default:
			throw new Error(`Unknown captured float ${JSON.stringify(value)}`);
	}
}

export class CaptureCodec {
	private readonly types: DriverTypes;

	constructor(types: DriverTypes) {
		this.types = types;
	}

	encode(value: unknown): CaptureValue {
		if (value === null || typeof value === 'string' || typeof value === 'boolean') return value;
		if (typeof value === 'number') return encodeNumber(value);
		if (typeof value === 'bigint') return {$t: 'i64', v: value.toString()};
		if (Array.isArray(value)) {
			const entries = value.map((entry) => this.encode(entry));
			return isTupleArray(value) ? {$t: 'tuple', v: entries} : entries;
		}
		if (typeof value !== 'object') throw new Error(`Cannot capture a ${typeof value} value`);
		if (Buffer.isBuffer(value)) return {$t: 'blob', v: value.toString('base64')};
		if (value instanceof Date) return {$t: 'ts', v: value.getTime()};
		if (value instanceof Set) return {$t: 'set', v: [...value].map((entry) => this.encode(entry))};
		if (value instanceof Map) {
			return {$t: 'map', v: [...value].map(([key, entry]) => [this.encode(key), this.encode(entry)])};
		}
		if (value instanceof this.types.TimeUuid) return {$t: 'timeuuid', v: value.toString()};
		if (value instanceof this.types.LocalDate) return {$t: 'date', v: value.toString()};
		if (value instanceof this.types.Tuple) return {$t: 'tuple', v: value.elements.map((entry) => this.encode(entry))};
		if (isPlainObject(value)) {
			return {
				$t: 'udt',
				v: Object.fromEntries(Object.entries(value).map(([key, entry]) => [key, this.encode(entry)])),
			};
		}
		throw new Error(`Cannot capture a ${value.constructor.name} value`);
	}

	decode(value: CaptureValue): unknown {
		if (value === null || typeof value !== 'object') return value;
		if (Array.isArray(value)) return withIndexGetter(value.map((entry) => this.decode(entry)));
		switch (value.$t) {
			case 'i64':
				return BigInt(value.v);
			case 'blob':
				return Buffer.from(value.v, 'base64');
			case 'ts':
				return new Date(value.v);
			case 'timeuuid':
				return this.types.TimeUuid.fromString(value.v);
			case 'date':
				return this.types.LocalDate.fromString(value.v);
			case 'f':
				return decodeNumber(value.v);
			case 'set':
				return new Set(value.v.map((entry) => this.decode(entry)));
			case 'tuple':
				return this.types.Tuple.fromArray(value.v.map((entry) => this.decode(entry)));
			case 'map':
				return new Map(value.v.map(([key, entry]) => [this.decode(key), this.decode(entry)]));
			case 'udt':
				return Object.fromEntries(Object.entries(value.v).map(([key, entry]) => [key, this.decode(entry)]));
		}
	}

	encodeRecord(values: Readonly<Record<string, unknown>>): Record<string, CaptureValue> {
		return Object.fromEntries(Object.entries(values).map(([key, value]) => [key, this.encode(value)]));
	}

	decodeRecord(values: Readonly<Record<string, CaptureValue>>): Record<string, unknown> {
		return Object.fromEntries(Object.entries(values).map(([key, value]) => [key, this.decode(value)]));
	}
}

const decodedArrays = new WeakSet<Array<unknown>>();

function withIndexGetter(entries: Array<unknown>): Array<unknown> {
	Object.defineProperty(entries, 'get', {value: (index: number) => entries[index], enumerable: false});
	decodedArrays.add(entries);
	return entries;
}

function isTupleArray(value: Array<unknown>): boolean {
	return !decodedArrays.has(value) && typeof (value as {get?: unknown}).get === 'function';
}
