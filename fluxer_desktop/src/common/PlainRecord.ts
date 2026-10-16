// SPDX-License-Identifier: AGPL-3.0-or-later

export const REQUEST_ID_MAX_LENGTH = 128;

interface ExactPlainRecordRequest {
	readonly expectedKeys: ReadonlyArray<string>;
	readonly value: unknown;
}

export function isPlainRecord(value: unknown): value is Record<string, unknown> {
	if (typeof value !== 'object' || value === null || Array.isArray(value)) {
		return false;
	}
	const prototype = Object.getPrototypeOf(value) as unknown;
	return prototype === Object.prototype || prototype === null;
}

function readPlainRecord(value: unknown): Readonly<Record<string, unknown>> | null {
	if (!isPlainRecord(value)) {
		return null;
	}
	const record: Record<string, unknown> = Object.create(null) as Record<string, unknown>;
	for (const key of Reflect.ownKeys(value)) {
		if (typeof key !== 'string') {
			return null;
		}
		const descriptor = Object.getOwnPropertyDescriptor(value, key);
		if (descriptor == null || !Object.hasOwn(descriptor, 'value')) {
			return null;
		}
		record[key] = descriptor.value;
	}
	return Object.freeze(record);
}

export function readExactPlainRecord({
	value,
	expectedKeys,
}: ExactPlainRecordRequest): Readonly<Record<string, unknown>> | null {
	const record = readPlainRecord(value);
	if (record == null) {
		return null;
	}
	const keys = Object.keys(record);
	if (keys.length !== expectedKeys.length) {
		return null;
	}
	for (const key of expectedKeys) {
		if (!Object.hasOwn(record, key)) {
			return null;
		}
	}
	return record;
}

export function readBoundedString(value: unknown, maximumLength: number): string | null {
	if (typeof value !== 'string') {
		return null;
	}
	const trimmed = value.trim();
	if (trimmed.length === 0 || trimmed.length > maximumLength) {
		return null;
	}
	return trimmed;
}
