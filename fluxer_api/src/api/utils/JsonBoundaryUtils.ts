// SPDX-License-Identifier: AGPL-3.0-or-later

type JsonRecord = Record<string, unknown>;

export function isJsonRecord(value: unknown): value is JsonRecord {
	return typeof value === 'object' && value !== null && !Array.isArray(value);
}

export function parseJsonUnknown(text: string): unknown {
	const parsed: unknown = JSON.parse(text);
	return parsed;
}

export function parseJsonRecord(text: string): JsonRecord | null {
	return parseJsonWithGuard(text, isJsonRecord);
}

export function parseJsonArray(text: string): Array<unknown> | null {
	return parseJsonWithGuard(text, Array.isArray);
}

export function parseJsonWithGuard<T>(text: string, guard: (value: unknown) => value is T): T | null {
	try {
		const parsed = parseJsonUnknown(text);
		return guard(parsed) ? parsed : null;
	} catch {
		return null;
	}
}
