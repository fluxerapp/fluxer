// SPDX-License-Identifier: AGPL-3.0-or-later

export function isObjectLike(value: unknown): value is Record<string, unknown> {
	return typeof value === 'object' && value !== null;
}

export function isRecord(value: unknown): value is Record<string, unknown> {
	return isObjectLike(value) && !Array.isArray(value);
}
