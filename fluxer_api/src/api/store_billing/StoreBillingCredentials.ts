// SPDX-License-Identifier: AGPL-3.0-or-later

export function hasText(value: string | undefined): value is string {
	return value !== undefined && value.trim().length > 0;
}

export function normalizePem(value: string): string {
	return value.replaceAll('\\n', '\n');
}
