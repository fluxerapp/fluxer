// SPDX-License-Identifier: AGPL-3.0-or-later

export function toIsoOrNull(value: Date | null | undefined): string | null {
	return value ? value.toISOString() : null;
}
