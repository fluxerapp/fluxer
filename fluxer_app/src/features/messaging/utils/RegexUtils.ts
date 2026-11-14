// SPDX-License-Identifier: AGPL-3.0-or-later

export function escapeRegex(str: string): string {
	return str.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}
