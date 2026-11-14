// SPDX-License-Identifier: AGPL-3.0-or-later

export function errorMessage(error: unknown): string {
	return error instanceof Error ? error.message : String(error);
}
