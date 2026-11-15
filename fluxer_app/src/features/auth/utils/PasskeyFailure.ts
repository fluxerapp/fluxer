// SPDX-License-Identifier: AGPL-3.0-or-later

export function isPasskeyCeremonyAborted(error: unknown): boolean {
	return error instanceof Error && error.name === 'AbortError';
}
