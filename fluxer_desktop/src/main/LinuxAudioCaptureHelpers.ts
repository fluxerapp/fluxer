// SPDX-License-Identifier: AGPL-3.0-or-later

const MAX_WINDOW_SOURCE_ID_LENGTH = 256;
const MAX_WINDOW_TOKEN_LENGTH = 128;

export function parseWindowSourceToken(sourceId: unknown): string | null {
	if (typeof sourceId !== 'string' || sourceId.length > MAX_WINDOW_SOURCE_ID_LENGTH) return null;
	const match = /^window:([^:]+):(?:0|1)$/.exec(sourceId);
	const token = match?.[1] ?? null;
	if (!token || token.length > MAX_WINDOW_TOKEN_LENGTH) return null;
	return token;
}

export function isX11WindowToken(token: string): boolean {
	return /^(?:0x[0-9a-fA-F]+|[0-9]+)$/.test(token);
}

export function isDBusObjectPathSegment(token: string): boolean {
	return /^[A-Za-z0-9_]+$/.test(token);
}
