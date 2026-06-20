// SPDX-License-Identifier: AGPL-3.0-or-later

const TIMESTAMP_SECONDS_MAX = 10_000_000_000;
const TIMESTAMP_MILLISECONDS_MAX = 10_000_000_000_000;

export function normalizeTimestamps(timestamps?: Record<string, unknown>): void {
	if (!timestamps) return;
	for (const key of ['start', 'end'] as const) {
		const value = timestamps[key];
		if (typeof value !== 'number' || !Number.isFinite(value)) continue;
		if (value > TIMESTAMP_MILLISECONDS_MAX) {
			timestamps[key] = Math.floor(value / 1_000_000);
		} else if (value > TIMESTAMP_SECONDS_MAX) {
			timestamps[key] = Math.floor(value / 1_000);
		}
	}
}

function parseListeningDetailsTitle(details: string, state?: string): string | null {
	const trimmed = details.trim();
	if (!trimmed) return null;
	const normalizedState = state?.trim();
	if (normalizedState) {
		const artistPrefix = `${normalizedState} - `;
		if (trimmed.startsWith(artistPrefix)) {
			return trimmed.slice(artistPrefix.length).trim() || null;
		}
		const firstDash = trimmed.indexOf(' - ');
		if (firstDash > 0) {
			const first = trimmed.slice(0, firstDash).trim();
			const rest = trimmed.slice(firstDash + 3).trim();
			if (first.localeCompare(normalizedState, undefined, {sensitivity: 'accent'}) === 0) {
				return rest || null;
			}
			if (rest.localeCompare(normalizedState, undefined, {sensitivity: 'accent'}) === 0) {
				return rest || null;
			}
		}
		return trimmed;
	}
	const firstDash = trimmed.indexOf(' - ');
	if (firstDash > 0) {
		return trimmed.slice(firstDash + 3).trim() || null;
	}
	return trimmed;
}

function isUnknownActivityName(name: string | undefined): boolean {
	return name?.trim().localeCompare('unknown', undefined, {sensitivity: 'accent'}) === 0;
}

export function resolveRpcActivityName(
	appName: string,
	rawName: string | undefined,
	details: string | undefined,
	state: string | undefined,
): string {
	const fallbackName = appName.trim() || 'Unknown';
	const name = rawName?.trim();
	if (name && !isUnknownActivityName(name)) return name;
	const normalizedState = state?.trim();
	const detailsText = details?.trim();
	const title = detailsText ? parseListeningDetailsTitle(detailsText, normalizedState) : null;
	return title ?? detailsText ?? normalizedState ?? fallbackName;
}

export function encodeIpcMessage(type: number, data: unknown): Buffer {
	const dataStr = JSON.stringify(data);
	const dataSize = Buffer.byteLength(dataStr);
	const buf = Buffer.allocUnsafe(dataSize + 8);
	buf.writeInt32LE(type, 0);
	buf.writeInt32LE(dataSize, 4);
	buf.write(dataStr, 8, dataSize);
	return buf;
}

export function getUnixSocketBaseDir(): string {
	return process.env.XDG_RUNTIME_DIR ?? process.env.TMPDIR ?? process.env.TMP ?? process.env.TEMP ?? '/tmp';
}
