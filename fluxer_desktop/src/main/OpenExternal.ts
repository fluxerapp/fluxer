// SPDX-License-Identifier: AGPL-3.0-or-later

import {APP_PROTOCOL} from '@electron/common/Constants';
import {shell} from 'electron';

const DEDUPE_WINDOW_MS = 500;
const ALLOWED_EXTERNAL_URL_PROTOCOLS = new Set([
	'http:',
	'https:',
	'mailto:',
	'tel:',
	'appstream:',
	`${APP_PROTOCOL}:`,
]);
const recentOpens = new Map<string, number>();

function getSafeExternalUrl(url: string): string | null {
	try {
		const parsed = new URL(url);
		const protocol = parsed.protocol.toLowerCase();
		return ALLOWED_EXTERNAL_URL_PROTOCOLS.has(protocol) ? parsed.toString() : null;
	} catch {
		return null;
	}
}

function pruneStale(now: number): void {
	for (const [url, timestamp] of recentOpens) {
		if (now - timestamp > DEDUPE_WINDOW_MS) {
			recentOpens.delete(url);
		}
	}
}

export function shouldOpenExternalUrl(url: string): boolean {
	return getSafeExternalUrl(url) !== null;
}

export async function openExternalDeduped(url: string): Promise<void> {
	const safeUrl = getSafeExternalUrl(url);
	if (!safeUrl) {
		throw new Error('External URL open request blocked');
	}
	const now = Date.now();
	pruneStale(now);
	const previous = recentOpens.get(safeUrl);
	if (previous !== undefined && now - previous < DEDUPE_WINDOW_MS) {
		recentOpens.set(safeUrl, now);
		return;
	}
	recentOpens.set(safeUrl, now);
	await shell.openExternal(safeUrl);
}
