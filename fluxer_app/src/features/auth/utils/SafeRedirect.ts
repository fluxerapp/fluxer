// SPDX-License-Identifier: AGPL-3.0-or-later

import {resolveDocumentURLFromRoot} from '@app/features/platform/URLOriginUtils';

const MAX_SAFE_REDIRECT_LENGTH = 2048;

export function safeRedirectTarget(target: string | null | undefined): string | null {
	if (!target) {
		return null;
	}
	const trimmed = target.trim();
	if (
		!trimmed ||
		trimmed.length > MAX_SAFE_REDIRECT_LENGTH ||
		trimmed.includes('\r') ||
		trimmed.includes('\n') ||
		!trimmed.startsWith('/') ||
		trimmed.startsWith('//') ||
		trimmed.startsWith('/\\')
	) {
		return null;
	}
	try {
		const url = resolveDocumentURLFromRoot(trimmed);
		if (url.origin !== window.location.origin) {
			return null;
		}
		return `${url.pathname}${url.search}${url.hash}`;
	} catch {
		return null;
	}
}

export function safeRedirectTargetOrFallback(target: string | null | undefined, fallback: string): string {
	return safeRedirectTarget(target) ?? fallback;
}
