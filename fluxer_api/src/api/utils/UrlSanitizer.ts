// SPDX-License-Identifier: AGPL-3.0-or-later

function sanitizeOptionalAbsoluteUrl(url: string | null | undefined): string | undefined {
	if (typeof url !== 'string') {
		return;
	}
	const trimmedUrl = url.trim();
	if (!trimmedUrl) {
		return;
	}
	try {
		return new URL(trimmedUrl).toString();
	} catch {
		return;
	}
}

export function sanitizeOptionalAbsoluteUrlOrNull(url: string | null | undefined): string | null {
	return sanitizeOptionalAbsoluteUrl(url) ?? null;
}
