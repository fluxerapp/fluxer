// SPDX-License-Identifier: AGPL-3.0-or-later

function normaliseTenorSlugId(value: string): string | null {
	const trimmed = value.trim();
	if (!trimmed) return null;
	const withoutLeadingSlash = trimmed.startsWith('/') ? trimmed.slice(1) : trimmed;
	if (withoutLeadingSlash.toLowerCase().startsWith('view/')) {
		return withoutLeadingSlash.replace(/\/+$/, '');
	}
	if (!withoutLeadingSlash.includes('/')) {
		return `view/${withoutLeadingSlash.replace(/\/+$/, '')}`;
	}
	return null;
}

export function extractTenorSlugId(url: string): string | null {
	try {
		const parsedUrl = new URL(url);
		const hostname = parsedUrl.hostname.toLowerCase();
		if (hostname !== 'tenor.com' && hostname !== 'www.tenor.com') {
			return null;
		}
		const match = parsedUrl.pathname.match(/^\/(?:[a-z]{2}(?:-[a-z]{2})?\/)?view\/([^/]+)/i);
		if (!match?.[1]) {
			return null;
		}
		const slugId = decodeURIComponent(match[1]).trim();
		if (!slugId) {
			return null;
		}
		return `view/${slugId}`;
	} catch {
		return normaliseTenorSlugId(url);
	}
}

export function buildTenorShareUrl(tenorSlugId: string): string {
	const trimmed = tenorSlugId.trim();
	if (!trimmed) return 'https://tenor.com/';
	const extractedFromUrl = extractTenorSlugId(trimmed);
	if (extractedFromUrl) {
		return `https://tenor.com/${extractedFromUrl}`;
	}
	const normalized = normaliseTenorSlugId(trimmed);
	if (normalized) {
		return `https://tenor.com/${normalized}`;
	}
	return trimmed;
}
