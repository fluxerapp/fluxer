// SPDX-License-Identifier: AGPL-3.0-or-later

interface KlipyPath {
	type: 'gif' | 'clip';
	slug: string;
}

function parseKlipyPath(url: string): KlipyPath | null {
	if (!url) return null;
	try {
		const parsedUrl = new URL(url);
		const hostname = parsedUrl.hostname.toLowerCase();
		if (hostname !== 'klipy.com' && hostname !== 'www.klipy.com') {
			return null;
		}
		const pathMatch = parsedUrl.pathname.match(/^\/(gif|gifs|clip|clips)\/([^/]+)/i);
		if (!pathMatch?.[1] || !pathMatch[2]) {
			return null;
		}
		const type = pathMatch[1].toLowerCase().startsWith('clip') ? 'clip' : 'gif';
		const slug = decodeURIComponent(pathMatch[2]).trim();
		if (!slug) {
			return null;
		}
		return {type, slug};
	} catch {
		return null;
	}
}

export function extractKlipySlug(url: string): string | null {
	return parseKlipyPath(url)?.slug ?? null;
}

export function buildKlipyShareUrl({slug, type = 'gif'}: {slug: string; type?: 'gif' | 'clip'}): string {
	const normalizedSlug = slug.trim();
	if (!normalizedSlug) {
		return 'https://klipy.com/gifs';
	}
	const parsed = parseKlipyPath(normalizedSlug);
	if (parsed) {
		const path = parsed.type === 'clip' ? 'clips' : 'gifs';
		return `https://klipy.com/${path}/${encodeURIComponent(parsed.slug)}`;
	}
	const path = type === 'clip' ? 'clips' : 'gifs';
	return `https://klipy.com/${path}/${encodeURIComponent(normalizedSlug)}`;
}

export function parseTitleFromUrl(url: string): string {
	if (!url) return '';
	const klipyPath = parseKlipyPath(url);
	if (klipyPath) {
		return klipyPath.slug
			.split('-')
			.map((word) => word.charAt(0).toUpperCase() + word.slice(1))
			.join(' ');
	}
	const srcMatch = url.match(/\/([^/]+?)(?:\.[^.]+)?$/);
	if (srcMatch?.[1]) {
		return srcMatch[1]
			.split('-')
			.map((word) => word.charAt(0).toUpperCase() + word.slice(1))
			.join(' ');
	}
	return 'GIF';
}
