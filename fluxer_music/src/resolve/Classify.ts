// SPDX-License-Identifier: AGPL-3.0-or-later

export type Classified =
	| {kind: 'youtube-video'; url: string}
	| {kind: 'youtube-playlist'; url: string}
	| {kind: 'spotify'; type: 'track' | 'album' | 'playlist'; id: string}
	| {kind: 'search'; query: string};

const YOUTUBE_HOSTS = new Set(['youtube.com', 'www.youtube.com', 'm.youtube.com', 'music.youtube.com']);
const SPOTIFY_TYPES = new Set(['track', 'album', 'playlist']);

export function classifyInput(input: string): Classified {
	const trimmed = input.trim();
	const search: Classified = {kind: 'search', query: trimmed};

	let url: URL;
	try {
		url = new URL(trimmed);
	} catch {
		return search;
	}
	if (url.protocol !== 'http:' && url.protocol !== 'https:') return search;

	const host = url.hostname.toLowerCase();
	if (host === 'youtu.be') {
		return url.pathname.length > 1 ? {kind: 'youtube-video', url: trimmed} : search;
	}
	if (YOUTUBE_HOSTS.has(host)) {
		if (url.searchParams.get('v')) return {kind: 'youtube-video', url: trimmed};
		if (url.searchParams.get('list')) return {kind: 'youtube-playlist', url: trimmed};
		return search;
	}
	if (host === 'open.spotify.com') {
		const parts = url.pathname.split('/').filter(Boolean);
		if (parts[0]?.startsWith('intl-')) parts.shift();
		const [type, id] = parts;
		if (type && id && SPOTIFY_TYPES.has(type)) {
			return {kind: 'spotify', type: type as 'track' | 'album' | 'playlist', id};
		}
	}
	return search;
}
