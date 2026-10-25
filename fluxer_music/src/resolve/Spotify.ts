// SPDX-License-Identifier: AGPL-3.0-or-later

import {MusicError} from '../Errors';
import type {ResolvedTrack} from '../Types';

const TOKEN_URL = 'https://accounts.spotify.com/api/token';
const API = 'https://api.spotify.com/v1';
const MAX_TRACKS = 100;
const TOKEN_MARGIN_MS = 60_000;

interface SpotifyImage {
	url?: string;
}

interface SpotifyTrack {
	id?: string | null;
	name?: string;
	duration_ms?: number;
	artists?: Array<{name?: string}>;
	album?: {images?: Array<SpotifyImage>};
	is_local?: boolean;
}

interface Page<T> {
	items?: Array<T>;
	next?: string | null;
}

type SpotifyType = 'track' | 'album' | 'playlist';

export interface SpotifyClientOptions {
	clientId: string;
	clientSecret: string;
	fetch?: typeof fetch;
	now?: () => number;
}

function toTrack(t: SpotifyTrack, image: string | null): ResolvedTrack {
	return {
		title: t.name ?? '',
		author: (t.artists ?? []).map((a) => a.name ?? '').join(', '),
		durationMs: t.duration_ms ?? 0,
		thumbnailUrl: image,
		url: `https://open.spotify.com/track/${t.id}`,
		source: 'spotify',
	};
}

export class SpotifyClient {
	private readonly fetchFn: typeof fetch;
	private readonly now: () => number;
	private token: {value: string; expiresAt: number} | null = null;

	constructor(private readonly opts: SpotifyClientOptions) {
		this.fetchFn = opts.fetch ?? fetch;
		this.now = opts.now ?? Date.now;
	}

	async tracks(type: SpotifyType, id: string): Promise<Array<ResolvedTrack>> {
		try {
			return await this.load(type, id);
		} catch (e) {
			if (e instanceof MusicError) throw e;
			throw new MusicError('SOURCE_FAILED');
		}
	}

	private async load(type: SpotifyType, id: string): Promise<Array<ResolvedTrack>> {
		const enc = encodeURIComponent(id);
		if (type === 'track') {
			const t = await this.get<SpotifyTrack>(`${API}/tracks/${enc}`, type);
			return [toTrack(t, t.album?.images?.[0]?.url ?? null)];
		}
		if (type === 'album') {
			const album = await this.get<{images?: Array<SpotifyImage>; tracks?: Page<SpotifyTrack>}>(
				`${API}/albums/${enc}`,
				type,
			);
			const image = album.images?.[0]?.url ?? null;
			const items = await this.collect(album.tracks, (t: SpotifyTrack) => t, type);
			return items.map((t) => toTrack(t, image));
		}
		const playlist = await this.get<{tracks?: Page<{track?: SpotifyTrack | null}>}>(`${API}/playlists/${enc}`, type);
		const items = await this.collect(playlist.tracks, (i: {track?: SpotifyTrack | null}) => i.track, type);
		return items.map((t) => toTrack(t, t.album?.images?.[0]?.url ?? null));
	}

	/** Walk `next` pages, keeping usable tracks until MAX_TRACKS. */
	private async collect<I>(
		first: Page<I> | undefined,
		pick: (item: I) => SpotifyTrack | null | undefined,
		type: SpotifyType,
	): Promise<Array<SpotifyTrack>> {
		const out: Array<SpotifyTrack> = [];
		let page = first;
		while (page) {
			for (const item of page.items ?? []) {
				const t = item ? pick(item) : null;
				if (!t || t.is_local || !t.id) continue;
				out.push(t);
				if (out.length >= MAX_TRACKS) return out;
			}
			page = page.next ? await this.get<Page<I>>(page.next, type) : undefined;
		}
		return out;
	}

	private async get<T>(url: string, type: SpotifyType, retried = false): Promise<T> {
		const res = await this.fetchFn(url, {headers: {Authorization: `Bearer ${await this.accessToken()}`}});
		if (res.status === 401 && !retried) {
			this.token = null;
			return this.get<T>(url, type, true);
		}
		if (res.status === 404) {
			if (type === 'playlist') {
				throw new MusicError(
					'NOT_FOUND',
					"Spotify won't share that playlist with bots (Spotify-made playlists are private).",
				);
			}
			throw new MusicError('NOT_FOUND');
		}
		if (!res.ok) throw new MusicError('SOURCE_FAILED');
		return (await res.json()) as T;
	}

	private async accessToken(): Promise<string> {
		if (this.token && this.now() < this.token.expiresAt) return this.token.value;
		const basic = Buffer.from(`${this.opts.clientId}:${this.opts.clientSecret}`).toString('base64');
		const res = await this.fetchFn(TOKEN_URL, {
			method: 'POST',
			headers: {Authorization: `Basic ${basic}`, 'Content-Type': 'application/x-www-form-urlencoded'},
			body: 'grant_type=client_credentials',
		});
		if (!res.ok) throw new MusicError('SOURCE_FAILED');
		const body = (await res.json()) as {access_token?: string; expires_in?: number};
		if (!body.access_token) throw new MusicError('SOURCE_FAILED');
		this.token = {
			value: body.access_token,
			expiresAt: this.now() + (body.expires_in ?? 0) * 1000 - TOKEN_MARGIN_MS,
		};
		return body.access_token;
	}
}
