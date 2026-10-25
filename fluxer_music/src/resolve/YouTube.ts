// SPDX-License-Identifier: AGPL-3.0-or-later

import {MusicError} from '../Errors';
import type {ResolvedTrack} from '../Types';
import type {YtDlpRunner} from './YtDlp';

const MAX_DURATION_MS = 10_800_000;
const PLAYLIST_LIMIT = 100;

interface YtDlpInfo {
	id?: string;
	title?: string;
	channel?: string | null;
	uploader?: string | null;
	duration?: number | null;
	thumbnail?: string | null;
	thumbnails?: Array<{url?: string}> | null;
	webpage_url?: string | null;
	is_live?: boolean | null;
	live_status?: string | null;
	entries?: Array<YtDlpInfo>;
}

function isLive(info: YtDlpInfo): boolean {
	return info.is_live === true || info.live_status === 'is_live' || info.live_status === 'is_upcoming';
}

function toTrack(info: YtDlpInfo): ResolvedTrack {
	return {
		title: info.title ?? '',
		author: info.channel ?? info.uploader ?? '',
		durationMs: Math.round((info.duration ?? 0) * 1000),
		thumbnailUrl: info.thumbnail ?? info.thumbnails?.at(-1)?.url ?? null,
		url: info.webpage_url ?? `https://www.youtube.com/watch?v=${info.id}`,
		source: 'youtube',
	};
}

/** Drops live and upcoming entries, over-long ones and those with no duration (private or deleted videos). */
function playable(infos: Array<YtDlpInfo>): Array<ResolvedTrack> {
	return infos
		.filter(
			(i) => !isLive(i) && typeof i.duration === 'number' && i.duration > 0 && i.duration * 1000 <= MAX_DURATION_MS,
		)
		.map(toTrack);
}

function parseJson(text: string): YtDlpInfo {
	try {
		return JSON.parse(text) as YtDlpInfo;
	} catch {
		throw new MusicError('SOURCE_FAILED');
	}
}

export class YouTubeResolver {
	constructor(private readonly run: YtDlpRunner) {}

	async video(url: string): Promise<ResolvedTrack> {
		const info = parseJson(await this.run(['-J', '--no-playlist', '--', url]));
		if (info.entries || !info.id) throw new MusicError('SOURCE_FAILED');
		if (isLive(info)) throw new MusicError('LIVE_NOT_SUPPORTED');
		if ((info.duration ?? 0) * 1000 > MAX_DURATION_MS) throw new MusicError('TOO_LONG');
		return toTrack(info);
	}

	async playlist(url: string): Promise<Array<ResolvedTrack>> {
		const info = parseJson(
			await this.run(['-J', '--flat-playlist', '--playlist-end', String(PLAYLIST_LIMIT), '--', url]),
		);
		const tracks = playable(info.entries ?? []).slice(0, PLAYLIST_LIMIT);
		if (tracks.length === 0) throw new MusicError('NO_RESULTS');
		return tracks;
	}

	async search(query: string, limit = 10): Promise<Array<ResolvedTrack>> {
		const out = await this.run(['--dump-json', '--flat-playlist', '--', `ytsearch${limit}:${query}`]);
		const infos = out
			.split('\n')
			.filter((l) => l.trim())
			.map(parseJson);
		const tracks = playable(infos).slice(0, limit);
		if (tracks.length === 0) throw new MusicError('NO_RESULTS');
		return tracks;
	}

	async streamUrl(url: string): Promise<string> {
		const out = await this.run(['-f', 'bestaudio', '--no-playlist', '--get-url', '--', url]);
		const first = out.split('\n')[0]?.trim();
		if (!first) throw new MusicError('SOURCE_FAILED');
		return first;
	}
}
