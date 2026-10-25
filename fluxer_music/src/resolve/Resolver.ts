// SPDX-License-Identifier: AGPL-3.0-or-later

import {MusicError} from '../Errors';
import type {ResolvedTrack} from '../Types';
import {classifyInput} from './Classify';
import {pickMatch} from './Match';
import type {SpotifyClient} from './Spotify';
import type {YouTubeResolver} from './YouTube';

export class Resolver {
	constructor(
		private readonly youtube: YouTubeResolver,
		private readonly spotify: SpotifyClient | null,
	) {}

	async resolve(input: string): Promise<Array<ResolvedTrack>> {
		const classified = classifyInput(input);
		switch (classified.kind) {
			case 'youtube-video':
				return [await this.youtube.video(classified.url)];
			case 'youtube-playlist':
				return this.youtube.playlist(classified.url);
			case 'spotify':
				if (!this.spotify) throw new MusicError('SOURCE_FAILED', "Spotify links aren't set up on this server.");
				return this.spotify.tracks(classified.type, classified.id);
			case 'search': {
				const [first] = await this.youtube.search(classified.query);
				if (!first) throw new MusicError('NO_RESULTS');
				return [first];
			}
		}
	}

	search(query: string): Promise<Array<ResolvedTrack>> {
		return this.youtube.search(query, 10);
	}

	async matchSpotify(track: ResolvedTrack): Promise<ResolvedTrack | null> {
		const candidates = await this.youtube.search(`${track.author} - ${track.title}`, 5);
		return pickMatch(track, candidates);
	}

	async streamUrl(track: ResolvedTrack): Promise<string> {
		if (track.source !== 'youtube') throw new MusicError('BAD_REQUEST');
		return this.youtube.streamUrl(track.url);
	}
}
