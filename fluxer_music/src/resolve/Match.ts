// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ResolvedTrack} from '../Types';

const WINDOW_MS = 10_000;
const PREFERRED_BONUS_MS = 5000;

function score(target: {durationMs: number}, candidate: ResolvedTrack): number {
	const preferred = candidate.author.endsWith(' - Topic') || /official audio/i.test(candidate.title);
	return Math.abs(candidate.durationMs - target.durationMs) - (preferred ? PREFERRED_BONUS_MS : 0);
}

/** Pick the best YouTube candidate for a Spotify track, or null if none is close enough in length. */
export function pickMatch(target: {durationMs: number}, candidates: Array<ResolvedTrack>): ResolvedTrack | null {
	let best: ResolvedTrack | null = null;
	let bestScore = Number.POSITIVE_INFINITY;
	for (const c of candidates) {
		if (Math.abs(c.durationMs - target.durationMs) > WINDOW_MS) continue;
		const s = score(target, c);
		if (s < bestScore) {
			best = c;
			bestScore = s;
		}
	}
	return best;
}
