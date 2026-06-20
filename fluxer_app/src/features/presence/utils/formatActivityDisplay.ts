// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserActivity} from '@fluxer/schema/src/domains/user/UserResponseSchemas';

export interface ActivityDisplayLines {
	headerSuffix: string | null;
	listeningSource: string | null;
	primary: string;
	secondary: string | null;
}

function prefersDetailsFirst(activity: UserActivity): boolean {
	return activity.status_display_type === 1;
}

function prefersStateFirst(activity: UserActivity): boolean {
	return activity.status_display_type === 2;
}

function normalize(value: string | undefined): string | null {
	if (!value) return null;
	const trimmed = value.trim();
	return trimmed.length > 0 ? trimmed : null;
}

function dedupeLine(value: string | null, ...others: Array<string | null>): string | null {
	if (!value) return null;
	for (const other of others) {
		if (other && other.localeCompare(value, undefined, {sensitivity: 'accent'}) === 0) {
			return null;
		}
	}
	return value;
}

function equalsIgnoreCase(a: string, b: string): boolean {
	return a.localeCompare(b, undefined, {sensitivity: 'accent'}) === 0;
}

function splitArtistTitle(details: string): {artist: string; title: string} | null {
	const firstDash = details.indexOf(' - ');
	if (firstDash <= 0) return null;
	const artist = details.slice(0, firstDash).trim();
	const title = details.slice(firstDash + 3).trim();
	if (!artist || !title) return null;
	return {artist, title};
}

function resolveListeningTitleAndArtist(
	details: string | null,
	state: string | null,
	name: string | null,
): {title: string | null; artist: string | null} {
	if (!details) {
		return {title: name, artist: state};
	}
	if (state) {
		const artistPrefix = `${state} - `;
		if (details.startsWith(artistPrefix)) {
			return {title: details.slice(artistPrefix.length).trim() || null, artist: state};
		}
		const split = splitArtistTitle(details);
		if (split) {
			if (equalsIgnoreCase(split.artist, state)) {
				return {title: split.title, artist: state};
			}
			if (equalsIgnoreCase(split.title, state)) {
				return {title: split.title, artist: split.artist};
			}
			if (!details.includes(state)) {
				if (name && equalsIgnoreCase(name, details)) {
					return {title: details, artist: state};
				}
				if (name && equalsIgnoreCase(name, split.title)) {
					return {title: split.title, artist: split.artist};
				}
			}
		}
		if (!details.includes(' - ')) {
			return {title: details, artist: state};
		}
		return {title: details, artist: state};
	}
	const split = splitArtistTitle(details);
	if (split) {
		return {title: split.title, artist: split.artist};
	}
	return {title: details, artist: null};
}

function resolveListeningSource(name: string | null, title: string, artist: string | null): string | null {
	if (!name) return null;
	if (equalsIgnoreCase(name, title)) return null;
	if (artist && equalsIgnoreCase(name, artist)) return null;
	if (artist && equalsIgnoreCase(`${artist} - ${name}`, title)) return null;
	if (title.endsWith(` - ${name}`)) return null;
	return name;
}

function formatListeningActivity(activity: UserActivity): ActivityDisplayLines {
	const name = normalize(activity.name);
	const details = normalize(activity.details);
	const state = normalize(activity.state);
	const resolved = resolveListeningTitleAndArtist(details, state, name);

	const title = resolved.title ?? name ?? details ?? 'Unknown';
	const artist = dedupeLine(resolved.artist, title, name);
	const listeningSource = resolveListeningSource(name, title, artist);

	return {headerSuffix: listeningSource, listeningSource, primary: title, secondary: artist};
}

function formatDefaultActivity(activity: UserActivity): ActivityDisplayLines {
	const name = normalize(activity.name);
	const details = normalize(activity.details);
	const state = normalize(activity.state);
	let primary = details ?? name ?? 'Unknown';
	let secondary = dedupeLine(state, primary) ?? dedupeLine(name, primary);
	if (prefersStateFirst(activity) && state) {
		primary = state;
		secondary = dedupeLine(details, primary) ?? dedupeLine(name, primary);
	} else if (prefersDetailsFirst(activity) && details) {
		primary = details;
		secondary = dedupeLine(state, primary) ?? dedupeLine(name, primary);
	}
	return {headerSuffix: null, listeningSource: null, primary, secondary};
}

export function formatActivityDisplay(activity: UserActivity): ActivityDisplayLines {
	if (activity.type === 2) {
		return formatListeningActivity(activity);
	}
	return formatDefaultActivity(activity);
}

export type ActivityMemberListKind = 'playing' | 'listening' | 'watching' | 'competing';

export interface ActivityMemberListLine {
	kind: ActivityMemberListKind;
	text: string;
}

export function formatActivityMemberListLine(activity: UserActivity): ActivityMemberListLine {
	const display = formatActivityDisplay(activity);
	const detail = display.secondary ? `${display.primary} — ${display.secondary}` : display.primary;

	switch (activity.type) {
		case 2:
			return {kind: 'listening', text: detail};
		case 3:
			return {kind: 'watching', text: display.primary};
		case 5:
			return {kind: 'competing', text: normalize(activity.name) ?? display.primary};
		default: {
			const state = normalize(activity.state);
			const details = normalize(activity.details);
			const name = normalize(activity.name);
			if (prefersDetailsFirst(activity) && details) return {kind: 'playing', text: details};
			if (prefersStateFirst(activity) && state) return {kind: 'playing', text: state};
			if (state) return {kind: 'playing', text: state};
			if (details) return {kind: 'playing', text: details};
			return {kind: 'playing', text: name ?? display.primary};
		}
	}
}
