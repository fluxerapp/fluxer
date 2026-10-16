// SPDX-License-Identifier: AGPL-3.0-or-later

export const DirectSelectionSurface = Object.freeze({
	GUILD_RAIL: 'guild-rail',
	CHANNEL_LIST: 'channel-list',
	FAVORITES_LIST: 'favorites-list',
} as const);

export type DirectSelectionSurface = (typeof DirectSelectionSurface)[keyof typeof DirectSelectionSurface];

const markedSurfaces = new Set<DirectSelectionSurface>();

export function markDirectSelection(surface: DirectSelectionSurface): void {
	markedSurfaces.add(surface);
}

export function peekDirectSelection(surface: DirectSelectionSurface): boolean {
	return markedSurfaces.has(surface);
}

export function consumeDirectSelection(surface: DirectSelectionSurface): boolean {
	const marked = markedSurfaces.has(surface);
	markedSurfaces.delete(surface);
	return marked;
}
