// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ForwardDestination} from '@app/features/app/components/dialogs/shared/ForwardDefaultDestinations';

export const MAX_FORWARD_DESTINATIONS = 5;

export interface ForwardPinnedDestinations {
	readonly engineQuery: string | null;
	readonly pinned: ReadonlyArray<ForwardDestination>;
}

export function forwardDestinationKey(destination: ForwardDestination): string {
	return `${destination.type}:${destination.id}`;
}

export function toggleForwardDestination(
	selected: ReadonlyArray<ForwardDestination>,
	destination: ForwardDestination,
): ReadonlyArray<ForwardDestination> {
	const key = forwardDestinationKey(destination);
	const remaining = selected.filter((entry) => forwardDestinationKey(entry) !== key);
	if (remaining.length < selected.length) return Object.freeze(remaining);
	if (selected.length >= MAX_FORWARD_DESTINATIONS) return selected;
	return Object.freeze([destination, ...selected]);
}

export function pinForwardDestinations(
	previous: ForwardPinnedDestinations,
	engineQuery: string,
	selected: ReadonlyArray<ForwardDestination>,
): ForwardPinnedDestinations {
	if (engineQuery === previous.engineQuery) return previous;
	return Object.freeze({engineQuery, pinned: selected});
}
