// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ForwardResultType} from '@app/features/app/components/dialogs/shared/ForwardDestinationQuery';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';

const DEFAULT_DESTINATION_LIMIT = 15;
const HISTORY_LIMIT = 8;
const FREQUENT_LIMIT = 100;

export interface ForwardDestination {
	readonly id: string;
	readonly type: 'user' | 'channel';
}

export interface ForwardRowIdentity {
	readonly id: string;
	readonly type: ForwardResultType;
}

export interface ForwardDestinationRow extends ForwardRowIdentity {
	readonly destination: ForwardDestination;
}

interface ForwardOriginChannel {
	readonly recipientIds: ReadonlyArray<string>;
	readonly type: number;
}

interface BuildForwardDefaultDestinationsRequest {
	readonly frequentIds: ReadonlyArray<string>;
	readonly history: ReadonlyArray<string>;
	readonly accepts: (row: ForwardRowIdentity) => boolean;
	readonly mode: ForwardResultType | null;
	readonly origin: ForwardDestination | null;
	readonly pinned: ReadonlyArray<ForwardDestination>;
	readonly resolveChannel: (channelId: string) => ForwardRowIdentity | null;
	readonly resolveDestination: (destination: ForwardDestination) => ForwardRowIdentity | null;
	readonly selected: ReadonlyArray<ForwardDestination>;
}

export function resolveForwardOrigin(
	channelId: string,
	channel: ForwardOriginChannel | null | undefined,
): ForwardDestination {
	if (channel != null && channel.type === ChannelTypes.DM && channel.recipientIds.length > 0) {
		return Object.freeze({id: channel.recipientIds[0], type: 'user'});
	}
	return Object.freeze({id: channelId, type: 'channel'});
}

export function buildForwardDefaultDestinations({
	frequentIds,
	history,
	accepts,
	mode,
	origin,
	pinned,
	resolveChannel,
	resolveDestination,
	selected,
}: BuildForwardDefaultDestinationsRequest): ReadonlyArray<ForwardDestinationRow> {
	const candidates = [
		...pinned.map((destination) => resolveDestination(destination)),
		...history.slice(0, HISTORY_LIMIT).map((channelId) => resolveChannel(channelId)),
		...frequentIds.slice(0, FREQUENT_LIMIT).map((channelId) => resolveChannel(channelId)),
	];
	const rows = candidates.filter((row): row is ForwardRowIdentity => row != null && accepts(row));
	const originSelected =
		origin != null && selected.some((destination) => destination.type === origin.type && destination.id === origin.id);
	const hiddenIds = origin == null || originSelected ? [] : [origin.id];
	if (mode != null) {
		return dedupeRows(
			rows.filter((row) => row.type === mode),
			hiddenIds,
		);
	}
	return dedupeRows(rows, hiddenIds).slice(0, DEFAULT_DESTINATION_LIMIT);
}

export function filterForwardSearchRows(
	results: ReadonlyArray<ForwardRowIdentity>,
	accepts: (row: ForwardRowIdentity) => boolean,
): ReadonlyArray<ForwardDestinationRow> {
	return dedupeRows(
		results.filter((result) => accepts(result)),
		[],
	);
}

function dedupeRows(
	rows: ReadonlyArray<ForwardRowIdentity>,
	hiddenIds: ReadonlyArray<string>,
): Array<ForwardDestinationRow> {
	const seenIds = new Set(hiddenIds);
	const deduped: Array<ForwardDestinationRow> = [];
	for (const {id, type} of rows) {
		if (seenIds.has(id)) continue;
		seenIds.add(id);
		const destination: ForwardDestination = Object.freeze({id, type: type === 'user' ? 'user' : 'channel'});
		deduped.push(Object.freeze({destination, id, type}));
	}
	return deduped;
}
