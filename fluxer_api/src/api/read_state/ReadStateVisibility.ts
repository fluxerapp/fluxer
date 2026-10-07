// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {recipientActive, viewerActive} from '@app/api/experiment/ChannelThreadsGate';
import type {ReadState} from '@app/api/models/ReadState';

export function visibleReadStates(
	readStates: Array<ReadState>,
	viewer: {userId: UserID; capable: boolean; bot?: boolean},
): Array<ReadState> {
	if (!readStates.some((readState) => readState.isMarked)) return readStates;
	const threadViewer = {
		kind: 'user',
		userId: viewer.userId,
		bot: viewer.bot === true,
		capable: viewer.capable,
	} as const;
	return readStates.filter((readState) => readState.guildId === null || viewerActive(threadViewer, readState.guildId));
}

export function badgeReadStates(readStates: Array<ReadState>, userId: UserID): Array<ReadState> {
	if (!readStates.some((readState) => readState.isMarked)) return readStates;
	return readStates.filter(
		(readState) => readState.guildId === null || recipientActive(readState.guildId, userId, false),
	);
}
