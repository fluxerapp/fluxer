// SPDX-License-Identifier: AGPL-3.0-or-later

import Dimension from '@app/features/ui/state/Dimension';

export function updateChannelListScroll(guildId: string, scrollTop: number): void {
	Dimension.updateGuildDimensions(guildId, scrollTop, undefined);
}

export function clearChannelListScrollTo(guildId: string): void {
	Dimension.updateGuildDimensions(guildId, undefined, null);
}

export function updateGuildListScroll(scrollTop: number): void {
	Dimension.updateGuildListDimensions(scrollTop);
}
