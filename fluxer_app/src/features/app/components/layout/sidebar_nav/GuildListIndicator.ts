// SPDX-License-Identifier: AGPL-3.0-or-later

import {remFromPx} from '@app/features/theme/layout/RemFromPx';

export interface ResolveGuildListIndicatorHeightRequest {
	readonly isSelected: boolean;
	readonly showHoverState: boolean;
}

export type GuildListIndicatorBarTarget = {
	readonly opacity: number;
	readonly height: `${number}rem`;
};

function resolveGuildListIndicatorHeight({isSelected, showHoverState}: ResolveGuildListIndicatorHeightRequest): number {
	if (isSelected) return 40;
	if (showHoverState) return 20;
	return 8;
}

export function resolveGuildListIndicatorBarTarget(
	request: ResolveGuildListIndicatorHeightRequest,
): GuildListIndicatorBarTarget {
	return {opacity: 1, height: remFromPx(resolveGuildListIndicatorHeight(request))};
}
