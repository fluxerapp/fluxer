// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	SKELETON_DISCOVERY_GRID_GAP_PX,
	SKELETON_DISCOVERY_MAX_CARD_WIDTH_PX,
	SKELETON_DISCOVERY_MIN_CARD_WIDTH_PX,
} from '@app/features/app/components/skeleton/SkeletonLayoutMemory';
import type {SkeletonInjectedToken} from '@app/features/app/components/skeleton/SkeletonSurfaceContract';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import type {CSSProperties} from 'react';

const DISCOVERY_GRID_METRICS = {
	'--discovery-card-max-width': remFromPx(SKELETON_DISCOVERY_MAX_CARD_WIDTH_PX),
	'--discovery-card-min-width': remFromPx(SKELETON_DISCOVERY_MIN_CARD_WIDTH_PX),
	'--discovery-grid-gap': remFromPx(SKELETON_DISCOVERY_GRID_GAP_PX),
} satisfies Record<
	Extract<SkeletonInjectedToken, '--discovery-card-max-width' | '--discovery-card-min-width' | '--discovery-grid-gap'>,
	string
>;

export const DISCOVERY_GRID_METRICS_STYLE = DISCOVERY_GRID_METRICS as CSSProperties;
