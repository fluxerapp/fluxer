// SPDX-License-Identifier: AGPL-3.0-or-later

import type {SkeletonInjectedToken} from '@app/features/app/components/skeleton/SkeletonSurfaceContract';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import type {CSSProperties} from 'react';

export const MEMBER_LIST_ITEM_HEIGHT_PX = 44;
export const MEMBER_LIST_GROUP_DM_ITEM_HEIGHT_PX = 40;
export const MEMBER_LIST_GROUP_HEADER_HEIGHT_PX = 32;
export const MEMBER_LIST_SCROLL_CHUNK_PX = 256;
export const MEMBER_LIST_SUBSCRIPTION_VIEWPORT_RATIO = 0.5;

const MEMBER_LIST_METRICS = {
	'--member-list-item-height': remFromPx(MEMBER_LIST_ITEM_HEIGHT_PX),
	'--member-list-item-height-group-dm': remFromPx(MEMBER_LIST_GROUP_DM_ITEM_HEIGHT_PX),
	'--member-list-group-header-height': remFromPx(MEMBER_LIST_GROUP_HEADER_HEIGHT_PX),
} satisfies Partial<Record<SkeletonInjectedToken, string>>;

export const MEMBER_LIST_METRICS_STYLE = MEMBER_LIST_METRICS as CSSProperties;
