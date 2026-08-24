// SPDX-License-Identifier: AGPL-3.0-or-later

import {snapMediaProxyImageSize} from '@app/features/messaging/utils/MediaProxyUtils';

export interface ProxyRequestSize {
	width: number;
	height: number;
}

export function resolveProxyRequestSize(
	layoutWidth: number,
	layoutHeight: number,
	sourceWidth: number,
	sourceHeight: number,
): ProxyRequestSize | null {
	if (!(sourceWidth > 0) || !(sourceHeight > 0)) return null;
	if (!(layoutWidth > 0) || !(layoutHeight > 0)) return null;
	const longestLayoutEdge = Math.max(layoutWidth, layoutHeight);
	const snappedLongestEdge = snapMediaProxyImageSize(longestLayoutEdge, true);
	if (snappedLongestEdge >= Math.max(sourceWidth, sourceHeight)) return null;
	const requestScale = snappedLongestEdge / longestLayoutEdge;
	const width = Math.min(sourceWidth, Math.max(1, Math.round(layoutWidth * requestScale)));
	const height = Math.min(sourceHeight, Math.max(1, Math.round(layoutHeight * requestScale)));
	if (width === sourceWidth && height === sourceHeight) return null;
	return {width, height};
}
