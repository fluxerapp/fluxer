// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	SkeletonEmphasis,
	SkeletonRadius,
	skeletonClassName,
	skeletonDimensionsStyle,
} from '@app/features/app/components/skeleton/SkeletonStyle';
import type React from 'react';

export interface SkeletonLineProps {
	readonly width?: string;
	readonly height?: string;
	readonly emphasis?: SkeletonEmphasis;
	readonly className?: string;
	readonly style?: React.CSSProperties;
}

export const SkeletonLine = ({
	width,
	height,
	emphasis = SkeletonEmphasis.DEFAULT,
	className,
	style,
}: SkeletonLineProps) => (
	<flx-skeleton-line
		className={skeletonClassName({radius: SkeletonRadius.PILL, emphasis, className})}
		style={skeletonDimensionsStyle({style, width, height})}
		data-flx="app.skeleton.skeleton-line.flx-skeleton-line"
	/>
);
