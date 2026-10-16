// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	SkeletonEmphasis,
	SkeletonRadius,
	skeletonClassName,
	skeletonDimensionsStyle,
} from '@app/features/app/components/skeleton/SkeletonStyle';
import type React from 'react';

export interface SkeletonBlockProps {
	readonly width?: string;
	readonly height?: string;
	readonly radius?: SkeletonRadius;
	readonly emphasis?: SkeletonEmphasis;
	readonly className?: string;
	readonly style?: React.CSSProperties;
}

export const SkeletonBlock = ({
	width,
	height,
	radius = SkeletonRadius.MEDIUM,
	emphasis = SkeletonEmphasis.DEFAULT,
	className,
	style,
}: SkeletonBlockProps) => (
	<flx-skeleton-block
		className={skeletonClassName({radius, emphasis, className})}
		style={skeletonDimensionsStyle({style, width, height})}
		data-flx="app.skeleton.skeleton-block.flx-skeleton-block"
	/>
);
