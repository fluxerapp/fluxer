// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	SkeletonEmphasis,
	SkeletonRadius,
	skeletonClassName,
	skeletonDimensionsStyle,
} from '@app/features/app/components/skeleton/SkeletonStyle';
import type React from 'react';

export interface SkeletonCircleProps {
	readonly size: string;
	readonly emphasis?: SkeletonEmphasis;
	readonly className?: string;
	readonly style?: React.CSSProperties;
}

export const SkeletonCircle = ({size, emphasis = SkeletonEmphasis.DEFAULT, className, style}: SkeletonCircleProps) => (
	<flx-skeleton-circle
		className={skeletonClassName({radius: SkeletonRadius.CIRCLE, emphasis, className})}
		style={skeletonDimensionsStyle({style, width: size, height: size})}
		data-flx="app.skeleton.skeleton-circle.flx-skeleton-circle"
	/>
);
