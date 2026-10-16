// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/skeleton/Skeleton.module.css';
import {flxElementClassName} from '@app/lib/react';
import type {CSSProperties} from 'react';

export const SkeletonEmphasis = Object.freeze({
	STRONG: 'strong',
	DEFAULT: 'default',
	MUTED: 'muted',
} as const);

export type SkeletonEmphasis = (typeof SkeletonEmphasis)[keyof typeof SkeletonEmphasis];

export const SkeletonRadius = Object.freeze({
	SHARP: 'sharp',
	SMALL: 'sm',
	MEDIUM: 'md',
	LARGE: 'lg',
	EXTRA_LARGE: 'xl',
	PILL: 'pill',
	CIRCLE: 'circle',
} as const);

export type SkeletonRadius = (typeof SkeletonRadius)[keyof typeof SkeletonRadius];

const EMPHASIS_CLASS_NAMES: Record<SkeletonEmphasis, string | undefined> = {
	[SkeletonEmphasis.STRONG]: styles.emphasisStrong,
	[SkeletonEmphasis.DEFAULT]: undefined,
	[SkeletonEmphasis.MUTED]: styles.emphasisMuted,
};

const RADIUS_CLASS_NAMES: Record<SkeletonRadius, string> = {
	[SkeletonRadius.SHARP]: styles.radiusSharp,
	[SkeletonRadius.SMALL]: styles.radiusSm,
	[SkeletonRadius.MEDIUM]: styles.radiusMd,
	[SkeletonRadius.LARGE]: styles.radiusLg,
	[SkeletonRadius.EXTRA_LARGE]: styles.radiusXl,
	[SkeletonRadius.PILL]: styles.radiusPill,
	[SkeletonRadius.CIRCLE]: styles.radiusCircle,
};

export interface SkeletonClassNameRequest {
	readonly className: string | undefined;
	readonly emphasis: SkeletonEmphasis;
	readonly radius: SkeletonRadius;
}

export interface SkeletonDimensionsStyle extends CSSProperties {
	readonly '--skeleton-height': string | undefined;
	readonly '--skeleton-width': string | undefined;
}

export interface SkeletonDimensionsStyleRequest {
	readonly height: string | undefined;
	readonly style: CSSProperties | undefined;
	readonly width: string | undefined;
}

export function skeletonClassName({radius, emphasis, className}: SkeletonClassNameRequest): string {
	return flxElementClassName(styles.skeleton, RADIUS_CLASS_NAMES[radius], EMPHASIS_CLASS_NAMES[emphasis], className);
}

export function skeletonDimensionsStyle({
	height,
	style,
	width,
}: SkeletonDimensionsStyleRequest): SkeletonDimensionsStyle {
	const dimensionsStyle: SkeletonDimensionsStyle = {
		...style,
		'--skeleton-width': width,
		'--skeleton-height': height,
	};
	return dimensionsStyle;
}
