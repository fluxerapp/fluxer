/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {clsx} from 'clsx';
import type React from 'react';
import styles from './EmbedCard.module.css';

export interface EmbedCardProps {
	splashURL?: string | null;
	splashAspectRatio?: number;
	icon: React.ReactNode;
	title: React.ReactNode;
	subtitle?: React.ReactNode;
	body?: React.ReactNode;
	footer: React.ReactNode;
	className?: string;
	headerClassName?: string;
}

export const EmbedCard = ({
	splashURL,
	splashAspectRatio,
	icon,
	title,
	subtitle,
	body,
	footer,
	className,
	headerClassName,
}: EmbedCardProps) => {
	const hasSplashAspectRatio = splashAspectRatio != null && Number.isFinite(splashAspectRatio) && splashAspectRatio > 0;
	const hasSplash = splashURL != null && splashURL !== '';

	return (
		<div className={clsx(styles.wrapper, className)}>
			{hasSplash ? (
				<div className={styles.splashWrapper}>
					<div
						className={styles.splash}
						style={
							{
								['--embed-splash-url' as any]: `url(${splashURL})`,
								...(hasSplashAspectRatio ? {height: 'auto', aspectRatio: splashAspectRatio as number} : {}),
							} as React.CSSProperties
						}
					/>
				</div>
			) : null}

			<div className={styles.grid}>
				<div className={styles.iconSlot}>{icon}</div>

				<div className={styles.content}>
					<div className={clsx(styles.header, headerClassName)}>
						<div className={styles.titleRow}>{title}</div>
						{subtitle ? <div className={styles.subtitle}>{subtitle}</div> : null}
					</div>

					{body ? <div className={styles.body}>{body}</div> : null}
				</div>
			</div>

			<div className={styles.divider}>{footer}</div>
		</div>
	);
};

interface SkeletonProps {
	className?: string;
	style?: React.CSSProperties;
}

const Skeleton = ({className, style}: SkeletonProps) => (
	<div className={clsx(styles.skeleton, className)} style={style} />
);

export const EmbedSkeletonCircle = ({className}: SkeletonProps) => (
	<Skeleton className={clsx(styles.skeletonCircle, className)} />
);

export const EmbedSkeletonTitle = ({className}: SkeletonProps) => (
	<Skeleton className={clsx(styles.skeletonTitle, className)} />
);

export const EmbedSkeletonSubtitle = ({className}: SkeletonProps) => (
	<Skeleton className={clsx(styles.skeletonSubtitle, className)} />
);

export const EmbedSkeletonIcon = ({className}: SkeletonProps) => (
	<Skeleton className={clsx(styles.skeletonIcon, className)} />
);

export const EmbedSkeletonDot = ({className}: SkeletonProps) => (
	<Skeleton className={clsx(styles.skeletonDot, className)} />
);

export const EmbedSkeletonStatShort = ({className}: SkeletonProps) => (
	<Skeleton className={clsx(styles.skeletonStat, styles.skeletonStatShort, className)} />
);

export const EmbedSkeletonStatLong = ({className}: SkeletonProps) => (
	<Skeleton className={clsx(styles.skeletonStat, styles.skeletonStatLong, className)} />
);

export const EmbedSkeletonButton = ({className}: SkeletonProps) => (
	<Skeleton className={clsx(styles.skeletonButton, className)} />
);
