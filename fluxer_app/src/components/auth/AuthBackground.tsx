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

import {motion} from 'framer-motion';
import type React from 'react';
import {GuildSplashCardAlignment} from '~/Constants';
import styles from '~/components/layout/AuthLayout.module.css';

const getSplashAlignmentStyles = (
	alignment: (typeof GuildSplashCardAlignment)[keyof typeof GuildSplashCardAlignment],
) => {
	switch (alignment) {
		case GuildSplashCardAlignment.LEFT:
			return {transformOrigin: 'bottom left', objectPosition: 'left bottom'};
		case GuildSplashCardAlignment.RIGHT:
			return {transformOrigin: 'bottom right', objectPosition: 'right bottom'};
		default:
			return {transformOrigin: 'bottom center', objectPosition: 'center bottom'};
	}
};

export interface AuthBackgroundProps {
	splashUrl: string | null;
	splashLoaded: boolean;
	splashDimensions?: {width: number; height: number} | null;
	splashScale?: number | null;
	patternReady: boolean;
	patternImageUrl: string;
	className?: string;
	useFullCover?: boolean;
	splashAlignment?: (typeof GuildSplashCardAlignment)[keyof typeof GuildSplashCardAlignment];
}

export const AuthBackground: React.FC<AuthBackgroundProps> = ({
	splashUrl,
	splashLoaded,
	splashDimensions,
	splashScale,
	patternReady,
	patternImageUrl,
	className,
	useFullCover = false,
	splashAlignment = GuildSplashCardAlignment.CENTER,
}) => {
	const shouldShowSplash = splashUrl && splashDimensions && (useFullCover || splashScale);
	const {transformOrigin, objectPosition} = getSplashAlignmentStyles(splashAlignment);

	if (shouldShowSplash) {
		if (useFullCover) {
			return (
				<div className={className}>
					<motion.div
						initial={{opacity: 0}}
						animate={{opacity: splashLoaded ? 1 : 0}}
						transition={{duration: 0.5, ease: 'easeInOut'}}
						style={{position: 'absolute', inset: 0}}
					>
						<img
							src={splashUrl}
							alt=""
							style={{
								position: 'absolute',
								inset: 0,
								width: '100%',
								height: '100%',
								objectFit: 'cover',
								objectPosition,
							}}
						/>
						<div className={styles.splashOverlay} />
					</motion.div>
				</div>
			);
		}

		return (
			<div className={styles.rightSplit}>
				<motion.div
					className={styles.splashImage}
					initial={{opacity: 0}}
					animate={{opacity: splashLoaded ? 1 : 0}}
					transition={{duration: 0.5, ease: 'easeInOut'}}
					style={{
						width: splashDimensions.width,
						height: splashDimensions.height,
						transform: `scale(${splashScale})`,
						transformOrigin,
					}}
				>
					<img
						src={splashUrl}
						alt=""
						width={splashDimensions.width}
						height={splashDimensions.height}
						style={{
							position: 'absolute',
							left: 0,
							top: 0,
							width: '100%',
							height: '100%',
							objectFit: 'cover',
							objectPosition,
						}}
					/>
					<div className={styles.splashOverlay} />
				</motion.div>
			</div>
		);
	}

	if (patternReady) {
		return (
			<div
				className={className || styles.patternHost}
				style={{backgroundImage: `url(${patternImageUrl})`}}
				aria-hidden
			/>
		);
	}

	return null;
};
