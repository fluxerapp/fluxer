// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/layout/AuthLayout.module.css';
import {GuildSplashCardAlignment} from '@fluxer/constants/src/GuildConstants';
import type {ValueOf} from '@fluxer/constants/src/ValueOf';
import type React from 'react';

const getSplashAlignmentStyles = (alignment: ValueOf<typeof GuildSplashCardAlignment>) => {
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
	splashDimensions?: {width: number; height: number} | null;
	splashScale?: number | null;
	patternReady: boolean;
	patternImageUrl: string;
	className?: string;
	useFullCover?: boolean;
	splashAlignment?: ValueOf<typeof GuildSplashCardAlignment>;
}

export const AuthBackground: React.FC<AuthBackgroundProps> = ({
	splashUrl,
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
				<div className={className} data-flx="auth.flow.auth-background.div">
					<div style={{position: 'absolute', inset: 0}} data-flx="auth.flow.auth-background.div--2">
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
							data-flx="auth.flow.auth-background.img"
						/>
						<div className={styles.splashOverlay} data-flx="auth.flow.auth-background.splash-overlay" />
					</div>
				</div>
			);
		}
		return (
			<div className={styles.rightSplit} data-flx="auth.flow.auth-background.right-split">
				<div
					className={styles.splashImage}
					style={{
						width: splashDimensions.width,
						height: splashDimensions.height,
						transform: `scale(${splashScale})`,
						transformOrigin,
					}}
					data-flx="auth.flow.auth-background.splash-image"
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
						data-flx="auth.flow.auth-background.img--2"
					/>
					<div className={styles.splashOverlay} data-flx="auth.flow.auth-background.splash-overlay--2" />
				</div>
			</div>
		);
	}
	if (patternReady) {
		return (
			<div
				className={className || styles.patternHost}
				style={{backgroundImage: `url(${patternImageUrl})`}}
				aria-hidden
				data-flx="auth.flow.auth-background.pattern-host"
			/>
		);
	}
	return null;
};
