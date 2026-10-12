// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/layout/AuthLayout.module.css';
import {GuildSplashCardAlignment} from '@fluxer/constants/src/GuildConstants';
import type {ValueOf} from '@fluxer/constants/src/ValueOf';
import type React from 'react';

const getSplashObjectPosition = (alignment: ValueOf<typeof GuildSplashCardAlignment>) => {
	switch (alignment) {
		case GuildSplashCardAlignment.LEFT:
			return 'left bottom';
		case GuildSplashCardAlignment.RIGHT:
			return 'right bottom';
		default:
			return 'center bottom';
	}
};

export interface AuthBackgroundProps {
	splashUrl: string | null;
	splashDimensions?: {width: number; height: number} | null;
	patternImageUrl?: string | null;
	className?: string;
	splashAlignment?: ValueOf<typeof GuildSplashCardAlignment>;
}

export const AuthBackground: React.FC<AuthBackgroundProps> = ({
	splashUrl,
	splashDimensions,
	patternImageUrl,
	className,
	splashAlignment = GuildSplashCardAlignment.CENTER,
}) => {
	if (splashUrl && splashDimensions) {
		return (
			<div className={className} data-flx="auth.flow.auth-background.div">
				<img
					src={splashUrl}
					alt=""
					style={{
						position: 'absolute',
						inset: 0,
						width: '100%',
						height: '100%',
						objectFit: 'cover',
						objectPosition: getSplashObjectPosition(splashAlignment),
					}}
					data-flx="auth.flow.auth-background.img"
				/>
				<div className={styles.splashOverlay} data-flx="auth.flow.auth-background.splash-overlay" />
			</div>
		);
	}
	if (patternImageUrl != null) {
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
