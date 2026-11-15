// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/layout/AuthLayout.module.css';
import {GuildSplashCardAlignment} from '@fluxer/constants/src/GuildConstants';
import type {ValueOf} from '@fluxer/constants/src/ValueOf';
import clsx from 'clsx';
import type React from 'react';
import {useState} from 'react';

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

export const AuthBackdropScrim = Object.freeze({
	CARD: 'card',
	MOBILE: 'mobile',
} as const);

export type AuthBackdropScrim = (typeof AuthBackdropScrim)[keyof typeof AuthBackdropScrim];

export interface AuthBackgroundProps {
	splashUrl: string | null;
	splashDimensions?: {width: number; height: number} | null;
	backdropUrl?: string | null;
	backdropScrim?: AuthBackdropScrim;
	className?: string;
	splashAlignment?: ValueOf<typeof GuildSplashCardAlignment>;
}

interface AuthBackdropProps {
	url: string;
	scrim: AuthBackdropScrim;
}

function AuthBackdrop({url, scrim}: AuthBackdropProps): React.ReactElement | null {
	const [status, setStatus] = useState<'loading' | 'ready' | 'failed'>('loading');
	if (status === 'failed') {
		return null;
	}
	return (
		<div
			className={clsx(styles.backdropLayer, status === 'ready' && styles.backdropLayerReady)}
			data-flx="auth.flow.auth-background.backdrop"
		>
			<img
				className={styles.backdropImage}
				src={url}
				alt=""
				draggable={false}
				decoding="async"
				referrerPolicy="no-referrer"
				onLoad={() => setStatus('ready')}
				onError={() => setStatus('failed')}
				data-flx="auth.flow.auth-background.backdrop-image"
			/>
			<div
				className={clsx(styles.backdropScrim, scrim === AuthBackdropScrim.MOBILE && styles.backdropScrimMobile)}
				data-flx="auth.flow.auth-background.backdrop-scrim"
			/>
		</div>
	);
}

export const AuthBackground: React.FC<AuthBackgroundProps> = ({
	splashUrl,
	splashDimensions,
	backdropUrl = null,
	backdropScrim = AuthBackdropScrim.CARD,
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
	if (splashUrl == null && backdropUrl != null) {
		return (
			<div className={clsx(className, styles.operatorBackdrop)} data-flx="auth.flow.auth-background.operator-backdrop">
				<AuthBackdrop
					key={backdropUrl}
					url={backdropUrl}
					scrim={backdropScrim}
					data-flx="auth.flow.auth-background.auth-backdrop"
				/>
			</div>
		);
	}
	return null;
};
