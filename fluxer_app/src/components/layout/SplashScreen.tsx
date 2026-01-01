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

import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {FluxerIcon} from '~/components/icons/FluxerIcon';
import ConnectionStore from '~/stores/ConnectionStore';
import DeveloperOptionsStore from '~/stores/DeveloperOptionsStore';
import InitializationStore from '~/stores/InitializationStore';
import {NativeDragRegion} from './NativeDragRegion';
import styles from './SplashScreen.module.css';

const SPLASH_SCREEN_DELAY = 10000;

export const SplashScreen = observer(() => {
	const shouldBypass = DeveloperOptionsStore.bypassSplashScreen;
	const connected = ConnectionStore.isConnected;
	const isInitialized = InitializationStore.canNavigateToProtectedRoutes;
	const [showSplash, setShowSplash] = React.useState(true);

	React.useEffect(() => {
		if (connected && isInitialized) {
			setShowSplash(false);
			return;
		}

		const timer = setTimeout(() => setShowSplash(true), SPLASH_SCREEN_DELAY);
		return () => clearTimeout(timer);
	}, [connected, isInitialized]);

	if (shouldBypass) return null;
	return <AnimatePresence initial={false}>{showSplash && <SplashScreenContent />}</AnimatePresence>;
});

const SplashScreenContent = observer(() => {
	return (
		<motion.div
			initial={{opacity: 0}}
			animate={{opacity: 1}}
			exit={{opacity: 0}}
			transition={{duration: 0.5}}
			className={styles.splashOverlay}
		>
			<NativeDragRegion className={styles.topDragRegion} />
			<div className={styles.splashContent}>
				<div className={styles.iconWrapper}>
					<div className={styles.iconPulse} />
					<FluxerIcon className={styles.icon} />
				</div>
			</div>
		</motion.div>
	);
});
