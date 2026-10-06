// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {loadLazyModule} from '@app/features/platform/utils/LazyModuleLoader';
import {useEffect} from 'react';

const logger = new Logger('MediaDeviceStartupPreloadManager');

export const MediaDeviceStartupPreloadManager: React.FC = () => {
	useEffect(() => {
		let disposed = false;
		let stopPreload: (() => void) | null = null;
		void loadLazyModule(() => import('@app/features/voice/utils/MediaDeviceStartupPreload'))
			.then(({startMediaDeviceStartupPreload}) => {
				if (disposed) return;
				stopPreload = startMediaDeviceStartupPreload();
			})
			.catch((error) => {
				logger.warn('Failed to load media device startup preload', {error});
			});
		return () => {
			disposed = true;
			stopPreload?.();
		};
	}, []);
	return null;
};
