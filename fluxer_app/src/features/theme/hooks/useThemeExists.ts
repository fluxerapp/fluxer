// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {buildThemeCssFetchUrl} from '@app/features/theme/utils/ThemeUtils';
import {useEffect, useState} from 'react';

const logger = new Logger('useThemeExists');

export type ThemeExistsStatus = 'loading' | 'ready' | 'error';

export const useThemeExists = (themeId: string, runtimeSnapshot: RuntimeConfigSnapshot): ThemeExistsStatus => {
	const [status, setStatus] = useState<ThemeExistsStatus>('loading');
	useEffect(() => {
		let cancelled = false;
		const checkThemeExists = async () => {
			try {
				const themeUrl = buildThemeCssFetchUrl(runtimeSnapshot, themeId);
				if (themeUrl == null) throw new Error('Media endpoint not configured');
				const response = await fetch(themeUrl, {method: 'HEAD'});
				if (!response.ok) throw new Error('Theme not found');
				if (cancelled) return;
				setStatus('ready');
			} catch (error) {
				if (cancelled) return;
				logger.error('Failed to check theme', error);
				setStatus('error');
			}
		};
		setStatus('loading');
		void checkThemeExists();
		return () => {
			cancelled = true;
		};
	}, [runtimeSnapshot, themeId]);
	return status;
};
