// SPDX-License-Identifier: AGPL-3.0-or-later

import {clearRemScaleCache} from '@app/features/theme/layout/RemFromPx';
import type {NativePlatform} from '@app/features/ui/utils/NativeUtils';
import {useInsertionEffect} from 'react';

export function usePlatformClasses(platform: NativePlatform, isNative: boolean): void {
	useInsertionEffect(() => {
		const htmlNode = document.documentElement;
		const platformClasses = [isNative ? 'platform-native' : 'platform-web', `platform-${platform}`];
		htmlNode.classList.add(...platformClasses);
		clearRemScaleCache();
		return () => {
			htmlNode.classList.remove(...platformClasses);
			clearRemScaleCache();
		};
	}, [isNative, platform]);
}
