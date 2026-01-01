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

import React from 'react';
import DeveloperOptionsStore from '~/stores/DeveloperOptionsStore';
import {
	getNativePlatform,
	guessPlatform,
	isDesktop,
	isNativeLinux,
	isNativeMacOS,
	isNativeWindows,
	type NativePlatform,
} from '~/utils/NativeUtils';

export interface NativePlatformState {
	platform: NativePlatform;
	isNative: boolean;
	isMacOS: boolean;
	isWindows: boolean;
	isLinux: boolean;
}

export const useNativePlatform = (): NativePlatformState => {
	const [platform, setPlatform] = React.useState<NativePlatform>(guessPlatform());
	const platformOverride = DeveloperOptionsStore.mockTitlebarPlatformOverride;
	const hasOverride = platformOverride !== 'auto';
	const isNative = isDesktop() || hasOverride;

	React.useEffect(() => {
		if (!isNative) return;
		let cancelled = false;
		void getNativePlatform().then((value) => {
			if (!cancelled && value) {
				setPlatform(value);
			}
		});
		return () => {
			cancelled = true;
		};
	}, [isNative]);

	const effectivePlatform = hasOverride ? platformOverride : platform;

	return {
		platform: effectivePlatform,
		isNative,
		isMacOS: isNative && isNativeMacOS(effectivePlatform),
		isWindows: isNative && isNativeWindows(effectivePlatform),
		isLinux: isNative && isNativeLinux(effectivePlatform),
	};
};
