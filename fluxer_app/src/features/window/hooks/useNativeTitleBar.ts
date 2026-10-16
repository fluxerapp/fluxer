// SPDX-License-Identifier: AGPL-3.0-or-later

import {getDesktopWindowBehaviorSettings} from '@app/features/ui/utils/DesktopWindowBehaviorUtils';
import {getNativePlatformSync, isDesktop, isNativeMacOS} from '@app/features/ui/utils/NativeUtils';
import {useEffect, useState} from 'react';

const TITLEBAR_HEIGHT_CSS_VAR = '--native-titlebar-height';
const CUSTOM_TITLEBAR_HEIGHT = '32px';
const NATIVE_TITLEBAR_HEIGHT = '0px';
const NATIVE_SYSTEM_TITLEBAR_CLASS = 'native-system-titlebar';

function supportsSystemTitleBar(): boolean {
	return isDesktop() && !isNativeMacOS(getNativePlatformSync());
}

function getInitialUseSystemTitleBar(): boolean {
	return supportsSystemTitleBar() && document.documentElement.classList.contains(NATIVE_SYSTEM_TITLEBAR_CLASS);
}

export function useNativeTitleBar(): boolean {
	const [useSystemTitleBar, setUseSystemTitleBar] = useState(getInitialUseSystemTitleBar);
	useEffect(() => {
		if (!isDesktop()) {
			return;
		}
		if (!supportsSystemTitleBar()) {
			setUseSystemTitleBar(false);
			return;
		}
		let mounted = true;
		void getDesktopWindowBehaviorSettings().then((settings) => {
			if (!mounted || !settings) {
				return;
			}
			const active =
				typeof settings.activeUseNativeTitleBar === 'boolean'
					? settings.activeUseNativeTitleBar
					: settings.useNativeTitleBar;
			setUseSystemTitleBar(Boolean(active));
		});
		return () => {
			mounted = false;
		};
	}, []);
	useEffect(() => {
		if (!isDesktop()) {
			return;
		}
		document.documentElement.classList.toggle(NATIVE_SYSTEM_TITLEBAR_CLASS, useSystemTitleBar);
		document.documentElement.style.setProperty(
			TITLEBAR_HEIGHT_CSS_VAR,
			useSystemTitleBar ? NATIVE_TITLEBAR_HEIGHT : CUSTOM_TITLEBAR_HEIGHT,
		);
	}, [useSystemTitleBar]);
	return useSystemTitleBar;
}
