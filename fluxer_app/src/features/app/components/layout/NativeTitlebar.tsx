// SPDX-License-Identifier: AGPL-3.0-or-later

import {NativeWindowControls} from '@app/features/app/components/layout/NativeWindowControls';
import {FluxerWordmark} from '@app/features/ui/components/icons/FluxerWordmark';
import {getElectronAPI, type NativePlatform} from '@app/features/ui/utils/NativeUtils';
import {NATIVE_TITLEBAR_CLASS} from '@fluxer/desktop_ipc/src/NativeTitlebarShell';
import type React from 'react';
import {useLayoutEffect} from 'react';

const STARTUP_NATIVE_TITLEBAR_ID = 'fluxer-startup-native-titlebar';

interface NativeTitlebarProps {
	platform: NativePlatform;
}

export const NativeTitlebar: React.FC<NativeTitlebarProps> = ({platform}) => {
	const isMacOS = platform === 'macos';
	useLayoutEffect(() => {
		const startupTitlebar = document.getElementById(STARTUP_NATIVE_TITLEBAR_ID);
		if (startupTitlebar != null) {
			startupTitlebar.remove();
		}
	}, []);
	const handleDoubleClick = () => {
		const electronApi = getElectronAPI();
		if (!electronApi?.windowMaximize) return;
		electronApi.windowMaximize();
	};
	const brand = (
		<div className={NATIVE_TITLEBAR_CLASS.left} data-flx="app.native-titlebar.left">
			<FluxerWordmark className={NATIVE_TITLEBAR_CLASS.wordmark} data-flx="app.native-titlebar.wordmark" />
		</div>
	);
	return (
		<div
			role="group"
			className={NATIVE_TITLEBAR_CLASS.root}
			onDoubleClick={isMacOS ? undefined : handleDoubleClick}
			data-platform={platform}
			data-native-titlebar=""
			data-flx="app.native-titlebar.titlebar"
		>
			{isMacOS ? (
				<>
					<div className={NATIVE_TITLEBAR_CLASS.spacer} data-flx="app.native-titlebar.spacer" />
					{brand}
				</>
			) : (
				<>
					{brand}
					<div className={NATIVE_TITLEBAR_CLASS.spacer} data-flx="app.native-titlebar.spacer" />
					<NativeWindowControls data-flx="app.native-titlebar.controls" />
				</>
			)}
		</div>
	);
};
