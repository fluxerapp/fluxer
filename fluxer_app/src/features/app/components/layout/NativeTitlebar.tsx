// SPDX-License-Identifier: AGPL-3.0-or-later

import {NativeDownloads} from '@app/features/app/components/layout/NativeDownloads';
import {NativeWindowControls} from '@app/features/app/components/layout/NativeWindowControls';
import {FluxerWordmark} from '@app/features/ui/components/icons/FluxerWordmark';
import FocusRingScope from '@app/features/ui/focus_ring/FocusRingScope';
import {getElectronAPI, type NativePlatform} from '@app/features/ui/utils/NativeUtils';
import {NATIVE_TITLEBAR_CLASS} from '@fluxer/desktop_ipc/src/NativeTitlebarShell';
import type React from 'react';
import {useLayoutEffect, useRef} from 'react';

const STARTUP_NATIVE_TITLEBAR_ID = 'fluxer-startup-native-titlebar';

interface NativeTitlebarProps {
	platform: NativePlatform;
}

export const NativeTitlebar: React.FC<NativeTitlebarProps> = ({platform}) => {
	const isMacOS = platform === 'macos';
	const titlebarRef = useRef<HTMLDivElement>(null);
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
			ref={titlebarRef}
			role="group"
			className={NATIVE_TITLEBAR_CLASS.root}
			onDoubleClick={isMacOS ? undefined : handleDoubleClick}
			data-platform={platform}
			data-native-titlebar=""
			data-flx="app.native-titlebar.titlebar"
		>
			<FocusRingScope containerRef={titlebarRef} data-flx="app.native-titlebar.focus-ring-scope">
				{isMacOS ? (
					<>
						<div className={NATIVE_TITLEBAR_CLASS.spacer} data-flx="app.native-titlebar.spacer" />
						{brand}
						<NativeDownloads data-flx="app.native-titlebar.native-downloads" />
					</>
				) : (
					<>
						{brand}
						<div className={NATIVE_TITLEBAR_CLASS.spacer} data-flx="app.native-titlebar.spacer" />
						<NativeDownloads data-flx="app.native-titlebar.native-downloads" />
						<NativeWindowControls data-flx="app.native-titlebar.controls" />
					</>
				)}
			</FocusRingScope>
		</div>
	);
};
