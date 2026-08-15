// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/layout/NativeTitlebar.module.css';
import {FluxerWordmark} from '@app/features/ui/components/icons/FluxerWordmark';
import {getElectronAPI, type NativePlatform} from '@app/features/ui/utils/NativeUtils';
import type React from 'react';
import {useLayoutEffect} from 'react';
import {NativeWindowControls} from './NativeWindowControls';

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
		<div className={styles.left} data-flx="app.native-titlebar.left">
			<FluxerWordmark className={styles.wordmark} data-flx="app.native-titlebar.wordmark" />
		</div>
	);
	return (
		<div
			role="group"
			className={styles.titlebar}
			onDoubleClick={isMacOS ? undefined : handleDoubleClick}
			data-platform={platform}
			data-native-titlebar=""
			data-flx="app.native-titlebar.titlebar"
		>
			{isMacOS ? (
				<>
					<div className={styles.spacer} data-flx="app.native-titlebar.spacer" />
					{brand}
				</>
			) : (
				<>
					{brand}
					<div className={styles.spacer} data-flx="app.native-titlebar.spacer" />
					<NativeWindowControls data-flx="app.native-titlebar.controls" />
				</>
			)}
		</div>
	);
};
