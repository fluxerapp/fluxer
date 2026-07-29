// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/layout/NativeTitlebar.module.css';
import {FluxerWordmark} from '@app/features/ui/components/icons/FluxerWordmark';
import {getElectronAPI, type NativePlatform} from '@app/features/ui/utils/NativeUtils';
import type React from 'react';
import {NativeWindowControls} from './NativeWindowControls';
import { NativeTitlebarNav } from './NativeTitlebarNav';

interface NativeTitlebarProps {
	platform: NativePlatform;
}

export const NativeTitlebar: React.FC<NativeTitlebarProps> = ({platform}) => {
	const handleDoubleClick = () => {
		const electronApi = getElectronAPI();
		if (!electronApi?.windowMaximize) return;
		electronApi.windowMaximize();
	};
	return (
		<div
			role="group"
			className={styles.titlebar}
			onDoubleClick={handleDoubleClick}
			data-platform={platform}
			data-native-titlebar=""
			data-flx="app.native-titlebar.titlebar"
		>
			<div className={styles.left} data-flx="app.native-titlebar.left">
				<FluxerWordmark className={styles.wordmark} data-flx="app.native-titlebar.wordmark" />
				<NativeTitlebarNav 
					onDoubleClick={event?.preventDefault()} 
					direction="back" 
					data-flx="app.native-titlebar.nav"
				/>
				<NativeTitlebarNav 
					onDoubleClick={event?.preventDefault()} 
					direction="forward" 
					data-flx="app.native-titlebar.nav"
				/>
			</div>
			<div className={styles.spacer} data-flx="app.native-titlebar.spacer" />
			<NativeWindowControls data-flx="app.native-titlebar.controls" />
		</div>
	);
};
