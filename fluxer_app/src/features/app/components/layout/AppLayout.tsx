// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/layout/AppLayout.module.css';
import {useAppLayoutState} from '@app/features/app/components/layout/app_layout/AppLayoutHooks';
import Authentication from '@app/features/auth/state/Authentication';
import GatewayConnection from '@app/features/gateway/transport/GatewayConnection';
import {RecoveryKitReminderGate} from '@app/features/user/components/RecoveryKitReminderGate';
import {MediaDeviceStartupPreloadManager} from '@app/features/voice/components/MediaDeviceStartupPreloadManager';
import {NewDeviceMonitoringManager} from '@app/features/voice/components/NewDeviceMonitoringManager';
import {VoiceReconnectionManager} from '@app/features/voice/components/VoiceReconnectionManager';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';

export const AppLayout = observer(({children}: {children: React.ReactNode}) => {
	const isAuthenticated = Authentication.isAuthenticated;
	const socket = GatewayConnection.socket;
	const appState = useAppLayoutState();
	return (
		<>
			{isAuthenticated && socket && <VoiceReconnectionManager data-flx="app.app-layout.voice-reconnection-manager" />}
			{isAuthenticated && (
				<MediaDeviceStartupPreloadManager data-flx="app.app-layout.media-device-startup-preload-manager" />
			)}
			{isAuthenticated && <NewDeviceMonitoringManager data-flx="app.app-layout.new-device-monitoring-manager" />}
			{isAuthenticated && <RecoveryKitReminderGate data-flx="app.app-layout.recovery-kit-reminder-gate" />}
			<div
				className={clsx(styles.appLayout, appState.isStandalone && styles.appLayoutStandalone)}
				data-flx="app.app-layout.app-layout"
			>
				{children}
			</div>
		</>
	);
});
