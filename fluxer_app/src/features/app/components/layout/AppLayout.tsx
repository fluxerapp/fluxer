// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/layout/AppLayout.module.css';
import {useAppLayoutState} from '@app/features/app/components/layout/app_layout/AppLayoutHooks';
import {DateOfBirthGate} from '@app/features/auth/components/date_of_birth/DateOfBirthGate';
import Authentication from '@app/features/auth/state/Authentication';
import GatewaySessions from '@app/features/gateway/transport/GatewaySessionPool';
import {RecoveryKitReminderGate} from '@app/features/user/components/RecoveryKitReminderGate';
import Users from '@app/features/user/state/Users';
import {MediaDeviceStartupPreloadManager} from '@app/features/voice/components/MediaDeviceStartupPreloadManager';
import {NewDeviceMonitoringManager} from '@app/features/voice/components/NewDeviceMonitoringManager';
import {VoiceReconnectionManager} from '@app/features/voice/components/VoiceReconnectionManager';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';

export const AppLayout = observer(({children}: {children: React.ReactNode}) => {
	const isAuthenticated = Authentication.isAuthenticated;
	const socket = GatewaySessions.foregroundConnection.socket;
	const appState = useAppLayoutState();
	const dateOfBirthRequired = isAuthenticated && Users.currentUser?.dateOfBirthRequired === true;
	const managersEnabled = isAuthenticated && !dateOfBirthRequired;
	return (
		<>
			{managersEnabled && socket && <VoiceReconnectionManager data-flx="app.app-layout.voice-reconnection-manager" />}
			{managersEnabled && (
				<MediaDeviceStartupPreloadManager data-flx="app.app-layout.media-device-startup-preload-manager" />
			)}
			{managersEnabled && <NewDeviceMonitoringManager data-flx="app.app-layout.new-device-monitoring-manager" />}
			{managersEnabled && <RecoveryKitReminderGate data-flx="app.app-layout.recovery-kit-reminder-gate" />}
			<div
				className={clsx(styles.appLayout, appState.isStandalone && styles.appLayoutStandalone)}
				data-flx="app.app-layout.app-layout"
			>
				{children}
			</div>
			{dateOfBirthRequired && <DateOfBirthGate data-flx="app.app-layout.date-of-birth-gate" />}
		</>
	);
});
