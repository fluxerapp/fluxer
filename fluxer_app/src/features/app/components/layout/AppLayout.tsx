// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/layout/AppLayout.module.css';
import {useAppLayoutState} from '@app/features/app/components/layout/app_layout/AppLayoutHooks';
import RequiredActionGate from '@app/features/app/components/layout/RequiredActionGate';
import Initialization from '@app/features/app/state/Initialization';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import AccountManager from '@app/features/auth/state/AccountManager';
import Authentication from '@app/features/auth/state/Authentication';
import GatewayConnection from '@app/features/gateway/transport/GatewayConnection';
import {NewDeviceMonitoringManager} from '@app/features/voice/components/NewDeviceMonitoringManager';
import {VoiceReconnectionManager} from '@app/features/voice/components/VoiceReconnectionManager';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect} from 'react';

export const AppLayout = observer(({children}: {children: React.ReactNode}) => {
	const isAuthenticated = Authentication.isAuthenticated;
	const socket = GatewayConnection.socket;
	const appState = useAppLayoutState();
	useEffect(() => {
		if (Initialization.isLoading) {
			return;
		}
		void AuthenticationCommands.ensureSessionStarted();
	}, [
		isAuthenticated,
		socket,
		GatewayConnection.isConnected,
		GatewayConnection.isConnecting,
		Initialization.isLoading,
		AccountManager.isSwitching,
	]);
	return (
		<>
			{isAuthenticated && socket && <VoiceReconnectionManager data-flx="app.app-layout.voice-reconnection-manager" />}
			{isAuthenticated && <NewDeviceMonitoringManager data-flx="app.app-layout.new-device-monitoring-manager" />}
			{isAuthenticated && <RequiredActionGate data-flx="app.app-layout.required-action-gate" />}
			<div
				className={clsx(styles.appLayout, appState.isStandalone && styles.appLayoutStandalone)}
				data-flx="app.app-layout.app-layout"
			>
				{children}
			</div>
		</>
	);
});
