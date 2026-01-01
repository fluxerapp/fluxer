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

import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as AuthenticationActionCreators from '~/actions/AuthenticationActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {SplashScreen} from '~/components/layout/SplashScreen';
import RequiredActionModal from '~/components/modals/RequiredActionModal';
import {NewDeviceMonitoringManager} from '~/components/voice/NewDeviceMonitoringManager';
import {VoiceReconnectionManager} from '~/components/voice/VoiceReconnectionManager';
import AccountManager from '~/stores/AccountManager';
import AuthenticationStore from '~/stores/AuthenticationStore';
import ConnectionStore from '~/stores/ConnectionStore';
import InitializationStore from '~/stores/InitializationStore';
import ModalStore from '~/stores/ModalStore';
import UserStore from '~/stores/UserStore';
import styles from './AppLayout.module.css';
import {useAppLayoutState} from './app-layout/hooks';

export const AppLayout = observer(({children}: {children: React.ReactNode}) => {
	const isAuthenticated = AuthenticationStore.isAuthenticated;
	const socket = ConnectionStore.socket;
	const user = UserStore.currentUser;

	const appState = useAppLayoutState();

	React.useEffect(() => {
		if (InitializationStore.isLoading) {
			return;
		}
		void AuthenticationActionCreators.ensureSessionStarted();
	}, [
		isAuthenticated,
		socket,
		ConnectionStore.isConnected,
		ConnectionStore.isConnecting,
		InitializationStore.isLoading,
		AccountManager.isSwitching,
	]);

	React.useEffect(() => {
		const hasRequired = !!(user?.requiredActions && user.requiredActions.length > 0);
		const isOpen = ModalStore.getModal()?.key === 'required-actions';
		if (hasRequired && !isOpen) {
			ModalActionCreators.pushWithKey(
				modal(() => <RequiredActionModal mock={false} />),
				'required-actions',
			);
		}
		if (!hasRequired && isOpen) {
			ModalActionCreators.pop();
		}
	}, [user?.requiredActions?.length]);

	return (
		<>
			{isAuthenticated && <SplashScreen />}

			{isAuthenticated && socket && <VoiceReconnectionManager />}
			{isAuthenticated && <NewDeviceMonitoringManager />}
			<div className={clsx(styles.appLayout, appState.isStandalone && styles.appLayoutStandalone)}>{children}</div>
		</>
	);
});
