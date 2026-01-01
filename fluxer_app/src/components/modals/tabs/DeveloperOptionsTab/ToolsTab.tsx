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

import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useState} from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {testBulkDeleteAllMessages} from '~/actions/UserActionCreators';
import {CaptchaModal} from '~/components/modals/CaptchaModal';
import {ClaimAccountModal} from '~/components/modals/ClaimAccountModal';
import {KeyboardModeIntroModal} from '~/components/modals/KeyboardModeIntroModal';
import {Button} from '~/components/uikit/Button/Button';
import type {GatewaySocket} from '~/lib/GatewaySocket';
import NewDeviceMonitoringStore from '~/stores/NewDeviceMonitoringStore';
import MediaEngineStore from '~/stores/voice/MediaEngineFacade';
import styles from './ToolsTab.module.css';

interface ToolsTabContentProps {
	socket: GatewaySocket;
}

export const ToolsTabContent: React.FC<ToolsTabContentProps> = observer(({socket}) => {
	const {t} = useLingui();
	const [isTestingBulkDelete, setIsTestingBulkDelete] = useState(false);
	const [shouldCrash, setShouldCrash] = useState(false);

	const handleTestBulkDelete = useCallback(async () => {
		setIsTestingBulkDelete(true);
		try {
			await testBulkDeleteAllMessages();
		} finally {
			setIsTestingBulkDelete(false);
		}
	}, []);

	const handleOpenCaptchaModal = useCallback(() => {
		ModalActionCreators.push(
			ModalActionCreators.modal(() => (
				<CaptchaModal
					closeOnVerify={false}
					onVerify={(token, captchaType) => {
						console.debug('Captcha solved in Developer Options', {token, captchaType});
					}}
					onCancel={() => {
						console.debug('Captcha cancelled in Developer Options');
					}}
				/>
			)),
		);
	}, []);

	const handleOpenClaimAccountModal = useCallback(() => {
		ModalActionCreators.push(ModalActionCreators.modal(() => <ClaimAccountModal />));
	}, []);

	if (shouldCrash) {
		return {} as any;
	}

	return (
		<div className={styles.buttonGroup}>
			<Button onClick={() => socket.reset()}>{t`Reset Socket`}</Button>
			<Button onClick={() => socket.simulateNetworkDisconnect()}>{t`Disconnect Socket`}</Button>
			<Button
				onClick={() => {
					void MediaEngineStore.moveToAfkChannel();
				}}
			>
				{t`Force Move to AFK Channel`}
			</Button>
			<Button onClick={() => NewDeviceMonitoringStore.showTestModal()}>{t`Show New Device Modal`}</Button>
			<Button onClick={handleOpenCaptchaModal}>{t`Open Captcha Modal`}</Button>

			<Button
				onClick={() => {
					ModalActionCreators.push(ModalActionCreators.modal(() => <KeyboardModeIntroModal />));
				}}
			>
				{t`Show Keyboard Mode Intro`}
			</Button>
			<Button onClick={handleOpenClaimAccountModal}>{t`Open Claim Account Modal`}</Button>
			<Button onClick={() => void handleTestBulkDelete()} submitting={isTestingBulkDelete} variant="danger-primary">
				{t`Test Bulk Delete (60s)`}
			</Button>
			<Button onClick={() => setShouldCrash(true)} variant="danger-primary">
				{t`Trigger React Crash`}
			</Button>
		</div>
	);
});
