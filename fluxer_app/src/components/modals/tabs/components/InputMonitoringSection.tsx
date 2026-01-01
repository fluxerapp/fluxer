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

import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {Button} from '~/components/uikit/Button/Button';
import KeybindManager from '~/lib/KeybindManager';
import NativePermissionStore from '~/stores/NativePermissionStore';
import {openNativePermissionSettings, requestNativePermission} from '~/utils/NativePermissions';
import styles from '../KeybindsTab.module.css';

export const InputMonitoringSection: React.FC = observer(() => {
	const {t} = useLingui();
	const [requesting, setRequesting] = React.useState(false);

	const status = NativePermissionStore.inputMonitoringStatus;

	if (!NativePermissionStore.shouldShowInputMonitoringBanner) {
		return null;
	}

	const handleRequest = async () => {
		setRequesting(true);
		const result = await requestNativePermission('input-monitoring');
		setRequesting(false);
		NativePermissionStore.setInputMonitoringStatus(result);

		if (result === 'granted') {
			ToastActionCreators.createToast({type: 'success', children: t`Input Monitoring enabled`});
			await KeybindManager.reapplyGlobalShortcuts();
		} else if (result === 'denied') {
			await openNativePermissionSettings('input-monitoring');
			ToastActionCreators.error(t`Please enable Fluxer in System Settings → Privacy & Security → Input Monitoring.`);
		}
	};

	const statusLabel = status === 'denied' ? t`Not granted` : status === 'not-determined' ? t`Not granted` : t`Granted`;

	return (
		<div className={styles.permissionCard}>
			<div className={styles.permissionText}>
				<div className={styles.permissionTitle}>
					<Trans>Input Monitoring</Trans>
				</div>
				<p className={styles.permissionDescription}>
					<Trans>
						Fluxer needs Input Monitoring permission to keep push-to-talk and global shortcuts working while the window
						is in the background.
					</Trans>
				</p>
				{status === 'denied' ? (
					<p className={styles.permissionHelp}>
						<Trans>Click "Open Settings" to open System Settings, then enable Fluxer in Input Monitoring.</Trans>
					</p>
				) : null}
			</div>
			<div className={styles.permissionActions}>
				<span className={styles.permissionStatus} data-status={status}>
					{statusLabel}
				</span>
				<Button variant="secondary" small={true} onClick={handleRequest} submitting={requesting}>
					{status === 'denied' ? t`Open Settings` : t`Enable`}
				</Button>
			</div>
		</div>
	);
});
