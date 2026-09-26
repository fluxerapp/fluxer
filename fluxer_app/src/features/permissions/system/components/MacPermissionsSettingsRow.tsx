// SPDX-License-Identifier: AGPL-3.0-or-later

import {openMacPermissionsModal} from '@app/features/permissions/system/commands/MacPermissionsModalCommands';
import styles from '@app/features/permissions/system/components/MacPermissionsSettingsRow.module.css';
import {Button} from '@app/features/ui/button/Button';
import {getNativePlatformSync, isDesktop} from '@app/features/ui/utils/NativeUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const TITLE_DESCRIPTOR = msg({
	message: 'macOS permissions',
	comment: 'Settings row title for reviewing macOS permissions.',
});
const DESCRIPTION_DESCRIPTOR = msg({
	message: 'Review microphone, camera, screen recording, and Input Monitoring access.',
	comment: 'Settings row description for reviewing macOS permissions.',
});
const REVIEW_DESCRIPTOR = msg({
	message: 'Review',
	comment: 'Button label for opening the macOS permissions modal.',
});

interface MacPermissionsSettingsRowProps extends React.HTMLAttributes<HTMLDivElement> {
	readonly showTitle?: boolean;
}

export const MacPermissionsSettingsRow: React.FC<MacPermissionsSettingsRowProps> = observer(
	({showTitle = true, ...props}) => {
		const {i18n} = useLingui();
		if (!isDesktop() || getNativePlatformSync() !== 'macos') return null;
		return (
			<div className={styles.row} data-flx="permissions.mac-permissions-settings-row.row" {...props}>
				<div className={styles.text} data-flx="permissions.mac-permissions-settings-row.text">
					{showTitle ? (
						<div className={styles.title} data-flx="permissions.mac-permissions-settings-row.title">
							{i18n._(TITLE_DESCRIPTOR)}
						</div>
					) : null}
					<p className={styles.description} data-flx="permissions.mac-permissions-settings-row.description">
						{i18n._(DESCRIPTION_DESCRIPTOR)}
					</p>
				</div>
				<Button
					variant="secondary"
					small={true}
					onClick={() => openMacPermissionsModal()}
					data-flx="permissions.mac-permissions-settings-row.button.review"
				>
					{i18n._(REVIEW_DESCRIPTOR)}
				</Button>
			</div>
		);
	},
);
