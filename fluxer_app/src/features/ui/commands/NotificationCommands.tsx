// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import Notification from '@app/features/ui/state/Notification';
import {formatUserSettingsPath} from '@app/features/user/components/settings_utils/SettingsConstants';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans} from '@lingui/react/macro';

const NOTIFICATIONS_BLOCKED_DESCRIPTOR = msg({
	message: 'Notifications blocked',
	comment: 'Notification toast title shown when the OS has blocked notifications.',
});
const logger = new Logger('Notification');

export function permissionDenied(i18n: I18n, suppressModal = false): void {
	logger.debug('Notification permission denied');
	Notification.handleNotificationPermissionDenied();
	if (suppressModal) return;
	const notificationSettingsPath = formatUserSettingsPath(i18n, 'notifications', 'notifications');
	ModalCommands.push(
		modal(() => (
			<ConfirmModal
				title={i18n._(NOTIFICATIONS_BLOCKED_DESCRIPTOR)}
				description={
					<p data-flx="ui.notification-commands.permission-denied.p">
						<Trans>
							Desktop notifications have been blocked. You can enable them later in your browser settings or in user
							settings, or in {notificationSettingsPath}.
						</Trans>
					</p>
				}
				data-flx="ui.notification-commands.permission-denied.confirm-modal"
			/>
		)),
	);
}

export function permissionGranted(): void {
	logger.debug('Notification permission granted');
	Notification.handleNotificationPermissionGranted();
}

export function toggleUnreadMessageBadge(enabled: boolean): void {
	Notification.handleNotificationSoundToggle(enabled);
}
