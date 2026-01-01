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

import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans} from '@lingui/react/macro';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {Logger} from '~/lib/Logger';
import NotificationStore from '~/stores/NotificationStore';

const logger = new Logger('Notification');

export const permissionDenied = (i18n: I18n, suppressModal = false): void => {
	logger.debug('Notification permission denied');
	NotificationStore.handleNotificationPermissionDenied();

	if (suppressModal) return;

	ModalActionCreators.push(
		modal(() => (
			<ConfirmModal
				title={i18n._(msg`Notifications Blocked`)}
				description={
					<p>
						<Trans>
							Desktop notifications have been blocked. You can enable them later in your browser settings or in User
							Settings &gt; Notifications.
						</Trans>
					</p>
				}
				primaryText={i18n._(msg`OK`)}
				primaryVariant="primary"
				secondaryText={false}
				onPrimary={() => {}}
			/>
		)),
	);
};

export const permissionGranted = (): void => {
	logger.debug('Notification permission granted');
	NotificationStore.handleNotificationPermissionGranted();
};

export const toggleUnreadMessageBadge = (enabled: boolean): void => {
	NotificationStore.handleNotificationSoundToggle(enabled);
};
