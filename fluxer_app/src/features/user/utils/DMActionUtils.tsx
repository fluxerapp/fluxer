// SPDX-License-Identifier: AGPL-3.0-or-later

import {showDmActionErrorModal} from '@app/features/app/components/alerts/DmActionErrorModal';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import * as PrivateChannelCommands from '@app/features/channel/commands/PrivateChannelCommands';
import {
	BLOCKED_USER_DM_WARNING_DESCRIPTOR,
	OPEN_DM_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {modal} from '@app/features/ui/commands/ModalCommands';
import type {I18n} from '@lingui/core';

const logger = new Logger('DMActionUtils');
export async function openDMChannelOrShowError(userId: string): Promise<void> {
	try {
		await PrivateChannelCommands.openDMChannel(userId);
	} catch (error) {
		logger.error('Failed to open DM channel:', error);
		showDmActionErrorModal(error);
	}
}
export function blockedDmConfirmModal(
	i18n: I18n,
	userName: string,
	onPrimary: () => void | Promise<void>,
	dataFlx: string,
) {
	return modal(() => (
		<ConfirmModal
			title={i18n._(OPEN_DM_DESCRIPTOR)}
			description={i18n._(BLOCKED_USER_DM_WARNING_DESCRIPTOR, {userName})}
			primaryText={i18n._(OPEN_DM_DESCRIPTOR)}
			primaryVariant="primary"
			onPrimary={onPrimary}
			data-flx={dataFlx}
		/>
	));
}
