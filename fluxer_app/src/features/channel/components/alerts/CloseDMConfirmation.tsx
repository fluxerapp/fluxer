// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import * as ChannelCommands from '@app/features/channel/commands/ChannelCommands';
import {DMCloseFailedModal} from '@app/features/channel/components/alerts/DMCloseFailedModal';
import {CLOSE_DM_DESCRIPTOR} from '@app/features/channel/utils/ChannelMessageDescriptors';
import {DM_CLOSED_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {ME} from '@fluxer/constants/src/AppConstants';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const logger = new Logger('CloseDMConfirmation');
const CLOSE_DM_CONFIRMATION_DESCRIPTOR = msg({
	message: 'Close your DM with {displayName}? You can reopen it anytime.',
	comment: 'Confirm dialog body before closing a DM channel.',
});

export function showCloseDMConfirmation({
	i18n,
	channelId,
	displayName,
	beforeOpen,
}: {
	i18n: I18n;
	channelId: string;
	displayName: string;
	beforeOpen?: () => void;
}): void {
	const confirmation = modal(() => (
		<ConfirmModal
			title={i18n._(CLOSE_DM_DESCRIPTOR)}
			description={i18n._(CLOSE_DM_CONFIRMATION_DESCRIPTOR, {displayName})}
			primaryText={i18n._(CLOSE_DM_DESCRIPTOR)}
			primaryVariant="danger"
			onPrimary={async () => {
				try {
					await ChannelCommands.remove(channelId);
					if (SelectedChannel.selectedChannelIds.get(ME) === channelId) RouterUtils.transitionTo(Routes.ME);
					ToastCommands.createToast({type: 'success', children: i18n._(DM_CLOSED_DESCRIPTOR)});
				} catch (error) {
					logger.error('Failed to close DM:', error);
					window.setTimeout(
						() =>
							ModalCommands.push(
								modal(() => <DMCloseFailedModal data-flx="channel.close-dm-confirmation.dm-close-failed-modal" />),
							),
						0,
					);
				}
			}}
			data-flx="channel.close-dm-confirmation.confirm-modal"
		/>
	));
	if (beforeOpen) ModalCommands.pushAfterBottomSheetClose(beforeOpen, confirmation);
	else ModalCommands.push(confirmation);
}
