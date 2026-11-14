// SPDX-License-Identifier: AGPL-3.0-or-later

import {showGenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModalCommands';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {DELETE_MY_MESSAGES_DESCRIPTOR} from '@app/features/channel/utils/ChannelMessageDescriptors';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {useLingui} from '@lingui/react/macro';
import {useCallback} from 'react';

const logger = new Logger('useConfirmDeleteMyMessages');
interface ConfirmDeleteMyMessagesOptions {
	title: string;
	description: string;
	run: () => Promise<void>;
	dataFlx: string;
	toastText: string;
	errorTitle: string;
	errorMessage: string;
}
export function useConfirmDeleteMyMessages() {
	const {i18n} = useLingui();
	return useCallback(
		({title, description, run, dataFlx, toastText, errorTitle, errorMessage}: ConfirmDeleteMyMessagesOptions) => {
			ModalCommands.push(
				ModalCommands.modal(() => (
					<ConfirmModal
						title={title}
						description={description}
						primaryText={i18n._(DELETE_MY_MESSAGES_DESCRIPTOR)}
						primaryVariant="danger"
						onPrimary={async () => {
							try {
								await run();
								ToastCommands.createToast({type: 'success', children: toastText});
							} catch (error) {
								logger.error('Failed to delete user messages', error);
								showGenericErrorModal({
									title: errorTitle,
									message: errorMessage,
									dataFlx: dataFlx.replace('.confirm-modal', '.generic-error-modal'),
									defer: true,
								});
							}
						}}
						data-flx={dataFlx}
					/>
				)),
			);
		},
		[i18n],
	);
}
