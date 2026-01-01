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
import {useCallback} from 'react';
import * as MessageActionCreators from '~/actions/MessageActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import type {MessageRecord} from '~/records/MessageRecord';

export const useDeleteAttachment = (message: MessageRecord | undefined, attachmentId: string | undefined) => {
	const {t} = useLingui();

	return useCallback(
		(e: React.MouseEvent) => {
			e.preventDefault();
			e.stopPropagation();

			if (!message || !attachmentId) return;

			ModalActionCreators.push(
				modal(() => (
					<ConfirmModal
						title={t`Delete Attachment`}
						description={
							<Trans>
								Are you sure you want to delete this attachment? This action cannot be undone and will remove the
								attachment from this message.
							</Trans>
						}
						primaryText={t`Delete Attachment`}
						primaryVariant="danger-primary"
						onPrimary={async () => {
							await MessageActionCreators.deleteAttachment(message.channelId, message.id, attachmentId);
						}}
					/>
				)),
			);
		},
		[message, attachmentId, t],
	);
};
