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

import {Trans} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useState} from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import styles from '~/components/modals/ChannelDeleteModal.module.css';
import confirmStyles from '~/components/modals/ConfirmModal.module.css';
import * as Modal from '~/components/modals/Modal';
import {Button} from '~/components/uikit/Button/Button';
import {
	type ChannelDeleteModalProps,
	deleteChannel,
	getChannelDeleteInfo,
} from '~/utils/modals/ChannelDeleteModalUtils';

export const ChannelDeleteModal = observer(({channelId}: ChannelDeleteModalProps) => {
	const deleteInfo = getChannelDeleteInfo(channelId);
	const [isSubmitting, setIsSubmitting] = useState(false);

	const onSubmit = async () => {
		if (!deleteInfo) return;

		setIsSubmitting(true);
		try {
			await deleteChannel(channelId);
		} finally {
			setIsSubmitting(false);
		}
	};

	if (!deleteInfo) return null;

	const {channel, isCategory, title, confirmText} = deleteInfo;

	return (
		<Modal.Root size="small" centered>
			<Modal.Header title={title} />
			<Modal.Content className={confirmStyles.content}>
				<p className={clsx(styles.message, confirmStyles.descriptionText)}>
					{isCategory ? (
						<Trans>
							Are you sure you want to delete <strong>{channel.name}</strong>? This cannot be undone.
						</Trans>
					) : (
						<Trans>
							Are you sure you want to delete <strong>{channel.name}</strong>? This cannot be undone.
						</Trans>
					)}
				</p>
			</Modal.Content>
			<Modal.Footer>
				<Button onClick={ModalActionCreators.pop} variant="secondary">
					<Trans>Cancel</Trans>
				</Button>
				<Button onClick={onSubmit} submitting={isSubmitting} variant="danger-primary">
					<Trans>{confirmText}</Trans>
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});
