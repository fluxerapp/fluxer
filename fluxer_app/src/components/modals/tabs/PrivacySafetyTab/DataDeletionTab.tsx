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
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {useForm} from 'react-hook-form';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import * as UserActionCreators from '~/actions/UserActionCreators';
import {Form} from '~/components/form/Form';
import confirmStyles from '~/components/modals/ConfirmModal.module.css';
import * as Modal from '~/components/modals/Modal';
import {Button} from '~/components/uikit/Button/Button';
import {WarningAlert} from '~/components/uikit/WarningAlert/WarningAlert';
import {useFormSubmit} from '~/hooks/useFormSubmit';
import UserStore from '~/stores/UserStore';
import styles from './DataDeletionTab.module.css';

const DeleteAllMessagesModal = observer(() => {
	const {t} = useLingui();
	const form = useForm();

	const onSubmit = async () => {
		await UserActionCreators.bulkDeleteAllMessages();
		ModalActionCreators.pop();
	};

	const {handleSubmit, isSubmitting} = useFormSubmit({
		form,
		onSubmit,
		defaultErrorField: 'form',
	});

	return (
		<Modal.Root size="small" centered>
			<Form form={form} onSubmit={handleSubmit} aria-label={t`Delete all messages form`}>
				<Modal.Header title={t`Delete All Messages`} />
				<Modal.Content className={confirmStyles.content}>
					<div className={styles.infoSection}>
						<p className={confirmStyles.descriptionText}>
							<Trans>Are you sure you want to delete all your messages? This action cannot be undone.</Trans>
						</p>
						<div className={styles.infoBox}>
							<p className={styles.infoBoxTitle}>
								<Trans>What happens next:</Trans>
							</p>
							<ul className={styles.infoList}>
								<li>
									<Trans>Your deletion request will be queued and processed in the background</Trans>
								</li>
								<li>
									<Trans>The job starts 24 hours after you confirm and can be canceled or restarted anytime</Trans>
								</li>
								<li>
									<Trans>The time to complete depends on how many messages you have sent</Trans>
								</li>
								<li>
									<Trans>Once deleted, your messages cannot be recovered</Trans>
								</li>
							</ul>
						</div>
					</div>
				</Modal.Content>
				<Modal.Footer>
					<Button onClick={ModalActionCreators.pop} variant="secondary">
						<Trans>Cancel</Trans>
					</Button>
					<Button type="submit" submitting={isSubmitting} variant="danger-primary">
						<Trans>Delete All Messages</Trans>
					</Button>
				</Modal.Footer>
			</Form>
		</Modal.Root>
	);
});

export const DataDeletionTabContent: React.FC = observer(() => {
	const {t} = useLingui();
	const [isCancelling, setIsCancelling] = React.useState(false);
	const handleDeleteAllMessages = () => {
		ModalActionCreators.push(modal(() => <DeleteAllMessagesModal />));
	};
	const user = UserStore.currentUser;
	const pending = user?.getPendingBulkMessageDeletion();
	const countFormatter = React.useMemo(() => new Intl.NumberFormat(), []);
	const formattedChannelCount = countFormatter.format(pending?.channelCount ?? 0);
	const formattedMessageCount = countFormatter.format(pending?.messageCount ?? 0);
	const scheduledAtLabel = pending?.scheduledAt.toLocaleString() ?? '';

	const handleCancelPendingDeletion = React.useCallback(async () => {
		setIsCancelling(true);
		try {
			await UserActionCreators.cancelBulkDeleteAllMessages();
			ToastActionCreators.createToast({type: 'success', children: t`Pending deletion canceled`});
		} catch (error) {
			ToastActionCreators.error(t`Failed to cancel the deletion. Please try again.`);
			throw error;
		} finally {
			setIsCancelling(false);
		}
	}, []);

	return (
		<div className={styles.deleteSection}>
			{pending && (
				<WarningAlert
					title={<Trans>Pending deletion</Trans>}
					actions={
						<Button
							variant="secondary"
							onClick={handleCancelPendingDeletion}
							disabled={isCancelling}
							fitContainer={true}
						>
							<Trans>Cancel pending deletion</Trans>
						</Button>
					}
				>
					<Trans>
						Deletion will remove <strong>{formattedMessageCount}</strong> messages from{' '}
						<strong>{formattedChannelCount}</strong> channels. Scheduled to run on <strong>{scheduledAtLabel}</strong>.
					</Trans>
				</WarningAlert>
			)}
			<p className={clsx(styles.warningText, confirmStyles.descriptionText)}>
				<Trans>
					Once deleted, your messages cannot be recovered. The deletion process runs in the background and may take some
					time depending on how many messages you have sent.
				</Trans>
			</p>
			{!pending && (
				<Button variant="danger-primary" fitContainer={false} onClick={handleDeleteAllMessages}>
					<Trans>Delete all my messages</Trans>
				</Button>
			)}
		</div>
	);
});
