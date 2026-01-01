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
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as UserActionCreators from '~/actions/UserActionCreators';
import confirmStyles from '~/components/modals/ConfirmModal.module.css';
import * as Modal from '~/components/modals/Modal';
import {Button} from '~/components/uikit/Button/Button';
import styles from './DataDeletionTab.module.css';

const RequestDataExportModal = observer(() => {
	const {t} = useLingui();
	const [isSubmitting, setIsSubmitting] = React.useState(false);

	const handleSubmit = async () => {
		try {
			setIsSubmitting(true);
			await UserActionCreators.requestDataHarvest();
			ModalActionCreators.pop();
		} catch (_error) {
			setIsSubmitting(false);
		}
	};

	return (
		<Modal.Root size="small" centered>
			<Modal.Header title={t`Request Data Export`} />
			<Modal.Content className={confirmStyles.content}>
				<div className={styles.infoSection}>
					<p className={confirmStyles.descriptionText}>
						<Trans>
							Your data export will include all your user information, messages, and URLs to download any attachments.
						</Trans>
					</p>
					<div className={styles.infoBox}>
						<p className={styles.infoBoxTitle}>
							<Trans>What happens next:</Trans>
						</p>
						<ul className={styles.infoList}>
							<li>
								<Trans>Your export request will be processed</Trans>
							</li>
							<li>
								<Trans>You'll receive an email when your data package is ready</Trans>
							</li>
							<li>
								<Trans>The download link will be valid for 7 days</Trans>
							</li>
							<li>
								<Trans>You can request a new export once every 7 days</Trans>
							</li>
						</ul>
					</div>
					<p className={styles.warningText}>
						<Trans>
							Remember to download your attachments before deleting any messages, as deleting a message will also delete
							its attachments.
						</Trans>
					</p>
				</div>
			</Modal.Content>
			<Modal.Footer>
				<Button onClick={ModalActionCreators.pop} variant="secondary">
					<Trans>Cancel</Trans>
				</Button>
				<Button type="button" onClick={handleSubmit} submitting={isSubmitting} variant="primary">
					<Trans>Request Export</Trans>
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});

export const DataExportTabContent: React.FC = observer(() => {
	const handleRequestDataHarvest = () => {
		ModalActionCreators.push(modal(() => <RequestDataExportModal />));
	};

	return (
		<div className={styles.deleteSection}>
			<div className={styles.infoBox}>
				<p className={styles.infoBoxTitle}>
					<Trans>What's included in your export:</Trans>
				</p>
				<ul className={styles.infoList}>
					<li>
						<Trans>All your user account information</Trans>
					</li>
					<li>
						<Trans>All messages you have sent across the platform</Trans>
					</li>
					<li>
						<Trans>URLs to download any attachments from your messages</Trans>
					</li>
				</ul>
			</div>
			<p className={clsx(styles.warningText, confirmStyles.descriptionText)}>
				<Trans>
					You can request a data export once every 7 days. You'll receive an email with a download link valid for 7
					days.
				</Trans>
			</p>
			<Button variant="primary" fitContainer={false} onClick={handleRequestDataHarvest}>
				<Trans>Request Data Export</Trans>
			</Button>
		</div>
	);
});
