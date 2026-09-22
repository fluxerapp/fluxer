// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import styles from '@app/features/auth/components/modals/AccountDeleteModal.module.css';
import {DELETE_ACCOUNT_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import * as UserCommands from '@app/features/user/commands/UserCommands';
import {formatUserSettingsPath} from '@app/features/user/components/settings_utils/SettingsConstants';
import * as FormUtils from '@app/lib/forms';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useState} from 'react';

export const AccountDeleteModal = observer(() => {
	const {i18n} = useLingui();
	const [isSubmitting, setIsSubmitting] = useState(false);
	const privacyDashboardPath = formatUserSettingsPath(i18n, 'privacy_safety');
	const handleConfirm = async () => {
		setIsSubmitting(true);
		try {
			await UserCommands.deleteAccount();
			ModalCommands.pop();
			RouterUtils.transitionTo('/login');
		} catch (error) {
			FormUtils.pushApiErrorModal(i18n, error);
		} finally {
			setIsSubmitting(false);
		}
	};
	return (
		<Modal.Root size="small" centered data-flx="auth.account-delete-modal.modal-root">
			<Modal.Header title={i18n._(DELETE_ACCOUNT_DESCRIPTOR)} data-flx="auth.account-delete-modal.modal-header" />
			<Modal.Content data-flx="auth.account-delete-modal.modal-content">
				<Modal.ContentLayout data-flx="auth.account-delete-modal.modal-content-layout">
					<Modal.Description className={styles.infoSection} data-flx="auth.account-delete-modal.info-section">
						<p data-flx="auth.account-delete-modal.p">
							<Trans>
								Are you sure you want to delete your account? This action will schedule your account for permanent
								deletion.
							</Trans>
						</p>
						<div className={styles.infoBox} data-flx="auth.account-delete-modal.info-box">
							<p className={styles.infoBoxTitle} data-flx="auth.account-delete-modal.info-box-title">
								<Trans>Important information:</Trans>
							</p>
							<ul className={styles.infoList} data-flx="auth.account-delete-modal.info-list">
								<li data-flx="auth.account-delete-modal.li">
									<Trans>You can cancel the deletion process within 14 days</Trans>
								</li>
								<li data-flx="auth.account-delete-modal.li--2">
									<Trans>After 14 days, your account will be permanently deleted</Trans>
								</li>
								<li data-flx="auth.account-delete-modal.li--3">
									<Trans>Once deletion is processed, you cannot recover access to your account</Trans>
								</li>
								<li data-flx="auth.account-delete-modal.li--4">
									<Trans>After your account is deleted, you will not be able to delete the messages you sent</Trans>
								</li>
							</ul>
						</div>
						<p className={styles.disclaimer} data-flx="auth.account-delete-modal.disclaimer">
							<Trans>
								If you want to export your data or delete your messages first, please visit {privacyDashboardPath}{' '}
								before proceeding.
							</Trans>
						</p>
					</Modal.Description>
				</Modal.ContentLayout>
			</Modal.Content>
			<Modal.Footer data-flx="auth.account-delete-modal.modal-footer">
				<Button onClick={ModalCommands.pop} variant="secondary" data-flx="auth.account-delete-modal.button.pop">
					<Trans>Cancel</Trans>
				</Button>
				<Button
					onClick={handleConfirm}
					submitting={isSubmitting}
					variant="danger"
					data-flx="auth.account-delete-modal.button.confirm"
				>
					{i18n._(DELETE_ACCOUNT_DESCRIPTOR)}
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});
