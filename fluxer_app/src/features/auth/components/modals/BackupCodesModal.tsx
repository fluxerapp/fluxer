// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import type {BackupCodesChallenge} from '@app/features/auth/commands/MfaCommands';
import styles from '@app/features/auth/components/modals/BackupCodesModal.module.css';
import {BackupCodesRegenerateModal} from '@app/features/auth/components/modals/BackupCodesRegenerateModal';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import type {User} from '@app/features/user/models/User';
import type {BackupCode} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {CheckIcon, ClipboardIcon, DownloadIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';

const BACKUP_CODES_DESCRIPTOR = msg({
	message: 'Backup codes',
	comment: 'Security modal title for one-time account recovery codes.',
});

interface BackupCodesModalProps {
	backupCodes: ReadonlyArray<BackupCode>;
	user: User;
	challenge?: BackupCodesChallenge;
}

export const BackupCodesModal = observer(({backupCodes, user, challenge}: BackupCodesModalProps) => {
	const {i18n} = useLingui();
	return (
		<Modal.Root size="small" centered data-flx="auth.backup-codes-modal.modal-root">
			<Modal.Header title={i18n._(BACKUP_CODES_DESCRIPTOR)} data-flx="auth.backup-codes-modal.modal-header" />
			<Modal.Content data-flx="auth.backup-codes-modal.modal-content">
				<Modal.ContentLayout data-flx="auth.backup-codes-modal.modal-content-layout">
					<Modal.Description data-flx="auth.backup-codes-modal.description">
						<Trans>Use these codes to access your account if you lose your authenticator app.</Trans>
					</Modal.Description>
					<Modal.Description data-flx="auth.backup-codes-modal.description--2">
						<Trans>We recommend saving these codes now so that you don't get locked out of your account.</Trans>
					</Modal.Description>
					<div className={styles.codesGrid} data-flx="auth.backup-codes-modal.codes-grid">
						{backupCodes.map(({code, consumed}) => (
							<div
								key={code}
								className={`${styles.codeItem} ${consumed ? styles.codeItemConsumed : ''}`}
								data-flx="auth.backup-codes-modal.code-item"
							>
								<div
									className={`${styles.checkbox} ${consumed ? styles.checkboxChecked : styles.checkboxUnchecked}`}
									data-flx="auth.backup-codes-modal.checkbox"
								>
									{consumed && (
										<CheckIcon
											weight="bold"
											className={styles.checkIcon}
											data-flx="auth.backup-codes-modal.check-icon"
										/>
									)}
								</div>
								<code
									className={`${styles.code} ${consumed ? styles.codeConsumed : ''}`}
									data-flx="auth.backup-codes-modal.code"
								>
									{code}
								</code>
							</div>
						))}
					</div>
					<div className={styles.buttonRow} data-flx="auth.backup-codes-modal.button-row">
						<Button
							leftIcon={<DownloadIcon className={styles.buttonIcon} data-flx="auth.backup-codes-modal.button-icon" />}
							small={true}
							onClick={() => {
								const blob = new Blob([backupCodes.map(({code}) => code).join('\n')], {type: 'text/plain'});
								const url = URL.createObjectURL(blob);
								const a = document.createElement('a');
								a.href = url;
								a.download = `fluxer_${user.email}_backup_codes.txt`;
								a.click();
								URL.revokeObjectURL(url);
							}}
							data-flx="auth.backup-codes-modal.button.click"
						>
							<Trans comment="Button that downloads account backup codes as a text file.">Download</Trans>
						</Button>
						<Button
							variant="secondary"
							small={true}
							leftIcon={
								<ClipboardIcon className={styles.buttonIcon} data-flx="auth.backup-codes-modal.button-icon--2" />
							}
							onClick={() => TextCopyCommands.copy(i18n, backupCodes.map(({code}) => code).join('\n'))}
							data-flx="auth.backup-codes-modal.button.copy"
						>
							<Trans comment="Button that copies account backup codes to the clipboard.">Copy</Trans>
						</Button>
						<Button
							variant="danger"
							small={true}
							onClick={() =>
								ModalCommands.push(
									modal(() => (
										<BackupCodesRegenerateModal
											user={user}
											challenge={challenge}
											data-flx="auth.backup-codes-modal.backup-codes-regenerate-modal"
										/>
									)),
								)
							}
							data-flx="auth.backup-codes-modal.button.push"
						>
							<Trans comment="Destructive action that invalidates existing backup codes and creates new ones.">
								Regenerate
							</Trans>
						</Button>
					</div>
				</Modal.ContentLayout>
			</Modal.Content>
		</Modal.Root>
	);
});
