// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import type {BackupCodesChallenge} from '@app/features/auth/commands/MfaCommands';
import * as MfaCommands from '@app/features/auth/commands/MfaCommands';
import {BackupCodesModal} from '@app/features/auth/components/modals/BackupCodesModal';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import type {User} from '@app/features/user/models/User';
import * as FormUtils from '@app/lib/forms';
import type {BackupCode} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useState} from 'react';

const BACKUP_CODES_REGENERATED_DESCRIPTOR = msg({
	message: 'Backup codes regenerated',
	comment: 'Toast shown after new account recovery backup codes were generated.',
});
const REGENERATE_BACKUP_CODES_DESCRIPTOR = msg({
	message: 'Regenerate backup codes',
	comment: 'Security confirmation title for replacing account recovery backup codes.',
});

interface BackupCodesRegenerateModalProps {
	user: User;
	challenge?: BackupCodesChallenge;
}

interface RegenerateResult {
	backupCodes: Array<BackupCode>;
	challenge?: BackupCodesChallenge;
}

async function regenerateBackupCodes(challenge?: BackupCodesChallenge): Promise<RegenerateResult> {
	if (challenge) {
		try {
			return {backupCodes: await MfaCommands.regenerateBackupCodesWithChallenge(challenge), challenge};
		} catch (error) {
			if (!MfaCommands.isExpiredBackupCodesChallengeError(error)) {
				throw error;
			}
		}
	}
	return {backupCodes: await MfaCommands.getBackupCodes(true)};
}

export const BackupCodesRegenerateModal = observer(({user, challenge}: BackupCodesRegenerateModalProps) => {
	const {i18n} = useLingui();
	const [isSubmitting, setIsSubmitting] = useState(false);
	const handleConfirm = async () => {
		setIsSubmitting(true);
		try {
			const result = await regenerateBackupCodes(challenge);
			ModalCommands.pop();
			ModalCommands.update('backup-codes', () =>
				modal(() => (
					<BackupCodesModal
						backupCodes={result.backupCodes}
						user={user}
						challenge={result.challenge}
						data-flx="auth.backup-codes-regenerate-modal.handle-confirm.backup-codes-modal"
					/>
				)),
			);
			ToastCommands.createToast({
				type: 'success',
				children: i18n._(BACKUP_CODES_REGENERATED_DESCRIPTOR),
			});
		} catch (error) {
			FormUtils.pushApiErrorModal(i18n, error);
		} finally {
			setIsSubmitting(false);
		}
	};
	return (
		<Modal.Root size="small" centered data-flx="auth.backup-codes-regenerate-modal.modal-root">
			<Modal.Header
				title={i18n._(REGENERATE_BACKUP_CODES_DESCRIPTOR)}
				data-flx="auth.backup-codes-regenerate-modal.modal-header"
			/>
			<Modal.Content data-flx="auth.backup-codes-regenerate-modal.modal-content">
				<Modal.ContentLayout data-flx="auth.backup-codes-regenerate-modal.modal-content-layout">
					<Modal.Description data-flx="auth.backup-codes-regenerate-modal.modal-description">
						<Trans comment="Security warning shown before replacing account recovery backup codes.">
							This will invalidate your existing backup codes and generate new ones.
						</Trans>
					</Modal.Description>
				</Modal.ContentLayout>
			</Modal.Content>
			<Modal.Footer data-flx="auth.backup-codes-regenerate-modal.modal-footer">
				<Button
					onClick={ModalCommands.pop}
					variant="secondary"
					data-flx="auth.backup-codes-regenerate-modal.button.pop"
				>
					<Trans comment="Button that closes the regenerate backup codes confirmation without changes.">Cancel</Trans>
				</Button>
				<Button
					onClick={handleConfirm}
					submitting={isSubmitting}
					data-flx="auth.backup-codes-regenerate-modal.button.confirm"
				>
					<Trans comment="Button that confirms regenerating account recovery backup codes.">Continue</Trans>
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});
