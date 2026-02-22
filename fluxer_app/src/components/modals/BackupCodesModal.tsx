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

import * as ModalActionCreators from '@app/actions/ModalActionCreators';
import {modal} from '@app/actions/ModalActionCreators';
import * as TextCopyActionCreators from '@app/actions/TextCopyActionCreators';
import styles from '@app/components/modals/BackupCodesModal.module.css';
import {BackupCodesRegenerateModal} from '@app/components/modals/BackupCodesRegenerateModal';
import * as Modal from '@app/components/modals/Modal';
import {Button} from '@app/components/uikit/button/Button';
import type {UserRecord} from '@app/records/UserRecord';
import type {BackupCode} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {Trans, useLingui} from '@lingui/react/macro';
import {CheckIcon, ClipboardIcon, DownloadIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';

interface BackupCodesModalProps {
	backupCodes: ReadonlyArray<BackupCode>;
	user: UserRecord;
}

export const BackupCodesModal = observer(({backupCodes, user}: BackupCodesModalProps) => {
	const {t, i18n} = useLingui();

	return (
		<Modal.Root size="small" centered>
			<Modal.Header title={t`Backup Codes`} />
			<Modal.Content contentClassName={styles.content}>
				<p className={styles.description}>
					<Trans>Use these codes to access your account if you lose your authenticator app.</Trans>
				</p>
				<p className={styles.description}>
					<Trans>We recommend saving these codes now so that you don't get locked out of your account.</Trans>
				</p>

				<div className={styles.codesGrid}>
					{backupCodes.map(({code, consumed}) => (
						<div key={code} className={`${styles.codeItem} ${consumed ? styles.codeItemConsumed : ''}`}>
							<div className={`${styles.checkbox} ${consumed ? styles.checkboxChecked : styles.checkboxUnchecked}`}>
								{consumed && <CheckIcon weight="bold" className={styles.checkIcon} />}
							</div>
							<code className={`${styles.code} ${consumed ? styles.codeConsumed : ''}`}>{code}</code>
						</div>
					))}
				</div>

				<div className={styles.buttonRow}>
					<Button
						leftIcon={<DownloadIcon className={styles.buttonIcon} />}
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
					>
						<Trans>Download</Trans>
					</Button>

					<Button
						variant="secondary"
						small={true}
						leftIcon={<ClipboardIcon className={styles.buttonIcon} />}
						onClick={() => TextCopyActionCreators.copy(i18n, backupCodes.map(({code}) => code).join('\n'))}
					>
						<Trans>Copy</Trans>
					</Button>

					<Button
						variant="danger-secondary"
						small={true}
						onClick={() => ModalActionCreators.push(modal(() => <BackupCodesRegenerateModal user={user} />))}
					>
						<Trans>Regenerate</Trans>
					</Button>
				</div>
			</Modal.Content>
			<Modal.Footer>
				<Button onClick={ModalActionCreators.pop}>
					<Trans>I have saved the codes</Trans>
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});
