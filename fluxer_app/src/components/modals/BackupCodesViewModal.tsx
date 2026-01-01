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
import {observer} from 'mobx-react-lite';
import {useForm} from 'react-hook-form';
import * as MfaActionCreators from '~/actions/MfaActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {Form} from '~/components/form/Form';
import {BackupCodesModal} from '~/components/modals/BackupCodesModal';
import styles from '~/components/modals/ConfirmModal.module.css';
import * as Modal from '~/components/modals/Modal';
import {Button} from '~/components/uikit/Button/Button';
import {useFormSubmit} from '~/hooks/useFormSubmit';

export const BackupCodesViewModal = observer(() => {
	const {t} = useLingui();
	const form = useForm();

	const onSubmit = async () => {
		const backupCodes = await MfaActionCreators.getBackupCodes();
		ModalActionCreators.pop();
		ModalActionCreators.pushWithKey(
			modal(() => <BackupCodesModal backupCodes={backupCodes} />),
			'backup-codes',
		);
	};

	const {handleSubmit} = useFormSubmit({
		form,
		onSubmit,
		defaultErrorField: 'form',
	});

	return (
		<Modal.Root size="small" centered>
			<Form form={form} onSubmit={handleSubmit} aria-label={t`View backup codes form`}>
				<Modal.Header title={t`View backup codes`} />
				<Modal.Content className={styles.content}>
					<p>
						<Trans>Verification may be required before viewing your backup codes.</Trans>
					</p>
				</Modal.Content>
				<Modal.Footer>
					<Button onClick={ModalActionCreators.pop} variant="secondary">
						<Trans>Cancel</Trans>
					</Button>
					<Button type="submit" submitting={form.formState.isSubmitting}>
						<Trans>Continue</Trans>
					</Button>
				</Modal.Footer>
			</Form>
		</Modal.Root>
	);
});
