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

import * as MfaActionCreators from '@app/actions/MfaActionCreators';
import * as ModalActionCreators from '@app/actions/ModalActionCreators';
import {modal} from '@app/actions/ModalActionCreators';
import * as ToastActionCreators from '@app/actions/ToastActionCreators';
import {Form} from '@app/components/form/Form';
import {FormErrorText} from '@app/components/form/FormErrorText';
import {BackupCodesModal} from '@app/components/modals/BackupCodesModal';
import * as Modal from '@app/components/modals/Modal';
import {Button} from '@app/components/uikit/button/Button';
import {useFormSubmit} from '@app/hooks/useFormSubmit';
import type {UserRecord} from '@app/records/UserRecord';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useForm} from 'react-hook-form';

interface FormInputs {
	form: string;
}

interface BackupCodesRegenerateModalProps {
	user: UserRecord;
}

export const BackupCodesRegenerateModal = observer(({user}: BackupCodesRegenerateModalProps) => {
	const {t} = useLingui();
	const form = useForm<FormInputs>();

	const onSubmit = async () => {
		const backupCodes = await MfaActionCreators.getBackupCodes(true);
		ModalActionCreators.pop();
		ModalActionCreators.update('backup-codes', () =>
			modal(() => <BackupCodesModal backupCodes={backupCodes} user={user} />),
		);
		ToastActionCreators.createToast({
			type: 'success',
			children: t`Backup codes regenerated`,
		});
	};

	const {handleSubmit} = useFormSubmit({
		form,
		onSubmit,
		defaultErrorField: 'form',
	});

	return (
		<Modal.Root size="small" centered>
			<Form form={form} onSubmit={handleSubmit}>
				<Modal.Header title={t`Regenerate Backup Codes`} />
				<Modal.Content>
					<Modal.ContentLayout>
						<Modal.Description>
							<Trans>This will invalidate your existing backup codes and generate new ones.</Trans>
						</Modal.Description>
						<FormErrorText message={form.formState.errors.form?.message} />
					</Modal.ContentLayout>
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
