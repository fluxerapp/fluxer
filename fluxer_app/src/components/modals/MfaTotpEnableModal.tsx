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
import {Input} from '@app/components/form/Input';
import {BackupCodesModal} from '@app/components/modals/BackupCodesModal';
import styles from '@app/components/modals/MfaTotpEnableModal.module.css';
import * as Modal from '@app/components/modals/Modal';
import {Button} from '@app/components/uikit/button/Button';
import {QRCodeCanvas} from '@app/components/uikit/QRCodeCanvas';
import {useFormSubmit} from '@app/hooks/useFormSubmit';
import type {UserRecord} from '@app/records/UserRecord';
import * as MfaUtils from '@app/utils/MfaUtils';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useState} from 'react';
import {useForm} from 'react-hook-form';

interface FormInputs {
	code: string;
}

interface MfaTotpEnableModalProps {
	user: UserRecord;
}

export const MfaTotpEnableModal = observer(({user}: MfaTotpEnableModalProps) => {
	const {t} = useLingui();
	const form = useForm<FormInputs>();
	const [secret] = useState(() => MfaUtils.generateTotpSecret());

	const onSubmit = async (data: FormInputs) => {
		const backupCodes = await MfaActionCreators.enableMfaTotp(
			MfaUtils.encodeTotpSecret(secret),
			data.code.split(' ').join(''),
		);
		ModalActionCreators.pop();
		ToastActionCreators.createToast({type: 'success', children: <Trans>Two-factor authentication enabled</Trans>});
		ModalActionCreators.pushWithKey(
			modal(() => <BackupCodesModal backupCodes={backupCodes} user={user} />),
			'backup-codes',
		);
	};

	const {handleSubmit} = useFormSubmit({
		form,
		onSubmit,
		defaultErrorField: 'code',
	});

	return (
		<Modal.Root size="small" centered>
			<Form form={form} onSubmit={handleSubmit} aria-label={t`Enable two-factor authentication form`}>
				<Modal.Header title={t`Setup Authenticator App`} />
				<Modal.Content>
					<Modal.ContentLayout>
						<div className={styles.qrContainer}>
							<QRCodeCanvas data={MfaUtils.encodeTotpSecretAsURL(user.email!, secret)} />
							<div className={styles.instructionsContainer}>
								<div>
									<Trans>
										Scan the QR code with your authenticator app to generate codes for two-factor authentication.
									</Trans>
								</div>
								<pre className={styles.secretText}>{secret}</pre>
							</div>
						</div>

						<Input
							{...form.register('code')}
							autoComplete="one-time-code"
							autoFocus={true}
							error={form.formState.errors.code?.message}
							label={t`Code`}
							required={true}
							footer={
								<Modal.Description>
									<Trans>Enter the 6-digit code from your authenticator app.</Trans>
								</Modal.Description>
							}
						/>
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
