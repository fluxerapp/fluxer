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
import React from 'react';
import {useForm} from 'react-hook-form';
import * as MfaActionCreators from '~/actions/MfaActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {Form} from '~/components/form/Form';
import {Input} from '~/components/form/Input';
import {BackupCodesModal} from '~/components/modals/BackupCodesModal';
import confirmStyles from '~/components/modals/ConfirmModal.module.css';
import styles from '~/components/modals/MfaTotpEnableModal.module.css';
import * as Modal from '~/components/modals/Modal';
import {Button} from '~/components/uikit/Button/Button';
import {QRCodeCanvas} from '~/components/uikit/QRCodeCanvas';
import {useFormSubmit} from '~/hooks/useFormSubmit';
import UserStore from '~/stores/UserStore';
import * as MfaUtils from '~/utils/MfaUtils';

interface FormInputs {
	code: string;
}

export const MfaTotpEnableModal = observer(() => {
	const {t} = useLingui();
	const user = UserStore.getCurrentUser()!;
	const form = useForm<FormInputs>();
	const [secret] = React.useState(() => MfaUtils.generateTotpSecret());

	const onSubmit = async (data: FormInputs) => {
		const backupCodes = await MfaActionCreators.enableMfaTotp(
			MfaUtils.encodeTotpSecret(secret),
			data.code.split(' ').join(''),
		);
		ModalActionCreators.pop();
		ToastActionCreators.createToast({type: 'success', children: <Trans>Two-factor authentication enabled</Trans>});
		ModalActionCreators.pushWithKey(
			modal(() => <BackupCodesModal backupCodes={backupCodes} />),
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
				<Modal.Header title={t`Setup authenticator app`} />
				<Modal.Content className={confirmStyles.content}>
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
						autoFocus={true}
						autoComplete="one-time-code"
						error={form.formState.errors.code?.message}
						label={t`Code`}
						required={true}
						footer={
							<p className={styles.footer}>
								<Trans>Enter the 6-digit code from your authenticator app.</Trans>
							</p>
						}
					/>
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
