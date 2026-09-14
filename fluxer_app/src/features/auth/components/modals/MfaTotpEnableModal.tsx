// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {MFA_CODE_DIGIT_COUNT} from '@app/features/app/config/I18nDisplayConstants';
import {useFormSubmit} from '@app/features/app/hooks/useFormSubmit';
import * as MfaCommands from '@app/features/auth/commands/MfaCommands';
import {BackupCodesModal} from '@app/features/auth/components/modals/BackupCodesModal';
import styles from '@app/features/auth/components/modals/MfaTotpEnableModal.module.css';
import * as MfaUtils from '@app/features/auth/utils/AuthMfaUtils';
import {CANCEL_DESCRIPTOR, CONTINUE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Form} from '@app/features/ui/components/form/Form';
import {Input} from '@app/features/ui/components/form/FormInput';
import {QRCodeCanvas} from '@app/features/ui/components/QRCodeCanvas';
import {isMobileExperienceEnabled} from '@app/features/ui/utils/MobileExperience';
import type {User} from '@app/features/user/models/User';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {ClipboardIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useState} from 'react';
import {useForm} from 'react-hook-form';

const ENABLE_TWO_FACTOR_AUTHENTICATION_FORM_DESCRIPTOR = msg({
	message: 'Enable two-factor authentication form',
	comment: 'Accessible label for the form that adds an authenticator app.',
});
const SETUP_AUTHENTICATOR_APP_DESCRIPTOR = msg({
	message: 'Set up authenticator app',
	comment: 'Security modal title for adding a TOTP authenticator app.',
});
const CODE_DESCRIPTOR = msg({
	message: 'Code',
	comment: 'Input label for a one-time authenticator app verification code.',
});
const TWO_FACTOR_AUTHENTICATION_ENABLED_DESCRIPTOR = msg({
	message: 'Two-factor authentication enabled',
	comment: 'Toast shown after an authenticator app is added for two-factor authentication.',
});
const TOTP_ENABLE_CODE_HELPER_DESCRIPTOR = msg({
	message: 'Enter the {digitCount}-digit code from your authenticator app.',
	comment: 'Helper text for the TOTP setup code field. digitCount is the expected authenticator-code length.',
});
const SETUP_KEY_DESCRIPTOR = msg({
	message: 'Setup key',
	comment: 'Label for the manual authenticator app setup secret.',
});
const COPY_SETUP_KEY_DESCRIPTOR = msg({
	message: 'Copy setup key',
	comment: 'Accessible label for copying the manual authenticator app setup secret.',
});

interface FormInputs {
	code: string;
}

interface MfaTotpEnableModalProps {
	user: User;
}

export const MfaTotpEnableModal = observer(({user}: MfaTotpEnableModalProps) => {
	const {i18n} = useLingui();
	const form = useForm<FormInputs>();
	const [secret] = useState(() => MfaUtils.generateTotpSecret());
	const encodedSecret = MfaUtils.encodeTotpSecret(secret);
	const isMobileExperience = isMobileExperienceEnabled();
	const onSubmit = async (data: FormInputs) => {
		const backupCodes = await MfaCommands.enableMfaTotp(encodedSecret, data.code.split(' ').join(''));
		ModalCommands.pop();
		ToastCommands.createToast({
			type: 'success',
			children: i18n._(TWO_FACTOR_AUTHENTICATION_ENABLED_DESCRIPTOR),
		});
		ModalCommands.pushWithKey(
			modal(() => (
				<BackupCodesModal
					backupCodes={backupCodes}
					user={user}
					data-flx="auth.mfa-totp-enable-modal.on-submit.backup-codes-modal"
				/>
			)),
			'backup-codes',
		);
	};
	const {handleSubmit} = useFormSubmit({
		form,
		onSubmit,
		defaultErrorField: 'code',
	});
	const setupDescription = isMobileExperience ? (
		<Trans>Copy the setup key into your authenticator app.</Trans>
	) : (
		<Trans>Scan the QR code with your authenticator app.</Trans>
	);
	return (
		<Modal.Root size="small" centered data-flx="auth.mfa-totp-enable-modal.modal-root">
			<Form
				form={form}
				onSubmit={handleSubmit}
				aria-label={i18n._(ENABLE_TWO_FACTOR_AUTHENTICATION_FORM_DESCRIPTOR)}
				data-flx="auth.mfa-totp-enable-modal.form.submit"
			>
				<Modal.Header
					title={i18n._(SETUP_AUTHENTICATOR_APP_DESCRIPTOR)}
					data-flx="auth.mfa-totp-enable-modal.modal-header"
				/>
				<Modal.Content data-flx="auth.mfa-totp-enable-modal.modal-content">
					<Modal.ContentLayout data-flx="auth.mfa-totp-enable-modal.modal-content-layout">
						<div className={styles.qrContainer} data-flx="auth.mfa-totp-enable-modal.qr-container">
							{!isMobileExperience && (
								<div className={styles.qrCode} data-flx="auth.mfa-totp-enable-modal.qr-code">
									<QRCodeCanvas
										data={MfaUtils.encodeTotpSecretAsURL(user.email!, secret)}
										data-flx="auth.mfa-totp-enable-modal.qr-code-canvas"
									/>
								</div>
							)}
							<div
								className={styles.instructionsContainer}
								data-flx="auth.mfa-totp-enable-modal.instructions-container"
							>
								<Modal.Description
									className={styles.setupDescription}
									data-flx="auth.mfa-totp-enable-modal.description"
								>
									{setupDescription}
								</Modal.Description>
								<div className={styles.secretBlock} data-flx="auth.mfa-totp-enable-modal.secret-block">
									<span className={styles.secretLabel} data-flx="auth.mfa-totp-enable-modal.secret-label">
										{i18n._(SETUP_KEY_DESCRIPTOR)}
									</span>
									<div className={styles.secretRow} data-flx="auth.mfa-totp-enable-modal.secret-row">
										<code className={styles.secretText} data-flx="auth.mfa-totp-enable-modal.secret-text">
											{encodedSecret}
										</code>
										<Button
											variant="secondary"
											square
											compact
											aria-label={i18n._(COPY_SETUP_KEY_DESCRIPTOR)}
											icon={
												<ClipboardIcon
													className={styles.buttonIcon}
													data-flx="auth.mfa-totp-enable-modal.button-icon.copy"
												/>
											}
											onClick={() => TextCopyCommands.copy(i18n, encodedSecret)}
											data-flx="auth.mfa-totp-enable-modal.button.copy-secret"
										/>
									</div>
								</div>
							</div>
						</div>
						<Input
							data-flx="auth.mfa-totp-enable-modal.input"
							{...form.register('code')}
							autoComplete="one-time-code"
							autoFocus={true}
							error={form.formState.errors.code?.message}
							label={i18n._(CODE_DESCRIPTOR)}
							required={true}
							footer={
								<Modal.Description data-flx="auth.mfa-totp-enable-modal.modal-description">
									{i18n._(TOTP_ENABLE_CODE_HELPER_DESCRIPTOR, {digitCount: MFA_CODE_DIGIT_COUNT})}
								</Modal.Description>
							}
						/>
					</Modal.ContentLayout>
				</Modal.Content>
				<Modal.Footer data-flx="auth.mfa-totp-enable-modal.modal-footer">
					<Button onClick={ModalCommands.pop} variant="secondary" data-flx="auth.mfa-totp-enable-modal.button.pop">
						{i18n._(CANCEL_DESCRIPTOR)}
					</Button>
					<Button
						type="submit"
						submitting={form.formState.isSubmitting}
						data-flx="auth.mfa-totp-enable-modal.button.submit"
					>
						{i18n._(CONTINUE_DESCRIPTOR)}
					</Button>
				</Modal.Footer>
			</Form>
		</Modal.Root>
	);
});
