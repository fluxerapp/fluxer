// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {MFA_CODE_DIGIT_COUNT} from '@app/features/app/config/I18nDisplayConstants';
import {useFormSubmit} from '@app/features/app/hooks/useFormSubmit';
import * as MfaCommands from '@app/features/auth/commands/MfaCommands';
import {
	CANCEL_DESCRIPTOR,
	CODE_DESCRIPTOR,
	CONTINUE_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Form} from '@app/features/ui/components/form/Form';
import {Input} from '@app/features/ui/components/form/FormInput';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useForm} from 'react-hook-form';

const DISABLE_TWO_FACTOR_AUTHENTICATION_FORM_DESCRIPTOR = msg({
	message: 'Disable two-factor authentication form',
	comment: 'Accessible label for the form that removes an authenticator app.',
});
const REMOVE_AUTHENTICATOR_APP_DESCRIPTOR = msg({
	message: 'Remove authenticator app',
	comment: 'Security modal title for removing a TOTP authenticator app.',
});
const TWO_FACTOR_AUTHENTICATION_DISABLED_DESCRIPTOR = msg({
	message: 'Two-factor authentication disabled',
	comment: 'Toast shown after an authenticator app is removed from two-factor authentication.',
});
const TOTP_DISABLE_CODE_HELPER_DESCRIPTOR = msg({
	message: 'Enter the {digitCount}-digit code from your authenticator app or one of your backup codes.',
	comment: 'Helper text for the TOTP removal code field. digitCount is the expected authenticator-code length.',
});

interface FormInputs {
	code: string;
}

export const MfaTotpDisableModal = observer(() => {
	const {i18n} = useLingui();
	const form = useForm<FormInputs>();
	const onSubmit = async (data: FormInputs) => {
		await MfaCommands.disableMfaTotp(data.code.split(' ').join(''));
		ModalCommands.pop();
		ToastCommands.createToast({
			type: 'success',
			children: i18n._(TWO_FACTOR_AUTHENTICATION_DISABLED_DESCRIPTOR),
		});
	};
	const {handleSubmit} = useFormSubmit({
		form,
		onSubmit,
		defaultErrorField: 'code',
	});
	return (
		<Modal.Root size="small" centered data-flx="auth.mfa-totp-disable-modal.modal-root">
			<Form
				form={form}
				onSubmit={handleSubmit}
				aria-label={i18n._(DISABLE_TWO_FACTOR_AUTHENTICATION_FORM_DESCRIPTOR)}
				data-flx="auth.mfa-totp-disable-modal.form.submit"
			>
				<Modal.Header
					title={i18n._(REMOVE_AUTHENTICATOR_APP_DESCRIPTOR)}
					data-flx="auth.mfa-totp-disable-modal.modal-header"
				/>
				<Modal.Content data-flx="auth.mfa-totp-disable-modal.modal-content">
					<Modal.ContentLayout data-flx="auth.mfa-totp-disable-modal.modal-content-layout">
						<Input
							data-flx="auth.mfa-totp-disable-modal.input"
							{...form.register('code')}
							autoComplete="one-time-code"
							autoFocus={true}
							error={form.formState.errors.code?.message}
							label={i18n._(CODE_DESCRIPTOR)}
							required={true}
							footer={
								<Modal.Description data-flx="auth.mfa-totp-disable-modal.modal-description">
									{i18n._(TOTP_DISABLE_CODE_HELPER_DESCRIPTOR, {digitCount: MFA_CODE_DIGIT_COUNT})}
								</Modal.Description>
							}
						/>
					</Modal.ContentLayout>
				</Modal.Content>
				<Modal.Footer data-flx="auth.mfa-totp-disable-modal.modal-footer">
					<Button onClick={ModalCommands.pop} variant="secondary" data-flx="auth.mfa-totp-disable-modal.button.pop">
						{i18n._(CANCEL_DESCRIPTOR)}
					</Button>
					<Button
						type="submit"
						submitting={form.formState.isSubmitting}
						data-flx="auth.mfa-totp-disable-modal.button.submit"
					>
						{i18n._(CONTINUE_DESCRIPTOR)}
					</Button>
				</Modal.Footer>
			</Form>
		</Modal.Root>
	);
});
