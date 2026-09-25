// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {PasswordManagerPasskeyAction} from '@app/features/auth/components/PasswordManagerPasskeyAction';
import {describePasskeyBridgeFailure, shouldSuggestPasskeyBridge} from '@app/features/auth/utils/PasskeyBridge';
import {
	PASSKEY_DOMAIN_UNSUPPORTED_DESCRIPTOR,
	PasskeyDomainUnsupportedError,
} from '@app/features/auth/utils/WebAuthnUtils';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {Form} from '@app/features/ui/components/form/Form';
import {Input} from '@app/features/ui/components/form/FormInput';
import * as FormUtils from '@app/lib/forms';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useState} from 'react';
import {useForm} from 'react-hook-form';

const NAME_PASSKEY_FORM_DESCRIPTOR = msg({
	message: 'Name passkey form',
	comment: 'Accessible form label in the authentication passkey name modal. Keep the tone plain and specific.',
});
const NAME_PASSKEY_DESCRIPTOR = msg({
	message: 'Name passkey',
	comment: 'Short label in the authentication passkey name modal. Keep the tone plain and specific.',
});
const PASSKEY_NAME_DESCRIPTOR = msg({
	message: 'Passkey name',
	comment: 'Short label in the authentication passkey name modal. Keep the tone plain and specific.',
});
const E_G_YUBIKEY_IPHONE_WORK_COMPUTER_DESCRIPTOR = msg({
	message: 'e.g., YubiKey, iPhone, work computer',
	comment: 'Placeholder or example text in the authentication passkey name modal. Keep the tone plain and specific.',
});

interface FormInputs {
	name: string;
}

interface PasskeyNameModalProps {
	onSubmit: (name: string) => void | Promise<void>;
	onSubmitWithPasswordManager?: (name: string) => Promise<void>;
}

export const PasskeyNameModal = observer(({onSubmit, onSubmitWithPasswordManager}: PasskeyNameModalProps) => {
	const {i18n} = useLingui();
	const form = useForm<FormInputs>();
	const [passkeyBridgeSuggested, setPasskeyBridgeSuggested] = useState(false);
	const [passkeyBridgeSubmitting, setPasskeyBridgeSubmitting] = useState(false);
	const handlePasswordManagerSubmit = (
		event: React.MouseEvent<HTMLButtonElement> | React.KeyboardEvent<HTMLButtonElement>,
	) => {
		if (!onSubmitWithPasswordManager || passkeyBridgeSubmitting || form.formState.isSubmitting) {
			return;
		}
		if (event.currentTarget.form?.reportValidity() === false) {
			return;
		}
		const submission = onSubmitWithPasswordManager(form.getValues('name').trim());
		form.clearErrors('name');
		setPasskeyBridgeSubmitting(true);
		submission
			.then(() => {
				ModalCommands.pop();
			})
			.catch((error: unknown) => {
				if (error instanceof HttpError) {
					FormUtils.handleError(i18n, form, error, 'name');
					return;
				}
				const descriptor = describePasskeyBridgeFailure(error);
				if (descriptor) {
					form.setError('name', {type: 'server', message: i18n._(descriptor)});
				}
			})
			.finally(() => {
				setPasskeyBridgeSubmitting(false);
			});
	};
	const handleSubmit = async (data: FormInputs) => {
		try {
			await onSubmit(data.name.trim());
			ModalCommands.pop();
		} catch (error) {
			if (onSubmitWithPasswordManager && shouldSuggestPasskeyBridge(error)) {
				setPasskeyBridgeSuggested(true);
			}
			if (error instanceof HttpError) {
				FormUtils.handleError(i18n, form, error, 'name');
			} else if (error instanceof PasskeyDomainUnsupportedError) {
				form.setError('name', {type: 'server', message: i18n._(PASSKEY_DOMAIN_UNSUPPORTED_DESCRIPTOR)});
			} else {
				form.setError('name', {type: 'server', message: FormUtils.extractErrorMessage(i18n, error)});
			}
		}
	};
	return (
		<Modal.Root size="small" centered data-flx="auth.passkey-name-modal.modal-root">
			<Form
				form={form}
				onSubmit={handleSubmit}
				aria-label={i18n._(NAME_PASSKEY_FORM_DESCRIPTOR)}
				data-flx="auth.passkey-name-modal.form.submit"
			>
				<Modal.Header title={i18n._(NAME_PASSKEY_DESCRIPTOR)} data-flx="auth.passkey-name-modal.modal-header" />
				<Modal.Content data-flx="auth.passkey-name-modal.modal-content">
					<Modal.ContentLayout data-flx="auth.passkey-name-modal.modal-content-layout">
						<Input
							data-flx="auth.passkey-name-modal.input.text"
							{...form.register('name', {
								setValueAs: (value) => (typeof value === 'string' ? value.trim() : value),
							})}
							autoFocus={true}
							error={form.formState.errors.name?.message}
							label={i18n._(PASSKEY_NAME_DESCRIPTOR)}
							maxLength={64}
							minLength={1}
							placeholder={i18n._(E_G_YUBIKEY_IPHONE_WORK_COMPUTER_DESCRIPTOR)}
							required={true}
							type="text"
						/>
						{onSubmitWithPasswordManager && (
							<PasswordManagerPasskeyAction
								suggested={passkeyBridgeSuggested}
								disabled={form.formState.isSubmitting || passkeyBridgeSubmitting}
								onClick={handlePasswordManagerSubmit}
								data-flx="auth.passkey-name-modal.password-manager-passkey-action"
							/>
						)}
					</Modal.ContentLayout>
				</Modal.Content>
				<Modal.Footer data-flx="auth.passkey-name-modal.modal-footer">
					<Button onClick={ModalCommands.pop} variant="secondary" data-flx="auth.passkey-name-modal.button.pop">
						<Trans>Cancel</Trans>
					</Button>
					<Button
						type="submit"
						submitting={form.formState.isSubmitting}
						disabled={passkeyBridgeSubmitting}
						data-flx="auth.passkey-name-modal.button.submit"
					>
						<Trans>Save</Trans>
					</Button>
				</Modal.Footer>
			</Form>
		</Modal.Root>
	);
});
