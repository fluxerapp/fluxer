// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {
	createPasskeyCancelledError,
	type PasskeyPinFailure,
	parsePasskeyPinFailure,
} from '@app/features/auth/utils/PasskeyPinErrors';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {Form} from '@app/features/ui/components/form/Form';
import {Input} from '@app/features/ui/components/form/FormInput';
import * as FormUtils from '@app/lib/forms';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useEffect, useRef} from 'react';
import {useForm} from 'react-hook-form';

const SECURITY_KEY_PIN_FORM_DESCRIPTOR = msg({
	message: 'Security key PIN form',
	comment: 'Accessible form label in the security key PIN modal. Keep the tone plain and specific.',
});
const SECURITY_KEY_PIN_DESCRIPTOR = msg({
	message: 'Security key PIN',
	comment: 'Short label in the security key PIN modal. Keep the tone plain and specific.',
});
const ENTER_SECURITY_KEY_PIN_DESCRIPTOR = msg({
	message: 'Enter the PIN for your security key to continue.',
	comment: 'Helper text in the security key PIN modal. Keep the tone plain and specific.',
});
const INCORRECT_PIN_DESCRIPTOR = msg({
	message: 'Incorrect PIN. Try again.',
	comment: 'Error text in the security key PIN modal shown after a wrong PIN. Keep the tone plain and specific.',
});
const INCORRECT_PIN_RETRIES_DESCRIPTOR = msg({
	message:
		'Incorrect PIN. {retriesRemaining, plural, one {# attempt} other {# attempts}} left before the security key locks.',
	comment:
		'Error text in the security key PIN modal shown after a wrong PIN, with the remaining attempt count. Keep the tone plain and specific.',
});

function incorrectPinMessage(i18n: I18n, failure: Extract<PasskeyPinFailure, {kind: 'invalid'}>): string {
	if (failure.retriesRemaining == null) {
		return i18n._(INCORRECT_PIN_DESCRIPTOR);
	}
	return i18n._(INCORRECT_PIN_RETRIES_DESCRIPTOR, {retriesRemaining: failure.retriesRemaining});
}

interface FormInputs {
	pin: string;
}

const PasskeyPinModal = observer(
	({onSubmit, onDismiss}: {onSubmit: (pin: string) => Promise<void>; onDismiss: () => void}) => {
		const {i18n} = useLingui();
		const form = useForm<FormInputs>();
		const dismissedRef = useRef(onDismiss);
		dismissedRef.current = onDismiss;
		useEffect(() => () => dismissedRef.current(), []);
		const handleSubmit = async (data: FormInputs) => {
			try {
				await onSubmit(data.pin);
				ModalCommands.pop();
			} catch (error) {
				const failure = parsePasskeyPinFailure(error);
				if (failure?.kind === 'invalid') {
					form.setError('pin', {type: 'server', message: incorrectPinMessage(i18n, failure)});
					form.resetField('pin', {keepError: true});
					return;
				}
				form.setError('pin', {type: 'server', message: FormUtils.extractErrorMessage(i18n, error)});
			}
		};
		return (
			<Modal.Root size="small" centered data-flx="auth.passkey-pin-modal.modal-root">
				<Form
					form={form}
					onSubmit={handleSubmit}
					aria-label={i18n._(SECURITY_KEY_PIN_FORM_DESCRIPTOR)}
					data-flx="auth.passkey-pin-modal.form.submit"
				>
					<Modal.Header title={i18n._(SECURITY_KEY_PIN_DESCRIPTOR)} data-flx="auth.passkey-pin-modal.modal-header" />
					<Modal.Content data-flx="auth.passkey-pin-modal.modal-content">
						<Modal.ContentLayout data-flx="auth.passkey-pin-modal.modal-content-layout">
							<p data-flx="auth.passkey-pin-modal.helper-text">{i18n._(ENTER_SECURITY_KEY_PIN_DESCRIPTOR)}</p>
							<Input
								data-flx="auth.passkey-pin-modal.input.password"
								{...form.register('pin')}
								autoComplete="off"
								autoFocus={true}
								error={form.formState.errors.pin?.message}
								label={i18n._(SECURITY_KEY_PIN_DESCRIPTOR)}
								maxLength={63}
								minLength={4}
								required={true}
								type="password"
							/>
						</Modal.ContentLayout>
					</Modal.Content>
					<Modal.Footer data-flx="auth.passkey-pin-modal.modal-footer">
						<Button onClick={ModalCommands.pop} variant="secondary" data-flx="auth.passkey-pin-modal.button.pop">
							<Trans>Cancel</Trans>
						</Button>
						<Button
							type="submit"
							submitting={form.formState.isSubmitting}
							data-flx="auth.passkey-pin-modal.button.submit"
						>
							<Trans>Continue</Trans>
						</Button>
					</Modal.Footer>
				</Form>
			</Modal.Root>
		);
	},
);

export function promptForSecurityKeyPin<T>(attempt: (pin: string) => Promise<T>): Promise<T> {
	return new Promise<T>((resolve, reject) => {
		let settled = false;
		const settleResolve = (value: T) => {
			if (!settled) {
				settled = true;
				resolve(value);
			}
		};
		const settleReject = (error: unknown) => {
			if (!settled) {
				settled = true;
				reject(error);
			}
		};
		ModalCommands.push(
			ModalCommands.modal(() => (
				<PasskeyPinModal
					onSubmit={async (pin) => {
						try {
							settleResolve(await attempt(pin));
						} catch (error) {
							if (parsePasskeyPinFailure(error)?.kind === 'invalid') {
								throw error;
							}
							settleReject(error);
						}
					}}
					onDismiss={() => settleReject(createPasskeyCancelledError())}
					data-flx="auth.passkey-pin-modal.prompt-for-security-key-pin.passkey-pin-modal"
				/>
			)),
		);
	});
}
