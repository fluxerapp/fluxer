// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UseFormReturn} from '@app/features/app/hooks/useForm';
import {useForm} from '@app/features/app/hooks/useForm';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import type {AuthRequestTarget} from '@app/features/auth/state/AuthRequestTarget';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {useId, useState} from 'react';

const FORGOT_PASSWORD_SEND_FAILED_DESCRIPTOR = msg({
	message: 'Failed to send reset link. Try again.',
	comment: 'Validation error shown when a password reset email request fails.',
});

export interface ForgotPasswordFormState {
	readonly form: UseFormReturn;
	readonly emailId: string;
	readonly isSent: boolean;
}

export function useForgotPasswordForm(target: AuthRequestTarget): ForgotPasswordFormState {
	const {i18n} = useLingui();
	const emailId = useId();
	const [isSent, setIsSent] = useState(false);
	const form = useForm({
		initialValues: {email: ''},
		onSubmit: async (submission) => {
			try {
				await AuthenticationCommands.forgotPassword(submission.getValue('email'), target);
				if (!submission.isCurrent()) {
					return;
				}
				setIsSent(true);
			} catch {
				if (!submission.isCurrent()) {
					return;
				}
				form.setError('email', i18n._(FORGOT_PASSWORD_SEND_FAILED_DESCRIPTOR));
			}
		},
	});
	return {form, emailId, isSent};
}
