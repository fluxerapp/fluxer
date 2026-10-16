// SPDX-License-Identifier: AGPL-3.0-or-later

import i18n from '@app/app/I18n';
import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import {VerificationResult} from '@app/features/auth/commands/AuthenticationCommands';
import {currentInstanceTarget} from '@app/features/platform/transport/InstanceHTTP';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {WarningAlert} from '@app/features/ui/warning_alert/WarningAlert';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useState} from 'react';

const VERIFICATION_EMAIL_SENT_PLEASE_CHECK_YOUR_INBOX_DESCRIPTOR = msg({
	message: 'Verification email sent. Check your inbox.',
	comment: 'Toast confirmation in the email verification alert after a verification email has been sent.',
});
const TOO_MANY_REQUESTS_TITLE_DESCRIPTOR = msg({
	message: "You're going too fast",
	comment: 'Title of the error modal shown when resending a verification email is rate-limited.',
});
const TOO_MANY_REQUESTS_MESSAGE_DESCRIPTOR = msg({
	message: "You've requested too many verification emails. Please wait a moment and try again.",
	comment: 'Body of the error modal shown when resending a verification email is rate-limited.',
});
const SEND_FAILED_TITLE_DESCRIPTOR = msg({
	message: "Couldn't send the verification email",
	comment: 'Title of the error modal shown when sending the verification email fails unexpectedly.',
});
const SEND_FAILED_MESSAGE_DESCRIPTOR = msg({
	message: 'Something went wrong while sending your verification email. Please try again in a moment.',
	comment: 'Body of the error modal shown when sending the verification email fails unexpectedly.',
});

function showVerificationEmailErrorModal(result: VerificationResult): void {
	const isRateLimited = result === VerificationResult.RATE_LIMITED;
	ModalCommands.push(
		modal(() => (
			<GenericErrorModal
				title={i18n._(isRateLimited ? TOO_MANY_REQUESTS_TITLE_DESCRIPTOR : SEND_FAILED_TITLE_DESCRIPTOR)}
				message={i18n._(isRateLimited ? TOO_MANY_REQUESTS_MESSAGE_DESCRIPTOR : SEND_FAILED_MESSAGE_DESCRIPTOR)}
				data-flx="app.email-verification-alert.resend.generic-error-modal"
			/>
		)),
	);
}

interface EmailVerificationAlertProps {
	title?: React.ReactNode;
	children?: React.ReactNode;
}

export const EmailVerificationAlert = observer(({title, children}: EmailVerificationAlertProps) => {
	const {i18n} = useLingui();
	const [isResending, setIsResending] = useState(false);
	const handleResend = async () => {
		if (isResending) return;
		setIsResending(true);
		const result = await AuthenticationCommands.resendVerificationEmail(currentInstanceTarget());
		switch (result) {
			case VerificationResult.SUCCESS:
				ToastCommands.success(i18n._(VERIFICATION_EMAIL_SENT_PLEASE_CHECK_YOUR_INBOX_DESCRIPTOR));
				break;
			case VerificationResult.RATE_LIMITED:
				showVerificationEmailErrorModal(VerificationResult.RATE_LIMITED);
				break;
			case VerificationResult.SERVER_ERROR:
				showVerificationEmailErrorModal(VerificationResult.SERVER_ERROR);
				break;
		}
		setIsResending(false);
	};
	if (!RuntimeConfig.emailsEnabled) {
		return null;
	}
	return (
		<WarningAlert
			title={title ?? <Trans>Email verification required</Trans>}
			actions={
				<Button
					variant="primary"
					small
					disabled={isResending}
					submitting={isResending}
					onClick={handleResend}
					data-flx="app.email-verification-alert.button.resend"
				>
					<Trans>Resend email</Trans>
				</Button>
			}
			data-flx="app.email-verification-alert.warning-alert"
		>
			{children ?? <Trans>Check your inbox for a verification email.</Trans>}
		</WarningAlert>
	);
});
