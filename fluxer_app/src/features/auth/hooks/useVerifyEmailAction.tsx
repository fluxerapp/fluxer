// SPDX-License-Identifier: AGPL-3.0-or-later

import i18n from '@app/app/I18n';
import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {UserSettingsModal} from '@app/features/app/components/dialogs/LoadableSettingsModals';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import {VerificationResult} from '@app/features/auth/commands/AuthenticationCommands';
import {VERIFY_EMAIL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {currentInstanceTarget} from '@app/features/platform/transport/InstanceHTTP';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {BouncedEmailChangeModal} from '@app/features/user/components/modals/BouncedEmailChangeModal';
import type {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {useState} from 'react';

const VERIFICATION_EMAIL_SENT_PLEASE_CHECK_YOUR_INBOX_DESCRIPTOR = msg({
	message: 'Verification email sent. Check your inbox.',
	comment: 'Toast confirmation shown after a verification email has been sent.',
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
const SEND_VERIFICATION_EMAIL_DESCRIPTOR = msg({
	message: 'Send verification email',
	comment:
		'Button label on prompts that ask the user to verify their email, such as the add friend form. Sends a verification email right away. Keep it concise.',
});
const RESEND_EMAIL_DESCRIPTOR = msg({
	message: 'Resend email',
	comment: 'Button label that sends another verification email after one was already sent. Keep it concise.',
});

function showVerificationEmailErrorModal(result: VerificationResult): void {
	const isRateLimited = result === VerificationResult.RATE_LIMITED;
	ModalCommands.push(
		modal(() => (
			<GenericErrorModal
				title={i18n._(isRateLimited ? TOO_MANY_REQUESTS_TITLE_DESCRIPTOR : SEND_FAILED_TITLE_DESCRIPTOR)}
				message={i18n._(isRateLimited ? TOO_MANY_REQUESTS_MESSAGE_DESCRIPTOR : SEND_FAILED_MESSAGE_DESCRIPTOR)}
				data-flx="auth.verify-email-action.resend.generic-error-modal"
			/>
		)),
	);
}

export function canResendVerificationEmail(user: User | null | undefined): boolean {
	return Boolean(
		RuntimeConfig.emailsEnabled &&
			user?.isClaimed() &&
			!RuntimeConfig.usesUsernameSignIn &&
			user.email &&
			!user.verified,
	);
}

export function useResendVerificationEmail() {
	const {i18n} = useLingui();
	const [isSending, setIsSending] = useState(false);
	const [sent, setSent] = useState(false);
	const resend = async () => {
		if (isSending) return;
		setIsSending(true);
		const result = await AuthenticationCommands.resendVerificationEmail(currentInstanceTarget());
		switch (result) {
			case VerificationResult.SUCCESS:
				setSent(true);
				ToastCommands.success(i18n._(VERIFICATION_EMAIL_SENT_PLEASE_CHECK_YOUR_INBOX_DESCRIPTOR));
				break;
			case VerificationResult.RATE_LIMITED:
				showVerificationEmailErrorModal(VerificationResult.RATE_LIMITED);
				break;
			case VerificationResult.SERVER_ERROR:
				showVerificationEmailErrorModal(VerificationResult.SERVER_ERROR);
				break;
		}
		setIsSending(false);
	};
	return {isSending, sent, resend};
}

function openAccountSecuritySettings(): void {
	ModalCommands.push(
		modal(
			() => <UserSettingsModal initialTab="account_security" data-flx="auth.verify-email-action.user-settings-modal" />,
			'user-settings',
		),
	);
}

function openBouncedEmailChange(): void {
	ModalCommands.push(
		modal(() => <BouncedEmailChangeModal data-flx="auth.verify-email-action.bounced-email-change-modal" />),
	);
}

export function useVerifyEmailAction() {
	const {i18n} = useLingui();
	const {isSending, sent, resend} = useResendVerificationEmail();
	const user = Users.currentUser;
	if (user?.emailBounced === true) {
		return {text: i18n._(VERIFY_EMAIL_DESCRIPTOR), onClick: openBouncedEmailChange, submitting: false};
	}
	if (!canResendVerificationEmail(user)) {
		return {text: i18n._(VERIFY_EMAIL_DESCRIPTOR), onClick: openAccountSecuritySettings, submitting: false};
	}
	return {
		text: i18n._(sent ? RESEND_EMAIL_DESCRIPTOR : SEND_VERIFICATION_EMAIL_DESCRIPTOR),
		onClick: () => void resend(),
		submitting: isSending,
	};
}
