// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {openClaimAccountModal} from '@app/features/auth/components/modals/ClaimAccountModal';
import {useResendVerificationEmail} from '@app/features/auth/hooks/useVerifyEmailAction';
import {Button} from '@app/features/ui/button/Button';
import {WarningAlert} from '@app/features/ui/warning_alert/WarningAlert';
import type {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import {AccountTiers} from '@fluxer/constants/src/AccountTierConstants';
import type {ValueOf} from '@fluxer/constants/src/ValueOf';
import {Trans} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

export const EmailVerificationModes = {
	ENTER_CODE: 'enter_code',
	RESEND: 'resend',
} as const;

type EmailVerificationMode = ValueOf<typeof EmailVerificationModes>;

export function verificationMode(
	user: Pick<User, 'reportsAccountTier' | 'accountTier'> | null | undefined,
): EmailVerificationMode {
	return user?.reportsAccountTier === true && user.accountTier === AccountTiers.UNVERIFIED
		? EmailVerificationModes.ENTER_CODE
		: EmailVerificationModes.RESEND;
}

export const VerifyEmailButton = observer(({children}: {children: React.ReactNode}) => {
	const {isSending, resend} = useResendVerificationEmail();
	if (!RuntimeConfig.emailsEnabled) return null;
	return (
		<Button
			variant="primary"
			small
			disabled={isSending}
			submitting={isSending}
			onClick={
				verificationMode(Users.currentUser) === EmailVerificationModes.ENTER_CODE
					? () => openClaimAccountModal()
					: resend
			}
			data-flx="app.email-verification-alert.button.verify-email"
		>
			{children}
		</Button>
	);
});

interface EmailVerificationAlertProps {
	title?: React.ReactNode;
	children?: React.ReactNode;
}

export const EmailVerificationAlert = observer(({title, children}: EmailVerificationAlertProps) => {
	const {isSending, resend} = useResendVerificationEmail();
	if (!RuntimeConfig.emailsEnabled) {
		return null;
	}
	if (verificationMode(Users.currentUser) === EmailVerificationModes.ENTER_CODE) {
		return (
			<WarningAlert
				title={title ?? <Trans>Email verification required</Trans>}
				actions={
					<Button
						variant="primary"
						small
						onClick={() => openClaimAccountModal()}
						data-flx="app.email-verification-alert.button.enter-code"
					>
						<Trans>Enter code</Trans>
					</Button>
				}
				data-flx="app.email-verification-alert.code-warning-alert"
			>
				{children ?? (
					<Trans comment="Warning shown when the account email is not confirmed yet. The person confirms it by entering a code sent to their email.">
						Confirm your email with a code to send messages and join communities.
					</Trans>
				)}
			</WarningAlert>
		);
	}
	return (
		<WarningAlert
			title={title ?? <Trans>Email verification required</Trans>}
			actions={
				<Button
					variant="primary"
					small
					disabled={isSending}
					submitting={isSending}
					onClick={resend}
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
