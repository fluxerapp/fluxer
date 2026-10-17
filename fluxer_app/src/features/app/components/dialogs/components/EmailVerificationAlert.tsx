// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {useResendVerificationEmail} from '@app/features/auth/hooks/useVerifyEmailAction';
import {Button} from '@app/features/ui/button/Button';
import {WarningAlert} from '@app/features/ui/warning_alert/WarningAlert';
import {Trans} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

interface EmailVerificationAlertProps {
	title?: React.ReactNode;
	children?: React.ReactNode;
}

export const EmailVerificationAlert = observer(({title, children}: EmailVerificationAlertProps) => {
	const {isSending, resend} = useResendVerificationEmail();
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
