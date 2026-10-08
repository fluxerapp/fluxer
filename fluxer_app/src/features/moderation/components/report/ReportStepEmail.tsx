// SPDX-License-Identifier: AGPL-3.0-or-later

import {EXAMPLE_REPORT_EMAIL} from '@app/features/app/config/I18nDisplayConstants';
import {CONTINUE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import styles from '@app/features/moderation/components/pages/ReportPage.module.css';
import {ResendCooldownLabel} from '@app/features/moderation/components/report/ReportStepVerification';
import {Button} from '@app/features/ui/button/Button';
import {Input} from '@app/features/ui/components/form/FormInput';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import type React from 'react';

const EMAIL_ADDRESS_DESCRIPTOR = msg({
	message: 'Email address',
	comment: 'Short label in the moderation report step email. Keep it concise. Keep the tone plain and specific.',
});

interface Props {
	email: string;
	errorMessage: string | null;
	isSending: boolean;
	verified: boolean;
	resendCooldownSeconds: number;
	onEmailChange: (value: string) => void;
	onSubmit: () => void;
	onStartOver: () => void;
}

export const ReportStepEmail: React.FC<Props> = ({
	email,
	errorMessage,
	isSending,
	verified,
	resendCooldownSeconds,
	onEmailChange,
	onSubmit,
	onStartOver,
}) => {
	const {i18n} = useLingui();
	const normalizedEmail = email.trim();
	const emailLooksValid = normalizedEmail.length > 0 && /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(normalizedEmail);
	return (
		<div className={styles.card} data-flx="moderation.report.report-step-email.card">
			<header className={styles.cardHeader} data-flx="moderation.report.report-step-email.card-header">
				<p className={styles.eyebrow} data-flx="moderation.report.report-step-email.eyebrow">
					<Trans>Step 2</Trans>
				</p>
				<h1 className={styles.title} data-flx="moderation.report.report-step-email.title">
					<Trans>Verify your email</Trans>
				</h1>
				<p className={styles.description} data-flx="moderation.report.report-step-email.description">
					<Trans>We'll send a short code to confirm you can receive updates about this report.</Trans>
				</p>
			</header>
			<div className={styles.cardBody} data-flx="moderation.report.report-step-email.card-body">
				{errorMessage && (
					<div
						className={styles.errorBox}
						role="alert"
						aria-live="polite"
						data-flx="moderation.report.report-step-email.error-box"
					>
						{errorMessage}
					</div>
				)}
				<form
					className={styles.form}
					onSubmit={(e) => {
						e.preventDefault();
						onSubmit();
					}}
					data-flx="moderation.report.report-step-email.form.prevent-default"
				>
					<Input
						label={i18n._(EMAIL_ADDRESS_DESCRIPTOR)}
						type="email"
						dir="ltr"
						value={email}
						onChange={(e) => onEmailChange(e.target.value)}
						placeholder={EXAMPLE_REPORT_EMAIL}
						autoComplete="email"
						data-flx="moderation.report.report-step-email.input.email-change"
					/>
					<div className={styles.actionRow} data-flx="moderation.report.report-step-email.action-row">
						<Button
							fitContent
							type="submit"
							disabled={!emailLooksValid || isSending || (!verified && resendCooldownSeconds > 0)}
							submitting={isSending}
							className={styles.actionButton}
							data-flx="moderation.report.report-step-email.action-button.submit"
						>
							{verified ? (
								i18n._(CONTINUE_DESCRIPTOR)
							) : resendCooldownSeconds > 0 ? (
								<ResendCooldownLabel
									resendCooldownSeconds={resendCooldownSeconds}
									data-flx="moderation.report.report-step-email.resend-cooldown-label"
								/>
							) : (
								<Trans>Send verification code</Trans>
							)}
						</Button>
						<Button
							variant="secondary"
							fitContent
							type="button"
							onClick={onStartOver}
							disabled={isSending}
							className={styles.actionButton}
							data-flx="moderation.report.report-step-email.action-button.start-over"
						>
							<Trans>Start over</Trans>
						</Button>
					</div>
				</form>
			</div>
		</div>
	);
};
