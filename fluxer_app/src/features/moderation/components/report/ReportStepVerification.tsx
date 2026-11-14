// SPDX-License-Identifier: AGPL-3.0-or-later

import {EXAMPLE_VERIFICATION_CODE} from '@app/features/app/config/I18nDisplayConstants';
import {
	CONTINUE_DESCRIPTOR,
	MINUTES_DURATION_PLURAL_DESCRIPTOR,
	SECONDS_DURATION_PLURAL_DESCRIPTOR,
	VERIFICATION_CODE_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import styles from '@app/features/moderation/components/pages/ReportPage.module.css';
import {Button} from '@app/features/ui/button/Button';
import {Input} from '@app/features/ui/components/form/FormInput';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import type {I18n} from '@lingui/core';
import {Trans, useLingui} from '@lingui/react/macro';
import type React from 'react';

function cooldownInSeconds(seconds: number): boolean {
	return seconds <= 60;
}

export function formatCooldownDuration(i18n: I18n, seconds: number): string {
	if (cooldownInSeconds(seconds)) return i18n._(SECONDS_DURATION_PLURAL_DESCRIPTOR, {seconds});
	return i18n._(MINUTES_DURATION_PLURAL_DESCRIPTOR, {minutes: Math.ceil(seconds / 60)});
}

export const ResendCooldownLabel: React.FC<{resendCooldownSeconds: number}> = ({resendCooldownSeconds}) => {
	const {i18n} = useLingui();
	if (cooldownInSeconds(resendCooldownSeconds)) return <Trans>Resend ({resendCooldownSeconds}s)</Trans>;
	const duration = formatCooldownDuration(i18n, resendCooldownSeconds);
	return <Trans>Resend ({duration})</Trans>;
};

interface Props {
	email: string;
	verified: boolean;
	verificationCode: string;
	errorMessage: string | null;
	isVerifying: boolean;
	isResending: boolean;
	resendCooldownSeconds: number;
	onChangeEmail: () => void;
	onResend: () => void;
	onVerify: () => void;
	onCodeChange: (value: string) => void;
	onStartOver: () => void;
}

export const ReportStepVerification: React.FC<Props> = ({
	email,
	verified,
	verificationCode,
	errorMessage,
	isVerifying,
	isResending,
	resendCooldownSeconds,
	onChangeEmail,
	onResend,
	onVerify,
	onCodeChange,
	onStartOver,
}) => {
	const {i18n} = useLingui();
	const codeForValidation = verificationCode.trim().toUpperCase();
	const codeLooksValid = /^[A-Z0-9]{4}-[A-Z0-9]{4}$/.test(codeForValidation);
	return (
		<div className={styles.card} data-flx="moderation.report.report-step-verification.card">
			<header className={styles.cardHeader} data-flx="moderation.report.report-step-verification.card-header">
				<p className={styles.eyebrow} data-flx="moderation.report.report-step-verification.eyebrow">
					<Trans>Step 3</Trans>
				</p>
				<h1 className={styles.title} data-flx="moderation.report.report-step-verification.title">
					<Trans>Enter verification code</Trans>
				</h1>
				<p className={styles.description} data-flx="moderation.report.report-step-verification.description">
					<Trans>We sent a code to {email}.</Trans>
				</p>
			</header>
			<div className={styles.cardBody} data-flx="moderation.report.report-step-verification.card-body">
				{errorMessage && (
					<div
						className={styles.errorBox}
						role="alert"
						aria-live="polite"
						data-flx="moderation.report.report-step-verification.error-box"
					>
						{errorMessage}
					</div>
				)}
				<form
					className={styles.form}
					onSubmit={(e) => {
						e.preventDefault();
						onVerify();
					}}
					data-flx="moderation.report.report-step-verification.form.prevent-default"
				>
					<Input
						label={i18n._(VERIFICATION_CODE_DESCRIPTOR)}
						type="text"
						value={verificationCode}
						onChange={(e) => onCodeChange(e.target.value)}
						placeholder={EXAMPLE_VERIFICATION_CODE}
						autoComplete="one-time-code"
						data-flx="moderation.report.report-step-verification.input.code-change.text"
					/>
					<div className={styles.actionRow} data-flx="moderation.report.report-step-verification.action-row">
						<Button
							fitContent
							type="submit"
							disabled={!(verified || codeLooksValid) || isVerifying}
							submitting={isVerifying}
							className={styles.actionButton}
							data-flx="moderation.report.report-step-verification.action-button.submit"
						>
							{verified ? i18n._(CONTINUE_DESCRIPTOR) : <Trans>Verify code</Trans>}
						</Button>
						{!verified && (
							<Button
								variant="secondary"
								fitContent
								type="button"
								onClick={onResend}
								disabled={isResending || isVerifying || resendCooldownSeconds > 0}
								submitting={isResending}
								data-flx="moderation.report.report-step-verification.button.resend"
							>
								{resendCooldownSeconds > 0 ? (
									<ResendCooldownLabel
										resendCooldownSeconds={resendCooldownSeconds}
										data-flx="moderation.report.report-step-verification.resend-cooldown-label"
									/>
								) : (
									<Trans>Resend code</Trans>
								)}
							</Button>
						)}
					</div>
				</form>
			</div>
			<footer className={styles.footerLinks} data-flx="moderation.report.report-step-verification.footer-links">
				<p className={styles.linkRow} data-flx="moderation.report.report-step-verification.link-row">
					<FocusRing offset={-2} data-flx="moderation.report.report-step-verification.focus-ring.change-email">
						<button
							type="button"
							className={styles.linkButton}
							onClick={onChangeEmail}
							data-flx="moderation.report.report-step-verification.link-button.change-email"
						>
							<Trans>Change email</Trans>
						</button>
					</FocusRing>
					<span
						aria-hidden="true"
						className={styles.linkSeparator}
						data-flx="moderation.report.report-step-verification.link-separator"
					>
						·
					</span>
					<FocusRing offset={-2} data-flx="moderation.report.report-step-verification.focus-ring.start-over">
						<button
							type="button"
							className={styles.linkButton}
							onClick={onStartOver}
							data-flx="moderation.report.report-step-verification.link-button.start-over"
						>
							<Trans>Start over</Trans>
						</button>
					</FocusRing>
				</p>
			</footer>
		</div>
	);
};
