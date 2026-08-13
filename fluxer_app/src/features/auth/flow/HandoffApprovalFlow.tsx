// SPDX-License-Identifier: AGPL-3.0-or-later

import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import type {DesktopHandoffInfoResponse} from '@app/features/auth/commands/AuthenticationCommands';
import type {DesktopHandoffMode} from '@app/features/auth/flow/auth_login_core/useDesktopHandoffFlow';
import styles from '@app/features/auth/flow/HandoffApprovalFlow.module.css';
import {TRY_AGAIN_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import {Input} from '@app/features/ui/components/form/FormInput';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {CheckCircleIcon, ShieldWarningIcon} from '@phosphor-icons/react';
import type React from 'react';
import {useCallback, useState} from 'react';

const SIGN_IN_CODE_DESCRIPTOR = msg({
	message: 'Sign-in code',
	comment: 'Short label in the authentication handoff approval flow. Keep the tone plain and specific.',
});
const PRODUCT_DESKTOP_DESCRIPTOR = msg({
	message: '{productName} Desktop',
	comment:
		'Display name for the product desktop client in the authentication handoff approval flow when the raw client name is Electron. Preserve {productName}; it is inserted by code.',
});
const CODE_LENGTH = 12;
const VALID_CODE_PATTERN = /^[A-Za-z0-9]{12}$/;

function formatCodeForDisplay(raw: string): string {
	const cleaned = raw
		.replace(/[^A-Za-z0-9]/g, '')
		.toUpperCase()
		.slice(0, CODE_LENGTH);
	if (cleaned.length <= 6) {
		return cleaned;
	}
	return `${cleaned.slice(0, 6)}-${cleaned.slice(6)}`;
}

function extractRawCode(formatted: string): string {
	return formatted
		.replace(/[^A-Za-z0-9]/g, '')
		.toUpperCase()
		.slice(0, CODE_LENGTH);
}

function formatLocation(location: {
	city?: string | null;
	region?: string | null;
	country?: string | null;
}): string | null {
	const parts = [location.city, location.region, location.country].filter(Boolean);
	return parts.length > 0 ? parts.join(', ') : null;
}

function isElectronClientLabel(label: string): boolean {
	return label.trim().toLowerCase() === 'electron';
}

interface HandoffApprovalFlowProps {
	mode: DesktopHandoffMode;
	error: string | null;
	clientInfo: DesktopHandoffInfoResponse['client_info'];
	onProceedToCodeInput: () => void;
	onSubmitCode: (code: string) => void;
	onApprove: () => void;
	onDeny: () => void;
	onRetry: () => void;
}

export function HandoffApprovalFlow({
	mode,
	error,
	clientInfo,
	onProceedToCodeInput,
	onSubmitCode,
	onApprove,
	onDeny,
	onRetry,
}: HandoffApprovalFlowProps) {
	const {i18n} = useLingui();
	const [codeInput, setCodeInput] = useState('');
	const handleCodeChange = useCallback(
		(e: React.ChangeEvent<HTMLInputElement>) => {
			const rawCode = extractRawCode(e.target.value);
			setCodeInput(rawCode);
			if (VALID_CODE_PATTERN.test(rawCode)) {
				onSubmitCode(rawCode);
			}
		},
		[onSubmitCode],
	);
	if (mode === 'warning') {
		return (
			<div className={styles.container} data-flx="auth.flow.handoff-approval-flow.container">
				<div className={styles.warningBox} data-flx="auth.flow.handoff-approval-flow.warning-box">
					<ShieldWarningIcon
						size={remFromPx(32)}
						weight="fill"
						className={styles.warningIcon}
						data-flx="auth.flow.handoff-approval-flow.warning-icon"
					/>
					<h1 className={styles.warningTitle} data-flx="auth.flow.handoff-approval-flow.warning-title">
						<Trans>Sign-in request</Trans>
					</h1>
					<p className={styles.warningDescription} data-flx="auth.flow.handoff-approval-flow.warning-description">
						<Trans>
							You are about to authorize an app to sign in to your account. Only continue if you initiated this request
							from a device you control.
						</Trans>
					</p>
				</div>
				<Button
					onClick={onProceedToCodeInput}
					variant="primary"
					fitContainer
					data-flx="auth.flow.handoff-approval-flow.button.proceed-to-code-input"
				>
					<Trans>Continue</Trans>
				</Button>
			</div>
		);
	}
	if (mode === 'code_input') {
		return (
			<div className={styles.container} data-flx="auth.flow.handoff-approval-flow.container--2">
				<h1 className={styles.title} data-flx="auth.flow.handoff-approval-flow.title">
					<Trans>Enter the code</Trans>
				</h1>
				<p className={styles.description} data-flx="auth.flow.handoff-approval-flow.description">
					<Trans>Enter the code shown in the app to continue.</Trans>
				</p>
				<div className={styles.codeInputSection} data-flx="auth.flow.handoff-approval-flow.code-input-section">
					<Input
						aria-label={i18n._(SIGN_IN_CODE_DESCRIPTOR)}
						name="desktop_handoff_code"
						value={formatCodeForDisplay(codeInput)}
						onChange={handleCodeChange}
						autoComplete="off"
						autoCapitalize="characters"
						autoFocus
						enterKeyHint="done"
						spellCheck={false}
						data-flx="auth.flow.handoff-approval-flow.input.code-change"
					/>
				</div>
			</div>
		);
	}
	if (mode === 'fetching_info') {
		return (
			<div className={styles.container} data-flx="auth.flow.handoff-approval-flow.container--3">
				<h1 className={styles.title} data-flx="auth.flow.handoff-approval-flow.title--2">
					<Trans>Verifying code…</Trans>
				</h1>
				<div className={styles.spinner} data-flx="auth.flow.handoff-approval-flow.spinner">
					<span className={styles.spinnerIcon} data-flx="auth.flow.handoff-approval-flow.spinner-icon" />
				</div>
			</div>
		);
	}
	if (mode === 'approving') {
		const platform = clientInfo?.platform ?? null;
		const os = clientInfo?.os ?? null;
		const location = clientInfo?.location ? formatLocation(clientInfo.location) : null;
		const hasAnyDeviceInfo = Boolean(platform || os || location);
		const platformLabel = platform
			? isElectronClientLabel(platform)
				? i18n._(PRODUCT_DESKTOP_DESCRIPTOR, {productName: PRODUCT_NAME})
				: platform
			: null;
		return (
			<div className={styles.container} data-flx="auth.flow.handoff-approval-flow.container--4">
				<h1 className={styles.title} data-flx="auth.flow.handoff-approval-flow.title--3">
					<Trans>Was this you?</Trans>
				</h1>
				<p className={styles.description} data-flx="auth.flow.handoff-approval-flow.description--2">
					{hasAnyDeviceInfo ? (
						<Trans>A sign-in request was made from the following device. Only approve if you recognize it.</Trans>
					) : (
						<Trans>A sign-in request was made for your account. Only approve if you initiated it.</Trans>
					)}
				</p>
				{hasAnyDeviceInfo ? (
					<div className={styles.deviceCard} data-flx="auth.flow.handoff-approval-flow.device-card">
						{platformLabel ? (
							<div className={styles.deviceRow} data-flx="auth.flow.handoff-approval-flow.device-row">
								<span className={styles.deviceLabel} data-flx="auth.flow.handoff-approval-flow.device-label">
									<Trans>Platform</Trans>
								</span>
								<span className={styles.deviceValue} data-flx="auth.flow.handoff-approval-flow.device-value">
									{platformLabel}
								</span>
							</div>
						) : null}
						{os ? (
							<div className={styles.deviceRow} data-flx="auth.flow.handoff-approval-flow.device-row--2">
								<span className={styles.deviceLabel} data-flx="auth.flow.handoff-approval-flow.device-label--2">
									<Trans>Operating system</Trans>
								</span>
								<span className={styles.deviceValue} data-flx="auth.flow.handoff-approval-flow.device-value--2">
									{os}
								</span>
							</div>
						) : null}
						{location ? (
							<div className={styles.deviceRow} data-flx="auth.flow.handoff-approval-flow.device-row--3">
								<span className={styles.deviceLabel} data-flx="auth.flow.handoff-approval-flow.device-label--3">
									<Trans>Location</Trans>
								</span>
								<span className={styles.deviceValue} data-flx="auth.flow.handoff-approval-flow.device-value--3">
									{location}
								</span>
							</div>
						) : null}
					</div>
				) : null}
				<div className={styles.buttonRow} data-flx="auth.flow.handoff-approval-flow.button-row">
					<Button onClick={onDeny} variant="secondary" data-flx="auth.flow.handoff-approval-flow.button.deny">
						<Trans>Deny</Trans>
					</Button>
					<Button onClick={onApprove} variant="primary" data-flx="auth.flow.handoff-approval-flow.button.approve">
						<Trans>Approve</Trans>
					</Button>
				</div>
			</div>
		);
	}
	if (mode === 'completing') {
		return (
			<div className={styles.container} data-flx="auth.flow.handoff-approval-flow.container--5">
				<h1 className={styles.title} data-flx="auth.flow.handoff-approval-flow.title--4">
					<Trans>Completing sign-in…</Trans>
				</h1>
				<div className={styles.spinner} data-flx="auth.flow.handoff-approval-flow.spinner--2">
					<span className={styles.spinnerIcon} data-flx="auth.flow.handoff-approval-flow.spinner-icon--2" />
				</div>
			</div>
		);
	}
	if (mode === 'done') {
		return (
			<div className={styles.container} data-flx="auth.flow.handoff-approval-flow.container--6">
				<CheckCircleIcon
					size={remFromPx(48)}
					weight="fill"
					className={styles.successIcon}
					data-flx="auth.flow.handoff-approval-flow.success-icon"
				/>
				<h1 className={styles.title} data-flx="auth.flow.handoff-approval-flow.title--5">
					<Trans>Sign-in approved</Trans>
				</h1>
				<p className={styles.description} data-flx="auth.flow.handoff-approval-flow.description--3">
					<Trans>The app should now be signed in to your account.</Trans>
				</p>
			</div>
		);
	}
	if (mode === 'error') {
		return (
			<div className={styles.container} data-flx="auth.flow.handoff-approval-flow.container--7">
				<h1 className={styles.title} data-flx="auth.flow.handoff-approval-flow.title--6">
					<Trans>Something went wrong</Trans>
				</h1>
				{error ? (
					<p className={styles.error} role="alert" data-flx="auth.flow.handoff-approval-flow.error">
						{error}
					</p>
				) : null}
				<Button onClick={onRetry} fitContainer data-flx="auth.flow.handoff-approval-flow.button.retry">
					{i18n._(TRY_AGAIN_DESCRIPTOR)}
				</Button>
			</div>
		);
	}
	return null;
}

interface ConnectedHandoffApprovalFlowProps {
	handoff: {
		mode: DesktopHandoffMode;
		error: string | null;
		clientInfo: DesktopHandoffInfoResponse['client_info'];
		proceedToCodeInput: () => void;
		submitCode: (code: string) => void;
		approve: () => void;
		deny: () => void;
		retry: () => void;
	};
}

export function ConnectedHandoffApprovalFlow({handoff}: ConnectedHandoffApprovalFlowProps) {
	return (
		<HandoffApprovalFlow
			mode={handoff.mode}
			error={handoff.error}
			clientInfo={handoff.clientInfo}
			onProceedToCodeInput={handoff.proceedToCodeInput}
			onSubmitCode={handoff.submitCode}
			onApprove={handoff.approve}
			onDeny={handoff.deny}
			onRetry={handoff.retry}
			data-flx="auth.flow.handoff-approval-flow.connected-handoff-approval-flow.handoff-approval-flow"
		/>
	);
}
