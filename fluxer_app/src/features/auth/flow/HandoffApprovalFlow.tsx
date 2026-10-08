// SPDX-License-Identifier: AGPL-3.0-or-later

import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {DesktopHandoffMode} from '@app/features/auth/flow/auth_login_core/AuthLoginStepTypes';
import type {DesktopHandoffFlow} from '@app/features/auth/flow/auth_login_core/useDesktopHandoffFlow';
import styles from '@app/features/auth/flow/HandoffApprovalFlow.module.css';
import {
	CANCEL_DESCRIPTOR,
	SOMETHING_WENT_WRONG_DESCRIPTOR,
	TRY_AGAIN_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import {Input} from '@app/features/ui/components/form/FormInput';
import {Spinner} from '@app/features/ui/components/Spinner';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {
	formatDesktopHandoffCode,
	isDesktopHandoffCode,
	parseDesktopHandoffCodeInput,
} from '@fluxer/schema/src/domains/auth/DesktopHandoffCode';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {CheckCircleIcon, ProhibitIcon} from '@phosphor-icons/react';
import type React from 'react';
import {useCallback, useState} from 'react';

const APP_CODE_LABEL_DESCRIPTOR = msg({
	message: 'Code from the {productName} app',
	comment:
		'Label of the field in the browser where the user types the 12-character code shown in the desktop or mobile app. productName is the app name, such as Fluxer.',
});
const SIGN_IN_DESCRIPTOR = msg({
	message: 'Sign in',
	comment: 'Button in the browser that approves signing in to the desktop or mobile app that asked for it.',
});

function formatLocation(location: {
	city?: string | null;
	region?: string | null;
	country?: string | null;
}): string | null {
	const parts = [location.city, location.region, location.country].filter(Boolean);
	return parts.length > 0 ? parts.join(', ') : null;
}

function HandoffCode({code}: {code: string}) {
	return (
		<span className={styles.code} data-flx="auth.flow.handoff-approval-flow.code">
			{formatDesktopHandoffCode(code)}
		</span>
	);
}

function ApprovalTitle({handoff}: {handoff: DesktopHandoffFlow}) {
	if (handoff.returnMethod === 'deep_link' || handoff.clientInfo?.device === 'desktop') {
		return (
			<Trans comment="Title of the browser page where the user approves signing in to the desktop app that opened it. PRODUCT_NAME is the app name, such as Fluxer.">
				Sign in to the {PRODUCT_NAME} desktop app
			</Trans>
		);
	}
	if (handoff.clientInfo?.device === 'mobile') {
		return (
			<Trans comment="Title of the browser page where the user approves signing in to the mobile app that asked for it. PRODUCT_NAME is the app name, such as Fluxer.">
				Sign in to the {PRODUCT_NAME} mobile app
			</Trans>
		);
	}
	return (
		<Trans comment="Title of the browser page where the user approves signing in to an app that asked for it. PRODUCT_NAME is the app name, such as Fluxer.">
			Sign in to the {PRODUCT_NAME} app
		</Trans>
	);
}

function DeviceCard({clientInfo}: {clientInfo: DesktopHandoffFlow['clientInfo']}) {
	const platform = clientInfo?.platform ?? null;
	const os = clientInfo?.os ?? null;
	const location = clientInfo?.location ? formatLocation(clientInfo.location) : null;
	if (!platform && !os && !location) {
		return null;
	}
	return (
		<div className={styles.deviceCard} data-flx="auth.flow.handoff-approval-flow.device-card">
			{platform ? (
				<div className={styles.deviceRow} data-flx="auth.flow.handoff-approval-flow.device-row">
					<span className={styles.deviceLabel} data-flx="auth.flow.handoff-approval-flow.device-label">
						<Trans>Platform</Trans>
					</span>
					<span className={styles.deviceValue} data-flx="auth.flow.handoff-approval-flow.device-value">
						{platform}
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
	);
}

function AccountLine({handoff, onUseAnotherAccount}: {handoff: DesktopHandoffFlow; onUseAnotherAccount: () => void}) {
	const account = handoff.account;
	if (account == null) {
		return null;
	}
	const name = account.globalName ?? account.username;
	const requestedIdentifier = handoff.requestedIdentifier;
	return (
		<div className={styles.accountLine} data-flx="auth.flow.handoff-approval-flow.account-line">
			{requestedIdentifier != null ? (
				<p className={styles.wrongAccount} role="alert" data-flx="auth.flow.handoff-approval-flow.wrong-account">
					<Trans comment="Warning on the browser approval page when the app asked for a different account than the one chosen. requestedIdentifier is the email or username the app asked for, name is the chosen account's display name.">
						The {PRODUCT_NAME} app asked to sign in as {requestedIdentifier}, but you chose {name}.
					</Trans>
				</p>
			) : (
				<span data-flx="auth.flow.handoff-approval-flow.account-name">
					<Trans comment="Shown on the browser approval page above the buttons. name is the display name of the account that will be signed in to the app.">
						Signing in as {name}
					</Trans>
				</span>
			)}
			<FocusRing offset={-2} data-flx="auth.flow.handoff-approval-flow.focus-ring.use-another-account">
				<button
					type="button"
					className={styles.textButton}
					onClick={onUseAnotherAccount}
					data-flx="auth.flow.handoff-approval-flow.button.use-another-account"
				>
					<Trans comment="Text button on the browser approval page that goes back to choosing which account signs in to the app.">
						Use a different account
					</Trans>
				</button>
			</FocusRing>
		</div>
	);
}

interface HandoffApprovalFlowProps {
	handoff: DesktopHandoffFlow;
	onRetry?: () => void;
}

export function HandoffApprovalFlow({handoff, onRetry}: HandoffApprovalFlowProps) {
	const {i18n} = useLingui();
	const [codeInput, setCodeInput] = useState('');
	const retry = onRetry ?? handoff.retry;
	const {submitCode} = handoff;
	const handleCodeChange = useCallback(
		(e: React.ChangeEvent<HTMLInputElement>) => {
			const rawCode = parseDesktopHandoffCodeInput(e.target.value);
			setCodeInput(rawCode);
			if (isDesktopHandoffCode(rawCode)) {
				void submitCode(rawCode);
			}
		},
		[submitCode],
	);
	if (handoff.mode === DesktopHandoffMode.CODE_INPUT) {
		return (
			<div className={styles.container} data-flx="auth.flow.handoff-approval-flow.container--2">
				<h1 className={styles.title} data-flx="auth.flow.handoff-approval-flow.title">
					<Trans comment="Title of the browser page where the user types the 12-character code the app shows under Signing in on another device. PRODUCT_NAME is the app name, such as Fluxer.">
						Enter the code from the {PRODUCT_NAME} app
					</Trans>
				</h1>
				<p className={styles.description} data-flx="auth.flow.handoff-approval-flow.description">
					<Trans comment="Explains where the code comes from, under the title of the browser page where it is typed. Signing in on another device is the label of a button in the app and must match its translation. PRODUCT_NAME is the app name, such as Fluxer.">
						The {PRODUCT_NAME} app on your other device shows it under Signing in on another device. Only enter a code
						from a device you control.
					</Trans>
				</p>
				<div className={styles.codeInputSection} data-flx="auth.flow.handoff-approval-flow.code-input-section">
					<Input
						label={i18n._(APP_CODE_LABEL_DESCRIPTOR, {productName: PRODUCT_NAME})}
						name="desktop_handoff_code"
						className={styles.codeInput}
						value={formatDesktopHandoffCode(codeInput)}
						onChange={handleCodeChange}
						placeholder="XXXXXX-XXXXXX"
						autoComplete="off"
						autoCapitalize="characters"
						autoFocus
						enterKeyHint="done"
						spellCheck={false}
						data-flx="auth.flow.handoff-approval-flow.input.code-change"
					/>
				</div>
				<Button
					onClick={retry}
					variant="secondary"
					fitContainer
					data-flx="auth.flow.handoff-approval-flow.button.cancel"
				>
					{i18n._(CANCEL_DESCRIPTOR)}
				</Button>
			</div>
		);
	}
	if (handoff.mode === DesktopHandoffMode.FETCHING_INFO || handoff.mode === DesktopHandoffMode.COMPLETING) {
		return (
			<div className={styles.container} data-flx="auth.flow.handoff-approval-flow.container--3">
				<h1 className={styles.title} data-flx="auth.flow.handoff-approval-flow.title--2">
					{handoff.mode === DesktopHandoffMode.FETCHING_INFO ? (
						<Trans comment="Title while the browser looks up the sign-in request from the app.">
							Checking the sign-in request…
						</Trans>
					) : (
						<Trans comment="Title while the browser approves the sign-in to the app.">Signing in…</Trans>
					)}
				</h1>
				<div className={styles.spinner} data-flx="auth.flow.handoff-approval-flow.spinner">
					<Spinner data-flx="auth.flow.handoff-approval-flow.spinner-icon" />
				</div>
			</div>
		);
	}
	if (handoff.mode === DesktopHandoffMode.APPROVING) {
		const matchCode = handoff.codeSource === 'link' && handoff.returnMethod === 'code' ? handoff.handoffCode : null;
		return (
			<div className={styles.container} data-flx="auth.flow.handoff-approval-flow.container--4">
				<h1 className={styles.title} data-flx="auth.flow.handoff-approval-flow.title--3">
					<ApprovalTitle handoff={handoff} />
				</h1>
				<p className={styles.description} data-flx="auth.flow.handoff-approval-flow.description--2">
					<Trans comment="Warning under the title of the browser approval page, so people do not approve a sign-in someone else started.">
						Only continue if you started this sign-in on a device you control.
					</Trans>
				</p>
				<AccountLine handoff={handoff} onUseAnotherAccount={retry} />
				{matchCode != null ? (
					<div className={styles.matchCheck} data-flx="auth.flow.handoff-approval-flow.match-check">
						<span data-flx="auth.flow.handoff-approval-flow.match-check-label">
							<Trans comment="Shown above the code on the browser approval page when the app cannot be signed in automatically. PRODUCT_NAME is the app name, such as Fluxer.">
								Make sure this code matches the one in the {PRODUCT_NAME} app.
							</Trans>
						</span>
						<HandoffCode code={matchCode} />
					</div>
				) : null}
				<DeviceCard clientInfo={handoff.clientInfo} />
				<div className={styles.buttonRow} data-flx="auth.flow.handoff-approval-flow.button-row">
					<Button onClick={handoff.deny} variant="secondary" data-flx="auth.flow.handoff-approval-flow.button.deny">
						{i18n._(CANCEL_DESCRIPTOR)}
					</Button>
					<Button onClick={handoff.approve} variant="primary" data-flx="auth.flow.handoff-approval-flow.button.approve">
						{i18n._(SIGN_IN_DESCRIPTOR)}
					</Button>
				</div>
			</div>
		);
	}
	if (handoff.mode === DesktopHandoffMode.DONE) {
		return (
			<div className={styles.container} data-flx="auth.flow.handoff-approval-flow.container--6">
				<CheckCircleIcon
					size={remFromPx(48)}
					weight="fill"
					className={styles.successIcon}
					data-flx="auth.flow.handoff-approval-flow.success-icon"
				/>
				<h1 className={styles.title} data-flx="auth.flow.handoff-approval-flow.title--5">
					<Trans comment="Title of the browser page after the user approved signing in to the app.">
						You're signed in
					</Trans>
				</h1>
				<p className={styles.description} data-flx="auth.flow.handoff-approval-flow.description--3">
					{handoff.returnUrl != null ? (
						<Trans comment="Shown after approving, when the browser has already handed the sign-in back to the desktop app. PRODUCT_NAME is the app name, such as Fluxer.">
							Go back to the {PRODUCT_NAME} app. You can close this tab.
						</Trans>
					) : (
						<Trans comment="Shown after approving a sign-in for an app on another device, which signs in by itself shortly. PRODUCT_NAME is the app name, such as Fluxer.">
							Go back to the {PRODUCT_NAME} app. It finishes signing in on its own.
						</Trans>
					)}
				</p>
				{handoff.returnUrl != null ? (
					<Button
						onClick={handoff.reopenApp}
						variant="secondary"
						fitContainer
						data-flx="auth.flow.handoff-approval-flow.button.open-app"
					>
						<Trans comment="Button on the browser page after approving that opens the desktop app again if it did not come to the front. PRODUCT_NAME is the app name, such as Fluxer.">
							Open {PRODUCT_NAME}
						</Trans>
					</Button>
				) : null}
			</div>
		);
	}
	if (handoff.mode === DesktopHandoffMode.DENIED) {
		return (
			<div className={styles.container} data-flx="auth.flow.handoff-approval-flow.container--8">
				<ProhibitIcon
					size={remFromPx(48)}
					weight="regular"
					className={styles.deniedIcon}
					data-flx="auth.flow.handoff-approval-flow.denied-icon"
				/>
				<h1 className={styles.title} data-flx="auth.flow.handoff-approval-flow.title--7">
					<Trans comment="Title of the browser page after the user cancelled the sign-in request from the app.">
						Sign-in cancelled
					</Trans>
				</h1>
				<p className={styles.description} data-flx="auth.flow.handoff-approval-flow.description--4">
					<Trans comment="Shown after the user cancelled the sign-in request in the browser. PRODUCT_NAME is the app name, such as Fluxer.">
						The {PRODUCT_NAME} app was not signed in. You can close this tab.
					</Trans>
				</p>
				<Button
					onClick={retry}
					variant="secondary"
					fitContainer
					data-flx="auth.flow.handoff-approval-flow.button.restart"
				>
					<Trans comment="Button on the browser page after cancelling that goes back to choosing an account.">
						Start over
					</Trans>
				</Button>
			</div>
		);
	}
	if (handoff.mode === DesktopHandoffMode.ERROR) {
		return (
			<div className={styles.container} data-flx="auth.flow.handoff-approval-flow.container--7">
				<h1 className={styles.title} data-flx="auth.flow.handoff-approval-flow.title--6">
					{i18n._(SOMETHING_WENT_WRONG_DESCRIPTOR)}
				</h1>
				{handoff.error ? (
					<p className={styles.error} role="alert" data-flx="auth.flow.handoff-approval-flow.error">
						{handoff.error}
					</p>
				) : null}
				<Button onClick={retry} fitContainer data-flx="auth.flow.handoff-approval-flow.button.retry">
					{i18n._(TRY_AGAIN_DESCRIPTOR)}
				</Button>
			</div>
		);
	}
	return null;
}
