// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import styles from '@app/features/auth/components/pages/LoginPage.module.css';
import {useAuthSingleUseRequest} from '@app/features/auth/flow/AuthSingleUseRequest';
import MfaScreen from '@app/features/auth/flow/MfaScreen';
import {useAuthPresentation} from '@app/features/auth/flow/useAuthPresentation';
import {
	completeSsoLogin,
	type LoginSuccessPayload,
	type MfaChallenge,
	startSsoLogin,
} from '@app/features/auth/state/AuthFlow';
import {AuthCardVariant} from '@app/features/auth/state/AuthLayoutContext';
import {type AuthRuntimeTarget, useAuthRuntimeTarget} from '@app/features/auth/state/AuthRuntimeTarget';
import {
	consumeSsoPendingContext,
	getSsoPendingContext,
	isValidOAuthState,
	type SsoPendingContext,
} from '@app/features/auth/state/SsoPendingContext';
import {safeRedirectTarget} from '@app/features/auth/utils/SafeRedirect';
import {
	BACK_TO_SIGN_IN_DESCRIPTOR,
	SIGN_IN_DESCRIPTOR,
	TRY_AGAIN_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {failureCode} from '@app/features/platform/utils/ResponseInspection';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Spinner} from '@app/features/ui/components/Spinner';
import {navigateToExternalURL} from '@app/features/ui/utils/NativeUtils';
import {useFluxerDocumentTitle} from '@app/features/window/hooks/useFluxerDocumentTitle';
import * as FormUtils from '@app/lib/forms';
import {flxElementClassName} from '@app/lib/react';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {SSO_MOBILE_CALLBACK_URI, SSO_MOBILE_STATE_PREFIX} from '@fluxer/constants/src/SsoConstants';
import type {I18n, MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useState} from 'react';

interface SsoCallbackError {
	readonly message: string;
	readonly awaitingApproval: boolean;
}

interface SsoCallbackMfa {
	readonly challenge: MfaChallenge;
	readonly redirectTo: string;
	readonly runtimeSnapshot: RuntimeConfigSnapshot;
}

const SSO_SIGN_IN_TIMED_OUT_PLEASE_TRY_AGAIN_DESCRIPTOR = msg({
	message: 'SSO sign-in timed out. Try again.',
	comment: 'SSO callback page error shown when the SSO sign-in timed out before the callback arrived.',
});
const MISSING_SSO_CODE_OR_STATE_PLEASE_TRY_SIGNING_DESCRIPTOR = msg({
	message: 'Missing SSO code. Sign in again.',
	comment: 'SSO callback page error shown when the SSO callback is missing code or state parameters.',
});
const FAILED_TO_COMPLETE_SSO_SIGN_IN_DESCRIPTOR = msg({
	message: 'Failed to complete SSO sign-in',
	comment: 'Short label in the authentication SSO callback page. Keep the tone plain and specific.',
});
const OPEN_PRODUCT_DESCRIPTOR = msg({
	message: 'Open {productName}',
	comment: 'Button that hands SSO sign-in back to the mobile app. productName is the app name.',
});
const SSO_SIGN_IN_CANCELLED_DESCRIPTOR = msg({
	message: 'Sign-in was cancelled.',
	comment: 'SSO callback page error shown when the person cancelled the sign-in at their sign-in provider.',
});
const SSO_PROVIDER_NEEDS_SIGN_IN_DESCRIPTOR = msg({
	message: 'Your provider needs you to sign in again.',
	comment: 'SSO callback page error shown when the sign-in provider asks the person to sign in or give consent again.',
});
const SSO_PROVIDER_REPORTED_ERROR_DESCRIPTOR = msg({
	message: 'Your provider reported an error.',
	comment: 'SSO callback page error shown when the sign-in provider sent back any other error.',
});
const SSO_PROVIDER_SIGN_IN_AGAIN_ERRORS = new Set(['login_required', 'consent_required', 'interaction_required']);
const SSO_CONNECTED_DESCRIPTOR = msg({
	message: 'Your account is connected to single sign-on.',
	comment: 'Toast shown after the account was connected to single sign-on from account settings.',
});
const SSO_TIMEOUT_MS = 30_000;
const logger = new Logger('SsoCallbackPage');

interface SsoCallbackParameters {
	code: string;
	state: string;
}

interface SsoCallbackAttemptDependencies {
	callbackContext: SsoPendingContext | null;
	callbackParameters: SsoCallbackParameters | null;
	callbackState: string | null;
	i18n: I18n;
	isCurrent: () => boolean;
	onError: (error: SsoCallbackError) => void;
	onRequireMfa: (mfa: SsoCallbackMfa) => void;
	onProcessingChange: (isProcessing: boolean) => void;
	providerError: string | null;
	providerErrorDescription: string | null;
	runtimeTarget: AuthRuntimeTarget;
}

function resolveCallbackParameters(code: string | null, state: string | null): SsoCallbackParameters | null {
	if (code == null || code.length === 0 || state == null || state.length === 0) {
		return null;
	}
	return {code, state};
}

function pendingRedirectTo(context: SsoPendingContext | null): string | null {
	return context?.redirectTo ?? null;
}

function resolveSsoRedirectTarget(
	payloadRedirect: string | undefined,
	context: SsoPendingContext | null,
	runtimeSnapshot: RuntimeConfigSnapshot,
): string {
	const target = safeRedirectTarget(payloadRedirect) ?? safeRedirectTarget(pendingRedirectTo(context)) ?? Routes.ME;
	const singleCommunityGuildId = runtimeSnapshot.community.single_community
		? runtimeSnapshot.community.single_community_guild_id
		: null;
	return target === Routes.ME && singleCommunityGuildId ? Routes.guildChannel(singleCommunityGuildId) : target;
}

class SsoCallbackAttempt {
	private readonly dependencies: SsoCallbackAttemptDependencies;
	private settled = false;
	private timeoutId: number | null = null;

	constructor(dependencies: SsoCallbackAttemptDependencies) {
		this.dependencies = dependencies;
	}

	public start(): void {
		this.timeoutId = window.setTimeout(() => this.handleTimeout(), SSO_TIMEOUT_MS);
		this.run().catch((error: unknown) => this.handleFailure(error));
	}

	private async run(): Promise<void> {
		const {providerError, callbackContext, i18n} = this.dependencies;
		if (providerError != null && providerError.length > 0) {
			this.finishWithError(i18n._(this.providerErrorDescriptor(providerError, callbackContext)));
			return;
		}
		const callbackParameters = this.dependencies.callbackParameters;
		if (callbackParameters == null) {
			this.finishWithError(this.dependencies.i18n._(MISSING_SSO_CODE_OR_STATE_PLEASE_TRY_SIGNING_DESCRIPTOR));
			return;
		}
		const runtimeSnapshot = this.dependencies.callbackContext?.runtimeSnapshot ?? null;
		if (runtimeSnapshot == null) {
			this.finishWithError(this.dependencies.i18n._(FAILED_TO_COMPLETE_SSO_SIGN_IN_DESCRIPTOR));
			return;
		}
		const result = await completeSsoLogin({
			code: callbackParameters.code,
			state: callbackParameters.state,
			clientBinding: this.dependencies.callbackContext?.clientBinding ?? null,
			runtimeSnapshot,
		});
		consumeSsoPendingContext(callbackParameters.state);
		if (this.settled || !this.dependencies.isCurrent()) {
			return;
		}
		this.stopTimeout();
		if (result.type === 'mfa') {
			this.settled = true;
			AuthenticationCommands.setMfaTicket({...result.challenge, runtimeSnapshot});
			this.dependencies.onRequireMfa({
				challenge: result.challenge,
				redirectTo: resolveSsoRedirectTarget(result.redirectTo, this.dependencies.callbackContext, runtimeSnapshot),
				runtimeSnapshot,
			});
			return;
		}
		if (result.type === 'linked') {
			this.settled = true;
			ToastCommands.success(this.dependencies.i18n._(SSO_CONNECTED_DESCRIPTOR));
			RouterUtils.replaceWith(
				resolveSsoRedirectTarget(result.redirectTo, this.dependencies.callbackContext, runtimeSnapshot),
			);
			return;
		}
		const redirectTo = resolveSsoRedirectTarget(
			result.payload.redirect_to,
			this.dependencies.callbackContext,
			runtimeSnapshot,
		);
		await AuthenticationCommands.completeLogin(
			{
				...result.payload,
				runtimeSnapshot,
			},
			{redirectPath: redirectTo},
		);
		this.dependencies.runtimeTarget.reset();
		this.settled = true;
	}

	private handleTimeout(): void {
		this.timeoutId = null;
		this.finishWithError(this.dependencies.i18n._(SSO_SIGN_IN_TIMED_OUT_PLEASE_TRY_AGAIN_DESCRIPTOR));
	}

	private handleFailure(error: unknown): void {
		if (!this.settled && AuthenticationCommands.showAccountRestriction(error)) {
			this.settled = true;
			this.stopTimeout();
			return;
		}
		const message =
			error != null && typeof error === 'object' && 'body' in error
				? FormUtils.extractErrorMessage(this.dependencies.i18n, error)
				: this.dependencies.i18n._(FAILED_TO_COMPLETE_SSO_SIGN_IN_DESCRIPTOR);
		this.finishWithError(message, failureCode(error) === APIErrorCodes.REGISTRATION_PENDING_APPROVAL);
	}

	private finishWithError(message: string, awaitingApproval = false): void {
		if (this.settled) {
			return;
		}
		this.settled = true;
		this.stopTimeout();
		if (!this.dependencies.isCurrent()) {
			return;
		}
		this.dependencies.onError({message, awaitingApproval});
		this.dependencies.onProcessingChange(false);
	}

	private providerErrorDescriptor(providerError: string, callbackContext: SsoPendingContext | null): MessageDescriptor {
		const {callbackState, providerErrorDescription} = this.dependencies;
		if (callbackContext == null || callbackState == null) {
			return FAILED_TO_COMPLETE_SSO_SIGN_IN_DESCRIPTOR;
		}
		consumeSsoPendingContext(callbackState);
		logger.warn('SSO provider returned an error', {error: providerError, description: providerErrorDescription});
		if (providerError === 'access_denied') {
			return SSO_SIGN_IN_CANCELLED_DESCRIPTOR;
		}
		if (SSO_PROVIDER_SIGN_IN_AGAIN_ERRORS.has(providerError)) {
			return SSO_PROVIDER_NEEDS_SIGN_IN_DESCRIPTOR;
		}
		return SSO_PROVIDER_REPORTED_ERROR_DESCRIPTOR;
	}

	private stopTimeout(): void {
		if (this.timeoutId == null) {
			return;
		}
		window.clearTimeout(this.timeoutId);
		this.timeoutId = null;
	}
}

const SsoCallbackPage = observer(function SsoCallbackPage() {
	const {i18n} = useLingui();
	const runtimeTarget = useAuthRuntimeTarget();
	useFluxerDocumentTitle(i18n._(SIGN_IN_DESCRIPTOR));
	useAuthPresentation({variant: AuthCardVariant.COMPACT});
	const params = new URLSearchParams(window.location.search);
	const code = params['get']('code');
	const state = params['get']('state');
	const providerError = params['get']('error');
	const providerErrorDescription = params['get']('error_description');
	const mobileCallbackUrl = state?.startsWith(SSO_MOBILE_STATE_PREFIX)
		? `${SSO_MOBILE_CALLBACK_URI}${window.location.search}`
		: null;
	const [error, setError] = useState<SsoCallbackError | null>(null);
	const [isProcessing, setIsProcessing] = useState(true);
	const [pendingContext, setPendingContext] = useState<SsoPendingContext | null>(null);
	const [mfa, setMfa] = useState<SsoCallbackMfa | null>(null);
	const handleBackToLogin = useCallback(() => {
		if (state != null && state.length > 0) {
			consumeSsoPendingContext(state);
		}
		const runtimeSnapshot = pendingContext?.runtimeSnapshot ?? null;
		if (runtimeSnapshot == null) {
			runtimeTarget.reset();
		} else {
			runtimeTarget.select(runtimeSnapshot);
		}
		RouterUtils.replaceWith('/login');
	}, [pendingContext, runtimeTarget, state]);
	const handleRetry = useCallback(async () => {
		setError(null);
		setIsProcessing(true);
		try {
			const runtimeSnapshot = pendingContext?.runtimeSnapshot ?? runtimeTarget.snapshot;
			if (runtimeSnapshot == null) {
				RouterUtils.replaceWith('/login');
				return;
			}
			const {authorizationUrl} = await startSsoLogin({
				redirectTo: pendingRedirectTo(pendingContext),
				runtimeSnapshot,
			});
			await navigateToExternalURL(authorizationUrl);
		} catch {
			RouterUtils.replaceWith('/login');
		}
	}, [pendingContext, runtimeTarget]);
	const handleMfaSuccess = useCallback(
		async (payload: LoginSuccessPayload) => {
			if (mfa == null) {
				return;
			}
			await AuthenticationCommands.completeLogin(
				{...payload, runtimeSnapshot: mfa.runtimeSnapshot},
				{redirectPath: mfa.redirectTo},
			);
			AuthenticationCommands.clearMfaTicket();
			runtimeTarget.reset();
		},
		[mfa, runtimeTarget],
	);
	const handleMfaCancel = useCallback(() => {
		AuthenticationCommands.clearMfaTicket();
		setMfa(null);
		handleBackToLogin();
	}, [handleBackToLogin]);
	useAuthSingleUseRequest(`${code ?? ''}:${state ?? ''}`, (request) => {
		if (mobileCallbackUrl != null) {
			window.location.replace(mobileCallbackUrl);
			return;
		}
		const callbackContext = state == null || !isValidOAuthState(state) ? null : getSsoPendingContext(state);
		setPendingContext(callbackContext);
		const attempt = new SsoCallbackAttempt({
			callbackContext,
			callbackParameters: resolveCallbackParameters(code, state),
			callbackState: state,
			i18n,
			isCurrent: () => request.isCurrent(),
			onError: setError,
			onRequireMfa: setMfa,
			onProcessingChange: setIsProcessing,
			providerError,
			providerErrorDescription,
			runtimeTarget,
		});
		attempt.start();
	});
	if (mobileCallbackUrl != null) {
		const productName = RuntimeConfig.productName;
		return (
			<flx-auth-sso-callback-page
				className={flxElementClassName(styles.loginContainer)}
				data-flx="auth.sso-callback-page.login-container--mobile"
			>
				<h1 className={styles.title} data-flx="auth.sso-callback-page.title--mobile">
					<Trans>Completing sign-in…</Trans>
				</h1>
				<p className={styles.ssoProcessingHint} data-flx="auth.sso-callback-page.sso-processing-hint--mobile">
					<Trans comment="Shown on the web page after an SSO sign-in that started in the mobile app. productName is the app name.">
						Continue in the {productName} app.
					</Trans>
				</p>
				<flx-auth-sso-callback-page-actions
					className={flxElementClassName(styles.ssoCallbackActions)}
					data-flx="auth.sso-callback-page.sso-callback-actions--mobile"
				>
					<a
						href={mobileCallbackUrl}
						className={styles.ssoRetryButton}
						data-flx="auth.sso-callback-page.sso-open-app-button"
					>
						{i18n._(OPEN_PRODUCT_DESCRIPTOR, {productName: RuntimeConfig.productName})}
					</a>
				</flx-auth-sso-callback-page-actions>
			</flx-auth-sso-callback-page>
		);
	}
	if (mfa != null) {
		return (
			<MfaScreen
				challenge={mfa.challenge}
				onSuccess={handleMfaSuccess}
				onCancel={handleMfaCancel}
				data-flx="auth.sso-callback-page.mfa-screen"
			/>
		);
	}
	if (error != null) {
		return (
			<flx-auth-sso-callback-page
				className={flxElementClassName(styles.loginContainer)}
				data-flx="auth.sso-callback-page.login-container"
			>
				<h1 className={styles.title} data-flx="auth.sso-callback-page.title">
					{error.awaitingApproval ? (
						<Trans comment="Heading on the SSO callback page when the new account is waiting for an admin to approve it.">
							Waiting for approval
						</Trans>
					) : (
						<Trans>SSO sign-in failed</Trans>
					)}
				</h1>
				<flx-auth-sso-callback-page-notice
					aria-live="assertive"
					className={flxElementClassName(styles.loginNotice)}
					data-flx="auth.sso-callback-page.login-notice"
				>
					{error.message}
				</flx-auth-sso-callback-page-notice>
				<flx-auth-sso-callback-page-actions
					className={flxElementClassName(styles.ssoCallbackActions)}
					data-flx="auth.sso-callback-page.sso-callback-actions"
				>
					{error.awaitingApproval ? null : (
						<button
							type="button"
							onClick={handleRetry}
							className={styles.ssoRetryButton}
							data-flx="auth.sso-callback-page.sso-retry-button"
						>
							{i18n._(TRY_AGAIN_DESCRIPTOR)}
						</button>
					)}
					<button
						type="button"
						onClick={handleBackToLogin}
						className={styles.ssoBackButton}
						data-flx="auth.sso-callback-page.sso-back-button.back-to-login"
					>
						{i18n._(BACK_TO_SIGN_IN_DESCRIPTOR)}
					</button>
				</flx-auth-sso-callback-page-actions>
			</flx-auth-sso-callback-page>
		);
	}
	return (
		<flx-auth-sso-callback-page
			className={flxElementClassName(styles.loginContainer)}
			data-flx="auth.sso-callback-page.login-container--2"
		>
			<h1 className={styles.title} data-flx="auth.sso-callback-page.title--2">
				<Trans>Completing sign-in…</Trans>
			</h1>
			{isProcessing && (
				<p className={styles.ssoProcessingHint} role="status" data-flx="auth.sso-callback-page.sso-processing-hint">
					<Spinner data-flx="auth.sso-callback-page.spinner" />
				</p>
			)}
		</flx-auth-sso-callback-page>
	);
});

export default SsoCallbackPage;
