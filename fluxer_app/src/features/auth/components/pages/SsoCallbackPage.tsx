// SPDX-License-Identifier: AGPL-3.0-or-later

import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import styles from '@app/features/auth/components/pages/LoginPage.module.css';
import {
	clearPendingSsoRedirectTo,
	completeSsoLogin,
	getPendingSsoRedirectTo,
	startSsoLogin,
} from '@app/features/auth/state/AuthFlow';
import {LOCAL_SSO_LOGIN_PATH} from '@app/features/auth/utils/AutoSsoRedirect';
import {safeRedirectTarget} from '@app/features/auth/utils/SafeRedirect';
import {BACK_TO_SIGN_IN_DESCRIPTOR, TRY_AGAIN_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import * as FormUtils from '@app/lib/forms';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useRef, useState} from 'react';

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
const SSO_TIMEOUT_MS = 30_000;
const SsoCallbackPage = observer(function SsoCallbackPage() {
	const {i18n} = useLingui();
	const params = new URLSearchParams(window.location.search);
	const code = params['get']('code');
	const state = params['get']('state');
	const providerError = params['get']('error');
	const providerErrorDescription = params['get']('error_description');
	const [error, setError] = useState<string | null>(null);
	const [isProcessing, setIsProcessing] = useState(true);
	const abortControllerRef = useRef<AbortController | null>(null);
	const handleBackToLogin = useCallback(() => {
		RouterUtils.replaceWith(LOCAL_SSO_LOGIN_PATH);
	}, []);
	const handleRetry = useCallback(async () => {
		setError(null);
		setIsProcessing(true);
		try {
			const {authorizationUrl} = await startSsoLogin({redirectTo: getPendingSsoRedirectTo()});
			window.location.assign(authorizationUrl);
		} catch {
			RouterUtils.replaceWith(LOCAL_SSO_LOGIN_PATH);
		}
	}, []);
	useEffect(() => {
		const controller = new AbortController();
		abortControllerRef.current = controller;
		const timeoutId = setTimeout(() => {
			if (!controller.signal.aborted) {
				controller.abort();
				setError(i18n._(SSO_SIGN_IN_TIMED_OUT_PLEASE_TRY_AGAIN_DESCRIPTOR));
				setIsProcessing(false);
			}
		}, SSO_TIMEOUT_MS);
		(async () => {
			if (providerError) {
				setError(providerErrorDescription ? `${providerError}: ${providerErrorDescription}` : providerError);
				setIsProcessing(false);
				return;
			}
			if (!code || !state) {
				setError(i18n._(MISSING_SSO_CODE_OR_STATE_PLEASE_TRY_SIGNING_DESCRIPTOR));
				setIsProcessing(false);
				return;
			}
			try {
				const result = await completeSsoLogin({code, state});
				if (controller.signal.aborted) return;
				await AuthenticationCommands.completeLogin(result);
				if (controller.signal.aborted) return;
				const redirectTo =
					safeRedirectTarget(result.redirect_to) ?? safeRedirectTarget(getPendingSsoRedirectTo()) ?? '/';
				RouterUtils.replaceWith(redirectTo);
				clearPendingSsoRedirectTo();
			} catch (err) {
				if (controller.signal.aborted) return;
				const message =
					err && typeof err === 'object' && 'body' in err
						? FormUtils.extractErrorMessage(i18n, err)
						: i18n._(FAILED_TO_COMPLETE_SSO_SIGN_IN_DESCRIPTOR);
				setError(message);
				setIsProcessing(false);
			}
		})();
		return () => {
			clearTimeout(timeoutId);
			controller.abort();
		};
	}, [code, state, providerError, providerErrorDescription, i18n]);
	if (error) {
		return (
			<div className={styles.loginContainer} data-flx="auth.sso-callback-page.login-container">
				<h1 className={styles.title} data-flx="auth.sso-callback-page.title">
					<Trans>SSO sign-in failed</Trans>
				</h1>
				<div className={styles.loginNotice} data-flx="auth.sso-callback-page.login-notice">
					{error}
				</div>
				<div className={styles.ssoCallbackActions} data-flx="auth.sso-callback-page.sso-callback-actions">
					<button
						type="button"
						onClick={handleRetry}
						className={styles.ssoRetryButton}
						data-flx="auth.sso-callback-page.sso-retry-button"
					>
						{i18n._(TRY_AGAIN_DESCRIPTOR)}
					</button>
					<button
						type="button"
						onClick={handleBackToLogin}
						className={styles.ssoBackButton}
						data-flx="auth.sso-callback-page.sso-back-button.back-to-login"
					>
						{i18n._(BACK_TO_SIGN_IN_DESCRIPTOR)}
					</button>
				</div>
			</div>
		);
	}
	return (
		<div className={styles.loginContainer} data-flx="auth.sso-callback-page.login-container--2">
			<h1 className={styles.title} data-flx="auth.sso-callback-page.title--2">
				<Trans>Completing sign-in…</Trans>
			</h1>
			{isProcessing && (
				<p className={styles.ssoProcessingHint} data-flx="auth.sso-callback-page.sso-processing-hint">
					<Trans>Completing your sign-in…</Trans>
				</p>
			)}
		</div>
	);
});

export default SsoCallbackPage;
