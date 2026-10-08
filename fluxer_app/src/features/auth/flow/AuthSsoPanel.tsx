// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import RuntimeConfig, {type InstanceSsoConfig} from '@app/features/app/state/RuntimeConfig';
import {SIGN_IN_WITH_BROWSER_DESCRIPTOR} from '@app/features/auth/AuthMessageDescriptors';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import styles from '@app/features/auth/components/pages/LoginPage.module.css';
import {AuthLoginBrowserStep} from '@app/features/auth/flow/auth_login_core/AuthLoginBrowserStep';
import {getAuthErrorMessage} from '@app/features/auth/hooks/useAuthForm';
import {type LoginSuccessPayload, startSsoLogin} from '@app/features/auth/state/AuthFlow';
import {Button} from '@app/features/ui/button/Button';
import {isDesktop, navigateToExternalURL} from '@app/features/ui/utils/NativeUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {type ReactNode, useCallback, useState} from 'react';

export const ORGANIZATION_SSO_PROVIDER_DESCRIPTOR = msg({
	message: "Sign in with your organization's single sign-on provider.",
	comment: 'Description shown when sign-in is restricted to the configured SSO provider.',
});
export const CONTINUE_WITH_SSO_DESCRIPTOR = msg({
	message: 'Continue with SSO',
	comment: 'Button label that starts single sign-on.',
});
const SSO_NOT_CONFIGURED_DESCRIPTOR = msg({
	message: 'This instance has not finished setting up single sign-on. Ask an administrator to enable it.',
	comment: 'Explanation shown in place of the single sign-on description when the instance offers no SSO provider.',
});

export function isInstanceSsoAvailable(ssoConfig: InstanceSsoConfig | null): boolean {
	return ssoConfig?.enabled === true;
}

export function resolveAuthPanelSso(runtimeSnapshot: RuntimeConfigSnapshot): InstanceSsoConfig | null {
	return runtimeSnapshot.sso ?? null;
}

export function isRuntimeSsoEnforced(): boolean {
	const ssoConfig = RuntimeConfig.sso;
	return isInstanceSsoAvailable(ssoConfig) && ssoConfig?.enforced === true;
}

interface AuthSsoPanelProps {
	redirectPath?: string;
	extraTopContent?: ReactNode;
	runtimeSnapshot?: RuntimeConfigSnapshot;
	showTitle?: boolean;
	dataFlx?: string;
	onStart?: () => void;
	onBrowserLoginSuccess?: (payload: LoginSuccessPayload) => Promise<void> | void;
}

export const AuthSsoPanel = observer(function AuthSsoPanel({
	redirectPath,
	extraTopContent,
	runtimeSnapshot,
	showTitle = true,
	dataFlx = 'auth.flow.auth-sso-panel',
	onStart,
	onBrowserLoginSuccess,
}: AuthSsoPanelProps) {
	const {i18n} = useLingui();
	const panelSnapshot = runtimeSnapshot ?? RuntimeConfig.getSnapshot();
	const ssoConfig = resolveAuthPanelSso(panelSnapshot);
	const isAvailable = isInstanceSsoAvailable(ssoConfig);
	const ssoDisplayName = ssoConfig?.display_name ?? 'Single Sign-On';
	const [error, setError] = useState<string | null>(null);
	const [isStartingSso, setIsStartingSso] = useState(false);
	const [showBrowserHandoff, setShowBrowserHandoff] = useState(false);
	const handleStartSso = useCallback(async () => {
		if (!isAvailable) return;
		if (onStart != null) {
			onStart();
			return;
		}
		if (isDesktop()) {
			setError(null);
			setShowBrowserHandoff(true);
			return;
		}
		try {
			setError(null);
			setIsStartingSso(true);
			const {authorizationUrl} = await startSsoLogin({
				redirectTo: redirectPath,
				runtimeSnapshot: panelSnapshot,
			});
			await navigateToExternalURL(authorizationUrl);
		} catch (err) {
			setError(getAuthErrorMessage(err, i18n));
		} finally {
			setIsStartingSso(false);
		}
	}, [isAvailable, onStart, redirectPath, panelSnapshot, i18n]);
	const handleBackFromBrowserHandoff = useCallback(() => {
		setShowBrowserHandoff(false);
	}, []);
	const handleBrowserLoginSuccess = useCallback(
		async (payload: LoginSuccessPayload) => {
			if (onBrowserLoginSuccess != null) {
				await onBrowserLoginSuccess(payload);
				return;
			}
			await AuthenticationCommands.completeLogin({...payload, runtimeSnapshot: panelSnapshot}, {redirectPath});
		},
		[onBrowserLoginSuccess, panelSnapshot, redirectPath],
	);
	if (showBrowserHandoff) {
		return (
			<AuthLoginBrowserStep
				extraTopContent={extraTopContent}
				showTitle={showTitle}
				title={i18n._(SIGN_IN_WITH_BROWSER_DESCRIPTOR)}
				runtimeSnapshot={panelSnapshot}
				onBack={handleBackFromBrowserHandoff}
				onChangeInstance={null}
				onSuccess={handleBrowserLoginSuccess}
				data-flx={`${dataFlx}.browser-step`}
			/>
		);
	}
	return (
		<div className={styles.ssoPane} data-flx={dataFlx}>
			{extraTopContent}
			{showTitle ? (
				<h1 className={styles.title} data-flx={`${dataFlx}.title`}>
					{ssoDisplayName}
				</h1>
			) : null}
			<p className={styles.ssoSubtitle} data-flx={`${dataFlx}.subtitle`}>
				{isAvailable ? i18n._(ORGANIZATION_SSO_PROVIDER_DESCRIPTOR) : i18n._(SSO_NOT_CONFIGURED_DESCRIPTOR)}
			</p>
			<Button
				fitContainer
				onClick={handleStartSso}
				submitting={isStartingSso}
				type="button"
				disabled={!isAvailable || isStartingSso}
				data-flx={`${dataFlx}.button.start-sso`}
			>
				{i18n._(CONTINUE_WITH_SSO_DESCRIPTOR)}
			</Button>
			{error && (
				<div className={styles.loginNotice} role="alert" data-flx={`${dataFlx}.login-notice`}>
					{error}
				</div>
			)}
		</div>
	);
});
