// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import styles from '@app/features/auth/components/pages/LoginPage.module.css';
import {startSsoLogin} from '@app/features/auth/state/AuthFlow';
import {Button} from '@app/features/ui/button/Button';
import * as FormUtils from '@app/lib/forms';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {type ReactNode, useCallback, useState} from 'react';

export const FAILED_TO_START_SSO_DESCRIPTOR = msg({
	message: 'Failed to start SSO',
	comment: 'Login flow error shown when the single sign-on redirect cannot be started.',
});
export const ORGANIZATION_SSO_PROVIDER_DESCRIPTOR = msg({
	message: "Sign in with your organization's single sign-on provider.",
	comment: 'Description shown when sign-in is restricted to the configured SSO provider.',
});
export const CONTINUE_WITH_SSO_DESCRIPTOR = msg({
	message: 'Continue with SSO',
	comment: 'Button label that starts single sign-on.',
});
export const SSO_REQUIRED_DESCRIPTOR = msg({
	message: 'SSO is required to access this workspace.',
	comment: 'Short sign-in note shown when the instance requires single sign-on.',
});
export const PREFER_SSO_DESCRIPTOR = msg({
	message: 'Prefer using SSO? Continue with {ssoDisplayName}.',
	comment: 'Optional sign-in note. ssoDisplayName is the configured provider display name.',
});

export function isRuntimeSsoEnforced(): boolean {
	const ssoConfig = RuntimeConfig.sso;
	return Boolean(ssoConfig?.enabled && ssoConfig.enforced);
}

interface AuthSsoPanelProps {
	redirectPath?: string;
	extraTopContent?: ReactNode;
	showTitle?: boolean;
	dataFlx?: string;
}

export const AuthSsoPanel = observer(function AuthSsoPanel({
	redirectPath,
	extraTopContent,
	showTitle = true,
	dataFlx = 'auth.flow.auth-sso-panel',
}: AuthSsoPanelProps) {
	const {i18n} = useLingui();
	const ssoConfig = RuntimeConfig.sso;
	const ssoDisplayName = ssoConfig?.display_name ?? 'Single Sign-On';
	const [error, setError] = useState<string | null>(null);
	const [isStartingSso, setIsStartingSso] = useState(false);
	const handleStartSso = useCallback(async () => {
		if (!ssoConfig?.enabled) return;
		try {
			setError(null);
			setIsStartingSso(true);
			const {authorizationUrl} = await startSsoLogin({redirectTo: redirectPath});
			window.location.assign(authorizationUrl);
		} catch (err) {
			setError(
				err && typeof err === 'object' && 'body' in err
					? FormUtils.extractErrorMessage(i18n, err)
					: i18n._(FAILED_TO_START_SSO_DESCRIPTOR),
			);
		} finally {
			setIsStartingSso(false);
		}
	}, [ssoConfig?.enabled, redirectPath, i18n]);
	return (
		<div className={styles.ssoPane} data-flx={dataFlx}>
			{extraTopContent}
			{showTitle ? (
				<h1 className={styles.title} data-flx={`${dataFlx}.title`}>
					{ssoDisplayName}
				</h1>
			) : null}
			<p className={styles.ssoSubtitle} data-flx={`${dataFlx}.subtitle`}>
				{i18n._(ORGANIZATION_SSO_PROVIDER_DESCRIPTOR)}
			</p>
			<Button
				fitContainer
				onClick={handleStartSso}
				submitting={isStartingSso}
				type="button"
				disabled={!ssoConfig?.enabled}
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
