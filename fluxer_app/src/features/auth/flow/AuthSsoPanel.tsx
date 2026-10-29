// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import RuntimeConfig, {type InstanceSsoConfig} from '@app/features/app/state/RuntimeConfig';
import {SIGN_IN_WITH_BROWSER_DESCRIPTOR} from '@app/features/auth/AuthMessageDescriptors';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import styles from '@app/features/auth/components/pages/LoginPage.module.css';
import {AuthLoginBrowserStep} from '@app/features/auth/flow/auth_login_core/AuthLoginBrowserStep';
import {markSsoAutoStartSpent} from '@app/features/auth/flow/SsoAutoStart';
import {type StepTitleChangeHandler, useReportStepTitle} from '@app/features/auth/flow/useReportStepTitle';
import {getAuthErrorMessage} from '@app/features/auth/hooks/useAuthForm';
import {type LoginSuccessPayload, startSsoLogin} from '@app/features/auth/state/AuthFlow';
import {isolateBidi} from '@app/features/i18n/utils/BidiIsolate';
import {Button} from '@app/features/ui/button/Button';
import {isDesktop, navigateToExternalURL} from '@app/features/ui/utils/NativeUtils';
import type {I18n, MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {type ReactNode, useCallback, useState} from 'react';

const ORGANIZATION_SSO_PROVIDER_DESCRIPTOR = msg({
	message: "Sign in with your organization's single sign-on provider.",
	comment: 'Description shown when sign-in is restricted to the configured SSO provider.',
});
const CONTINUE_WITH_SSO_DESCRIPTOR = msg({
	message: 'Continue with SSO',
	comment: 'Button label that starts single sign-on.',
});
const CONTINUE_WITH_PROVIDER_DESCRIPTOR = msg({
	message: 'Continue with {providerName}',
	comment:
		"Button label that starts single sign-on. providerName is the name of the organisation's sign-in provider, shown as typed by the admin, never translated.",
});
const SINGLE_SIGN_ON_TITLE_DESCRIPTOR = msg({
	message: 'Single sign-on',
	comment: 'Heading of the single sign-on panel when the admin has not given the sign-in provider a name.',
});
const SSO_PROVIDER_UNAVAILABLE_DESCRIPTOR = msg({
	message: 'Your sign-in provider is unavailable. Try again later.',
	comment:
		'Explanation shown in place of the single sign-on description when the instance requires single sign-on but cannot connect to its sign-in provider right now.',
});
const SSO_NOT_CONFIGURED_DESCRIPTOR = msg({
	message: 'This instance has not finished setting up single sign-on. Ask an administrator to enable it.',
	comment: 'Explanation shown in place of the single sign-on description when the instance offers no SSO provider.',
});

interface SsoButtonLabelProps {
	readonly ssoConfig: InstanceSsoConfig | null;
	readonly 'data-flx': string;
}

function resolveSsoButtonLabel(ssoConfig: InstanceSsoConfig | null, i18n: I18n): string | null {
	if (ssoConfig?.button_label != null) {
		return ssoConfig.button_label;
	}
	if (ssoConfig?.display_name != null) {
		return i18n._(CONTINUE_WITH_PROVIDER_DESCRIPTOR, {providerName: isolateBidi(ssoConfig.display_name)});
	}
	return null;
}

export function SsoButtonLabel({ssoConfig, 'data-flx': dataFlx}: SsoButtonLabelProps) {
	const {i18n} = useLingui();
	const label = resolveSsoButtonLabel(ssoConfig, i18n);
	return (
		<bdi className={styles.ssoButtonLabel} title={label ?? undefined} data-flx={dataFlx}>
			{label ?? i18n._(CONTINUE_WITH_SSO_DESCRIPTOR)}
		</bdi>
	);
}

export function isInstanceSsoAvailable(ssoConfig: InstanceSsoConfig | null): boolean {
	return ssoConfig?.enabled === true;
}

function resolveSsoSubtitle(ssoConfig: InstanceSsoConfig | null): MessageDescriptor {
	if (isInstanceSsoAvailable(ssoConfig)) return ORGANIZATION_SSO_PROVIDER_DESCRIPTOR;
	return isSsoLoginEnforced(ssoConfig) ? SSO_PROVIDER_UNAVAILABLE_DESCRIPTOR : SSO_NOT_CONFIGURED_DESCRIPTOR;
}

export function resolveAuthPanelSso(runtimeSnapshot: RuntimeConfigSnapshot): InstanceSsoConfig | null {
	return runtimeSnapshot.sso ?? null;
}

interface AuthSsoPanelProps {
	redirectPath?: string;
	extraTopContent?: ReactNode;
	runtimeSnapshot?: RuntimeConfigSnapshot;
	showTitle?: boolean;
	dataFlx?: string;
	onStart?: () => void;
	onBrowserLoginSuccess?: (payload: LoginSuccessPayload) => Promise<void> | void;
	onStepTitleChange?: StepTitleChangeHandler | null;
}

export const AuthSsoPanel = observer(function AuthSsoPanel({
	redirectPath,
	extraTopContent,
	runtimeSnapshot,
	showTitle: showTitleProp = true,
	dataFlx = 'auth.flow.auth-sso-panel',
	onStart,
	onBrowserLoginSuccess,
	onStepTitleChange = null,
}: AuthSsoPanelProps) {
	const {i18n} = useLingui();
	const panelSnapshot = runtimeSnapshot ?? RuntimeConfig.getSnapshot();
	const ssoConfig = resolveAuthPanelSso(panelSnapshot);
	const isAvailable = isInstanceSsoAvailable(ssoConfig);
	const ssoDisplayName = ssoConfig?.display_name ?? i18n._(SINGLE_SIGN_ON_TITLE_DESCRIPTOR);
	const [error, setError] = useState<string | null>(null);
	const [isStartingSso, setIsStartingSso] = useState(false);
	const [showBrowserHandoff, setShowBrowserHandoff] = useState(false);
	const showTitle = showTitleProp && onStepTitleChange == null;
	useReportStepTitle(onStepTitleChange, showBrowserHandoff ? i18n._(SIGN_IN_WITH_BROWSER_DESCRIPTOR) : ssoDisplayName);
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
			markSsoAutoStartSpent();
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
				startSso
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
				{i18n._(resolveSsoSubtitle(ssoConfig))}
			</p>
			<Button
				fitContainer
				onClick={handleStartSso}
				submitting={isStartingSso}
				type="button"
				disabled={!isAvailable || isStartingSso}
				data-flx={`${dataFlx}.button.start-sso`}
			>
				<SsoButtonLabel ssoConfig={ssoConfig} data-flx={`${dataFlx}.sso-button-label`} />
			</Button>
			{error && (
				<div className={styles.loginNotice} role="alert" data-flx={`${dataFlx}.login-notice`}>
					{error}
				</div>
			)}
		</div>
	);
});

export function isSsoLoginEnforced(ssoConfig: InstanceSsoConfig | null): boolean {
	return ssoConfig?.enforced === true;
}
