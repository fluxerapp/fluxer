// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import {AuthLoginLayout} from '@app/features/auth/flow/AuthLoginLayout';
import {AuthRouterLink} from '@app/features/auth/flow/AuthRouterLink';
import {
	isApprovalFlowMode,
	isHandoffRequest,
	useDesktopHandoffFlow,
} from '@app/features/auth/flow/auth_login_core/useDesktopHandoffFlow';
import {ConnectedHandoffApprovalFlow} from '@app/features/auth/flow/HandoffApprovalFlow';
import MfaScreen from '@app/features/auth/flow/MfaScreen';
import AccountManager from '@app/features/auth/state/AccountManager';
import Authentication from '@app/features/auth/state/Authentication';
import type {LoginSuccessPayload} from '@app/features/auth/state/AuthFlow';
import {safeRedirectTarget, safeRedirectTargetOrFallback} from '@app/features/auth/utils/SafeRedirect';
import {REGISTER_DESCRIPTOR, SIGN_IN_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {useLocation} from '@app/features/platform/components/router/RouterReact';
import {SteppedCarousel} from '@app/features/ui/stepped_carousel/SteppedCarousel';
import {useFluxerDocumentTitle} from '@app/features/window/hooks/useFluxerDocumentTitle';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useMemo} from 'react';

const LOGIN_PAGE_STEP_ORDER = ['default', 'mfa'] as const;
const LoginPage = observer(function LoginPage() {
	const {i18n} = useLingui();
	const location = useLocation();
	const params = useMemo(() => new URLSearchParams(location.search), [location.search]);
	const rawRedirect = params['get']('redirect_to');
	const safeRedirect = safeRedirectTarget(rawRedirect);
	const isHandoff = isHandoffRequest(params);
	const initialIdentifier = params['get'](RuntimeConfig.usesUsernameSignIn ? 'login' : 'email') ?? undefined;
	const registerSearch = safeRedirect ? {redirect_to: safeRedirect} : undefined;
	const redirectPath = isHandoff ? undefined : (safeRedirect ?? '/');
	return (
		<AuthLoginLayout
			redirectPath={redirectPath}
			desktopHandoff={isHandoff}
			excludeCurrentUser={false}
			initialIdentifier={initialIdentifier}
			registerLink={
				<AuthRouterLink to="/register" search={registerSearch} data-flx="auth.login-page.auth-router-link">
					{i18n._(REGISTER_DESCRIPTOR)}
				</AuthRouterLink>
			}
			data-flx="auth.login-page.auth-login-layout"
		/>
	);
});
const LoginPageMFA = observer(function LoginPageMFA() {
	const location = useLocation();
	const params = useMemo(() => new URLSearchParams(location.search), [location.search]);
	const isHandoff = isHandoffRequest(params);
	const rawRedirect = params['get']('redirect_to');
	const redirectTo = isHandoff ? undefined : safeRedirectTargetOrFallback(rawRedirect, '/');
	const mfaTicket = Authentication.currentMfaTicket ?? Authentication.mfaTicket;
	const mfaMethods = Authentication.availableMfaMethods ?? Authentication.mfaMethods;
	const hasStoredAccounts = AccountManager.orderedAccounts.length > 0;
	const handoff = useDesktopHandoffFlow({
		enabled: isHandoff,
		hasStoredAccounts,
		initialMode: 'idle',
	});
	const handleMfaSuccess = useCallback(
		async (payload: LoginSuccessPayload) => {
			if (isHandoff) {
				await AccountManager.refreshStoredAccount(payload.userId, payload.token, payload.userData);
				await handoff.start(payload);
				return;
			} else {
				await AuthenticationCommands.completeLogin(payload);
				AuthenticationCommands.clearMfaTicket();
				RouterUtils.replaceWith(redirectTo || '/');
				return;
			}
		},
		[handoff, isHandoff, redirectTo],
	);
	const handleCancel = useCallback(() => {
		AuthenticationCommands.clearMfaTicket();
	}, []);
	if (!mfaTicket || !mfaMethods) {
		return null;
	}
	if (isHandoff && isApprovalFlowMode(handoff.mode)) {
		return (
			<ConnectedHandoffApprovalFlow
				handoff={handoff}
				data-flx="auth.login-page.login-page-mfa.connected-handoff-approval-flow"
			/>
		);
	}
	return (
		<MfaScreen
			challenge={{ticket: mfaTicket, ...mfaMethods}}
			onSuccess={handleMfaSuccess}
			onCancel={handleCancel}
			data-flx="auth.login-page.login-page-mfa.mfa-screen"
		/>
	);
});
const LoginPageContainer = observer(() => {
	const {i18n} = useLingui();
	const loginState = Authentication.loginState;
	useFluxerDocumentTitle(i18n._(SIGN_IN_DESCRIPTOR));
	return (
		<SteppedCarousel
			step={loginState}
			steps={LOGIN_PAGE_STEP_ORDER}
			focusOnStepChange
			ariaLabel={i18n._(SIGN_IN_DESCRIPTOR)}
			data-flx="auth.login-page.container-carousel"
		>
			{loginState === 'mfa' ? (
				<LoginPageMFA data-flx="auth.login-page.login-page-container.login-page-mfa" />
			) : (
				<LoginPage data-flx="auth.login-page.login-page-container.login-page" />
			)}
		</SteppedCarousel>
	);
});

export default LoginPageContainer;
