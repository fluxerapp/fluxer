// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {AuthFloatingBackButton} from '@app/features/auth/flow/AuthFloatingBackButton';
import {AuthLoginLayout} from '@app/features/auth/flow/AuthLoginLayout';
import {AuthRouterLink} from '@app/features/auth/flow/AuthRouterLink';
import {AUTH_LOGIN_METHOD_QUERY_PARAM} from '@app/features/auth/flow/auth_login_core/AuthLoginStepTypes';
import {isHandoffRequest} from '@app/features/auth/flow/auth_login_core/useDesktopHandoffFlow';
import {DesktopHandoffMfaStep} from '@app/features/auth/flow/DesktopHandoffMfaStep';
import Authentication, {LoginState} from '@app/features/auth/state/Authentication';
import {useAuthRuntimeTarget} from '@app/features/auth/state/AuthRuntimeTarget';
import {safeRedirectTarget} from '@app/features/auth/utils/SafeRedirect';
import {
	GO_BACK_DESCRIPTOR,
	REGISTER_DESCRIPTOR,
	SIGN_IN_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {useLocation} from '@app/features/platform/components/router/RouterReact';
import {SteppedCarousel} from '@app/features/ui/stepped_carousel/SteppedCarousel';
import {useFluxerDocumentTitle} from '@app/features/window/hooks/useFluxerDocumentTitle';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useMemo, useState} from 'react';

const LOGIN_PAGE_STEP_ORDER: ReadonlyArray<LoginState> = [LoginState.DEFAULT, LoginState.MFA];

function resolveLoginRedirectPath(isHandoff: boolean, safeRedirect: string | null): string | null {
	if (isHandoff) {
		return null;
	}
	return safeRedirect ?? Routes.ME;
}

function resolveRegisterSearch(safeRedirect: string | null): Record<string, string> {
	const search: Record<string, string> = {
		[AUTH_LOGIN_METHOD_QUERY_PARAM.name]: AUTH_LOGIN_METHOD_QUERY_PARAM.value,
	};
	if (safeRedirect != null && safeRedirect !== '') {
		search.redirect_to = safeRedirect;
	}
	return search;
}

interface LoginPageProps {
	readonly onBackActionChange: (action: (() => void) | null) => void;
}

const LoginPage = observer(function LoginPage({onBackActionChange}: LoginPageProps) {
	const {i18n} = useLingui();
	const runtimeTarget = useAuthRuntimeTarget();
	const location = useLocation();
	const params = useMemo(() => new URLSearchParams(location.search), [location.search]);
	const safeRedirect = safeRedirectTarget(params.get('redirect_to'));
	const isHandoff = isHandoffRequest(params);
	return (
		<AuthLoginLayout
			redirectPath={resolveLoginRedirectPath(isHandoff, safeRedirect)}
			inviteCode={null}
			desktopHandoff={isHandoff}
			excludeCurrentUser={false}
			extraTopContent={null}
			forgotPasswordAction={null}
			showTitle={true}
			title={null}
			onLoginComplete={runtimeTarget.reset}
			onBackActionChange={onBackActionChange}
			completeLoginRedirectPath={null}
			forceCredentials={false}
			startWithAddAccount={false}
			runtimeTarget={runtimeTarget}
			showInstanceSelector={null}
			ssoRedirectPath={null}
			suppressInlineBackButtons={true}
			initialIdentifier={params.get('login') ?? params.get('email')}
			registerLink={
				<AuthRouterLink
					to="/register"
					search={resolveRegisterSearch(safeRedirect)}
					data-flx="auth.login-page.auth-router-link"
				>
					{i18n._(REGISTER_DESCRIPTOR)}
				</AuthRouterLink>
			}
			data-flx="auth.login-page.auth-login-layout"
		/>
	);
});
const LoginPageContainer = observer(() => {
	const {i18n} = useLingui();
	const runtimeTarget = useAuthRuntimeTarget();
	const loginState = Authentication.loginState;
	const [backAction, setBackAction] = useState<(() => void) | null>(null);
	const handleBackActionChange = useCallback((action: (() => void) | null) => {
		setBackAction(() => action);
	}, []);
	useFluxerDocumentTitle(i18n._(SIGN_IN_DESCRIPTOR));
	return (
		<>
			{backAction == null ? null : (
				<AuthFloatingBackButton
					ariaLabel={i18n._(GO_BACK_DESCRIPTOR)}
					onBack={backAction}
					data-flx="auth.login-page.login-page-container.auth-floating-back-button"
				/>
			)}
			<SteppedCarousel
				step={loginState}
				steps={LOGIN_PAGE_STEP_ORDER}
				focusOnStepChange
				ariaLabel={i18n._(SIGN_IN_DESCRIPTOR)}
				data-flx="auth.login-page.container-carousel"
			>
				{loginState === LoginState.MFA ? (
					<DesktopHandoffMfaStep
						fallbackRedirectPath={Routes.ME}
						onLoginComplete={runtimeTarget.reset}
						data-flx="auth.login-page.login-page-container.desktop-handoff-mfa-step"
					/>
				) : (
					<LoginPage
						onBackActionChange={handleBackActionChange}
						data-flx="auth.login-page.login-page-container.login-page"
					/>
				)}
			</SteppedCarousel>
		</>
	);
});

export default LoginPageContainer;
