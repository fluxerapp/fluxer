// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {readHashParam} from '@app/features/app/hooks/useHashParam';
import {runtimeInstanceKey} from '@app/features/app/state/InstanceSnapshotStore';
import {SignInLinkPage} from '@app/features/auth/components/pages/SignInLinkPage';
import {AuthFloatingBackButton} from '@app/features/auth/flow/AuthFloatingBackButton';
import {AuthLoginLayout} from '@app/features/auth/flow/AuthLoginLayout';
import {AuthRouterLink} from '@app/features/auth/flow/AuthRouterLink';
import {AUTH_LOGIN_METHOD_QUERY_PARAM} from '@app/features/auth/flow/auth_login_core/AuthLoginStepTypes';
import {isHandoffRequest} from '@app/features/auth/flow/auth_login_core/useDesktopHandoffFlow';
import {DesktopHandoffMfaStep} from '@app/features/auth/flow/DesktopHandoffMfaStep';
import Authentication, {LoginState} from '@app/features/auth/state/Authentication';
import {type AuthRuntimeTarget, useAuthRuntimeTarget} from '@app/features/auth/state/AuthRuntimeTarget';
import {safeRedirectTarget} from '@app/features/auth/utils/SafeRedirect';
import {
	GO_BACK_DESCRIPTOR,
	REGISTER_DESCRIPTOR,
	SIGN_IN_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {useLocation} from '@app/features/platform/components/router/RouterReact';
import {hasPurchaseIntent} from '@app/features/premium/state/PurchaseIntent';
import {getPremiumProductName} from '@app/features/premium/utils/PremiumUtils';
import {SteppedCarousel} from '@app/features/ui/stepped_carousel/SteppedCarousel';
import {useFluxerDocumentTitle} from '@app/features/window/hooks/useFluxerDocumentTitle';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useMemo, useState} from 'react';

const GET_PREMIUM_HEADING_DESCRIPTOR = msg({
	message: 'Sign in or create an account to get {premiumProductName}',
	comment:
		'Heading of the sign-in screen when someone came from a purchase button. premiumProductName is the subscription name.',
});

const SIGN_IN_LINK_HASH_PARAM = 'sign_in_link';

const LOGIN_PAGE_STEP_ORDER: ReadonlyArray<LoginState> = [LoginState.DEFAULT, LoginState.MFA];

function resolveLoginRedirectPath(
	isHandoff: boolean,
	safeRedirect: string | null,
	runtimeTarget: AuthRuntimeTarget,
): string | null {
	if (isHandoff) {
		return null;
	}
	const {initialSnapshot, snapshot} = runtimeTarget;
	const changedInstance =
		initialSnapshot != null && snapshot != null && runtimeInstanceKey(initialSnapshot) !== runtimeInstanceKey(snapshot);
	return changedInstance ? Routes.ME : (safeRedirect ?? Routes.ME);
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
			redirectPath={resolveLoginRedirectPath(isHandoff, safeRedirect, runtimeTarget)}
			inviteCode={null}
			desktopHandoff={isHandoff}
			excludeCurrentUser={false}
			extraTopContent={null}
			forgotPasswordAction={null}
			showTitle={true}
			title={
				hasPurchaseIntent()
					? i18n._(GET_PREMIUM_HEADING_DESCRIPTOR, {premiumProductName: getPremiumProductName()})
					: null
			}
			onLoginComplete={runtimeTarget.reset}
			onBackActionChange={onBackActionChange}
			completeLoginRedirectPath={Routes.ME}
			forceCredentials={false}
			startWithAddAccount={false}
			runtimeTarget={runtimeTarget}
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
	const [signInLink, setSignInLink] = useState(() => readHashParam(window.location.hash, SIGN_IN_LINK_HASH_PARAM));
	const handleBackActionChange = useCallback((action: (() => void) | null) => {
		setBackAction(() => action);
	}, []);
	const handleSignInLinkBack = useCallback(() => setSignInLink(null), []);
	useEffect(() => {
		if (signInLink != null) RouterUtils.replaceWith(Routes.LOGIN);
	}, [signInLink]);
	useFluxerDocumentTitle(i18n._(SIGN_IN_DESCRIPTOR));
	if (signInLink != null) {
		return (
			<SignInLinkPage
				token={signInLink}
				onBack={handleSignInLinkBack}
				data-flx="auth.login-page.login-page-container.sign-in-link-page"
			/>
		);
	}
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
