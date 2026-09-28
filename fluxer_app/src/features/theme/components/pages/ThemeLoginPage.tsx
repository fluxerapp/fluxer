// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import {AuthErrorState} from '@app/features/auth/flow/AuthErrorState';
import {AuthLoadingState} from '@app/features/auth/flow/AuthLoadingState';
import {AuthLoginLayout} from '@app/features/auth/flow/AuthLoginLayout';
import {AuthPageHeader} from '@app/features/auth/flow/AuthPageHeader';
import sharedStyles from '@app/features/auth/flow/AuthPageStyles.module.css';
import {AuthRouterLink} from '@app/features/auth/flow/AuthRouterLink';
import {
	isApprovalFlowMode,
	isHandoffRequest,
	useDesktopHandoffFlow,
} from '@app/features/auth/flow/auth_login_core/useDesktopHandoffFlow';
import {DesktopDeepLinkPrompt} from '@app/features/auth/flow/DesktopDeepLinkPrompt';
import {ConnectedHandoffApprovalFlow} from '@app/features/auth/flow/HandoffApprovalFlow';
import MfaScreen from '@app/features/auth/flow/MfaScreen';
import AccountManager from '@app/features/auth/state/AccountManager';
import Authentication from '@app/features/auth/state/Authentication';
import type {LoginSuccessPayload} from '@app/features/auth/state/AuthFlow';
import {safeRedirectTarget, safeRedirectTargetOrFallback} from '@app/features/auth/utils/SafeRedirect';
import {REGISTER_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {setPathQueryParams} from '@app/features/messaging/utils/MessagingUrlUtils';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {useLocation, useParams} from '@app/features/platform/components/router/RouterReact';
import * as ThemeCommands from '@app/features/theme/commands/ThemeCommands';
import {useThemeExists} from '@app/features/theme/hooks/useThemeExists';
import {SteppedCarousel} from '@app/features/ui/stepped_carousel/SteppedCarousel';
import {useFluxerDocumentTitle} from '@app/features/window/hooks/useFluxerDocumentTitle';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {PaletteIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback, useMemo} from 'react';

const YOU_VE_GOT_CSS_DESCRIPTOR = msg({
	message: "You've got CSS!",
	comment: 'Short label in the theme login page. Keep it concise. Keep the tone plain and specific.',
});
const SHARED_THEME_DESCRIPTOR = msg({
	message: 'Shared theme',
	comment: 'Button or menu action label in the theme login page. Keep it concise. Keep the tone plain and specific.',
});
const APPLY_THEME_DESCRIPTOR = msg({
	message: 'Apply theme',
	comment: 'Button or menu action label in the theme login page. Keep it concise. Keep the tone plain and specific.',
});
const THEME_LOGIN_PAGE_STEP_ORDER = ['default', 'mfa'] as const;
const ThemeLoginPage = observer(function ThemeLoginPage() {
	const {i18n} = useLingui();
	const {themeId} = useParams() as {themeId: string};
	const location = useLocation();
	const params = useMemo(() => new URLSearchParams(location.search), [location.search]);
	const rawRedirect = params['get']('redirect_to');
	const safeRedirect = safeRedirectTarget(rawRedirect);
	const isHandoff = isHandoffRequest(params);
	const registerSearch = safeRedirect ? {redirect_to: safeRedirect} : undefined;
	const redirectPath = useMemo(() => {
		if (!safeRedirect) {
			return Routes.theme(themeId);
		}
		return setPathQueryParams(Routes.theme(themeId), {redirect_to: safeRedirect});
	}, [themeId, safeRedirect]);
	const handleLoginComplete = useCallback(() => {
		if (!themeId) return;
		ThemeCommands.openAcceptModal(themeId, i18n);
	}, [themeId, i18n]);
	return (
		<AuthLoginLayout
			redirectPath={redirectPath}
			desktopHandoff={isHandoff}
			extraTopContent={
				<>
					<DesktopDeepLinkPrompt
						code={themeId}
						kind="theme"
						data-flx="theme.theme-login-page.desktop-deep-link-prompt"
					/>
					<AuthPageHeader
						icon={
							<div className={sharedStyles.themeIconSpot} data-flx="theme.theme-login-page.div">
								<PaletteIcon
									className={sharedStyles.themeIcon}
									weight="fill"
									data-flx="theme.theme-login-page.palette-icon"
								/>
							</div>
						}
						title={i18n._(YOU_VE_GOT_CSS_DESCRIPTOR)}
						subtitle={i18n._(SHARED_THEME_DESCRIPTOR)}
						data-flx="theme.theme-login-page.auth-page-header"
					/>
				</>
			}
			showTitle={false}
			registerLink={
				<AuthRouterLink
					to={Routes.themeRegister(themeId)}
					search={registerSearch}
					data-flx="theme.theme-login-page.auth-router-link"
				>
					{i18n._(REGISTER_DESCRIPTOR)}
				</AuthRouterLink>
			}
			onLoginComplete={handleLoginComplete}
			data-flx="theme.theme-login-page.auth-login-layout"
		/>
	);
});
const ThemeLoginPageMFA = observer(function ThemeLoginPageMFA() {
	const {i18n} = useLingui();
	const {themeId} = useParams() as {themeId: string};
	const location = useLocation();
	const params = useMemo(() => new URLSearchParams(location.search), [location.search]);
	const isHandoff = isHandoffRequest(params);
	const rawRedirect = params['get']('redirect_to');
	const redirectTo = isHandoff ? undefined : safeRedirectTargetOrFallback(rawRedirect, Routes.theme(themeId));
	const mfaTicket = Authentication.currentMfaTicket;
	const mfaMethods = Authentication.availableMfaMethods;
	const hasStoredAccounts = AccountManager.orderedAccounts.length > 0;
	const handoff = useDesktopHandoffFlow({
		enabled: isHandoff,
		hasStoredAccounts,
		initialMode: 'idle',
	});
	const handleMfaSuccess = useCallback(
		async ({token, userId}: LoginSuccessPayload) => {
			if (isHandoff) {
				await AccountManager.refreshStoredAccount(userId, token);
				await handoff.start({token, userId});
				return;
			}
			await AuthenticationCommands.completeLogin({token, userId});
			ThemeCommands.openAcceptModal(themeId, i18n);
			AuthenticationCommands.clearMfaTicket();
			RouterUtils.replaceWith(redirectTo || '/');
		},
		[handoff, isHandoff, redirectTo, themeId, i18n],
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
				data-flx="theme.theme-login-page.theme-login-page-mfa.connected-handoff-approval-flow"
			/>
		);
	}
	return (
		<MfaScreen
			challenge={{ticket: mfaTicket, ...mfaMethods}}
			onSuccess={handleMfaSuccess}
			onCancel={handleCancel}
			data-flx="theme.theme-login-page.theme-login-page-mfa.mfa-screen"
		/>
	);
});
const ThemeLoginPageContainer = observer(() => {
	const {i18n} = useLingui();
	const loginState = Authentication.loginState;
	const {themeId} = useParams() as {themeId: string};
	useFluxerDocumentTitle(i18n._(APPLY_THEME_DESCRIPTOR));
	const themeStatus = useThemeExists(themeId);
	if (themeStatus === 'loading') {
		return <AuthLoadingState data-flx="theme.theme-login-page.theme-login-page-container.auth-loading-state" />;
	}
	if (themeStatus === 'error') {
		return (
			<AuthErrorState
				title={<Trans>Theme not found</Trans>}
				text={<Trans>This theme may have been removed or the link is invalid.</Trans>}
				data-flx="theme.theme-login-page.theme-login-page-container.auth-error-state"
			/>
		);
	}
	switch (loginState) {
		case 'default':
			return (
				<SteppedCarousel
					step={loginState}
					steps={THEME_LOGIN_PAGE_STEP_ORDER}
					focusOnStepChange
					ariaLabel={i18n._(APPLY_THEME_DESCRIPTOR)}
					data-flx="theme.theme-login-page.container-carousel"
				>
					<ThemeLoginPage data-flx="theme.theme-login-page.theme-login-page-container.theme-login-page" />
				</SteppedCarousel>
			);
		case 'mfa':
			return (
				<SteppedCarousel
					step={loginState}
					steps={THEME_LOGIN_PAGE_STEP_ORDER}
					focusOnStepChange
					ariaLabel={i18n._(APPLY_THEME_DESCRIPTOR)}
					data-flx="theme.theme-login-page.container-carousel"
				>
					<ThemeLoginPageMFA data-flx="theme.theme-login-page.theme-login-page-container.theme-login-page-mfa" />
				</SteppedCarousel>
			);
		default:
			return null;
	}
});

export default ThemeLoginPageContainer;
