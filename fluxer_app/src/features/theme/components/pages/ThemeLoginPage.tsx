// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import {AuthErrorState} from '@app/features/auth/flow/AuthErrorState';
import {AuthLoadingState} from '@app/features/auth/flow/AuthLoadingState';
import {AuthLoginLayout} from '@app/features/auth/flow/AuthLoginLayout';
import {AuthPageHeader} from '@app/features/auth/flow/AuthPageHeader';
import sharedStyles from '@app/features/auth/flow/AuthPageStyles.module.css';
import {AuthRouterLink} from '@app/features/auth/flow/AuthRouterLink';
import {AuthRuntimeTargetGate} from '@app/features/auth/flow/AuthRuntimeTargetGate';
import {AuthRuntimeTargetResetAction} from '@app/features/auth/flow/AuthRuntimeTargetResetAction';
import {isHandoffRequest} from '@app/features/auth/flow/auth_login_core/useDesktopHandoffFlow';
import {DesktopDeepLinkPrompt} from '@app/features/auth/flow/DesktopDeepLinkPrompt';
import {DesktopHandoffMfaStep} from '@app/features/auth/flow/DesktopHandoffMfaStep';
import Authentication, {LoginState} from '@app/features/auth/state/Authentication';
import {useAuthRuntimeTarget} from '@app/features/auth/state/AuthRuntimeTarget';
import {safeRedirectTarget} from '@app/features/auth/utils/SafeRedirect';
import {REGISTER_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {setPathQueryParams} from '@app/features/messaging/utils/MessagingUrlUtils';
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

const THEME_LOGIN_PAGE_STEP_ORDER: ReadonlyArray<LoginState> = [LoginState.DEFAULT, LoginState.MFA];

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
interface ThemeLoginPageProps {
	readonly themeId: string;
	readonly onLoginComplete: () => void;
}

const ThemeLoginPage = observer(function ThemeLoginPage({themeId, onLoginComplete}: ThemeLoginPageProps) {
	const {i18n} = useLingui();
	const runtimeTarget = useAuthRuntimeTarget();
	const location = useLocation();
	const params = useMemo(() => new URLSearchParams(location.search), [location.search]);
	const safeRedirect = safeRedirectTarget(params.get('redirect_to'));
	const isHandoff = isHandoffRequest(params);
	const registerSearch = safeRedirect == null ? undefined : {redirect_to: safeRedirect};
	const redirectPath = useMemo(() => {
		if (safeRedirect == null) {
			return Routes.theme(themeId);
		}
		return setPathQueryParams(Routes.theme(themeId), {redirect_to: safeRedirect});
	}, [themeId, safeRedirect]);
	return (
		<AuthLoginLayout
			redirectPath={redirectPath}
			inviteCode={null}
			desktopHandoff={isHandoff}
			excludeCurrentUser={false}
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
			forgotPasswordAction={null}
			showTitle={false}
			title={null}
			onBackActionChange={null}
			completeLoginRedirectPath={null}
			forceCredentials={false}
			startWithAddAccount={false}
			runtimeTarget={runtimeTarget}
			showInstanceSelector={null}
			ssoRedirectPath={null}
			suppressInlineBackButtons={false}
			initialIdentifier={null}
			registerLink={
				<AuthRouterLink
					to={Routes.themeRegister(themeId)}
					search={registerSearch}
					data-flx="theme.theme-login-page.auth-router-link"
				>
					{i18n._(REGISTER_DESCRIPTOR)}
				</AuthRouterLink>
			}
			onLoginComplete={onLoginComplete}
			data-flx="theme.theme-login-page.auth-login-layout"
		/>
	);
});
interface ThemeLoginPageContentProps {
	readonly runtimeSnapshot: RuntimeConfigSnapshot;
}

const ThemeLoginPageContent = observer(function ThemeLoginPageContent({runtimeSnapshot}: ThemeLoginPageContentProps) {
	const {i18n} = useLingui();
	const runtimeTarget = useAuthRuntimeTarget();
	const loginState = Authentication.loginState;
	const {themeId} = useParams() as {themeId: string};
	const handleLoginComplete = useCallback(() => {
		ThemeCommands.openAcceptModal(themeId, i18n, runtimeSnapshot);
		runtimeTarget.reset();
	}, [i18n, runtimeSnapshot, runtimeTarget, themeId]);
	useFluxerDocumentTitle(i18n._(APPLY_THEME_DESCRIPTOR));
	const themeStatus = useThemeExists(themeId, runtimeSnapshot);
	if (themeStatus === 'loading') {
		return <AuthLoadingState data-flx="theme.theme-login-page.theme-login-page-container.auth-loading-state" />;
	}
	if (themeStatus === 'error') {
		return (
			<AuthErrorState
				title={<Trans>Theme not found</Trans>}
				text={<Trans>This theme may have been removed or the link is invalid.</Trans>}
				action={
					<AuthRuntimeTargetResetAction data-flx="theme.theme-login-page.theme-login-page-container.auth-runtime-target-reset-action" />
				}
				data-flx="theme.theme-login-page.theme-login-page-container.auth-error-state"
			/>
		);
	}
	switch (loginState) {
		case LoginState.DEFAULT:
			return (
				<SteppedCarousel
					step={loginState}
					steps={THEME_LOGIN_PAGE_STEP_ORDER}
					focusOnStepChange
					ariaLabel={i18n._(APPLY_THEME_DESCRIPTOR)}
					data-flx="theme.theme-login-page.container-carousel"
				>
					<ThemeLoginPage
						themeId={themeId}
						onLoginComplete={handleLoginComplete}
						data-flx="theme.theme-login-page.theme-login-page-container.theme-login-page"
					/>
				</SteppedCarousel>
			);
		case LoginState.MFA:
			return (
				<SteppedCarousel
					step={loginState}
					steps={THEME_LOGIN_PAGE_STEP_ORDER}
					focusOnStepChange
					ariaLabel={i18n._(APPLY_THEME_DESCRIPTOR)}
					data-flx="theme.theme-login-page.container-carousel"
				>
					<DesktopHandoffMfaStep
						fallbackRedirectPath={Routes.theme(themeId)}
						onLoginComplete={handleLoginComplete}
						data-flx="theme.theme-login-page.theme-login-page-container.desktop-handoff-mfa-step"
					/>
				</SteppedCarousel>
			);
		default:
			return null;
	}
});

const ThemeLoginPageContainer = observer(() => (
	<AuthRuntimeTargetGate data-flx="theme.theme-login-page.runtime-target-gate">
		{(runtimeSnapshot) => (
			<ThemeLoginPageContent
				runtimeSnapshot={runtimeSnapshot}
				data-flx="theme.theme-login-page.theme-login-page-content"
			/>
		)}
	</AuthRuntimeTargetGate>
));

export default ThemeLoginPageContainer;
