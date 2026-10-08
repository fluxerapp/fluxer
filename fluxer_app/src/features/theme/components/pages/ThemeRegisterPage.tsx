// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import {AuthBottomLink} from '@app/features/auth/flow/AuthBottomLink';
import {AuthErrorState} from '@app/features/auth/flow/AuthErrorState';
import {AuthLoadingState} from '@app/features/auth/flow/AuthLoadingState';
import {AuthMinimalRegisterFormCore} from '@app/features/auth/flow/AuthMinimalRegisterFormCore';
import {AuthPageHeader} from '@app/features/auth/flow/AuthPageHeader';
import sharedStyles from '@app/features/auth/flow/AuthPageStyles.module.css';
import {AuthRuntimeTargetGate} from '@app/features/auth/flow/AuthRuntimeTargetGate';
import {AuthRuntimeTargetResetAction} from '@app/features/auth/flow/AuthRuntimeTargetResetAction';
import {AuthSsoPanel, resolveAuthPanelSso} from '@app/features/auth/flow/AuthSsoPanel';
import {DesktopDeepLinkPrompt} from '@app/features/auth/flow/DesktopDeepLinkPrompt';
import {useAuthPresentation} from '@app/features/auth/flow/useAuthPresentation';
import {AuthCardVariant} from '@app/features/auth/state/AuthLayoutContext';
import {useAuthRuntimeTarget} from '@app/features/auth/state/AuthRuntimeTarget';
import {safeRedirectTarget} from '@app/features/auth/utils/SafeRedirect';
import {setPathQueryParams} from '@app/features/messaging/utils/MessagingUrlUtils';
import {useLocation, useParams} from '@app/features/platform/components/router/RouterReact';
import * as ThemeCommands from '@app/features/theme/commands/ThemeCommands';
import {useThemeExists} from '@app/features/theme/hooks/useThemeExists';
import {useFluxerDocumentTitle} from '@app/features/window/hooks/useFluxerDocumentTitle';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {PaletteIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback, useMemo} from 'react';

const APPLY_THEME_DESCRIPTOR = msg({
	message: 'Apply theme',
	comment: 'Button or menu action label in the theme register page. Keep it concise.',
});
const YOU_VE_GOT_CSS_DESCRIPTOR = msg({
	message: "You've got CSS!",
	comment: 'Short label in the theme register page. Keep it concise.',
});
const SHARED_THEME_DESCRIPTOR = msg({
	message: 'Shared theme',
	comment: 'Button or menu action label in the theme register page. Keep it concise.',
});
interface ThemeRegisterPageContentProps {
	readonly runtimeSnapshot: RuntimeConfigSnapshot;
}

const ThemeRegisterPageContent = observer(function ThemeRegisterPageContent({
	runtimeSnapshot,
}: ThemeRegisterPageContentProps) {
	const {i18n} = useLingui();
	const runtimeTarget = useAuthRuntimeTarget();
	const {themeId} = useParams() as {themeId: string};
	const location = useLocation();
	const params = useMemo(() => new URLSearchParams(location.search), [location.search]);
	const rawRedirect = params['get']('redirect_to');
	const safeRedirect = safeRedirectTarget(rawRedirect);
	const themePath = safeRedirect
		? setPathQueryParams(Routes.theme(themeId), {redirect_to: safeRedirect})
		: Routes.theme(themeId);
	const loginPath = safeRedirect
		? setPathQueryParams(Routes.themeLogin(themeId), {redirect_to: safeRedirect})
		: Routes.themeLogin(themeId);
	const themeStatus = useThemeExists(themeId, runtimeSnapshot);
	const handleRegisterComplete = useCallback(
		async (response: AuthenticationCommands.TokenResponse) => {
			const userData = AuthenticationCommands.authResponseUserToUserData(response.user);
			await AuthenticationCommands.completeLogin({
				token: response.token,
				userId: response.user_id,
				runtimeSnapshot,
				...(userData ? {userData} : {}),
			});
			ThemeCommands.openAcceptModal(themeId, i18n, runtimeSnapshot);
		},
		[i18n, runtimeSnapshot, themeId],
	);
	useFluxerDocumentTitle(i18n._(APPLY_THEME_DESCRIPTOR));
	useAuthPresentation({variant: AuthCardVariant.STANDARD});
	if (themeStatus === 'loading') {
		return <AuthLoadingState data-flx="theme.theme-register-page.auth-loading-state" />;
	}
	if (themeStatus === 'error') {
		return (
			<AuthErrorState
				title={<Trans>Theme not found</Trans>}
				text={<Trans>This theme may have been removed or the link is invalid.</Trans>}
				action={<AuthRuntimeTargetResetAction data-flx="theme.theme-register-page.auth-runtime-target-reset-action" />}
				data-flx="theme.theme-register-page.auth-error-state"
			/>
		);
	}
	const runtimeSso = resolveAuthPanelSso(runtimeSnapshot);
	if (runtimeSso?.enabled === true && runtimeSso.enforced === true) {
		return (
			<div className={sharedStyles.container} data-flx="theme.theme-register-page.sso-container">
				<DesktopDeepLinkPrompt
					code={themeId}
					kind="theme"
					data-flx="theme.theme-register-page.desktop-deep-link-prompt.sso"
				/>
				<AuthPageHeader
					icon={
						<div className={sharedStyles.themeIconSpot} data-flx="theme.theme-register-page.div.sso">
							<PaletteIcon
								className={sharedStyles.themeIcon}
								weight="fill"
								data-flx="theme.theme-register-page.palette-icon.sso"
							/>
						</div>
					}
					title={i18n._(YOU_VE_GOT_CSS_DESCRIPTOR)}
					subtitle={i18n._(SHARED_THEME_DESCRIPTOR)}
					data-flx="theme.theme-register-page.auth-page-header.sso"
				/>
				<AuthSsoPanel
					redirectPath={themePath}
					runtimeSnapshot={runtimeSnapshot}
					dataFlx="theme.theme-register-page.sso-panel"
					data-flx="theme.theme-register-page.auth-sso-panel"
				/>
				<AuthBottomLink variant="login" to={loginPath} data-flx="theme.theme-register-page.auth-bottom-link.sso" />
			</div>
		);
	}
	return (
		<div className={sharedStyles.container} data-flx="theme.theme-register-page.div">
			<DesktopDeepLinkPrompt
				code={themeId}
				kind="theme"
				data-flx="theme.theme-register-page.desktop-deep-link-prompt"
			/>
			<AuthPageHeader
				icon={
					<div className={sharedStyles.themeIconSpot} data-flx="theme.theme-register-page.div--2">
						<PaletteIcon
							className={sharedStyles.themeIcon}
							weight="fill"
							data-flx="theme.theme-register-page.palette-icon"
						/>
					</div>
				}
				title={i18n._(YOU_VE_GOT_CSS_DESCRIPTOR)}
				subtitle={i18n._(SHARED_THEME_DESCRIPTOR)}
				data-flx="theme.theme-register-page.auth-page-header"
			/>
			<AuthMinimalRegisterFormCore
				submitLabel={<Trans>Create account</Trans>}
				redirectPath={themePath}
				onRegister={handleRegisterComplete}
				runtimeSnapshot={runtimeSnapshot}
				onAuthenticated={runtimeTarget.reset}
				extraContent={
					<p className={sharedStyles.subtext} data-flx="theme.theme-register-page.p">
						<Trans>Once your account is created, we'll take you back to the theme so you can apply it.</Trans>
					</p>
				}
				data-flx="theme.theme-register-page.auth-minimal-register-form-core"
			/>
			<AuthBottomLink variant="login" to={loginPath} data-flx="theme.theme-register-page.auth-bottom-link" />
		</div>
	);
});

const ThemeRegisterPage = observer(() => (
	<AuthRuntimeTargetGate data-flx="theme.theme-register-page.runtime-target-gate">
		{(runtimeSnapshot) => (
			<ThemeRegisterPageContent
				runtimeSnapshot={runtimeSnapshot}
				data-flx="theme.theme-register-page.theme-register-page-content"
			/>
		)}
	</AuthRuntimeTargetGate>
));

export default ThemeRegisterPage;
