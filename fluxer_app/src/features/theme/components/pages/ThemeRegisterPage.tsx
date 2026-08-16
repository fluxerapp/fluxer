// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import {AuthBottomLink} from '@app/features/auth/flow/AuthBottomLink';
import {AuthErrorState} from '@app/features/auth/flow/AuthErrorState';
import {AuthLoadingState} from '@app/features/auth/flow/AuthLoadingState';
import {AuthMinimalRegisterFormCore} from '@app/features/auth/flow/AuthMinimalRegisterFormCore';
import {AuthPageHeader} from '@app/features/auth/flow/AuthPageHeader';
import sharedStyles from '@app/features/auth/flow/AuthPageStyles.module.css';
import {AuthSsoPanel, isRuntimeSsoEnforced} from '@app/features/auth/flow/AuthSsoPanel';
import {DesktopDeepLinkPrompt} from '@app/features/auth/flow/DesktopDeepLinkPrompt';
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
const ThemeRegisterPage = observer(function ThemeRegisterPage() {
	const {i18n} = useLingui();
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
	const themeStatus = useThemeExists(themeId);
	const handleRegisterComplete = useCallback(
		async (response: AuthenticationCommands.TokenResponse) => {
			const userData = AuthenticationCommands.authResponseUserToUserData(response.user);
			await AuthenticationCommands.completeLogin({
				token: response.token,
				userId: response.user_id,
				...(userData ? {userData} : {}),
			});
			ThemeCommands.openAcceptModal(themeId, i18n);
		},
		[themeId, i18n],
	);
	useFluxerDocumentTitle(i18n._(APPLY_THEME_DESCRIPTOR));
	if (themeStatus === 'loading') {
		return <AuthLoadingState data-flx="theme.theme-register-page.auth-loading-state" />;
	}
	if (themeStatus === 'error') {
		return (
			<AuthErrorState
				title={<Trans>Theme not found</Trans>}
				text={<Trans>This theme may have been removed or the link is invalid.</Trans>}
				data-flx="theme.theme-register-page.auth-error-state"
			/>
		);
	}
	if (isRuntimeSsoEnforced()) {
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

export default ThemeRegisterPage;
