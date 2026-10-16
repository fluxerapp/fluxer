// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import sharedStyles from '@app/features/auth/flow/AuthPageStyles.module.css';
import type {ResourceAuthPageConfig} from '@app/features/auth/flow/ResourceAuthPages';
import {ThemeHeader} from '@app/features/auth/flow/ThemeHeader';
import {setPathQueryParams} from '@app/features/messaging/utils/MessagingUrlUtils';
import * as ThemeCommands from '@app/features/theme/commands/ThemeCommands';
import {useThemeExists} from '@app/features/theme/hooks/useThemeExists';
import {msg} from '@lingui/core/macro';
import {Trans} from '@lingui/react/macro';

const APPLY_THEME_DESCRIPTOR = msg({
	message: 'Apply theme',
	comment:
		'Button or menu action label in the theme login and register pages. Keep it concise. Keep the tone plain and specific.',
});
const themePath = (id: string, redirect: string | null) =>
	redirect ? setPathQueryParams(Routes.theme(id), {redirect_to: redirect}) : Routes.theme(id);
export const themeAuthPageConfig: ResourceAuthPageConfig<string> = {
	kind: 'theme',
	dataFlxPrefix: (mode) => `theme.theme-${mode}-page`,
	useResource: (id, snapshot) => {
		const status = useThemeExists(id, snapshot);
		return {loading: status === 'loading', error: status === 'error', resource: status === 'ready' ? id : null};
	},
	renderHeader: (_id, mode, sso) => (
		<ThemeHeader
			prefix={`theme.theme-${mode}-page`}
			sso={sso}
			data-flx="theme.theme-auth-page-config.render-header.theme-header"
		/>
	),
	documentTitle: (i18n) => i18n._(APPLY_THEME_DESCRIPTOR),
	errorTitle: () => <Trans>Theme not found</Trans>,
	errorText: <Trans>This theme may have been removed or the link is invalid.</Trans>,
	registerPath: Routes.themeRegister,
	loginPath: (id, redirect) =>
		redirect ? setPathQueryParams(Routes.themeLogin(id), {redirect_to: redirect}) : Routes.themeLogin(id),
	redirectPath: themePath,
	fallbackRedirectPath: Routes.theme,
	registerRedirectPath: themePath,
	onComplete: (id, i18n, snapshot) => ThemeCommands.openAcceptModal(id, i18n, snapshot),
	submitLabel: <Trans>Create account</Trans>,
	extraContent: (
		<p className={sharedStyles.subtext} data-flx="theme.theme-register-page.p">
			<Trans>Once your account is created, we'll take you back to the theme so you can apply it.</Trans>
		</p>
	),
};
