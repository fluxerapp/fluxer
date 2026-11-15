// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {KeyboardModeListener} from '@app/features/app/components/layout/KeyboardModeListener';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import Authentication from '@app/features/auth/state/Authentication';
import {setPathQueryParams} from '@app/features/messaging/utils/MessagingUrlUtils';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {createNamedLoadableComponent} from '@app/features/platform/components/loadable/LoadableComponent';
import {useLocation} from '@app/features/platform/components/router/RouterReact';
import {useHolidayClasses} from '@app/features/theme/hooks/useHolidayClasses';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useRef} from 'react';

const SelfHostedSetupWizardGate = createNamedLoadableComponent({
	displayName: 'SelfHostedSetupWizardGate',
	load: async () =>
		(await import('@app/features/app/components/setup/SelfHostedSetupWizardGate')).SelfHostedSetupWizardGate,
});

const STANDALONE_AUTH_PREFIXES = [
	Routes.LOGIN,
	Routes.REGISTER,
	Routes.FORGOT_PASSWORD,
	Routes.RESET_PASSWORD,
	Routes.RECOVER_ACCOUNT,
	Routes.VERIFY_EMAIL,
	Routes.AUTHORIZE_IP,
	Routes.EMAIL_REVERT,
	Routes.OAUTH_AUTHORIZE,
	Routes.SSO_CALLBACK,
	Routes.REPORT,
	Routes.PREMIUM_CALLBACK,
	Routes.CONNECTION_CALLBACK,
	'/invite/',
	'/gift/',
	'/theme/',
];

function isStandaloneAuthPath(pathname: string): boolean {
	return pathname === '/__notfound' || STANDALONE_AUTH_PREFIXES.some((prefix) => pathname.startsWith(prefix));
}

export const AuthRootComponent: React.FC<{children?: React.ReactNode}> = observer(({children}) => {
	useHolidayClasses();
	const pathname = useLocation().pathname;
	const isAuthenticated = Authentication.isAuthenticated;
	const requiresSelfHostedSetup = RuntimeConfig.requiresSelfHostedSetup();
	const isStandaloneRoute = isStandaloneAuthPath(pathname);
	const pendingRedirectRef = useRef<string | null>(null);
	useEffect(() => {
		if (isAuthenticated) return;
		if (requiresSelfHostedSetup || isStandaloneRoute) {
			if (requiresSelfHostedSetup && pathname === Routes.LOGIN && pendingRedirectRef.current) {
				RouterUtils.replaceWith(pendingRedirectRef.current);
				pendingRedirectRef.current = null;
			}
			return;
		}
		pendingRedirectRef.current ??= pathname + window.location.search;
		RouterUtils.replaceWith(setPathQueryParams(Routes.LOGIN, {redirect_to: pendingRedirectRef.current}));
	}, [isAuthenticated, isStandaloneRoute, pathname, requiresSelfHostedSetup]);
	if (requiresSelfHostedSetup && !isStandaloneRoute) {
		return <SelfHostedSetupWizardGate data-flx="app.router.auth-root-component.self-hosted-setup-wizard-gate" />;
	}
	return (
		<>
			<KeyboardModeListener data-flx="app.router.auth-root-component.keyboard-mode-listener" />
			{children}
		</>
	);
});
