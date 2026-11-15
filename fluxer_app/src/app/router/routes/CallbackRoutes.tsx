// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {requireAuthentication} from '@app/app/router/policy/AuthRouteRedirectPolicy';
import {createDefaultLoadableComponent} from '@app/features/platform/components/loadable/LoadableComponent';
import {createRoute} from '@app/features/platform/components/router/RouterBuilder';

type CallbackRouteComponentProps = Record<string, unknown>;

const ConnectionCallbackPage = createDefaultLoadableComponent<CallbackRouteComponentProps>({
	displayName: 'ConnectionCallbackPage',
	load: () => import('@app/features/connection/components/pages/ConnectionCallbackPage'),
});
const PremiumCallbackPage = createDefaultLoadableComponent<CallbackRouteComponentProps>({
	displayName: 'PremiumCallbackPage',
	load: () => import('@app/features/premium/components/pages/PremiumCallbackPage'),
});

export const premiumCallbackRoute = createRoute({
	id: 'premiumCallback',
	path: Routes.PREMIUM_CALLBACK,
	onEnter: (context) =>
		context.url.searchParams.get('product') === 'operator_pass' ? requireAuthentication(context) : undefined,
	preload: PremiumCallbackPage.preload,
	component: () => <PremiumCallbackPage data-flx="app.router.app-routes.premium-callback-page" />,
});
export const connectionCallbackRoute = createRoute({
	id: 'connectionCallback',
	path: Routes.CONNECTION_CALLBACK,
	preload: ConnectionCallbackPage.preload,
	component: () => <ConnectionCallbackPage data-flx="app.router.app-routes.connection-callback-page" />,
});
