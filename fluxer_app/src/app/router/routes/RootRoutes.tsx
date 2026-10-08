// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {RootComponent} from '@app/app/router/components/RootComponent';
import {afterSessionInitialization} from '@app/app/router/policy/AuthRouteRedirectPolicy';
import {NotFoundPage} from '@app/features/app/components/pages/NotFoundPage';
import {readAuthenticatedRuntimeContext} from '@app/features/auth/state/AuthenticatedRuntime';
import {getDefaultLandingPath} from '@app/features/navigation/utils/DefaultLandingUtils';
import {createRootRoute, createRoute} from '@app/features/platform/components/router/RouterBuilder';
import {Redirect} from '@app/features/platform/components/router/RouterTypes';

export const rootRoute = createRootRoute({
	layout: ({children}) => (
		<RootComponent data-flx="app.router.root-routes.layout.root-component">{children}</RootComponent>
	),
});
export const notFoundRoute = createRoute({
	id: '__notFound',
	path: '/__notfound',
	component: () => <NotFoundPage data-flx="app.router.root-routes.not-found-page" />,
});
export const homeRoute = createRoute({
	getParentRoute: () => rootRoute,
	id: 'home',
	path: '/',
	onEnter: afterSessionInitialization(() => {
		if (readAuthenticatedRuntimeContext() === null) {
			return new Redirect(Routes.LOGIN);
		}
		return new Redirect(getDefaultLandingPath());
	}),
});
export const appRoute = createRoute({
	getParentRoute: () => rootRoute,
	id: 'app',
	path: Routes.APP,
	onEnter: () => new Redirect(getDefaultLandingPath()),
});
