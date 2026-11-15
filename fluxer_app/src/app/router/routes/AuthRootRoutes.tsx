// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {AuthRootComponent} from '@app/app/router/components/AuthRootComponent';
import {requireAuthentication} from '@app/app/router/policy/AuthRouteRedirectPolicy';
import {authRouteTree} from '@app/app/router/routes/AuthRoutes';
import {connectionCallbackRoute, premiumCallbackRoute} from '@app/app/router/routes/CallbackRoutes';
import {appRoute, homeRoute, notFoundRoute} from '@app/app/router/routes/RootRoutes';
import {createRootRoute, createRoute} from '@app/features/platform/components/router/RouterBuilder';
import type {RouteConfig} from '@app/features/platform/components/router/RouterTypes';

const authRootRoute = createRootRoute({
	layout: ({children}) => (
		<AuthRootComponent data-flx="app.router.auth-root-routes.layout.auth-root-component">{children}</AuthRootComponent>
	),
});
const SIGNED_IN_APP_PATHS = [
	'/channels/*',
	Routes.NOTIFICATIONS,
	Routes.YOU,
	Routes.BOOKMARKS,
	Routes.MENTIONS,
	Routes.LEGACY_PLUTONIUM,
	Routes.USER_PROFILE,
];
const signedInAppRoutes = SIGNED_IN_APP_PATHS.map((path, index) =>
	createRoute({id: `signedInApp${index}`, path, onEnter: requireAuthentication}),
);
const authRootRouteTree = authRootRoute.addChildren([
	homeRoute,
	appRoute,
	notFoundRoute,
	premiumCallbackRoute,
	connectionCallbackRoute,
	authRouteTree,
	...signedInAppRoutes,
]);
export const buildAuthRoutes = (): Array<RouteConfig> => authRootRouteTree.build();
