// SPDX-License-Identifier: AGPL-3.0-or-later

import {rootRoute} from '@app/app/router/routes/AppRootRoute';
import {appRouteTree} from '@app/app/router/routes/AppRoutes';
import {authRouteTree} from '@app/app/router/routes/AuthRoutes';
import {connectionCallbackRoute, premiumCallbackRoute} from '@app/app/router/routes/CallbackRoutes';
import {appRoute, homeRoute, notFoundRoute} from '@app/app/router/routes/RootRoutes';
import type {RouteConfig} from '@app/features/platform/components/router/RouterTypes';

const routeTree = rootRoute.addChildren([
	homeRoute,
	appRoute,
	notFoundRoute,
	premiumCallbackRoute,
	connectionCallbackRoute,
	authRouteTree,
	appRouteTree,
]);
export const buildRoutes = (): Array<RouteConfig> => routeTree.build();
