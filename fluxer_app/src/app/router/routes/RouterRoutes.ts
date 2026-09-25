// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	appRouteTree,
	connectionCallbackRoute,
	matureContentCheckCallbackRoute,
	premiumCallbackRoute,
} from '@app/app/router/routes/AppRoutes';
import {authRouteTree} from '@app/app/router/routes/AuthRoutes';
import {appRoute, homeRoute, notFoundRoute, rootRoute} from '@app/app/router/routes/RootRoutes';
import type {RouteConfig} from '@app/features/platform/components/router/RouterTypes';

const routeTree = rootRoute.addChildren([
	homeRoute,
	appRoute,
	notFoundRoute,
	premiumCallbackRoute,
	matureContentCheckCallbackRoute,
	connectionCallbackRoute,
	authRouteTree,
	appRouteTree,
]);
export const buildRoutes = (): Array<RouteConfig> => routeTree.build();
