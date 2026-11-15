// SPDX-License-Identifier: AGPL-3.0-or-later

import {buildAuthRoutes} from '@app/app/router/routes/AuthRootRoutes';
import Navigation from '@app/features/navigation/state/Navigation';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {createRouter} from '@app/features/platform/components/router/RouterCore';

export const authRouter = createRouter({
	routes: buildAuthRoutes(),
	history: RouterUtils.getHistory() ?? undefined,
	notFoundRouteId: '__notFound',
	scrollRestoration: 'top',
});

Navigation.initialize(authRouter);

export function disposeAuthRouter(): void {
	authRouter.destroy();
}
