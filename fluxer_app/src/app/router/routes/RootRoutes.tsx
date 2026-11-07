// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {afterSessionInitialization} from '@app/app/router/policy/AuthRouteRedirectPolicy';
import {NotFoundPage} from '@app/features/app/components/pages/NotFoundPage';
import {readAuthenticatedRuntimeContext} from '@app/features/auth/state/AuthenticatedRuntime';
import {getDefaultLandingPath} from '@app/features/navigation/utils/DefaultLandingUtils';
import {createRoute} from '@app/features/platform/components/router/RouterBuilder';
import {Redirect} from '@app/features/platform/components/router/RouterTypes';

const enterDefaultLanding = afterSessionInitialization(() => {
	if (readAuthenticatedRuntimeContext() === null) {
		return new Redirect(Routes.LOGIN);
	}
	return new Redirect(getDefaultLandingPath());
});

export const notFoundRoute = createRoute({
	id: '__notFound',
	path: '/__notfound',
	component: () => <NotFoundPage data-flx="app.router.root-routes.not-found-page" />,
});
export const homeRoute = createRoute({
	id: 'home',
	path: '/',
	onEnter: enterDefaultLanding,
});
export const appRoute = createRoute({
	id: 'app',
	path: Routes.APP,
	onEnter: enterDefaultLanding,
});
