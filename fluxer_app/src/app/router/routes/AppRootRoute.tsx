// SPDX-License-Identifier: AGPL-3.0-or-later

import {RootComponent} from '@app/app/router/components/RootComponent';
import {createRootRoute} from '@app/features/platform/components/router/RouterBuilder';

export const rootRoute = createRootRoute({
	layout: ({children}) => (
		<RootComponent data-flx="app.router.root-routes.layout.root-component">{children}</RootComponent>
	),
});
