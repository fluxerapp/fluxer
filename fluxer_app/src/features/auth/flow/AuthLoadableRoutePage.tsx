// SPDX-License-Identifier: AGPL-3.0-or-later

import {AuthRouteLoadError} from '@app/features/auth/flow/AuthRouteLoadError';
import {AuthShellLoadingState} from '@app/features/auth/flow/AuthShellLoadingState';
import {
	createDefaultLoadableComponent,
	type LoadableComponent,
} from '@app/features/platform/components/loadable/LoadableComponent';

export type AuthRoutePageProps = Record<string, unknown>;
export type AuthRoutePage = LoadableComponent<AuthRoutePageProps>;

export function createAuthRoutePage(displayName: string, load: () => Promise<{default: unknown}>): AuthRoutePage {
	return createDefaultLoadableComponent<AuthRoutePageProps>({
		displayName,
		LoadingComponent: AuthShellLoadingState,
		ErrorComponent: AuthRouteLoadError,
		load,
	});
}
