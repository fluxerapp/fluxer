// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import * as AuthRouteRedirectPolicy from '@app/app/router/policy/AuthRouteRedirectPolicy';
import {Redirect, type RouteContext} from '@app/features/platform/components/router/RouterTypes';

export interface ShortLinkRouteEnterHandlers {
	readonly onRegisterEnter: AuthRouteRedirectPolicy.AuthRouteEnterHandler;
	readonly onLoginEnter: AuthRouteRedirectPolicy.AuthRouteEnterHandler;
}

interface ShortLinkRouteEnterRequest {
	readonly paramName: string;
	readonly openAcceptModal: (value: string) => void;
}

interface AcceptShortLinkRequest {
	readonly context: RouteContext;
	readonly paramName: string;
	readonly openAcceptModal: (value: string) => void;
}

function acceptShortLink({context, paramName, openAcceptModal}: AcceptShortLinkRequest): Redirect {
	const value = context.params[paramName];
	if (value != null && value !== '') {
		openAcceptModal(value);
	}
	return new Redirect(AuthRouteRedirectPolicy.resolveCurrentRedirectTarget(Routes.ME));
}

function acceptShortLinkUnlessDesktopHandoff(request: AcceptShortLinkRequest): Redirect | undefined {
	if (AuthRouteRedirectPolicy.isDesktopHandoffLocation()) {
		return undefined;
	}
	return acceptShortLink(request);
}

export function createHandlers({paramName, openAcceptModal}: ShortLinkRouteEnterRequest): ShortLinkRouteEnterHandlers {
	return {
		onRegisterEnter: AuthRouteRedirectPolicy.whenAuthenticated((context) =>
			acceptShortLink({context, paramName, openAcceptModal}),
		),
		onLoginEnter: AuthRouteRedirectPolicy.whenAuthenticated((context) =>
			acceptShortLinkUnlessDesktopHandoff({context, paramName, openAcceptModal}),
		),
	};
}
