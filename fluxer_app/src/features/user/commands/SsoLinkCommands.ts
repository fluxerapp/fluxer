// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {storeSsoPendingContext} from '@app/features/auth/state/SsoPendingContext';
import {http} from '@app/features/platform/transport/RestTransport';
import {navigateToExternalURL} from '@app/features/ui/utils/NativeUtils';
import Users from '@app/features/user/state/Users';
import type {SsoStartResponse} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import type {UserPrivate} from '@fluxer/schema/src/domains/user/UserResponseSchemas';

export async function connectSso(): Promise<void> {
	const {body} = await http.post<SsoStartResponse>(Endpoints.USER_SSO_LINK_START, {body: {}});
	await storeSsoPendingContext(body.state, {
		redirectTo: null,
		runtimeSnapshot: RuntimeConfig.getSnapshot(),
		clientBinding: body.client_binding,
	});
	await navigateToExternalURL(body.authorization_url);
}

export async function disconnectSso(): Promise<void> {
	await http.delete(Endpoints.USER_SSO_UNLINK, {body: {}});

	const {body: user} = await http.get<UserPrivate>(Endpoints.USER_ME);
	Users.handleUserUpdate(user);
}
