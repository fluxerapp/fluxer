// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {storeSsoPendingContext} from '@app/features/auth/state/SsoPendingContext';
import {http} from '@app/features/platform/transport/RestTransport';
import {navigateToExternalURL} from '@app/features/ui/utils/NativeUtils';
import type {SsoStartResponse} from '@fluxer/schema/src/domains/auth/AuthSchemas';

export async function connectSso(): Promise<void> {
	const {body} = await http.post<SsoStartResponse>(Endpoints.USER_SSO_LINK_START, {body: {}});
	await storeSsoPendingContext(body.state, {
		redirectTo: null,
		runtimeSnapshot: RuntimeConfig.getSnapshot(),
		clientBinding: body.client_binding,
	});
	await navigateToExternalURL(body.authorization_url);
}
