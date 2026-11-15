// SPDX-License-Identifier: AGPL-3.0-or-later

import {net} from 'electron';

export function moduleNetworkFetch(input: string | URL | Request, init?: RequestInit): Promise<Response> {
	return net.fetch(input instanceof URL ? input.href : input, {
		cache: 'no-store',
		...init,
		bypassCustomProtocolHandlers: true,
	});
}
