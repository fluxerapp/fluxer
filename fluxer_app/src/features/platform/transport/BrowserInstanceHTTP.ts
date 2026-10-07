// SPDX-License-Identifier: AGPL-3.0-or-later

import type {InstanceHTTP, InstanceHTTPDelivery} from '@app/features/platform/transport/InstanceHTTP';
import {http} from '@app/features/platform/transport/RestTransport';
import type {RestRequestOptions, RestResponse} from '@app/features/platform/types/TransportTypes';

function restOptions(delivery: InstanceHTTPDelivery): RestRequestOptions {
	const shared = {
		headers: delivery.headers,
		auth: delivery.auth,
		retries: delivery.retries,
		timeoutMs: delivery.timeoutMs,
		signal: delivery.signal,
		intercept: delivery.intercept,
	};
	if (delivery.body === undefined) {
		return shared;
	}
	return {...shared, body: delivery.body};
}

export function createBrowserInstanceHTTP(): InstanceHTTP {
	return {
		kind: 'browser',
		send<T>(delivery: InstanceHTTPDelivery): Promise<RestResponse<T>> {
			const options = restOptions(delivery);
			switch (delivery.accountScopedWork.kind) {
				case 'standard':
					return http.dispatch<T>(delivery.method, delivery.path, options);
				case 'account-transition':
					return http.dispatchWithinAccountTransition<T>(
						delivery.accountScopedWork.suspension,
						delivery.method,
						delivery.path,
						options,
					);
			}
		},
	};
}
