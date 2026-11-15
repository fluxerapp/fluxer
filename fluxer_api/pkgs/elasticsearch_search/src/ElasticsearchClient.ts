// SPDX-License-Identifier: AGPL-3.0-or-later

import {Client, type ClientOptions} from '@elastic/elasticsearch';

export interface ElasticsearchClientConfig {
	node: string;
	auth?: {
		apiKey?: string;
		username?: string;
		password?: string;
	};
	requestTimeoutMs: number;
	tlsRejectUnauthorized?: boolean;
}

export function createElasticsearchClient(config: ElasticsearchClientConfig): Client {
	const node = config.node;
	let auth: ClientOptions['auth'];
	if (config.auth?.apiKey) {
		auth = {apiKey: config.auth.apiKey};
	} else if (config.auth?.username) {
		auth = {username: config.auth.username, password: config.auth.password ?? ''};
	}
	return new Client({
		node,
		auth,
		requestTimeout: config.requestTimeoutMs,
		tls: config.tlsRejectUnauthorized === false ? {rejectUnauthorized: false} : undefined,
	});
}
