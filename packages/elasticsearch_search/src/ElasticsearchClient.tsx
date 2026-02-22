/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {Client} from '@elastic/elasticsearch';

export interface ElasticsearchClientConfig {
	node: string;
	auth?: {
		apiKey?: string;
		username?: string;
		password?: string;
	};
	requestTimeoutMs: number;
}

export function createElasticsearchClient(config: ElasticsearchClientConfig): Client {
	return new Client({
		node: config.node,
		auth: config.auth?.apiKey
			? {apiKey: config.auth.apiKey}
			: config.auth?.username
				? {username: config.auth.username, password: config.auth.password ?? ''}
				: undefined,
		requestTimeout: config.requestTimeoutMs,
	});
}
