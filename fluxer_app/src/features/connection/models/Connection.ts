// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ConnectionType} from '@fluxer/constants/src/ConnectionConstants';
import type {ConnectionResponse} from '@fluxer/schema/src/domains/connection/ConnectionSchemas';

export class Connection {
	readonly id: string;
	readonly type: ConnectionType;
	readonly name: string;
	readonly verified: boolean;
	readonly visibilityFlags: number;
	readonly sortOrder: number;
	readonly providerId: string | null;
	readonly url: string | null;

	constructor(connection: ConnectionResponse) {
		this.id = connection.id;
		this.type = connection.type;
		this.name = connection.name;
		this.verified = connection.verified;
		this.visibilityFlags = connection.visibility_flags;
		this.sortOrder = connection.sort_order;
		this.providerId = connection.provider_id ?? null;
		this.url = connection.url ?? null;
	}

	toJSON(): ConnectionResponse {
		return {
			id: this.id,
			type: this.type,
			name: this.name,
			verified: this.verified,
			visibility_flags: this.visibilityFlags,
			sort_order: this.sortOrder,
			provider_id: this.providerId,
			url: this.url,
		};
	}
}
