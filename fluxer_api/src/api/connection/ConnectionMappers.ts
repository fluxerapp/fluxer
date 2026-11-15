// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type ConnectionProviderIndex,
	findAvailableConnectionProvider,
	renderProfileUrl,
} from '@app/api/connection/ConnectionProviders';
import type {UserConnectionRow} from '@app/api/database/types/ConnectionTypes';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import {ConnectionTypes} from '@fluxer/constants/src/ConnectionConstants';
import type {ConnectionResponse} from '@fluxer/schema/src/domains/connection/ConnectionSchemas';

function connectionLink(
	row: UserConnectionRow,
	providers: ConnectionProviderIndex,
): Pick<ConnectionResponse, 'provider_id' | 'url'> {
	switch (row.connection_type) {
		case ConnectionTypes.BLUESKY:
			return {provider_id: null, url: `https://bsky.app/profile/${row.identifier}`};
		case ConnectionTypes.DOMAIN:
			return {provider_id: null, url: `https://${row.identifier}`};
		case ConnectionTypes.OAUTH: {
			const available = findAvailableConnectionProvider(row.identifier, providers);
			if (!available) return {provider_id: null, url: null};
			return {
				provider_id: available.entry.provider.id,
				url: renderProfileUrl(available.entry.provider.profile_url_template, available.parsed.subject, row.name),
			};
		}
	}
}

export function isConnectionAvailable(row: UserConnectionRow, providers: ConnectionProviderIndex): boolean {
	return (
		row.connection_type !== ConnectionTypes.OAUTH || findAvailableConnectionProvider(row.identifier, providers) !== null
	);
}

function mapConnectionToResponse(row: UserConnectionRow, providers: ConnectionProviderIndex): ConnectionResponse {
	return {
		id: row.connection_id,
		type: row.connection_type,
		name: row.name,
		verified: row.verified,
		visibility_flags: row.visibility_flags,
		sort_order: row.sort_order,
		...connectionLink(row, providers),
	};
}

export async function mapConnectionsToResponse(
	rows: ReadonlyArray<UserConnectionRow>,
): Promise<Array<ConnectionResponse>> {
	const providers = await getInstanceConfigRepository().getConnectionProviderIndex();
	return [...rows].sort((a, b) => a.sort_order - b.sort_order).map((row) => mapConnectionToResponse(row, providers));
}
