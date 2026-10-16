// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserConnectionRow} from '@app/api/database/types/ConnectionTypes';
import type {ConnectionResponse} from '@fluxer/schema/src/domains/connection/ConnectionSchemas';

export function mapConnectionToResponse(row: UserConnectionRow): ConnectionResponse {
	return {
		id: row.connection_id,
		type: row.connection_type,
		name: row.name,
		verified: row.verified,
		visibility_flags: row.visibility_flags,
		sort_order: row.sort_order,
	};
}
