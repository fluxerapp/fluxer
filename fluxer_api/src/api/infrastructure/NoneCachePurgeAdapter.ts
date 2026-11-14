// SPDX-License-Identifier: AGPL-3.0-or-later

import type {CachePurgeAdapter} from '@app/api/infrastructure/CachePurgeAdapter';

export function createNoneCachePurgeAdapter(): CachePurgeAdapter {
	return {
		purge: async () => ({kind: 'purged'}),
	};
}
