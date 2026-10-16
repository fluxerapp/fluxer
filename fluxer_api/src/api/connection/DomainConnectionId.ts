// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import type {UserID} from '@app/api/BrandedTypes';

export function createDomainConnectionId(userId: UserID, identifier: string): string {
	return createHash('sha256')
		.update(`${String(userId)}:${identifier.toLowerCase()}`)
		.digest('hex');
}
