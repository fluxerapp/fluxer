// SPDX-License-Identifier: AGPL-3.0-or-later

import {DELETED_USER_ID} from '@fluxer/constants/src/UserConstants';
import {createUserID, type UserID} from '../BrandedTypes';

export const SYSTEM_USER_ID = createUserID(0n);

export function isSyntheticUserId(userId: UserID): boolean {
	return userId === SYSTEM_USER_ID || userId === DELETED_USER_ID;
}
