// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/api/Logger';
import type {User} from '@app/api/models/User';
import {getUserSearchService} from '@app/api/SearchFactory';

export class UserSearchRepository {
	async indexUser(user: User): Promise<void> {
		const userSearchService = getUserSearchService();
		if (userSearchService) {
			await userSearchService.indexUser(user).catch((error) => {
				Logger.error({userId: user.id, error}, 'Failed to index user in search');
			});
		}
	}

	async updateUser(user: User): Promise<void> {
		const userSearchService = getUserSearchService();
		if (userSearchService) {
			await userSearchService.updateUser(user).catch((error) => {
				Logger.error({userId: user.id, error}, 'Failed to update user in search');
			});
		}
	}
}
