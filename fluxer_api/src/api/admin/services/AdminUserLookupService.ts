// SPDX-License-Identifier: AGPL-3.0-or-later

import type {LookupUserRequest} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';
import type {ApiContext} from '../../ApiContext';
import {createUserID} from '../../BrandedTypes';
import {Logger} from '../../Logger';
import {mapUserToAdminResponse} from '../models/UserTypes';

interface AdminUserLookupServiceDeps {
	apiContext: ApiContext;
}

export class AdminUserLookupService {
	constructor(private readonly deps: AdminUserLookupServiceDeps) {}

	async lookupUser(data: LookupUserRequest, acls: ReadonlySet<string>) {
		const {users: userRepository, cache: cacheService, gateway: gatewayService} = this.deps.apiContext.services;
		if ('user_ids' in data) {
			const userIds = data.user_ids.map((id) => createUserID(id));
			const users = await userRepository.listUsers(userIds);
			return {
				users: await Promise.all(
					users.map((user) =>
						mapUserToAdminResponse(user, cacheService, acls, gatewayService, {includeActivities: false}),
					),
				),
			};
		}
		let user = null;
		const query = data.query.trim();
		const fluxerTagMatch = query.match(/^(.+)#(\d{1,4})$/);
		if (fluxerTagMatch) {
			const username = fluxerTagMatch[1];
			const discriminator = parseInt(fluxerTagMatch[2], 10);
			user = await userRepository.findByUsernameDiscriminator(username, discriminator);
		} else if (/^\d+$/.test(query)) {
			try {
				const userId = createUserID(BigInt(query));
				user = await userRepository.findUnique(userId);
			} catch (error) {
				Logger.debug({query, error}, 'Failed to lookup user by numeric ID, invalid ID format');
				user = null;
			}
		} else if (query.includes('@')) {
			user = await userRepository.findByEmail(query);
		} else {
			user = await userRepository.findByStripeSubscriptionId(query);
		}
		return {
			users: user ? [await mapUserToAdminResponse(user, cacheService, acls, gatewayService)] : [],
		};
	}
}
