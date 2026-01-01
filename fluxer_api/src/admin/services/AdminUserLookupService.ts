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

import {mapUserToAdminResponse} from '~/admin/AdminModel';
import {createUserID} from '~/BrandedTypes';
import type {ICacheService} from '~/infrastructure/ICacheService';
import type {IUserRepository} from '~/user/IUserRepository';

interface LookupUserRequest {
	query: string;
}

interface AdminUserLookupServiceDeps {
	userRepository: IUserRepository;
	cacheService: ICacheService;
}

export class AdminUserLookupService {
	constructor(private readonly deps: AdminUserLookupServiceDeps) {}

	async lookupUser(data: LookupUserRequest) {
		const {userRepository, cacheService} = this.deps;
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
			} catch {}
		} else if (/^\+\d{1,15}$/.test(query)) {
			user = await userRepository.findByPhone(query);
		} else if (query.includes('@')) {
			user = await userRepository.findByEmail(query);
		} else {
			user = await userRepository.findByStripeSubscriptionId(query);
		}

		return {
			user: user ? await mapUserToAdminResponse(user, cacheService) : null,
		};
	}
}
