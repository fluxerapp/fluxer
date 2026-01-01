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

import type {UserID} from '~/BrandedTypes';
import type {ICacheService} from '~/infrastructure/ICacheService';

const PENDING_INVITE_TTL_SECONDS = 90 * 24 * 60 * 60;
const PENDING_INVITE_KEY_PREFIX = 'pending-join-invite';

export class PendingJoinInviteStore {
	constructor(private readonly cacheService: ICacheService) {}

	private getKey(userId: UserID): string {
		return `${PENDING_INVITE_KEY_PREFIX}:${userId}`;
	}

	async setPendingInvite(userId: UserID, inviteCode: string): Promise<void> {
		await this.cacheService.set(this.getKey(userId), inviteCode, PENDING_INVITE_TTL_SECONDS);
	}

	async getPendingInvite(userId: UserID): Promise<string | null> {
		return this.cacheService.get<string>(this.getKey(userId));
	}

	async deletePendingInvite(userId: UserID): Promise<void> {
		await this.cacheService.delete(this.getKey(userId));
	}
}
