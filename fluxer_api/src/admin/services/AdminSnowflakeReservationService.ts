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
import {SNOWFLAKE_RESERVATION_REFRESH_CHANNEL} from '~/constants/InstanceConfig';
import {InputValidationError} from '~/Errors';
import type {ICacheService} from '~/infrastructure/ICacheService';
import type {SnowflakeReservationRepository} from '~/instance/SnowflakeReservationRepository';
import type {AdminAuditService} from './AdminAuditService';

interface AdminSnowflakeReservationServiceDeps {
	repository: SnowflakeReservationRepository;
	cacheService: ICacheService;
	auditService: AdminAuditService;
}

export class AdminSnowflakeReservationService {
	constructor(private readonly deps: AdminSnowflakeReservationServiceDeps) {}

	async listReservations() {
		const {repository} = this.deps;
		const entries = await repository.listReservations();
		return entries.map((entry) => ({
			email: entry.emailKey,
			snowflake: entry.snowflake.toString(),
			updated_at: entry.updatedAt ? entry.updatedAt.toISOString() : null,
		}));
	}

	async setReservation(data: {email: string; snowflake: string}, adminUserId: UserID, auditLogReason: string | null) {
		const {repository, cacheService, auditService} = this.deps;
		const emailLower = data.email.toLowerCase();

		if (!emailLower) {
			throw InputValidationError.create('email', 'Invalid email address');
		}

		let snowflakeValue: bigint;
		try {
			snowflakeValue = BigInt(data.snowflake);
		} catch {
			throw InputValidationError.create('snowflake', 'Invalid snowflake');
		}

		await repository.setReservation(emailLower, snowflakeValue);
		await cacheService.publish(SNOWFLAKE_RESERVATION_REFRESH_CHANNEL, 'refresh');

		await auditService.createAuditLog({
			adminUserId,
			targetType: 'snowflake_reservation',
			targetId: BigInt(0),
			action: 'set_snowflake_reservation',
			auditLogReason,
			metadata: new Map([
				['email', emailLower],
				['snowflake', snowflakeValue.toString()],
			]),
		});
	}

	async deleteReservation(data: {email: string}, adminUserId: UserID, auditLogReason: string | null) {
		const {repository, cacheService, auditService} = this.deps;
		const emailLower = data.email.toLowerCase();

		if (!emailLower) {
			throw InputValidationError.create('email', 'Invalid email address');
		}

		await repository.deleteReservation(emailLower);
		await cacheService.publish(SNOWFLAKE_RESERVATION_REFRESH_CHANNEL, 'refresh');

		await auditService.createAuditLog({
			adminUserId,
			targetType: 'snowflake_reservation',
			targetId: BigInt(0),
			action: 'delete_snowflake_reservation',
			auditLogReason,
			metadata: new Map([['email', emailLower]]),
		});
	}
}
