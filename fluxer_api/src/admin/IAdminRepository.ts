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
import type {AdminAuditLogRow} from '~/database/types/AdminArchiveTypes';

export type {AdminAuditLogRow};

export interface AdminAuditLog {
	logId: bigint;
	adminUserId: UserID;
	targetType: string;
	targetId: bigint;
	action: string;
	auditLogReason: string | null;
	metadata: Map<string, string>;
	createdAt: Date;
}

export abstract class IAdminRepository {
	abstract createAuditLog(log: AdminAuditLogRow): Promise<AdminAuditLog>;
	abstract getAuditLog(logId: bigint): Promise<AdminAuditLog | null>;
	abstract listAuditLogsByIds(logIds: Array<bigint>): Promise<Array<AdminAuditLog>>;
	abstract listAllAuditLogsPaginated(limit: number, lastLogId?: bigint): Promise<Array<AdminAuditLog>>;

	abstract isIpBanned(ip: string): Promise<boolean>;
	abstract banIp(ip: string): Promise<void>;
	abstract unbanIp(ip: string): Promise<void>;
	abstract listBannedIps(limit?: number): Promise<Array<string>>;

	abstract isEmailBanned(email: string): Promise<boolean>;
	abstract banEmail(email: string): Promise<void>;
	abstract unbanEmail(email: string): Promise<void>;

	abstract isPhoneBanned(phone: string): Promise<boolean>;
	abstract banPhone(phone: string): Promise<void>;
	abstract unbanPhone(phone: string): Promise<void>;

	abstract loadAllBannedIps(): Promise<Set<string>>;

	abstract listPendingVerifications(
		limit?: number,
	): Promise<Array<{userId: UserID; createdAt: Date; metadata: Map<string, string>}>>;
	abstract removePendingVerification(userId: UserID): Promise<void>;
}
