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
import type {SnowflakeService} from '~/infrastructure/SnowflakeService';
import {Logger} from '~/Logger';
import type {AdminAuditLog, IAdminRepository} from '../IAdminRepository';

interface CreateAdminAuditLogParams {
	adminUserId: UserID;
	targetType: string;
	targetId: bigint;
	action: string;
	auditLogReason: string | null;
	metadata?: Map<string, string>;
}

export class AdminAuditService {
	constructor(
		private readonly adminRepository: IAdminRepository,
		private readonly snowflakeService: SnowflakeService,
	) {}

	async createAuditLog({
		adminUserId,
		targetType,
		targetId,
		action,
		auditLogReason,
		metadata = new Map(),
	}: CreateAdminAuditLogParams): Promise<void> {
		const log = await this.adminRepository.createAuditLog({
			log_id: this.snowflakeService.generate(),
			admin_user_id: adminUserId,
			target_type: targetType,
			target_id: targetId,
			action,
			audit_log_reason: auditLogReason ?? null,
			metadata,
			created_at: new Date(),
		});

		const {getAuditLogSearchService} = await import('~/Meilisearch');
		const auditLogSearchService = getAuditLogSearchService();
		if (auditLogSearchService) {
			auditLogSearchService.indexAuditLog(log).catch((error) => {
				Logger.error({error, logId: log.logId}, 'Failed to index audit log to Meilisearch');
			});
		}
	}

	async listAuditLogs(data: {
		adminUserId?: bigint;
		targetType?: string;
		targetId?: bigint;
		limit?: number;
		offset?: number;
	}): Promise<{logs: Array<AdminAuditLogResponse>; total: number}> {
		const auditLogSearchService = await this.requireAuditLogSearchService();

		const limit = data.limit || 50;
		const filters: Record<string, string> = {};

		if (data.adminUserId) {
			filters.adminUserId = data.adminUserId.toString();
		}
		if (data.targetType) {
			filters.targetType = data.targetType;
		}
		if (data.targetId) {
			filters.targetId = data.targetId.toString();
		}

		const {hits, total} = await auditLogSearchService.searchAuditLogs('', filters, {
			limit,
			offset: data.offset || 0,
		});

		const orderedLogs = await this.loadLogsInSearchOrder(hits.map((hit) => BigInt(hit.logId)));

		return {
			logs: orderedLogs.map((log) => this.toResponse(log)),
			total,
		};
	}

	async searchAuditLogs(data: {
		query?: string;
		adminUserId?: bigint;
		targetType?: string;
		targetId?: bigint;
		action?: string;
		sortBy?: 'createdAt' | 'relevance';
		sortOrder?: 'asc' | 'desc';
		limit?: number;
		offset?: number;
	}): Promise<{logs: Array<AdminAuditLogResponse>; total: number}> {
		const auditLogSearchService = await this.requireAuditLogSearchService();

		const filters: Record<string, string> = {};

		if (data.adminUserId) {
			filters.adminUserId = data.adminUserId.toString();
		}
		if (data.targetType) {
			filters.targetType = data.targetType;
		}
		if (data.targetId) {
			filters.targetId = data.targetId.toString();
		}
		if (data.action) {
			filters.action = data.action;
		}
		if (data.sortBy) {
			filters.sortBy = data.sortBy;
		}
		if (data.sortOrder) {
			filters.sortOrder = data.sortOrder;
		}

		const {hits, total} = await auditLogSearchService.searchAuditLogs(data.query || '', filters, {
			limit: data.limit || 50,
			offset: data.offset || 0,
		});

		const orderedLogs = await this.loadLogsInSearchOrder(hits.map((hit) => BigInt(hit.logId)));

		return {
			logs: orderedLogs.map((log) => this.toResponse(log)),
			total,
		};
	}

	private async requireAuditLogSearchService() {
		const {getAuditLogSearchService} = await import('~/Meilisearch');
		const auditLogSearchService = getAuditLogSearchService();

		if (!auditLogSearchService) {
			throw new Error('Audit log search service not available');
		}

		return auditLogSearchService;
	}

	private async loadLogsInSearchOrder(logIds: Array<bigint>): Promise<Array<AdminAuditLog>> {
		const logs = await this.adminRepository.listAuditLogsByIds(logIds);
		const logMap = new Map(logs.map((log) => [log.logId.toString(), log]));
		return logIds.map((logId) => logMap.get(logId.toString())).filter((log): log is AdminAuditLog => log !== undefined);
	}

	private toResponse(log: AdminAuditLog): AdminAuditLogResponse {
		return {
			log_id: log.logId.toString(),
			admin_user_id: log.adminUserId.toString(),
			target_type: log.targetType,
			target_id: log.targetId.toString(),
			action: log.action,
			audit_log_reason: log.auditLogReason,
			metadata: Object.fromEntries(log.metadata),
			created_at: log.createdAt.toISOString(),
		};
	}
}

interface AdminAuditLogResponse {
	log_id: string;
	admin_user_id: string;
	target_type: string;
	target_id: string;
	action: string;
	audit_log_reason: string | null;
	metadata: Record<string, string>;
	created_at: string;
}
