// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import {AdminBanManagementService} from '@app/api/admin/services/AdminBanManagementService';
import {AdminGuildService} from '@app/api/admin/services/AdminGuildService';
import {AdminUserService} from '@app/api/admin/services/AdminUserService';
import {createApiContext} from '@app/api/CreateApiContext';
import {
	getHistoricalOutcomeRepository,
	getIpInfoService,
	getReportServiceInstance,
	getSuspiciousIpRepository,
} from '@app/api/middleware/ServiceMiddleware';
import {
	getDiscriminatorService,
	getEntityAssetService,
	getGuildDiscoveryRepository,
	getInviteRepository,
} from '@app/api/middleware/ServiceSingletons';
import type {WorkerDependencies} from '@app/api/worker/WorkerDependencies';

interface AdminBulkServices {
	auditService: AdminAuditService;
	banManagementService: AdminBanManagementService;
	userService: AdminUserService;
	guildService: AdminGuildService;
}

export function createAdminBulkServices(deps: WorkerDependencies): AdminBulkServices {
	const apiContext = createApiContext();
	const auditService = new AdminAuditService(deps.adminRepository, deps.snowflakeService);
	const banManagementService = new AdminBanManagementService({
		apiContext,
		adminRepository: deps.adminRepository,
		auditService,
		ipInfoService: getIpInfoService(),
		suspiciousIpRepository: getSuspiciousIpRepository(),
	});
	const userService = new AdminUserService({
		apiContext,
		guildRepository: deps.guildRepository,
		channelRepository: deps.channelRepository,
		discriminatorService: getDiscriminatorService(),
		entityAssetService: getEntityAssetService(),
		auditService,
		userCacheService: deps.userCacheService,
		banManagementService,
		kvDeletionQueue: deps.deletionQueueService,
		bulkMessageDeletionQueue: deps.bulkMessageDeletionQueueService,
		stripe: deps.stripe,
		riskHistoryRepository: getHistoricalOutcomeRepository(),
		reportService: getReportServiceInstance(),
	});
	const guildService = new AdminGuildService({
		guildRepository: deps.guildRepository,
		userRepository: deps.userRepository,
		channelRepository: deps.channelRepository,
		inviteRepository: getInviteRepository(),
		guildService: deps.guildService,
		gatewayService: deps.gatewayService,
		entityAssetService: getEntityAssetService(),
		auditService,
		discoveryRepository: getGuildDiscoveryRepository(),
	});
	return {auditService, banManagementService, userService, guildService};
}
