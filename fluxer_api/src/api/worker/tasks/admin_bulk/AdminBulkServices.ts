// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import {AdminBanManagementService} from '@app/api/admin/services/AdminBanManagementService';
import {AdminGuildService} from '@app/api/admin/services/AdminGuildService';
import {AdminReportService} from '@app/api/admin/services/AdminReportService';
import {AdminUserService} from '@app/api/admin/services/AdminUserService';
import {ReporterResolutionNotifier} from '@app/api/admin/services/ReporterResolutionNotifier';
import {createApiContext} from '@app/api/CreateApiContext';
import {EnforcementDecisionService} from '@app/api/enforcement/EnforcementDecisionService';
import {EnforcementNoticeDelivery} from '@app/api/enforcement/EnforcementNoticeDelivery';
import {getReportServiceInstance} from '@app/api/middleware/ServiceMiddleware';
import {
	getDiscriminatorService,
	getEnforcementDecisionRepository,
	getEntityAssetService,
	getGuildDiscoveryRepository,
	getInviteRepository,
} from '@app/api/middleware/ServiceSingletons';
import type {SystemDmDeps} from '@app/api/user/services/SystemDmMessage';
import {UserChannelService} from '@app/api/user/services/UserChannelService';
import type {WorkerDependencies} from '@app/api/worker/WorkerDependencies';

interface AdminBulkServices {
	auditService: AdminAuditService;
	banManagementService: AdminBanManagementService;
	userService: AdminUserService;
	guildService: AdminGuildService;
}

interface WorkerEnforcementServices {
	apiContext: ApiContext;
	auditService: AdminAuditService;
	banManagementService: AdminBanManagementService;
	reporterResolutionNotifier: ReporterResolutionNotifier;
	delivery: EnforcementNoticeDelivery;
	enforcementService: EnforcementDecisionService;
}

function systemDmDeps(deps: WorkerDependencies): SystemDmDeps {
	return {
		channelService: deps.channelService,
		userChannelService: new UserChannelService(
			deps.userRepository,
			deps.channelService,
			deps.channelRepository,
			deps.gatewayService,
			deps.snowflakeService,
			deps.userPermissionUtils,
			deps.limitConfigService,
		),
		userCacheService: deps.userCacheService,
	};
}

export function createWorkerEnforcementServices(deps: WorkerDependencies): WorkerEnforcementServices {
	const apiContext = createApiContext();
	const auditService = new AdminAuditService(deps.adminRepository, deps.snowflakeService);
	const banManagementService = new AdminBanManagementService({
		apiContext,
		adminRepository: deps.adminRepository,
		auditService,
	});
	const systemDm = systemDmDeps(deps);
	const reporterResolutionNotifier = new ReporterResolutionNotifier({apiContext, systemDm});
	const reportService = getReportServiceInstance();
	const delivery = new EnforcementNoticeDelivery({
		apiContext,
		applicationRepository: deps.applicationRepository,
		systemDm,
	});
	const enforcementService = new EnforcementDecisionService({
		apiContext,
		userUpdateDeps: {
			userCacheService: deps.userCacheService,
			userRepository: deps.userRepository,
			guildRepository: deps.guildRepository,
			gatewayService: deps.gatewayService,
		},
		kvDeletionQueue: deps.deletionQueueService,
		repository: getEnforcementDecisionRepository(),
		delivery,
		auditService,
		banManagementService,
		channelRepository: deps.channelRepository,
		guildRepository: deps.guildRepository,
		reportService,
		adminReportService: new AdminReportService({
			apiContext,
			reportService,
			guildRepository: deps.guildRepository,
			channelRepository: deps.channelRepository,
			storageService: deps.storageService,
			auditService,
			userCacheService: deps.userCacheService,
			reporterResolutionNotifier,
		}),
	});
	return {apiContext, auditService, banManagementService, reporterResolutionNotifier, delivery, enforcementService};
}

export function createAdminBulkServices(deps: WorkerDependencies): AdminBulkServices {
	const {apiContext, auditService, banManagementService, reporterResolutionNotifier, delivery, enforcementService} =
		createWorkerEnforcementServices(deps);
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
		reportService: getReportServiceInstance(),
		storeEntitlementService: deps.storeEntitlementService,
		reporterResolutionNotifier,
		enforcement: enforcementService,
		delivery,
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
		enforcement: enforcementService,
	});
	return {auditService, banManagementService, userService, guildService};
}
