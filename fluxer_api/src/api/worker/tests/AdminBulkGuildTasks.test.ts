// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditLog} from '@app/api/admin/IAdminRepository';
import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuildID, createUserID} from '@app/api/BrandedTypes';
import {createApiContext} from '@app/api/CreateApiContext';
import {GuildDiscoveryRepository} from '@app/api/guild/repositories/GuildDiscoveryRepository';
import {createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {DisabledLiveKitService} from '@app/api/infrastructure/DisabledLiveKitService';
import {InMemoryVoiceRoomStore} from '@app/api/infrastructure/InMemoryVoiceRoomStore';
import {getMessages} from '@app/api/message/tests/MessageTestUtils';
import {createGuildStackServices} from '@app/api/middleware/GuildStackServiceFactory';
import {getIpInfoService} from '@app/api/middleware/ServiceMiddleware';
import {getGatewayService, getSnowflakeService, getVoiceAvailabilityService} from '@app/api/middleware/ServiceRegistry';
import {
	getAdminRepository,
	getAssetDeletionQueue,
	getAttachmentUploadTraceRepository,
	getAvatarService,
	getChannelRepository,
	getEmbedService,
	getEntityAssetService,
	getFavoriteMemeRepository,
	getGuildAuditLogService,
	getGuildRepository,
	getInviteRepository,
	getKVAccountDeletionQueue,
	getKVBulkMessageDeletionQueue,
	getLimitConfigService,
	getPurgeQueue,
	getReadStateService,
	getStorageService,
	getUserCacheService,
	getUserRepository,
	getVirusScanServiceInstance,
	getWebhookRepository,
} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import bulkAddGuildMembers from '@app/api/worker/tasks/admin_bulk/BulkAddGuildMembers';
import bulkUpdateGuildFeatures from '@app/api/worker/tasks/admin_bulk/BulkUpdateGuildFeatures';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '@app/api/worker/WorkerContext';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {DiscoveryApplicationStatus, DiscoveryCategories} from '@fluxer/constants/src/DiscoveryConstants';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

const JOB_ID = 4242n;
const MISSING_ID = '123456789012345678';

interface BulkJobResult {
	successful_count: number;
	failed_count: number;
	failed: Array<{
		id: string;
		error: string;
	}>;
}

function createHelpers(): WorkerTaskHelpers {
	return {
		logger: new NoopLogger(),
		jobId: JOB_ID,
		addJob: async () => 0n,
		reportProgress: async () => {},
		shouldCancel: async () => false,
		setContextLink: async () => {},
	};
}

function installWorkerDependencies(): void {
	const guildStack = createGuildStackServices({
		apiContext: createApiContext(),
		channelRepository: getChannelRepository(),
		userRepository: getUserRepository(),
		guildRepository: getGuildRepository(),
		inviteRepository: getInviteRepository(),
		webhookRepository: getWebhookRepository(),
		favoriteMemeRepository: getFavoriteMemeRepository(),
		avatarService: getAvatarService(),
		entityAssetService: getEntityAssetService(),
		assetDeletionQueue: getAssetDeletionQueue(),
		userCacheService: getUserCacheService(),
		limitConfigService: getLimitConfigService(),
		embedService: getEmbedService(),
		readStateService: getReadStateService(),
		storageService: getStorageService(),
		attachmentUploadTraceRepository: getAttachmentUploadTraceRepository(),
		virusScanService: getVirusScanServiceInstance(),
		purgeQueue: getPurgeQueue(),
		guildAuditLogService: getGuildAuditLogService(),
		voiceRoomStore: new InMemoryVoiceRoomStore(),
		liveKitService: new DisabledLiveKitService(),
		voiceAvailabilityService: getVoiceAvailabilityService(),
		ipInfoService: getIpInfoService(),
	});
	setWorkerDependenciesForTest({
		adminRepository: getAdminRepository(),
		snowflakeService: getSnowflakeService(),
		userRepository: getUserRepository(),
		guildRepository: getGuildRepository(),
		channelRepository: getChannelRepository(),
		userCacheService: getUserCacheService(),
		gatewayService: getGatewayService(),
		deletionQueueService: getKVAccountDeletionQueue(),
		bulkMessageDeletionQueueService: getKVBulkMessageDeletionQueue(),
		guildService: guildStack.guildService,
		stripe: null,
	});
}

async function listAuditLogs(): Promise<Array<AdminAuditLog>> {
	return getAdminRepository().listAllAuditLogsPaginated(500);
}

function findAuditLog(logs: Array<AdminAuditLog>, action: string, targetId?: bigint): AdminAuditLog | undefined {
	return logs.find((log) => log.action === action && (targetId === undefined || log.targetId === targetId));
}

describe('admin bulk guild worker tasks', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness({search: 'enabled'});
		installWorkerDependencies();
	});
	afterEach(async () => {
		clearWorkerDependencies();
		await harness.shutdown();
	});

	async function createAdmin(acls: Array<string>): Promise<TestAccount> {
		const admin = await createTestAccount(harness);
		return setUserACLs(harness, admin, ['admin:authenticate', ...acls]);
	}

	async function createDiscoveryApplicant(admin: TestAccount, name: string): Promise<GuildResponse> {
		const guild = await createGuild(harness, admin.token, name);
		await createBuilder(harness, '').post(`/test/guilds/${guild.id}/member-count`).body({member_count: 10}).execute();
		await createBuilder(harness, admin.token)
			.post(`/guilds/${guild.id}/discovery`)
			.body({description: 'A guild worth discovering', category_type: DiscoveryCategories.GAMING})
			.expect(HTTP_STATUS.OK)
			.execute();
		return guild;
	}

	async function runFeaturesJob(
		admin: TestAccount,
		guildIds: Array<string>,
		auditLogReason: string | null,
	): Promise<BulkJobResult> {
		const result = await bulkUpdateGuildFeatures(
			{
				guild_ids: guildIds,
				add_features: [GuildFeatures.DISCOVERABLE],
				remove_features: [],
				admin_user_id: admin.userId,
				audit_log_reason: auditLogReason,
			},
			createHelpers(),
		);
		return result as unknown as BulkJobResult;
	}

	async function runAddMembersJob(
		admin: TestAccount,
		guildId: string,
		userIds: Array<string>,
		auditLogReason: string | null,
	): Promise<BulkJobResult> {
		const result = await bulkAddGuildMembers(
			{
				guild_id: guildId,
				user_ids: userIds,
				admin_user_id: admin.userId,
				audit_log_reason: auditLogReason,
			},
			createHelpers(),
		);
		return result as unknown as BulkJobResult;
	}

	test('a bulk guild-features job writes a per-guild update_features row with the admin reason', async () => {
		const admin = await createAdmin(['guild:update:features']);
		const guild = await createGuild(harness, admin.token, `Bulk Features Guild ${Date.now()}`);
		const reason = 'Lilith ticket 4821';

		const result = await runFeaturesJob(admin, [guild.id], reason);

		expect(result.successful_count).toBe(1);
		expect(result.failed_count).toBe(0);
		const logs = await listAuditLogs();
		const perGuild = findAuditLog(logs, 'update_features', BigInt(guild.id));
		expect(perGuild?.targetType).toBe('guild');
		expect(perGuild?.auditLogReason).toBe(reason);
		expect(perGuild?.metadata.get('add_features')).toBe(GuildFeatures.DISCOVERABLE);
		expect(perGuild?.metadata.get('new_features')).toContain(GuildFeatures.DISCOVERABLE);
		const summary = findAuditLog(logs, 'bulk_update_guild_features');
		expect(summary?.targetType).toBe('bulk_job');
		expect(summary?.targetId).toBe(JOB_ID);
		expect(summary?.auditLogReason).toBe(reason);
		expect(summary?.metadata.get('guild_count')).toBe('1');
		expect(summary?.metadata.get('add_features')).toBe(GuildFeatures.DISCOVERABLE);
		expect(summary?.metadata.get('remove_features')).toBe('');
		expect(summary?.metadata.get('successful')).toBe('1');
		expect(summary?.metadata.get('failed')).toBe('0');
		const updatedGuild = await getGuildRepository().findUnique(createGuildID(BigInt(guild.id)));
		expect(updatedGuild?.features.has(GuildFeatures.DISCOVERABLE)).toBe(true);
	});

	test('a bulk guild-features job reconciles discovery exactly like the single-guild endpoint', async () => {
		const admin = await createAdmin(['guild:update:features']);
		const bulkGuild = await createDiscoveryApplicant(admin, `Bulk Discovery Guild ${Date.now()}`);
		const singleGuild = await createDiscoveryApplicant(admin, `Single Discovery Guild ${Date.now()}`);
		const reason = 'Lilith ticket 4822';

		await runFeaturesJob(admin, [bulkGuild.id], reason);
		await createBuilder(harness, admin.token)
			.patch(`/admin/guilds/${singleGuild.id}`)
			.header('X-Audit-Log-Reason', reason)
			.body({add_features: [GuildFeatures.DISCOVERABLE]})
			.expect(HTTP_STATUS.OK)
			.execute();

		const discoveryRepository = new GuildDiscoveryRepository();
		const bulkRow = await discoveryRepository.findByGuildId(createGuildID(BigInt(bulkGuild.id)));
		const singleRow = await discoveryRepository.findByGuildId(createGuildID(BigInt(singleGuild.id)));
		expect(bulkRow?.status).toBe(DiscoveryApplicationStatus.APPROVED);
		expect(singleRow?.status).toBe(DiscoveryApplicationStatus.APPROVED);
		expect(bulkRow?.reviewed_by?.toString()).toBe(admin.userId);
		expect(singleRow?.reviewed_by?.toString()).toBe(admin.userId);
		expect(bulkRow?.review_reason).toBe(singleRow?.review_reason);
	});

	test('a bulk guild-features job surfaces an unknown guild in the job result', async () => {
		const admin = await createAdmin(['guild:update:features']);
		const guild = await createGuild(harness, admin.token, `Partial Features Guild ${Date.now()}`);

		const result = await runFeaturesJob(admin, [guild.id, MISSING_ID], 'Lilith ticket 4823');

		expect(result.successful_count).toBe(1);
		expect(result.failed_count).toBe(1);
		expect(result.failed).toEqual([{id: MISSING_ID, error: APIErrorCodes.UNKNOWN_GUILD}]);
		const summary = findAuditLog(await listAuditLogs(), 'bulk_update_guild_features');
		expect(summary?.metadata.get('failed')).toBe('1');
	});

	test('a bulk add-members job adds each member with a per-user force_add_to_guild row and no join message', async () => {
		const admin = await createAdmin(['guild:force_add_member']);
		const guild = await createGuild(harness, admin.token, `Bulk Members Guild ${Date.now()}`);
		const bulkTarget = await createTestAccount(harness);
		const singleTarget = await createTestAccount(harness);
		const reason = 'Lilith ticket 4824';

		const result = await runAddMembersJob(admin, guild.id, [bulkTarget.userId], reason);
		await createBuilder(harness, admin.token)
			.put(`/admin/guilds/${guild.id}/members/${singleTarget.userId}`)
			.header('X-Audit-Log-Reason', reason)
			.body(null)
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(result.successful_count).toBe(1);
		expect(result.failed_count).toBe(0);
		const member = await getGuildRepository().getMember(
			createGuildID(BigInt(guild.id)),
			createUserID(BigInt(bulkTarget.userId)),
		);
		expect(member).not.toBeNull();
		const logs = await listAuditLogs();
		const perUser = findAuditLog(logs, 'force_add_to_guild', BigInt(bulkTarget.userId));
		expect(perUser?.targetType).toBe('user');
		expect(perUser?.auditLogReason).toBe(reason);
		expect(perUser?.metadata.get('guild_id')).toBe(guild.id);
		const summary = findAuditLog(logs, 'bulk_add_guild_members');
		expect(summary?.targetType).toBe('guild');
		expect(summary?.targetId).toBe(BigInt(guild.id));
		expect(summary?.auditLogReason).toBe(reason);
		expect(summary?.metadata.get('job_id')).toBe(JOB_ID.toString());
		expect(summary?.metadata.get('guild_id')).toBe(guild.id);
		expect(summary?.metadata.get('user_count')).toBe('1');
		expect(summary?.metadata.get('successful')).toBe('1');
		const systemChannelId = guild.system_channel_id;
		expect(systemChannelId).toBeTruthy();
		const messages = await getMessages(harness, admin.token, String(systemChannelId));
		const joinAuthorIds = messages
			.filter((message) => message.type === MessageTypes.USER_JOIN)
			.map((message) => message.author?.id);
		expect(joinAuthorIds).toContain(singleTarget.userId);
		expect(joinAuthorIds).not.toContain(bulkTarget.userId);
	});

	test('a bulk add-members job reports an unknown user as an unknown user, not an unknown guild', async () => {
		const admin = await createAdmin(['guild:force_add_member']);
		const guild = await createGuild(harness, admin.token, `Partial Members Guild ${Date.now()}`);
		const target = await createTestAccount(harness);

		const result = await runAddMembersJob(admin, guild.id, [target.userId, MISSING_ID], 'Lilith ticket 4825');

		expect(result.successful_count).toBe(1);
		expect(result.failed_count).toBe(1);
		expect(result.failed).toEqual([{id: MISSING_ID, error: APIErrorCodes.UNKNOWN_USER}]);
		const summary = findAuditLog(await listAuditLogs(), 'bulk_add_guild_members');
		expect(summary?.metadata.get('failed')).toBe('1');
	});
});
