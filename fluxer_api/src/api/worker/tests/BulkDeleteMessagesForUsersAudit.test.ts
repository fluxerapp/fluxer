// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditLog} from '@app/api/admin/AdminRepository';
import {sendChannelMessage, setupTestGuildWithMembers} from '@app/api/channel/tests/ChannelTestUtils';
import {createTestChannelService} from '@app/api/channel/tests/CrosspostTestUtils';
import {createStaff, decisionBody, listDecisions} from '@app/api/enforcement/tests/EnforcementTestUtils';
import {getGatewayService, getSnowflakeService} from '@app/api/middleware/ServiceRegistry';
import {
	createUserCacheService,
	getAdminRepository,
	getChannelRepository,
	getGuildRepository,
	getKVAccountDeletionQueue,
	getKVBulkMessageDeletionQueue,
	getLimitConfigService,
	getUserPermissionUtils,
	getUserRepository,
} from '@app/api/middleware/ServiceSingletons';
import {createTestStoreEntitlementService} from '@app/api/store_billing/tests/StoreBillingTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createTestWorkerHelpers} from '@app/api/test/SyncTaskWorkerService';
import bulkDeleteMessagesForUsers from '@app/api/worker/tasks/admin_bulk/BulkDeleteMessagesForUsers';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '@app/api/worker/WorkerContext';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface BulkJobResult {
	successful_count: number;
	failed_count: number;
	failed: Array<{
		id: string;
		error: string;
	}>;
}

const ADMIN_USER_ID = '9';

describe('Bulk delete messages for users', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		setWorkerDependenciesForTest({
			adminRepository: getAdminRepository(),
			snowflakeService: getSnowflakeService(),
			userRepository: getUserRepository(),
			userCacheService: createUserCacheService(),
			guildRepository: getGuildRepository(),
			channelRepository: getChannelRepository(),
			gatewayService: getGatewayService(),
			channelService: createTestChannelService(),
			userPermissionUtils: getUserPermissionUtils(),
			limitConfigService: getLimitConfigService(),
			deletionQueueService: getKVAccountDeletionQueue(),
			bulkMessageDeletionQueueService: getKVBulkMessageDeletionQueue(),
			stripe: null,
			storeEntitlementService: createTestStoreEntitlementService(),
		});
	});

	afterAll(async () => {
		clearWorkerDependencies();
		await harness?.shutdown();
	});

	async function listAuditLogs(): Promise<Array<AdminAuditLog>> {
		return getAdminRepository().listAllAuditLogsPaginated(100);
	}

	async function runTask(
		userIds: Array<string>,
		basis: Record<string, unknown>,
		auditLogReason: string | null = 'Spam cleanup',
	): Promise<BulkJobResult> {
		return (await bulkDeleteMessagesForUsers(
			{user_ids: userIds, basis, admin_user_id: ADMIN_USER_ID, audit_log_reason: auditLogReason},
			createTestWorkerHelpers({jobId: 909n}),
		)) as unknown as BulkJobResult;
	}

	it('records the basis and message count on the per-user audit row', async () => {
		const {members, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		await sendChannelMessage(harness, member.token, systemChannel.id, 'first');
		await sendChannelMessage(harness, member.token, systemChannel.id, 'second');

		const result = await runTask([member.userId], {type: 'housekeeping'});

		expect(result.successful_count).toBe(1);
		const perUser = (await listAuditLogs()).filter((log) => log.action === 'delete_all_user_messages');
		expect(perUser).toHaveLength(1);
		expect(perUser[0]!.adminUserId.toString()).toBe(ADMIN_USER_ID);
		expect(perUser[0]!.targetType).toBe('message_deletion');
		expect(perUser[0]!.targetId.toString()).toBe(member.userId);
		expect(perUser[0]!.auditLogReason).toBe('Spam cleanup');
		expect(Object.fromEntries(perUser[0]!.metadata)).toMatchObject({
			user_id: member.userId,
			dry_run: 'false',
			basis: 'housekeeping',
		});
		const summary = (await listAuditLogs()).find((log) => log.action === 'bulk_delete_user_messages');
		expect(summary?.metadata.get('message_count')).toBe(perUser[0]!.metadata.get('message_count'));
	});

	it('records one content removal decision for each account under an enforcement basis', async () => {
		const {members, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		const staff = await createStaff(harness);
		await sendChannelMessage(harness, member.token, systemChannel.id, 'invite spam');

		const result = await runTask([member.userId], {type: 'enforcement', decision: decisionBody()});

		expect(result.successful_count).toBe(1);
		const [decision] = await listDecisions(harness, staff, member.userId);
		expect(decision).toMatchObject({kind: 'content_removal', removed_all_messages: true, reason_code: 3});
		const summary = (await listAuditLogs()).find((log) => log.action === 'bulk_delete_user_messages');
		expect(Object.fromEntries(summary!.metadata)).toMatchObject({basis: 'enforcement', reason_code: '3'});
	});

	it('refuses a restricted reason for every account', async () => {
		const {members, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		await sendChannelMessage(harness, member.token, systemChannel.id, 'content');

		const result = await runTask([member.userId], {
			type: 'enforcement',
			decision: decisionBody({reason_code: DeletionReasons.VIOLENT_EXTREMISM, statement: 'Not shown'}),
		});

		expect(result.successful_count).toBe(0);
		expect(result.failed_count).toBe(1);
	});
});
