// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditLog} from '@app/api/admin/IAdminRepository';
import {sendChannelMessage, setupTestGuildWithMembers} from '@app/api/channel/tests/ChannelTestUtils';
import {getGatewayService, getSnowflakeService} from '@app/api/middleware/ServiceRegistry';
import {
	getAdminRepository,
	getChannelRepository,
	getPurgeQueue,
	getStorageService,
} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import bulkDeleteMessagesForUsers from '@app/api/worker/tasks/admin_bulk/BulkDeleteMessagesForUsers';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '@app/api/worker/WorkerContext';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
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

function createHelpers(): WorkerTaskHelpers {
	return {
		logger: new NoopLogger(),
		jobId: 909n,
		addJob: async () => 0n,
		reportProgress: async () => {},
		shouldCancel: async () => false,
		setContextLink: async () => {},
	};
}

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
			channelRepository: getChannelRepository(),
			gatewayService: getGatewayService(),
			storageService: getStorageService(),
			purgeQueue: getPurgeQueue(),
		});
	});

	afterAll(async () => {
		clearWorkerDependencies();
		await harness?.shutdown();
	});

	async function listAuditLogs(): Promise<Array<AdminAuditLog>> {
		return getAdminRepository().listAllAuditLogsPaginated(100);
	}

	async function runTask(userIds: Array<string>, auditLogReason: string | null): Promise<BulkJobResult> {
		return (await bulkDeleteMessagesForUsers(
			{user_ids: userIds, admin_user_id: ADMIN_USER_ID, audit_log_reason: auditLogReason},
			createHelpers(),
		)) as unknown as BulkJobResult;
	}

	async function listMessages(token: string, channelId: string): Promise<Array<MessageResponse>> {
		return createBuilder<Array<MessageResponse>>(harness, token).get(`/channels/${channelId}/messages`).execute();
	}

	async function countMessagesBy(token: string, channelId: string, userId: string): Promise<number> {
		const messages = await listMessages(token, channelId);
		return messages.filter((message) => message.author.id === userId).length;
	}

	it('carries the admin reason and the message count on the per-user audit row', async () => {
		const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		await sendChannelMessage(harness, member.token, systemChannel.id, 'first spam');
		await sendChannelMessage(harness, member.token, systemChannel.id, 'second spam');
		const authored = await countMessagesBy(owner.token, systemChannel.id, member.userId);

		const result = await runTask([member.userId], 'Spam cleanup');

		expect(result.successful_count).toBe(1);
		const logs = await listAuditLogs();
		const perUser = logs.filter((log) => log.action === 'delete_all_user_messages');
		expect(perUser).toHaveLength(1);
		expect(perUser[0]!.adminUserId.toString()).toBe(ADMIN_USER_ID);
		expect(perUser[0]!.targetType).toBe('message_deletion');
		expect(perUser[0]!.targetId.toString()).toBe(member.userId);
		expect(perUser[0]!.auditLogReason).toBe('Spam cleanup');
		expect(Object.fromEntries(perUser[0]!.metadata)).toEqual({
			user_id: member.userId,
			message_count: authored.toString(),
		});
		expect(await countMessagesBy(owner.token, systemChannel.id, member.userId)).toBe(0);
	});

	it('summarises the job and reports a user that could not be processed', async () => {
		const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		await sendChannelMessage(harness, member.token, systemChannel.id, 'only spam');
		const authored = await countMessagesBy(owner.token, systemChannel.id, member.userId);

		const result = await runTask(['0', member.userId], 'Spam cleanup');

		expect(result.successful_count).toBe(1);
		expect(result.failed_count).toBe(1);
		expect(result.failed[0]!.id).toBe('0');
		const summary = (await listAuditLogs()).find((log) => log.action === 'bulk_delete_user_messages');
		expect(summary).toBeDefined();
		expect(summary?.targetType).toBe('bulk_job');
		expect(summary?.auditLogReason).toBe('Spam cleanup');
		expect(Object.fromEntries(summary!.metadata)).toMatchObject({
			user_count: '2',
			message_count: authored.toString(),
			processed: '2',
			successful: '1',
			failed: '1',
		});
	});
});
