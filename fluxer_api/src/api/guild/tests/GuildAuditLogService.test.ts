// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildID, createUserID} from '@app/api/BrandedTypes';
import {GuildAuditLogService} from '@app/api/guild/GuildAuditLogService';
import {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import {getSnowflakeService} from '@app/api/middleware/ServiceRegistry';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import {RecordingWorkerService} from '@app/api/test/RecordingWorkerService';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {afterEach, assert, beforeEach, describe, expect, it} from 'vitest';

const GUILD_ID = createGuildID(1420000000000000000n);
const ACTOR_ID = createUserID(1420000000000000001n);
const MEMBER_ID = createUserID(1420000000000000002n);

type GuildDispatch = Parameters<NoopGatewayService['dispatchGuild']>[0];

class RecordingGateway extends NoopGatewayService {
	readonly dispatches: Array<GuildDispatch> = [];

	override async dispatchGuild(params: GuildDispatch): Promise<void> {
		this.dispatches.push(params);
	}
}

describe('Guild audit log service', () => {
	let harness: ApiTestHarness;
	let repository: GuildRepository;
	let gateway: RecordingGateway;
	let worker: RecordingWorkerService;
	let service: GuildAuditLogService;

	beforeEach(async () => {
		harness = await createApiTestHarness();
		repository = new GuildRepository();
		gateway = new RecordingGateway();
		worker = new RecordingWorkerService();
		service = new GuildAuditLogService(repository, getSnowflakeService(), worker, gateway);
	});

	afterEach(async () => {
		await harness?.shutdown();
	});

	it('dispatches the resolved reason for a legacy ban row', async () => {
		const log = await service
			.createBuilder(GUILD_ID, ACTOR_ID)
			.withAction(AuditLogActionType.MEMBER_BAN_ADD, MEMBER_ID.toString())
			.withMetadata({delete_member_days: '0'})
			.withChanges([
				{key: 'user_id', new_value: MEMBER_ID.toString()},
				{key: 'reason', new_value: 'spam'},
			])
			.commit();
		assert(log !== null);
		expect(gateway.dispatches).toEqual([
			{
				guildId: GUILD_ID,
				event: 'GUILD_AUDIT_LOG_ENTRY_CREATE',
				data: {
					id: log.logId.toString(),
					action_type: AuditLogActionType.MEMBER_BAN_ADD,
					user_id: ACTOR_ID.toString(),
					target_id: MEMBER_ID.toString(),
					reason: 'spam',
					options: {delete_member_days: '0'},
					changes: [
						{key: 'user_id', new_value: MEMBER_ID.toString()},
						{key: 'reason', new_value: 'spam'},
					],
					guild_id: GUILD_ID.toString(),
				},
			},
		]);
	});

	it('dispatches the timeout reason without raw timeout options', async () => {
		await service
			.createBuilder(GUILD_ID, ACTOR_ID)
			.withAction(AuditLogActionType.MEMBER_UPDATE, MEMBER_ID.toString())
			.withMetadata({timeout_reason: 'cool off', communication_disabled_until: '2026-09-13T00:00:00.000Z'})
			.withChanges([{key: 'communication_disabled_until', new_value: '2026-09-13T00:00:00.000Z'}])
			.commit();
		expect(gateway.dispatches.map(({data}) => data)).toEqual([
			expect.objectContaining({options: undefined, reason: 'cool off'}),
		]);
	});
});
