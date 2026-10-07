// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import {setCassandraQueryExecutorForTesting} from '@app/api/database/CassandraQueryExecution';
import type {PreparedQuery} from '@app/api/database/CassandraTypes';
import type {PushSubscriptionRow} from '@app/api/database/types/UserTypes';
import {syncChannelThreadsConfig} from '@app/api/experiment/ChannelThreadsGate';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {PushSubscriptionRepository} from '@app/api/user/repositories/PushSubscriptionRepository';
import {ChannelThreadsConfigSchema} from '@fluxer/schema/src/domains/admin/ChannelThreadsSchemas';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

const USER_ID = createUserID(1000000000000000000n);

class CountingExecutor extends InMemoryCassandraQueryExecutor {
	selects = 0;

	override async executeQuery<T>(query: PreparedQuery): Promise<Array<T>> {
		if (/^\s*SELECT/i.test(query.cql)) this.selects++;
		return super.executeQuery<T>(query);
	}
}

let executor: CountingExecutor;

function row(threadChannels?: boolean): PushSubscriptionRow {
	return {
		user_id: USER_ID,
		subscription_id: 'device',
		auth_session_id_hash: null,
		endpoint: 'fcm:token',
		p256dh_key: null,
		auth_key: null,
		user_agent: null,
		platform: 'android_fcm',
		app_id: null,
		provider_environment: null,
		...(threadChannels === undefined ? {} : {thread_channels: threadChannels}),
	};
}

function setEverEnabled(everEnabled: boolean) {
	syncChannelThreadsConfig(JSON.stringify({ever_enabled: everEnabled}), (raw) =>
		ChannelThreadsConfigSchema.parse(JSON.parse(raw ?? '{}')),
	);
}

async function storedCapability(repository: PushSubscriptionRepository) {
	const [subscription] = await repository.listPushSubscriptions(USER_ID);
	return subscription?.threadChannels;
}

describe('PushSubscriptionRepository.createPushSubscription', () => {
	beforeEach(() => {
		executor = new CountingExecutor();
		setCassandraQueryExecutorForTesting(executor);
	});
	afterEach(() => {
		executor.reset();
		setCassandraQueryExecutorForTesting(null);
		syncChannelThreadsConfig(null, () => ChannelThreadsConfigSchema.parse({}));
	});

	it('does no read and leaves the capability column unwritten on a never-enabled instance', async () => {
		const repository = new PushSubscriptionRepository();
		await repository.createPushSubscription(row(true));
		setEverEnabled(false);
		executor.selects = 0;
		await repository.createPushSubscription(row());
		expect(executor.selects).toBe(0);
		expect(await storedCapability(repository)).toBe(true);
	});

	it('clears a stored capability when a device re-registers without the header', async () => {
		setEverEnabled(true);
		const repository = new PushSubscriptionRepository();
		await repository.createPushSubscription(row(true));
		await repository.createPushSubscription(row());
		expect(await storedCapability(repository)).toBeFalsy();
	});
});
