// SPDX-License-Identifier: AGPL-3.0-or-later

import type {TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {Config} from '@app/api/Config';
import {sendChannelMessage} from '@app/api/channel/tests/ChannelTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {InstanceConfigResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import type {WellKnownFluxerResponse} from '@fluxer/schema/src/domains/instance/InstanceSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('personal notes instance policy', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;
	const originalSelfHosted = Config.instance.selfHosted;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_CONFIG_VIEW,
			AdminACLs.INSTANCE_CONFIG_UPDATE,
		]);
	});

	afterEach(() => {
		Config.instance.selfHosted = originalSelfHosted;
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	const setPersonalNotesEnabled = (enabled: boolean) =>
		createBuilder<InstanceConfigResponse>(harness, admin.token)
			.patch('/admin/instance/config')
			.body({policy: {personal_notes_enabled: enabled}})
			.execute();

	const discoveredPersonalNotesEnabled = async (): Promise<boolean | undefined> =>
		(await createBuilderWithoutAuth<WellKnownFluxerResponse>(harness).get('/.well-known/fluxer').execute()).community
			?.personal_notes_enabled;

	const startSession = (account: TestAccount) =>
		createBuilder<{data: {private_channels: Array<{id: string; type: number}>}}>(harness, '')
			.post('/test/rpc-session-init')
			.body({type: 'session', token: account.token, version: 1, ip: '127.0.0.1'})
			.expect(HTTP_STATUS.OK)
			.execute();

	const expectRefused = (request: ReturnType<typeof createBuilder>) =>
		request.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.PERSONAL_NOTES_DISABLED).execute();

	it('is on by default on Fluxer.app', async () => {
		Config.instance.selfHosted = false;
		const alice = await createTestAccount(harness);
		expect(await discoveredPersonalNotesEnabled()).toBe(true);
		await sendChannelMessage(harness, alice.token, alice.userId, 'a note to self');
	});

	it('is on by default on a self-hosted instance with direct messages enabled', async () => {
		Config.instance.selfHosted = true;
		const alice = await createTestAccount(harness);
		expect(await discoveredPersonalNotesEnabled()).toBe(true);
		await sendChannelMessage(harness, alice.token, alice.userId, 'a note to self');
	});

	it('refuses personal notes while off and brings existing notes back when turned on', async () => {
		const alice = await createTestAccount(harness);
		const note = await sendChannelMessage(harness, alice.token, alice.userId, 'kept while off');

		const off = await setPersonalNotesEnabled(false);
		expect(off.policy.personal_notes_enabled).toBe(false);
		await expectRefused(
			createBuilder(harness, alice.token).post(`/channels/${alice.userId}/messages`).body({content: 'hello'}),
		);
		await expectRefused(
			createBuilder(harness, alice.token)
				.patch(`/channels/${alice.userId}/messages/${note.id}`)
				.body({content: 'edited'}),
		);
		await expectRefused(createBuilder(harness, alice.token).get(`/channels/${alice.userId}/messages`));
		const session = await startSession(alice);
		expect(session.data.private_channels.map((channel) => channel.id)).not.toContain(alice.userId);

		await setPersonalNotesEnabled(true);
		const messages = await createBuilder<Array<MessageResponse>>(harness, alice.token)
			.get(`/channels/${alice.userId}/messages`)
			.execute();
		expect(messages.map((message) => message.id)).toContain(note.id);
	});
});
