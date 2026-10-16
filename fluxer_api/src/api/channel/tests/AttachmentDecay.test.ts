// SPDX-License-Identifier: AGPL-3.0-or-later

import {AttachmentDecayRepository} from '@app/api/attachment/AttachmentDecayRepository';
import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createAttachmentID, createChannelID, createMessageID} from '@app/api/BrandedTypes';
import {Config, getConfig} from '@app/api/Config';
import {
	createChannel,
	createGuild,
	loadFixture,
	sendMessageWithAttachments,
} from '@app/api/channel/tests/AttachmentTestUtils';
import {
	getAssetDeletionQueue,
	getChannelRepository,
	getInstanceConfigRepository,
} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopWorkerService} from '@app/api/test/NoopWorkerService';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {getExpiryBucket} from '@app/api/utils/AttachmentDecay';
import {processExpiredAttachments} from '@app/api/worker/tasks/ExpireAttachments';
import reconcileAttachmentDecay, {
	type ReconcileAttachmentDecayPayload,
	reconcileAttachmentDecayChunk,
} from '@app/api/worker/tasks/ReconcileAttachmentDecay';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '@app/api/worker/WorkerContext';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {MessageReferenceTypes} from '@fluxer/constants/src/ChannelConstants';
import type {WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

interface AttachmentDecayRow {
	attachment_id: string;
	channel_id: string;
	message_id: string;
	filename: string;
	size_bytes: string;
	expires_at: string;
	expiry_bucket: number;
	status: string | null;
}

interface ListedMessage {
	id: string;
	attachments?: Array<{id: string; url?: string | null; expires_at?: string | null}>;
}

describe('Attachment Decay', () => {
	let harness: ApiTestHarness;
	const originalSelfHosted = getConfig().instance.selfHosted;
	beforeEach(async () => {
		getConfig().instance.selfHosted = true;
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		getConfig().instance.selfHosted = originalSelfHosted;
		vi.restoreAllMocks();
		clearWorkerDependencies();
		await harness?.shutdown();
	});

	async function createInstanceAdmin(): Promise<TestAccount> {
		return setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_CONFIG_VIEW,
			AdminACLs.INSTANCE_CONFIG_UPDATE,
		]);
	}

	async function setAttachmentExpiry(admin: TestAccount, attachmentDecay: Record<string, unknown>): Promise<void> {
		await createBuilder(harness, admin.token)
			.patch('/admin/instance/config')
			.body({media: {attachment_decay: attachmentDecay}})
			.expect(HTTP_STATUS.OK)
			.execute();
	}

	async function listedAttachment(token: string, channelId: string, messageId: string) {
		const messages = await createBuilder<Array<ListedMessage>>(harness, token)
			.get(`/channels/${channelId}/messages?limit=10`)
			.execute();
		return messages.find((message) => message.id === messageId)?.attachments?.[0];
	}

	async function fetchDecayRow(attachmentId: string): Promise<AttachmentDecayRow | null> {
		const {row} = await createBuilderWithoutAuth<{row: AttachmentDecayRow | null}>(harness)
			.get(`/test/attachment-decay/${attachmentId}`)
			.execute();
		return row;
	}

	function installWorkerDependencies(): void {
		setWorkerDependenciesForTest({
			assetDeletionQueue: getAssetDeletionQueue(),
			channelRepository: getChannelRepository(),
			instanceConfigRepository: getInstanceConfigRepository(),
			kvClient: harness.kvProvider,
			storageService: harness.storageService,
		});
	}

	async function enableExpiryAndTakeReconcile(admin: TestAccount): Promise<ReconcileAttachmentDecayPayload> {
		const addJob = vi.spyOn(NoopWorkerService.prototype, 'addJob');
		await setAttachmentExpiry(admin, {enabled: true});
		expect(addJob).toHaveBeenCalledTimes(1);
		const [task, payload, options] = addJob.mock.calls[0] as unknown as [
			string,
			ReconcileAttachmentDecayPayload,
			{jobKey: string},
		];
		expect(task).toBe('reconcileAttachmentDecay');
		expect(payload).toEqual({runId: expect.any(String), chunk: 0});
		expect(options).toEqual({jobKey: `${payload.runId}:0`});
		addJob.mockRestore();
		return payload;
	}

	async function runReconcile(
		first: ReconcileAttachmentDecayPayload,
		limits?: {messagePageSize: number; messagePages: number},
	): Promise<number> {
		let chunks = 0;
		let next: ReconcileAttachmentDecayPayload | null = first;
		while (next !== null) {
			next = await reconcileAttachmentDecayChunk(next, limits);
			chunks++;
		}
		return chunks;
	}

	async function uploadWhileExpiryIsOff(token: string, channelId: string, filename: string) {
		const {json} = await sendMessageWithAttachments(
			harness,
			token,
			channelId,
			{content: filename, attachments: [{id: 0, filename}]},
			[{index: 0, filename, data: loadFixture('yeah.png')}],
		);
		return {messageId: json.id, attachmentId: json.attachments![0].id};
	}

	test('should stop reporting an expiry once attachment expiry is turned off', async () => {
		const admin = await createInstanceAdmin();
		const guild = await createGuild(harness, admin.token, 'Expiry Off Guild');
		const channelId = guild.system_channel_id!;
		await setAttachmentExpiry(admin, {enabled: true});
		const {json} = await sendMessageWithAttachments(
			harness,
			admin.token,
			channelId,
			{content: 'Uploaded while expiry is on', attachments: [{id: 0, filename: 'on.png'}]},
			[{index: 0, filename: 'on.png', data: loadFixture('yeah.png')}],
		);
		expect((await listedAttachment(admin.token, channelId, json.id))?.expires_at).toBeTruthy();

		await setAttachmentExpiry(admin, {enabled: false});

		const listed = await listedAttachment(admin.token, channelId, json.id);
		expect(listed?.expires_at ?? null).toBeNull();
		expect(listed?.url).toBeTruthy();
		expect(await fetchDecayRow(json.attachments![0].id)).not.toBeNull();
	});
	test('should apply an expiry to earlier attachments once attachment expiry is turned on', async () => {
		const admin = await createInstanceAdmin();
		const guild = await createGuild(harness, admin.token, 'Expiry On Guild');
		const channelId = guild.system_channel_id!;
		await setAttachmentExpiry(admin, {enabled: false});
		const {json} = await sendMessageWithAttachments(
			harness,
			admin.token,
			channelId,
			{content: 'Uploaded while expiry is off', attachments: [{id: 0, filename: 'off.png'}]},
			[{index: 0, filename: 'off.png', data: loadFixture('yeah.png')}],
		);
		const attachmentId = json.attachments![0].id;
		expect(await fetchDecayRow(attachmentId)).toBeNull();

		const reconcile = await enableExpiryAndTakeReconcile(admin);
		installWorkerDependencies();
		expect(await runReconcile(reconcile)).toBe(1);
		const row = await fetchDecayRow(attachmentId);
		expect(row?.channel_id).toBe(channelId);
		expect(row?.message_id).toBe(json.id);
		expect(new Date(row!.expires_at).getTime()).toBeGreaterThan(Date.now());
	});
	test('should only queue a reconcile when the expiry rules change', async () => {
		const admin = await createInstanceAdmin();
		await setAttachmentExpiry(admin, {enabled: true, max_lifetime_days: 400});
		const addJob = vi.spyOn(NoopWorkerService.prototype, 'addJob');

		await setAttachmentExpiry(admin, {enabled: true, max_lifetime_days: 400, renew_window_days: 10});
		expect(addJob).not.toHaveBeenCalled();

		await setAttachmentExpiry(admin, {max_lifetime_days: 200});
		expect(addJob).toHaveBeenCalledTimes(1);

		await setAttachmentExpiry(admin, {enabled: false});
		expect(addJob).toHaveBeenCalledTimes(1);
	});
	test('should not queue a reconcile on the hosted instance', async () => {
		const admin = await createInstanceAdmin();
		await setAttachmentExpiry(admin, {enabled: false});
		getConfig().instance.selfHosted = false;
		const addJob = vi.spyOn(NoopWorkerService.prototype, 'addJob');

		await setAttachmentExpiry(admin, {enabled: true});
		await setAttachmentExpiry(admin, {max_lifetime_days: 200});

		expect(addJob).not.toHaveBeenCalled();
		expect(await harness.kvProvider.exists('attachment_decay:reconcile_run')).toBe(0);
	});
	test('should not run a queued reconcile job on the hosted instance', async () => {
		const admin = await createInstanceAdmin();
		const guild = await createGuild(harness, admin.token, 'Hosted Guild');
		await setAttachmentExpiry(admin, {enabled: false});
		const upload = await uploadWhileExpiryIsOff(admin.token, guild.system_channel_id!, 'hosted.png');
		const reconcile = await enableExpiryAndTakeReconcile(admin);
		installWorkerDependencies();
		getConfig().instance.selfHosted = false;
		const addJob = vi.fn();

		await reconcileAttachmentDecay({...reconcile, __jobId: '1'}, {addJob} as unknown as WorkerTaskHelpers);

		expect(addJob).not.toHaveBeenCalled();
		expect(await fetchDecayRow(upload.attachmentId)).toBeNull();
		getConfig().instance.selfHosted = true;
		expect(await runReconcile(reconcile)).toBe(1);
		expect(await fetchDecayRow(upload.attachmentId)).not.toBeNull();
	});
	test('should reconcile in resumable chunks', async () => {
		const admin = await createInstanceAdmin();
		const guild = await createGuild(harness, admin.token, 'Chunked Guild');
		const second = await createChannel(harness, admin.token, guild.id, 'second');
		await setAttachmentExpiry(admin, {enabled: false});
		const uploads = [
			await uploadWhileExpiryIsOff(admin.token, guild.system_channel_id!, 'a.png'),
			await uploadWhileExpiryIsOff(admin.token, guild.system_channel_id!, 'b.png'),
			await uploadWhileExpiryIsOff(admin.token, guild.system_channel_id!, 'c.png'),
			await uploadWhileExpiryIsOff(admin.token, second.id, 'd.png'),
			await uploadWhileExpiryIsOff(admin.token, second.id, 'e.png'),
		];
		const reconcile = await enableExpiryAndTakeReconcile(admin);
		installWorkerDependencies();

		const next = await reconcileAttachmentDecayChunk(reconcile, {messagePageSize: 1, messagePages: 1});

		expect(next).toMatchObject({runId: reconcile.runId, chunk: 1, cursor: {beforeMessageId: expect.any(String)}});
		const tracked = await Promise.all(uploads.map((upload) => fetchDecayRow(upload.attachmentId)));
		expect(tracked.filter((row) => row !== null)).toHaveLength(1);
		expect(await runReconcile(next!, {messagePageSize: 1, messagePages: 1})).toBeGreaterThan(3);
		for (const upload of uploads) {
			expect((await fetchDecayRow(upload.attachmentId))?.message_id).toBe(upload.messageId);
		}
	});
	test('should run a queued reconcile job to the end', async () => {
		const admin = await createInstanceAdmin();
		const guild = await createGuild(harness, admin.token, 'Queued Job Guild');
		await setAttachmentExpiry(admin, {enabled: false});
		const upload = await uploadWhileExpiryIsOff(admin.token, guild.system_channel_id!, 'queued.png');
		const reconcile = await enableExpiryAndTakeReconcile(admin);
		installWorkerDependencies();
		const addJob = vi.fn();

		await reconcileAttachmentDecay({...reconcile, __jobId: '1'}, {addJob} as unknown as WorkerTaskHelpers);

		expect(addJob).not.toHaveBeenCalled();
		expect(await fetchDecayRow(upload.attachmentId)).not.toBeNull();
		expect(await harness.kvProvider.exists('attachment_decay:reconcile_run')).toBe(0);
	});
	test('should stop a reconcile once a newer one starts', async () => {
		const admin = await createInstanceAdmin();
		const guild = await createGuild(harness, admin.token, 'Superseded Guild');
		await setAttachmentExpiry(admin, {enabled: false});
		const upload = await uploadWhileExpiryIsOff(admin.token, guild.system_channel_id!, 'old.png');
		const first = await enableExpiryAndTakeReconcile(admin);
		const addJob = vi.spyOn(NoopWorkerService.prototype, 'addJob');
		await setAttachmentExpiry(admin, {max_lifetime_days: 200});
		expect(addJob).toHaveBeenCalledTimes(1);
		installWorkerDependencies();

		expect(await reconcileAttachmentDecayChunk(first)).toBeNull();

		expect(await fetchDecayRow(upload.attachmentId)).toBeNull();
	});
	test('should read the stored settings instead of a stale cache', async () => {
		const admin = await createInstanceAdmin();
		const guild = await createGuild(harness, admin.token, 'Stale Cache Guild');
		await setAttachmentExpiry(admin, {enabled: false});
		const upload = await uploadWhileExpiryIsOff(admin.token, guild.system_channel_id!, 'stale.png');
		const reconcile = await enableExpiryAndTakeReconcile(admin);
		installWorkerDependencies();
		const current = await getInstanceConfigRepository().getEffectiveAttachmentDecayConfig();
		vi.spyOn(getInstanceConfigRepository(), 'getEffectiveAttachmentDecayConfig').mockResolvedValue({
			...current,
			enabled: false,
		});

		await runReconcile(reconcile);

		expect(await fetchDecayRow(upload.attachmentId)).not.toBeNull();
	});
	test('should not give a new expiry to an attachment whose file is gone', async () => {
		const admin = await createInstanceAdmin();
		const guild = await createGuild(harness, admin.token, 'Purged Guild');
		const channelId = guild.system_channel_id!;
		await setAttachmentExpiry(admin, {enabled: false});
		const purged = await uploadWhileExpiryIsOff(admin.token, channelId, 'purged.png');
		const kept = await uploadWhileExpiryIsOff(admin.token, channelId, 'kept.png');
		await harness.storageService.deleteObject(
			Config.s3.buckets.cdn,
			`attachments/${channelId}/${purged.attachmentId}/purged.png`,
		);
		const reconcile = await enableExpiryAndTakeReconcile(admin);
		installWorkerDependencies();

		await runReconcile(reconcile);

		expect(await fetchDecayRow(purged.attachmentId)).toBeNull();
		expect(await fetchDecayRow(kept.attachmentId)).not.toBeNull();
	});
	test('should not delete expired attachments while a reconcile is running', async () => {
		const admin = await createInstanceAdmin();
		const guild = await createGuild(harness, admin.token, 'Paused Expiry Guild');
		const channelId = guild.system_channel_id!;
		await setAttachmentExpiry(admin, {enabled: true});
		const {json} = await sendMessageWithAttachments(
			harness,
			admin.token,
			channelId,
			{content: 'Lapsed while expiry was off', attachments: [{id: 0, filename: 'lapsed.png'}]},
			[{index: 0, filename: 'lapsed.png', data: loadFixture('yeah.png')}],
		);
		const attachmentId = json.attachments![0].id;
		await setAttachmentExpiry(admin, {enabled: false});
		const expiresAt = new Date(Date.now() - 60_000);
		await new AttachmentDecayRepository().upsert({
			attachment_id: createAttachmentID(BigInt(attachmentId)),
			channel_id: createChannelID(BigInt(channelId)),
			message_id: createMessageID(BigInt(json.id)),
			filename: 'lapsed.png',
			size_bytes: 1024n,
			uploaded_at: expiresAt,
			expires_at: expiresAt,
			last_accessed_at: expiresAt,
			cost: 1,
			lifetime_days: 1,
			status: null,
			expiry_bucket: getExpiryBucket(expiresAt),
		});
		const reconcile = await enableExpiryAndTakeReconcile(admin);
		installWorkerDependencies();

		await processExpiredAttachments();

		expect(new Date((await fetchDecayRow(attachmentId))!.expires_at).getTime()).toBeLessThan(Date.now());
		await runReconcile(reconcile);
		await processExpiredAttachments();
		expect(new Date((await fetchDecayRow(attachmentId))!.expires_at).getTime()).toBeGreaterThan(Date.now());
		const key = `attachments/${channelId}/${attachmentId}/lapsed.png`;
		expect(harness.storageService.hasObject(Config.s3.buckets.cdn, key)).toBe(true);
	});
	test('should create decay metadata when uploading attachment', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Decay Test Guild');
		const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
		const channelId = guild.system_channel_id ?? channel.id;
		const fileData = loadFixture('yeah.png');
		const {response, json} = await sendMessageWithAttachments(
			harness,
			account.token,
			channelId,
			{
				content: 'Attachment with decay tracking',
				attachments: [{id: 0, filename: 'test.png'}],
			},
			[{index: 0, filename: 'test.png', data: fileData}],
		);
		expect(response.status).toBe(HTTP_STATUS.OK);
		expect(json.attachments).toBeDefined();
		expect(json.attachments).not.toBeNull();
		expect(json.attachments!.length).toBe(1);
		const attachmentId = json.attachments![0].id;
		const decayResponse = await createBuilderWithoutAuth<{
			row: AttachmentDecayRow | null;
		}>(harness)
			.get(`/test/attachment-decay/${attachmentId}`)
			.execute();
		expect(decayResponse.row).not.toBeNull();
		expect(decayResponse.row?.attachment_id).toBe(attachmentId);
		expect(decayResponse.row?.channel_id).toBe(channelId);
		expect(decayResponse.row?.message_id).toBe(json.id);
	});
	test('should configure decay metadata for forwarded snapshot attachments', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Forwarded Decay Test Guild');
		const destinationChannel = await createChannel(harness, account.token, guild.id, 'forward-destination');
		const sourceChannelId = guild.system_channel_id!;
		const {response, json: originalMessage} = await sendMessageWithAttachments(
			harness,
			account.token,
			sourceChannelId,
			{
				content: 'Forward attachment decay metadata',
				attachments: [{id: 0, filename: 'forwarded.png'}],
			},
			[{index: 0, filename: 'forwarded.png', data: loadFixture('yeah.png')}],
		);
		expect(response.status).toBe(HTTP_STATUS.OK);
		expect(originalMessage.attachments).toHaveLength(1);
		const forwardedMessage = await createBuilder<{
			id: string;
			message_snapshots?: Array<{
				attachments?: Array<{
					id: string;
					expires_at?: string | null;
					url?: string | null;
				}>;
			}>;
		}>(harness, account.token)
			.post(`/channels/${destinationChannel.id}/messages`)
			.body({
				message_reference: {
					message_id: originalMessage.id,
					channel_id: sourceChannelId,
					guild_id: guild.id,
					type: MessageReferenceTypes.FORWARD,
				},
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		const snapshotAttachment = forwardedMessage.message_snapshots?.[0]?.attachments?.[0];
		expect(snapshotAttachment).toBeDefined();
		expect(snapshotAttachment?.url).toBeTruthy();
		expect(snapshotAttachment?.expires_at).toBeTruthy();
		const decayResponse = await createBuilderWithoutAuth<{
			row: AttachmentDecayRow | null;
		}>(harness)
			.get(`/test/attachment-decay/${snapshotAttachment!.id}`)
			.execute();
		expect(decayResponse.row).not.toBeNull();
		expect(decayResponse.row?.attachment_id).toBe(snapshotAttachment!.id);
		expect(decayResponse.row?.channel_id).toBe(destinationChannel.id);
		expect(decayResponse.row?.message_id).toBe(forwardedMessage.id);
	});
});
