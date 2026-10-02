// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {Config} from '@app/api/Config';
import {disableCrosspostWorker} from '@app/api/channel/tests/AnnouncementTestUtils';
import {
	copiesOf,
	type FanoutWorld,
	followInto,
	publishAndDrain,
	readRow,
	sendWithImage,
	setupFanoutWorld,
} from '@app/api/channel/tests/CrosspostWorkerTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {MessageFlags} from '@fluxer/constants/src/ChannelConstants';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test} from 'vitest';

function attachmentKeys(channelId: string, message: MessageResponse): Array<string> {
	return (message.attachments ?? []).map(
		(attachment) => `attachments/${channelId}/${attachment.id}/${attachment.filename}`,
	);
}

describe('Guild deletion attachment purge', () => {
	let harness: ApiTestHarness;
	let world: FanoutWorld;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		harness.storageService.reset();
		world = await setupFanoutWorld(harness);
	});

	afterEach(() => {
		disableCrosspostWorker();
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	function deletedKeys(): Array<string> {
		return harness.storageService.getDeletedObjects().map((entry) => entry.key);
	}

	function deletedKeysForAttachments(message: MessageResponse): Array<string> {
		const ids = (message.attachments ?? []).map((attachment) => `/${attachment.id}/`);
		return deletedKeys().filter((key) => ids.some((id) => key.includes(id)));
	}

	async function deleteGuild(owner: TestAccount, guildId: string): Promise<void> {
		await createBuilder(harness, owner.token)
			.post(`/guilds/${guildId}/delete`)
			.body({password: owner.password})
			.expect(HTTP_STATUS.NO_CONTENT)
			.executeWithResponse();
		await world.worker.drain();
	}

	test('purges the attachments of every message in every channel', async () => {
		const first = await sendWithImage(harness, world.b.owner.token, world.b.t1.id, {content: 'one'}, [
			'a.png',
			'b.gif',
		]);
		const second = await sendWithImage(harness, world.b.owner.token, world.b.t2.id, {content: 'two'});
		const expected = [...attachmentKeys(world.b.t1.id, first), ...attachmentKeys(world.b.t2.id, second)];
		expect(expected).toHaveLength(3);
		for (const key of expected) {
			expect(harness.storageService.hasObject(Config.s3.buckets.cdn, key)).toBe(true);
		}
		await deleteGuild(world.b.owner, world.b.guild.id);
		expect(deletedKeys()).toEqual(expect.arrayContaining(expected));
		for (const key of expected) {
			expect(harness.storageService.hasObject(Config.s3.buckets.cdn, key)).toBe(false);
		}
	});

	test('a follower guild deletion leaves the source objects of its copies in place', async () => {
		await followInto(harness, world, world.b.t1.id);
		const source = await sendWithImage(harness, world.a.owner.token, world.a.ann.id, {content: 'release'});
		await publishAndDrain(harness, world, source.id);
		const [copy] = await copiesOf(harness, world.b.owner.token, world.b.t1.id, source.id);
		expect(copy?.attachments?.map((attachment) => attachment.id)).toEqual(
			source.attachments?.map((attachment) => attachment.id),
		);
		const own = await sendWithImage(harness, world.b.owner.token, world.b.t1.id, {content: 'local'});
		const sourceKeys = attachmentKeys(world.a.ann.id, source);
		await deleteGuild(world.b.owner, world.b.guild.id);
		expect(deletedKeys()).toEqual(expect.arrayContaining(attachmentKeys(world.b.t1.id, own)));
		expect(deletedKeys().some((key) => key.startsWith(`attachments/${world.a.ann.id}/`))).toBe(false);
		expect(deletedKeysForAttachments(copy!)).toEqual([]);
		for (const key of sourceKeys) {
			expect(harness.storageService.hasObject(Config.s3.buckets.cdn, key)).toBe(true);
		}
		expect(await readRow(world.a.ann.id, source.id)).not.toBeNull();
	});

	test('an admin guild deletion purges every channel and leaves follower copy sources in place', async () => {
		await followInto(harness, world, world.b.t1.id);
		const source = await sendWithImage(harness, world.a.owner.token, world.a.ann.id, {content: 'release'});
		await publishAndDrain(harness, world, source.id);
		const [copy] = await copiesOf(harness, world.b.owner.token, world.b.t1.id, source.id);
		expect(copy?.attachments?.map((attachment) => attachment.id)).toEqual(
			source.attachments?.map((attachment) => attachment.id),
		);
		const first = await sendWithImage(harness, world.b.owner.token, world.b.t1.id, {content: 'one'});
		const second = await sendWithImage(harness, world.b.owner.token, world.b.t2.id, {content: 'two'});
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			'admin:authenticate',
			'guild:lookup',
			'guild:delete',
		]);
		await createBuilder(harness, admin.token)
			.delete(`/admin/guilds/${world.b.guild.id}`)
			.body(null)
			.expect(HTTP_STATUS.OK)
			.execute();
		await world.worker.drain();
		expect(deletedKeys()).toEqual(
			expect.arrayContaining([...attachmentKeys(world.b.t1.id, first), ...attachmentKeys(world.b.t2.id, second)]),
		);
		expect(deletedKeys().some((key) => key.startsWith(`attachments/${world.a.ann.id}/`))).toBe(false);
		expect(deletedKeysForAttachments(copy!)).toEqual([]);
		for (const key of attachmentKeys(world.a.ann.id, source)) {
			expect(harness.storageService.hasObject(Config.s3.buckets.cdn, key)).toBe(true);
		}
	});

	test('a source guild deletion purges its objects and marks follower copies source deleted', async () => {
		await followInto(harness, world, world.b.t1.id);
		const source = await sendWithImage(harness, world.a.owner.token, world.a.ann.id, {content: 'release'});
		await publishAndDrain(harness, world, source.id);
		const [copy] = await copiesOf(harness, world.b.owner.token, world.b.t1.id, source.id);
		expect(copy).toBeDefined();
		await deleteGuild(world.a.owner, world.a.guild.id);
		expect(deletedKeys()).toEqual(expect.arrayContaining(attachmentKeys(world.a.ann.id, source)));
		const marked = await readRow(world.b.t1.id, copy!.id);
		expect(marked?.flags).toBe(MessageFlags.IS_CROSSPOST | MessageFlags.SOURCE_MESSAGE_DELETED);
	});
});
