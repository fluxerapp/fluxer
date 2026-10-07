// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createReportID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {
	createChannel,
	createGuild,
	loadFixture,
	sendMessageWithAttachments,
} from '@app/api/channel/tests/AttachmentTestUtils';
import {acceptInvite, createChannelInvite} from '@app/api/channel/tests/ChannelTestUtils';
import {ReportRepository} from '@app/api/report/ReportRepository';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {MessageReferenceTypes} from '@fluxer/constants/src/ChannelConstants';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

function storageKey(url: string | null | undefined): string {
	const unsigned = (url ?? '').split('?')[0]!;
	expect(unsigned.startsWith(`${Config.endpoints.media}/attachments/`)).toBe(true);
	return unsigned.slice(`${Config.endpoints.media}/`.length);
}

describe('Embed attachment purge', () => {
	let harness: ApiTestHarness;
	let account: TestAccount;
	let channelId: string;
	let guildId: string;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		harness.storageService.reset();
		account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Embed Purge Guild');
		const channel = await createChannel(harness, account.token, guild.id, 'embed-purge');
		channelId = channel.id;
		guildId = guild.id;
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	function deletedKeys(): Array<string> {
		return harness.storageService
			.getDeletedObjects()
			.filter((entry) => entry.bucket === Config.s3.buckets.cdn)
			.map((entry) => entry.key);
	}

	function stored(key: string): boolean {
		return harness.storageService.hasObject(Config.s3.buckets.cdn, key);
	}

	async function sendEmbedOnly(): Promise<MessageResponse> {
		const fileData = loadFixture('yeah.png');
		const {response, json} = await sendMessageWithAttachments(
			harness,
			account.token,
			channelId,
			{
				content: 'embed only',
				attachments: [
					{id: 0, filename: 'image.png'},
					{id: 1, filename: 'thumb.png'},
				],
				embeds: [{title: 'Embed', image: {url: 'attachment://image.png'}, thumbnail: {url: 'attachment://thumb.png'}}],
			},
			[
				{index: 0, filename: 'image.png', data: fileData},
				{index: 1, filename: 'thumb.png', data: fileData},
			],
		);
		expect(response.status).toBe(200);
		expect(json.attachments ?? []).toHaveLength(0);
		return json;
	}

	async function sendVisible(): Promise<MessageResponse> {
		const {response, json} = await sendMessageWithAttachments(
			harness,
			account.token,
			channelId,
			{content: 'visible', attachments: [{id: 0, filename: 'visible.png'}]},
			[{index: 0, filename: 'visible.png', data: loadFixture('yeah.png')}],
		);
		expect(response.status).toBe(200);
		expect(json.attachments).toHaveLength(1);
		return json;
	}

	async function deleteMessage(messageId: string): Promise<void> {
		await createBuilder(harness, account.token)
			.delete(`/channels/${channelId}/messages/${messageId}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
	}

	it('removes files shown only through an embed when the message is deleted', async () => {
		const message = await sendEmbedOnly();
		const keys = [storageKey(message.embeds?.[0]?.image?.url), storageKey(message.embeds?.[0]?.thumbnail?.url)];
		for (const key of keys) {
			expect(key.startsWith(`attachments/${channelId}/`)).toBe(true);
			expect(stored(key)).toBe(true);
		}
		await deleteMessage(message.id);
		expect(deletedKeys()).toEqual(expect.arrayContaining(keys));
		for (const key of keys) {
			expect(stored(key)).toBe(false);
		}
	});

	it('keeps owning embed files after an edit that resends the embed', async () => {
		const message = await sendEmbedOnly();
		const embed = message.embeds![0]!;
		const keys = [storageKey(embed.image?.url), storageKey(embed.thumbnail?.url)];
		const edited = await createBuilder<MessageResponse>(harness, account.token)
			.patch(`/channels/${channelId}/messages/${message.id}`)
			.body({embeds: [{title: 'Edited', image: {url: embed.image!.url}, thumbnail: {url: embed.thumbnail!.url}}]})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(edited.embeds?.[0]?.title).toBe('Edited');
		await createBuilder<MessageResponse>(harness, account.token)
			.patch(`/channels/${channelId}/messages/${message.id}`)
			.body({content: 'content only edit'})
			.expect(HTTP_STATUS.OK)
			.execute();
		await deleteMessage(message.id);
		expect(deletedKeys()).toEqual(expect.arrayContaining(keys));
	});

	it('leaves files of other messages and external media alone', async () => {
		const owner = await sendVisible();
		const ownerKey = storageKey(owner.attachments![0]!.url);
		const embedOwner = await sendEmbedOnly();
		const embedOwnerKey = storageKey(embedOwner.embeds?.[0]?.image?.url);
		const borrower = await createBuilder<MessageResponse>(harness, account.token)
			.post(`/channels/${channelId}/messages`)
			.body({
				content: `${Config.endpoints.media}/${ownerKey}`,
				embeds: [
					{title: 'Borrowed', image: {url: `${Config.endpoints.media}/${ownerKey}`}},
					{title: 'Borrowed embed', image: {url: `${Config.endpoints.media}/${embedOwnerKey}`}},
					{title: 'External', image: {url: 'https://example.com/external.png'}},
				],
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		await deleteMessage(borrower.id);
		expect(deletedKeys()).toEqual([]);
		expect(stored(ownerKey)).toBe(true);
		expect(stored(embedOwnerKey)).toBe(true);
	});

	it('leaves the source file alone when a forward of an embed-only message is deleted', async () => {
		const source = await sendEmbedOnly();
		const sourceKey = storageKey(source.embeds?.[0]?.image?.url);
		const forward = await createBuilder<MessageResponse>(harness, account.token)
			.post(`/channels/${channelId}/messages`)
			.body({
				message_reference: {
					message_id: source.id,
					channel_id: channelId,
					guild_id: guildId,
					type: MessageReferenceTypes.FORWARD,
				},
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(forward.message_snapshots?.[0]?.embeds?.length).toBe(1);
		await deleteMessage(forward.id);
		expect(deletedKeys()).not.toContain(sourceKey);
		expect(stored(sourceKey)).toBe(true);
	});

	async function forward(
		sourceId: string,
		sourceChannelId: string,
		destinationChannelId: string,
	): Promise<MessageResponse> {
		return createBuilder<MessageResponse>(harness, account.token)
			.post(`/channels/${destinationChannelId}/messages`)
			.body({
				message_reference: {
					message_id: sourceId,
					channel_id: sourceChannelId,
					guild_id: guildId,
					type: MessageReferenceTypes.FORWARD,
				},
			})
			.expect(HTTP_STATUS.OK)
			.execute();
	}

	async function createAdmin(): Promise<TestAccount> {
		return setUserACLs(harness, await createTestAccount(harness), [AdminACLs.AUTHENTICATE, AdminACLs.MESSAGE_DELETE]);
	}

	function embedKeys(message: MessageResponse): Array<string> {
		const embed = message.embeds![0]!;
		return [storageKey(embed.image?.url), storageKey(embed.thumbnail?.url)];
	}

	it.each([
		['another channel', true],
		['the same channel', false],
	])('keeps a forwarded embed file in %s after the source is deleted', async (_label, otherChannel) => {
		const source = await sendEmbedOnly();
		const sourceKeys = embedKeys(source);
		const destinationChannelId = otherChannel
			? (await createChannel(harness, account.token, guildId, 'embed-purge-forward')).id
			: channelId;
		const forwarded = await forward(source.id, channelId, destinationChannelId);
		const snapshotEmbed = forwarded.message_snapshots![0]!.embeds![0]!;
		expect(snapshotEmbed.image?.flags).toBe(0);
		const copyKeys = [storageKey(snapshotEmbed.image?.url), storageKey(snapshotEmbed.thumbnail?.url)];
		for (const key of copyKeys) {
			expect(key.startsWith(`attachments/${destinationChannelId}/`)).toBe(true);
			expect(sourceKeys).not.toContain(key);
		}
		await deleteMessage(source.id);
		expect(deletedKeys()).toEqual(expect.arrayContaining(sourceKeys));
		for (const key of copyKeys) {
			expect(stored(key)).toBe(true);
		}
		const reforwarded = await forward(forwarded.id, destinationChannelId, channelId);
		const reforwardedEmbed = reforwarded.message_snapshots![0]!.embeds![0]!;
		const reforwardedKeys = [storageKey(reforwardedEmbed.image?.url), storageKey(reforwardedEmbed.thumbnail?.url)];
		for (const key of reforwardedKeys) {
			expect(key.startsWith(`attachments/${channelId}/`)).toBe(true);
			expect(copyKeys).not.toContain(key);
		}
		await createBuilder(harness, account.token)
			.delete(`/channels/${destinationChannelId}/messages/${forwarded.id}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		expect(deletedKeys()).toEqual(expect.arrayContaining(copyKeys));
		for (const key of reforwardedKeys) {
			expect(stored(key)).toBe(true);
		}
	});

	it('removes files shown only through an embed on an admin delete', async () => {
		const message = await sendEmbedOnly();
		const keys = embedKeys(message);
		const admin = await createAdmin();
		await createBuilder(harness, admin.token)
			.delete(`/admin/channels/${channelId}/messages/${message.id}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(deletedKeys()).toEqual(expect.arrayContaining(keys));
	});

	it('keeps reported embed files as report evidence after the author deletes the message', async () => {
		const message = await sendEmbedOnly();
		const [imageKey] = embedKeys(message);
		const reporter = await createTestAccount(harness);
		const invite = await createChannelInvite(harness, account.token, channelId);
		await acceptInvite(harness, reporter.token, invite.code);
		const report = await createBuilder<{report_id: string}>(harness, reporter.token)
			.post('/reports/message')
			.body({channel_id: channelId, message_id: message.id, category: 'harassment'})
			.expect(HTTP_STATUS.OK)
			.execute();
		await deleteMessage(message.id);
		expect(stored(imageKey!)).toBe(false);
		expect(harness.storageService.hasObject(Config.s3.buckets.reports, imageKey!)).toBe(true);
		const row = await new ReportRepository().getReport(createReportID(BigInt(report.report_id)));
		const context = row!.messageContext!.find((entry) => entry.messageId.toString() === message.id);
		expect(context?.attachments.map((attachment) => attachment.filename).sort()).toEqual(['image.png', 'thumb.png']);
	});

	it('does not expose the ownership marker on embed media flags', async () => {
		const message = await sendEmbedOnly();
		expect(message.embeds?.[0]?.image?.flags).toBe(0);
		expect(message.embeds?.[0]?.thumbnail?.flags).toBe(0);
	});
});
