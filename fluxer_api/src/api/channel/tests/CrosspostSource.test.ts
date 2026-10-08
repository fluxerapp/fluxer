// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuildID} from '@app/api/BrandedTypes';
import {addGuildMember, disableCrosspostWorker, setGuildFeatures} from '@app/api/channel/tests/AnnouncementTestUtils';
import {createPermissionOverwrite} from '@app/api/channel/tests/ChannelTestUtils';
import {
	copiesOf,
	type FanoutWorld,
	followInto,
	postAndPublish,
	sendMessage,
	setupFanoutWorld,
} from '@app/api/channel/tests/CrosspostWorkerTestUtils';
import {ensureSessionStarted, getMessages} from '@app/api/message/tests/MessageTestUtils';
import {getGuildDiscoveryRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {MessageTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {DiscoveryApplicationStatus, DiscoveryCategories} from '@fluxer/constants/src/DiscoveryConstants';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import type {CrosspostSourceResponse} from '@fluxer/schema/src/domains/message/CrosspostSourceSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test, vi} from 'vitest';

describe('Crosspost source', () => {
	let harness: ApiTestHarness;
	let world: FanoutWorld;
	let outsider: TestAccount;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		world = await setupFanoutWorld(harness);
		outsider = await createTestAccount(harness);
		await addGuildMember(harness, world.b.owner, world.b.guild, outsider);
		await ensureSessionStarted(harness, outsider.token);
	});

	afterEach(() => {
		disableCrosspostWorker();
		vi.restoreAllMocks();
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	function sourcePath(channelId: string, messageId: string): string {
		return `/channels/${channelId}/messages/${messageId}/crosspost-source`;
	}

	function getSource(token: string, channelId: string, messageId: string) {
		return createBuilder<CrosspostSourceResponse>(harness, token).get(sourcePath(channelId, messageId));
	}

	async function publishedCopy(): Promise<MessageResponse> {
		await followInto(harness, world, world.b.t1.id);
		const source = await postAndPublish(harness, world, {content: 'Version 2 is out'});
		const [copy] = await copiesOf(harness, world.b.owner.token, world.b.t1.id, source.id);
		expect(copy).toBeDefined();
		return copy!;
	}

	async function followNotice(): Promise<MessageResponse> {
		const messages = await getMessages(harness, world.b.owner.token, world.b.t1.id);
		const notice = messages.find((message) => message.type === MessageTypes.CHANNEL_FOLLOW_ADD);
		expect(notice).toBeDefined();
		return notice!;
	}

	test('returns the source community card to a viewer who is not a member of it', async () => {
		const copy = await publishedCopy();
		vi.spyOn(NoopGatewayService.prototype, 'getGuildCounts').mockResolvedValue({
			memberCount: 1234,
			presenceCount: 56,
		});
		const response = await getSource(outsider.token, world.b.t1.id, copy.id).execute();
		expect(response).toEqual({
			guild: {
				id: world.a.guild.id,
				name: world.a.guild.name,
				icon: null,
				banner: null,
				description: null,
				features: [],
				approximate_member_count: 1234,
				approximate_presence_count: 56,
				discoverable: false,
			},
		});
		expect(Object.keys(response.guild).sort()).toEqual(
			[
				'approximate_member_count',
				'approximate_presence_count',
				'banner',
				'description',
				'discoverable',
				'features',
				'icon',
				'id',
				'name',
			].sort(),
		);
	});

	test('returns the same card to a viewer who is a member of the source community', async () => {
		const copy = await publishedCopy();
		const asMember = await getSource(world.b.owner.token, world.b.t1.id, copy.id).execute();
		const asOutsider = await getSource(outsider.token, world.b.t1.id, copy.id).execute();
		expect(asMember).toEqual(asOutsider);
		expect(asMember.guild.id).toBe(world.a.guild.id);
	});

	async function writeDiscoveryApplication(status: string, description: string): Promise<void> {
		await getGuildDiscoveryRepository().upsert({
			guild_id: createGuildID(BigInt(world.a.guild.id)),
			status,
			category_type: DiscoveryCategories.OTHER,
			description,
			primary_language: null,
			custom_tags: null,
			applied_at: new Date(),
			reviewed_at: null,
			reviewed_by: null,
			review_reason: null,
			removed_at: null,
			removed_by: null,
			removal_reason: null,
		});
	}

	test('returns the community description while it is listed in discovery', async () => {
		const copy = await publishedCopy();
		await writeDiscoveryApplication(DiscoveryApplicationStatus.APPROVED, 'Release notes and outage reports');
		await setGuildFeatures(harness, world.a.guild.id, {add: [GuildFeatures.DISCOVERABLE]});
		const response = await getSource(outsider.token, world.b.t1.id, copy.id).execute();
		expect(response.guild.description).toBe('Release notes and outage reports');
	});

	test('hides the description of an application that is not approved', async () => {
		const copy = await publishedCopy();
		await writeDiscoveryApplication(DiscoveryApplicationStatus.PENDING, 'Pending application text');
		await setGuildFeatures(harness, world.a.guild.id, {add: [GuildFeatures.DISCOVERABLE]});
		const response = await getSource(outsider.token, world.b.t1.id, copy.id).execute();
		expect(response.guild.description).toBeNull();
	});

	test('hides an approved description once the community is no longer listed', async () => {
		const copy = await publishedCopy();
		await writeDiscoveryApplication(DiscoveryApplicationStatus.APPROVED, 'Delisted community text');
		const response = await getSource(outsider.token, world.b.t1.id, copy.id).execute();
		expect(response.guild.description).toBeNull();
	});

	test('reports a discoverable source community as joinable', async () => {
		const copy = await publishedCopy();
		await setGuildFeatures(harness, world.a.guild.id, {add: [GuildFeatures.DISCOVERABLE]});
		const response = await getSource(outsider.token, world.b.t1.id, copy.id).execute();
		expect(response.guild.discoverable).toBe(true);
		expect(response.guild.features).toEqual([GuildFeatures.DISCOVERABLE]);
	});

	test('does not report a discoverable community with invites disabled as joinable', async () => {
		const copy = await publishedCopy();
		await setGuildFeatures(harness, world.a.guild.id, {
			add: [GuildFeatures.DISCOVERABLE, GuildFeatures.INVITES_DISABLED],
		});
		const response = await getSource(outsider.token, world.b.t1.id, copy.id).execute();
		expect(response.guild.discoverable).toBe(false);
	});

	test('only exposes the badge features of the source community', async () => {
		const copy = await publishedCopy();
		await setGuildFeatures(harness, world.a.guild.id, {
			add: [
				GuildFeatures.VERIFIED,
				GuildFeatures.PARTNERED,
				GuildFeatures.INVITES_DISABLED,
				GuildFeatures.VIP_VOICE,
				GuildFeatures.VANITY_URL,
			],
		});
		const response = await getSource(outsider.token, world.b.t1.id, copy.id).execute();
		expect([...response.guild.features].sort()).toEqual([GuildFeatures.PARTNERED, GuildFeatures.VERIFIED].sort());
	});

	test('returns null counts when the gateway cannot provide them', async () => {
		const copy = await publishedCopy();
		vi.spyOn(NoopGatewayService.prototype, 'getGuildCounts').mockRejectedValue(new Error('gateway down'));
		const response = await getSource(outsider.token, world.b.t1.id, copy.id).execute();
		expect(response.guild.approximate_member_count).toBeNull();
		expect(response.guild.approximate_presence_count).toBeNull();
		expect(response.guild.name).toBe(world.a.guild.name);
	});

	test('accepts the follow system message', async () => {
		await followInto(harness, world, world.b.t1.id);
		const notice = await followNotice();
		const response = await getSource(outsider.token, world.b.t1.id, notice.id).execute();
		expect(response.guild.id).toBe(world.a.guild.id);
	});

	test('rejects a message that is not a copy', async () => {
		await followInto(harness, world, world.b.t1.id);
		const message = await sendMessage(harness, world.b.owner.token, world.b.t1.id, {content: 'hello'});
		await getSource(outsider.token, world.b.t1.id, message.id)
			.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_MESSAGE)
			.execute();
	});

	test('rejects the source message itself', async () => {
		await followInto(harness, world, world.b.t1.id);
		const source = await postAndPublish(harness, world, {content: 'published'});
		await getSource(world.b.owner.token, world.a.ann.id, source.id)
			.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_MESSAGE)
			.execute();
	});

	test('rejects a viewer who cannot view the channel of the copy', async () => {
		const copy = await publishedCopy();
		await createPermissionOverwrite(harness, world.b.owner.token, world.b.t1.id, outsider.userId, {
			type: 1,
			allow: '0',
			deny: Permissions.VIEW_CHANNEL.toString(),
		});
		const message = await createBuilder(harness, outsider.token)
			.get(`/channels/${world.b.t1.id}/messages/${copy.id}`)
			.executeRaw();
		expect(message.response.status).not.toBe(HTTP_STATUS.OK);
		const source = await getSource(outsider.token, world.b.t1.id, copy.id).executeRaw();
		expect(source.response.status).toBe(message.response.status);
		expect((source.json as {code?: string}).code).toBe((message.json as {code?: string}).code);
	});

	test('rejects a viewer who is not in the community of the copy', async () => {
		const copy = await publishedCopy();
		const stranger = await createTestAccount(harness);
		const message = await createBuilder(harness, stranger.token)
			.get(`/channels/${world.b.t1.id}/messages/${copy.id}`)
			.executeRaw();
		expect(message.response.status).not.toBe(HTTP_STATUS.OK);
		const source = await getSource(stranger.token, world.b.t1.id, copy.id).executeRaw();
		expect(source.response.status).toBe(message.response.status);
		expect((source.json as {code?: string}).code).toBe((message.json as {code?: string}).code);
	});

	test('returns unknown guild once the source community is deleted', async () => {
		const copy = await publishedCopy();
		await createBuilder(harness, world.a.owner.token)
			.post(`/guilds/${world.a.guild.id}/delete`)
			.body({password: world.a.owner.password})
			.expect(HTTP_STATUS.NO_CONTENT)
			.executeWithResponse();
		await world.worker.drain();
		await getSource(outsider.token, world.b.t1.id, copy.id)
			.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_GUILD)
			.execute();
		const notice = await followNotice();
		await getSource(outsider.token, world.b.t1.id, notice.id)
			.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_GUILD)
			.execute();
	});
});
