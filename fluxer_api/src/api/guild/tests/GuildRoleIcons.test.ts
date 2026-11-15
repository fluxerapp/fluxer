// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {Config} from '@app/api/Config';
import {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import {
	acceptInvite,
	addMemberRole,
	createChannelInvite,
	createGuild,
	createRole,
	getRoles,
	updateRole,
	updateRolePositions,
} from '@app/api/guild/tests/GuildTestUtils';
import {NoopPurgeQueue} from '@app/api/infrastructure/CachePurgeQueue';
import {EntityAssetService} from '@app/api/infrastructure/EntityAssetService';
import type {QueuedAssetDeletion} from '@app/api/infrastructure/IAssetDeletionQueue';
import {jsonAssetBodyLimit} from '@app/api/middleware/RequestBodyLimitMiddleware';
import {setInjectedGatewayService} from '@app/api/middleware/ServiceRegistry';
import {getAssetDeletionQueue} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import {createTestWorkerHelpers} from '@app/api/test/SyncTaskWorkerService';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import processAssetDeletionQueue from '@app/api/worker/tasks/ProcessAssetDeletionQueue';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '@app/api/worker/WorkerContext';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {GuildRoleResponse} from '@fluxer/schema/src/domains/guild/GuildRoleSchemas';
import sharp from 'sharp';
import {afterEach, assert, beforeEach, describe, expect, test, vi} from 'vitest';

type GuildDispatch = Parameters<NoopGatewayService['dispatchGuild']>[0];

class RecordingGateway extends NoopGatewayService {
	readonly dispatches: Array<GuildDispatch> = [];

	override async dispatchGuild(params: GuildDispatch): Promise<void> {
		this.dispatches.push(params);
	}
}

interface ValidationErrorBody {
	code: string;
	errors?: Array<{path: string; code: string}>;
}

async function pngDataUrl(width: number, height: number, red = 200): Promise<string> {
	const buffer = await sharp({
		create: {width, height, channels: 4, background: {r: red, g: 40, b: 40, alpha: 1}},
	})
		.png()
		.toBuffer();
	return `data:image/png;base64,${buffer.toString('base64')}`;
}

function dataUrl(mime: string, buffer: Buffer): string {
	return `data:${mime};base64,${buffer.toString('base64')}`;
}

function animatedWebpDataUrl(): string {
	return dataUrl(
		'image/webp',
		Buffer.concat([
			Buffer.from('RIFF'),
			Buffer.alloc(4),
			Buffer.from('WEBPVP8X'),
			Buffer.alloc(14),
			Buffer.from('ANIM'),
			Buffer.alloc(16),
		]),
	);
}

function roleIconUploads(harness: ApiTestHarness): Array<string> {
	return harness.storageService.uploadObjectSpy.mock.calls
		.map(([params]) => (params as {key: string}).key)
		.filter((key) => key.startsWith('role-icons/'));
}

async function drainDeletionQueue(): Promise<Array<QueuedAssetDeletion>> {
	return getAssetDeletionQueue().getBatch(100);
}

async function runDeletionWorker(harness: ApiTestHarness): Promise<void> {
	setWorkerDependenciesForTest({
		assetDeletionQueue: getAssetDeletionQueue(),
		purgeQueue: new NoopPurgeQueue(),
		storageService: harness.storageService,
		guildRepository: new GuildRepository(),
	});
	try {
		await processAssetDeletionQueue({}, createTestWorkerHelpers());
	} finally {
		clearWorkerDependencies();
	}
}

function storedRoleIconKeys(harness: ApiTestHarness, roleId: string): Array<string> {
	return roleIconUploads(harness)
		.concat(harness.storageService.getCopiedObjects().map((entry) => entry.destinationKey))
		.filter((key, index, keys) => key.startsWith(`role-icons/${roleId}/`) && keys.indexOf(key) === index)
		.filter((key) => harness.storageService.hasObject(Config.s3.buckets.cdn, key));
}

describe('Guild role icons', () => {
	let harness: ApiTestHarness;
	let gateway: RecordingGateway;
	let owner: TestAccount;
	let guild: GuildResponse;

	beforeEach(async () => {
		harness = await createApiTestHarness();
		gateway = new RecordingGateway();
		setInjectedGatewayService(gateway);
		await harness.resetData();
		owner = await createTestAccount(harness);
		guild = await createGuild(harness, owner.token, 'Role Icon Guild');
		harness.storageService.uploadObjectSpy.mockClear();
	});

	afterEach(async () => {
		await harness?.shutdown();
	});

	async function addMemberWithRole(roleId: string): Promise<TestAccount> {
		const member = await createTestAccount(harness);
		const invite = await createChannelInvite(harness, owner.token, guild.system_channel_id!);
		await acceptInvite(harness, member.token, invite.code);
		await addMemberRole(harness, owner.token, guild.id, member.userId, roleId);
		return member;
	}

	async function expectRoleIconError(
		token: string,
		body: Record<string, unknown>,
		path: string,
		code: string,
		roleId?: string,
	): Promise<void> {
		const builder = createBuilder<ValidationErrorBody>(harness, token);
		const request = roleId
			? builder.patch(`/guilds/${guild.id}/roles/${roleId}`)
			: builder.post(`/guilds/${guild.id}/roles`);
		const json = await request
			.body(roleId ? body : {name: 'Rejected', ...body})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_FORM_BODY)
			.execute();
		expect(json.errors?.[0]).toMatchObject({path, code});
	}

	describe('create', () => {
		test('stores an uploaded image and returns its hash', async () => {
			const role = await createRole(harness, owner.token, guild.id, {
				name: 'Admin',
				icon: await pngDataUrl(64, 64),
			});
			expect(role.icon).toMatch(/^[0-9a-f]{8}$/);
			expect(role.unicode_emoji).toBeNull();
			expect(roleIconUploads(harness)).toEqual([`role-icons/${role.id}/${role.icon}`]);
		});

		test('stores a Unicode emoji', async () => {
			const role = await createRole(harness, owner.token, guild.id, {name: 'Mods', unicode_emoji: '🛡️'});
			expect(role).toMatchObject({icon: null, unicode_emoji: '🛡️'});
			expect(roleIconUploads(harness)).toEqual([]);
		});

		test('returns both keys as null when no icon is set', async () => {
			const role = await createRole(harness, owner.token, guild.id, {name: 'Plain'});
			expect(role).toMatchObject({icon: null, unicode_emoji: null});
			expect(Object.keys(role)).toEqual(expect.arrayContaining(['icon', 'unicode_emoji']));
		});

		test('removes the uploaded image when the role write fails', async () => {
			const upsert = vi.spyOn(GuildRepository.prototype, 'upsertRole').mockRejectedValueOnce(new Error('write failed'));
			try {
				await createBuilder(harness, owner.token)
					.post(`/guilds/${guild.id}/roles`)
					.body({name: 'Unlucky', icon: await pngDataUrl(64, 64)})
					.expect(HTTP_STATUS.INTERNAL_SERVER_ERROR)
					.execute();
			} finally {
				upsert.mockRestore();
			}
			const [key] = roleIconUploads(harness);
			assert(key !== undefined);
			await runDeletionWorker(harness);
			expect(harness.storageService.hasObject(Config.s3.buckets.cdn, key)).toBe(false);
			const roles = await getRoles(harness, owner.token, guild.id);
			expect(roles.some((entry) => entry.name === 'Unlucky')).toBe(false);
		});

		test('rejects an image and an emoji together', async () => {
			await expectRoleIconError(
				owner.token,
				{icon: await pngDataUrl(64, 64), unicode_emoji: '🔥'},
				'icon',
				ValidationErrorCodes.ROLE_ICON_AND_EMOJI_EXCLUSIVE,
			);
			expect(roleIconUploads(harness)).toEqual([]);
		});

		test('rejects text and multiple emoji', async () => {
			for (const value of ['abc', '🔥🔥', '🔥'.repeat(40)]) {
				await expectRoleIconError(
					owner.token,
					{unicode_emoji: value},
					'unicode_emoji',
					ValidationErrorCodes.NOT_A_VALID_UNICODE_EMOJI,
				);
			}
		});
	});

	describe('update', () => {
		test('switches between image and emoji and queues replaced images', async () => {
			const role = await createRole(harness, owner.token, guild.id, {name: 'Artists'});
			const withImage = await updateRole(harness, owner.token, guild.id, role.id, {icon: await pngDataUrl(64, 64)});
			expect(withImage.icon).toMatch(/^[0-9a-f]{8}$/);

			const replaced = await updateRole(harness, owner.token, guild.id, role.id, {
				icon: await pngDataUrl(64, 64, 10),
			});
			expect(replaced.icon).not.toBe(withImage.icon);

			const withEmoji = await updateRole(harness, owner.token, guild.id, role.id, {unicode_emoji: '🎨'});
			expect(withEmoji).toMatchObject({icon: null, unicode_emoji: '🎨'});

			const queued = (await drainDeletionQueue()).filter((item) => item.reason === 'asset_replaced');
			expect(queued.map((item) => item.s3Key)).toEqual([
				`role-icons/${role.id}/${withImage.icon}`,
				`role-icons/${role.id}/${replaced.icon}`,
			]);
			for (const item of queued) {
				expect(item.staleReference).toMatchObject({
					entityType: 'role',
					assetType: 'role_icon',
					entityId: role.id,
					guildId: guild.id,
				});
			}
		});

		test('queues each new upload for a stale check so a lost concurrent write is cleaned up', async () => {
			const role = await createRole(harness, owner.token, guild.id, {name: 'Racers'});
			const updated = await updateRole(harness, owner.token, guild.id, role.id, {icon: await pngDataUrl(64, 64)});
			const queued = await drainDeletionQueue();
			expect(queued).toHaveLength(1);
			expect(queued[0]).toMatchObject({
				s3Key: `role-icons/${role.id}/${updated.icon}`,
				reason: 'asset_upload_stale_check',
				staleReference: {
					entityType: 'role',
					assetType: 'role_icon',
					entityId: role.id,
					guildId: guild.id,
					hash: updated.icon,
				},
			});
		});

		test('removes the uploaded image when the role write fails', async () => {
			const role = await createRole(harness, owner.token, guild.id, {name: 'Unlucky'});
			const upsert = vi.spyOn(GuildRepository.prototype, 'upsertRole').mockRejectedValueOnce(new Error('write failed'));
			try {
				await createBuilder(harness, owner.token)
					.patch(`/guilds/${guild.id}/roles/${role.id}`)
					.body({icon: await pngDataUrl(64, 64)})
					.expect(HTTP_STATUS.INTERNAL_SERVER_ERROR)
					.execute();
			} finally {
				upsert.mockRestore();
			}
			const [key] = roleIconUploads(harness);
			assert(key !== undefined);
			await runDeletionWorker(harness);
			expect(harness.storageService.hasObject(Config.s3.buckets.cdn, key)).toBe(false);
			expect(await getAssetDeletionQueue().getQueueSize()).toBe(0);
		});

		test('a failed save keeps the image that a parallel save of the same file stored', async () => {
			const role = await createRole(harness, owner.token, guild.id, {name: 'Twice', unicode_emoji: '🎨'});
			const image = await pngDataUrl(64, 64);
			let parallel: GuildRoleResponse | undefined;
			const upsert = vi.spyOn(GuildRepository.prototype, 'upsertRole').mockImplementationOnce(async () => {
				parallel = await updateRole(harness, owner.token, guild.id, role.id, {icon: image});
				throw new Error('write failed');
			});
			try {
				await createBuilder(harness, owner.token)
					.patch(`/guilds/${guild.id}/roles/${role.id}`)
					.body({icon: image})
					.expect(HTTP_STATUS.INTERNAL_SERVER_ERROR)
					.execute();
			} finally {
				upsert.mockRestore();
			}
			assert(parallel?.icon != null);
			await runDeletionWorker(harness);
			const roles = await getRoles(harness, owner.token, guild.id);
			expect(roles.find((entry) => entry.id === role.id)?.icon).toBe(parallel.icon);
			expect(storedRoleIconKeys(harness, role.id)).toEqual([`role-icons/${role.id}/${parallel.icon}`]);
		});

		test('parallel saves with different images leave only the stored image behind', async () => {
			const role = await createRole(harness, owner.token, guild.id, {name: 'Racers'});
			const first = await updateRole(harness, owner.token, guild.id, role.id, {icon: await pngDataUrl(64, 64, 1)});
			const writeRole = GuildRepository.prototype.upsertRole;
			const waiting: Array<() => void> = [];
			const upsert = vi.spyOn(GuildRepository.prototype, 'upsertRole').mockImplementation(async function (
				this: GuildRepository,
				...args
			) {
				await new Promise<void>((resolve) => {
					waiting.push(resolve);
					if (waiting.length === 2) {
						for (const release of waiting) release();
					}
				});
				return writeRole.apply(this, args);
			});
			let results: Array<GuildRoleResponse>;
			try {
				results = await Promise.all(
					[2, 3].map(async (red) =>
						updateRole(harness, owner.token, guild.id, role.id, {icon: await pngDataUrl(64, 64, red)}),
					),
				);
			} finally {
				upsert.mockRestore();
			}
			expect(new Set(results.map((result) => result.icon)).size).toBe(2);
			await runDeletionWorker(harness);
			const current = (await getRoles(harness, owner.token, guild.id)).find((entry) => entry.id === role.id)?.icon;
			assert(current != null);
			expect(current).not.toBe(first.icon);
			expect(storedRoleIconKeys(harness, role.id)).toEqual([`role-icons/${role.id}/${current}`]);
		});

		async function duringUpload(action: () => Promise<void>): Promise<() => void> {
			const prepare = EntityAssetService.prototype.prepareAssetUpload;
			const spy = vi.spyOn(EntityAssetService.prototype, 'prepareAssetUpload').mockImplementationOnce(async function (
				this: EntityAssetService,
				...args
			) {
				const prepared = await prepare.apply(this, args);
				await action();
				return prepared;
			});
			return () => spy.mockRestore();
		}

		test('a rename and a reorder made during the upload survive the icon save', async () => {
			const role = await createRole(harness, owner.token, guild.id, {name: 'Before', unicode_emoji: '🎨'});
			const other = await createRole(harness, owner.token, guild.id, {name: 'Other'});
			const restore = await duringUpload(async () => {
				await updateRole(harness, owner.token, guild.id, role.id, {name: 'Renamed'});
				await updateRolePositions(harness, owner.token, guild.id, [
					{id: role.id, position: 2},
					{id: other.id, position: 1},
				]);
			});
			const before = (await getRoles(harness, owner.token, guild.id)).find((entry) => entry.id === role.id);
			gateway.dispatches.length = 0;
			let saved: GuildRoleResponse;
			try {
				saved = await updateRole(harness, owner.token, guild.id, role.id, {icon: await pngDataUrl(64, 64)});
			} finally {
				restore();
			}
			const stored = (await getRoles(harness, owner.token, guild.id)).find((entry) => entry.id === role.id);
			expect(stored).toMatchObject({name: 'Renamed', icon: saved.icon, unicode_emoji: null});
			expect(stored?.position).not.toBe(before?.position);
			expect(saved).toMatchObject({name: 'Renamed', position: stored?.position});
			const dispatched = gateway.dispatches
				.filter((entry) => entry.event === 'GUILD_ROLE_UPDATE')
				.map((entry) => (entry.data as {role: GuildRoleResponse}).role)
				.filter((entry) => entry.id === role.id)
				.at(-1);
			expect(dispatched).toMatchObject({name: 'Renamed', icon: saved.icon, position: stored?.position});
		});

		test('deleting the role during the upload keeps it deleted and removes the upload', async () => {
			const role = await createRole(harness, owner.token, guild.id, {name: 'Doomed'});
			const restore = await duringUpload(async () => {
				await createBuilder(harness, owner.token)
					.delete(`/guilds/${guild.id}/roles/${role.id}`)
					.expect(HTTP_STATUS.NO_CONTENT)
					.execute();
			});
			gateway.dispatches.length = 0;
			try {
				await createBuilder(harness, owner.token)
					.patch(`/guilds/${guild.id}/roles/${role.id}`)
					.body({icon: await pngDataUrl(64, 64)})
					.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_ROLE)
					.execute();
			} finally {
				restore();
			}
			expect((await getRoles(harness, owner.token, guild.id)).some((entry) => entry.id === role.id)).toBe(false);
			expect(gateway.dispatches.some((entry) => entry.event === 'GUILD_ROLE_UPDATE')).toBe(false);
			expect(roleIconUploads(harness)).toHaveLength(1);
			await runDeletionWorker(harness);
			expect(storedRoleIconKeys(harness, role.id)).toEqual([]);
		});

		test('an image stored by a parallel save is removed when the deletion worker runs between the two writes', async () => {
			const role = await createRole(harness, owner.token, guild.id, {
				name: 'Racers',
				icon: await pngDataUrl(64, 64, 1),
			});
			let first: GuildRoleResponse | undefined;
			let keysBetweenWrites: Array<string> = [];
			const restore = await duringUpload(async () => {
				first = await updateRole(harness, owner.token, guild.id, role.id, {icon: await pngDataUrl(64, 64, 2)});
				await runDeletionWorker(harness);
				keysBetweenWrites = storedRoleIconKeys(harness, role.id);
			});
			let last: GuildRoleResponse;
			try {
				last = await updateRole(harness, owner.token, guild.id, role.id, {icon: await pngDataUrl(64, 64, 3)});
			} finally {
				restore();
			}
			assert(first?.icon != null && last.icon != null);
			expect(last.icon).not.toBe(first.icon);
			expect(keysBetweenWrites).toContain(`role-icons/${role.id}/${first.icon}`);
			expect(keysBetweenWrites).not.toContain(`role-icons/${role.id}/${role.icon}`);
			await runDeletionWorker(harness);
			expect(storedRoleIconKeys(harness, role.id)).toEqual([`role-icons/${role.id}/${last.icon}`]);
		});

		test('an image stored by a parallel save is removed when the other save switches to an emoji after the worker ran', async () => {
			const role = await createRole(harness, owner.token, guild.id, {
				name: 'Switchers',
				icon: await pngDataUrl(64, 64, 1),
			});
			let parallel: GuildRoleResponse | undefined;
			const restore = await duringUpload(async () => {
				parallel = await updateRole(harness, owner.token, guild.id, role.id, {icon: await pngDataUrl(64, 64, 4)});
				await runDeletionWorker(harness);
			});
			let saved: GuildRoleResponse;
			try {
				saved = await updateRole(harness, owner.token, guild.id, role.id, {unicode_emoji: '🎨'});
			} finally {
				restore();
			}
			assert(parallel?.icon != null);
			expect(saved).toMatchObject({icon: null, unicode_emoji: '🎨'});
			await runDeletionWorker(harness);
			expect(storedRoleIconKeys(harness, role.id)).toEqual([]);
		});

		test.each([
			{
				label: 'a reorder',
				write: async (roleId: string, otherId: string) =>
					updateRolePositions(harness, owner.token, guild.id, [
						{id: roleId, position: 2},
						{id: otherId, position: 1},
					]),
			},
			{
				label: 'a hoist update',
				write: async (roleId: string) => {
					await createBuilder(harness, owner.token)
						.patch(`/guilds/${guild.id}/roles/hoist-positions`)
						.body([{id: roleId, hoist_position: 3}])
						.expect(HTTP_STATUS.NO_CONTENT)
						.execute();
				},
			},
			{
				label: 'a hoist reset',
				write: async () => {
					await createBuilder(harness, owner.token)
						.delete(`/guilds/${guild.id}/roles/hoist-positions`)
						.expect(HTTP_STATUS.NO_CONTENT)
						.execute();
				},
			},
		])('an icon saved during $label survives the position write', async ({write}) => {
			const role = await createRole(harness, owner.token, guild.id, {
				name: 'Ordered',
				icon: await pngDataUrl(64, 64, 1),
			});
			const other = await createRole(harness, owner.token, guild.id, {name: 'Other'});
			await createBuilder(harness, owner.token)
				.patch(`/guilds/${guild.id}/roles/hoist-positions`)
				.body([{id: role.id, hoist_position: 1}])
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			let saved: GuildRoleResponse | undefined;
			const upsertRole = GuildRepository.prototype.upsertRole;
			let armed = true;
			const spy = vi.spyOn(GuildRepository.prototype, 'upsertRole').mockImplementation(async function (
				this: GuildRepository,
				...args
			) {
				if (armed && String(args[0].role_id) === role.id) {
					armed = false;
					saved = await updateRole(harness, owner.token, guild.id, role.id, {icon: await pngDataUrl(64, 64, 2)});
					await runDeletionWorker(harness);
				}
				return upsertRole.apply(this, args);
			});
			gateway.dispatches.length = 0;
			try {
				await write(role.id, other.id);
			} finally {
				spy.mockRestore();
			}
			assert(saved?.icon != null);
			expect(saved.icon).not.toBe(role.icon);
			const stored = (await getRoles(harness, owner.token, guild.id)).find((entry) => entry.id === role.id);
			expect(stored?.icon).toBe(saved.icon);
			const bulk = gateway.dispatches
				.filter((entry) => entry.event === 'GUILD_ROLE_UPDATE_BULK')
				.flatMap((entry) => (entry.data as {roles: Array<GuildRoleResponse>}).roles)
				.filter((entry) => entry.id === role.id);
			expect(bulk.length).toBeGreaterThan(0);
			expect(bulk.every((entry) => entry.icon === saved?.icon)).toBe(true);
			await runDeletionWorker(harness);
			expect(storedRoleIconKeys(harness, role.id)).toEqual([`role-icons/${role.id}/${saved.icon}`]);
		});

		test('an image upload clears the emoji', async () => {
			const role = await createRole(harness, owner.token, guild.id, {name: 'Mods', unicode_emoji: '🛡️'});
			const updated = await updateRole(harness, owner.token, guild.id, role.id, {icon: await pngDataUrl(48, 48)});
			expect(updated.unicode_emoji).toBeNull();
			expect(updated.icon).toMatch(/^[0-9a-f]{8}$/);
		});

		test('null clears and an absent field keeps the current value', async () => {
			const role = await createRole(harness, owner.token, guild.id, {name: 'Admin', icon: await pngDataUrl(64, 64)});
			const renamed = await updateRole(harness, owner.token, guild.id, role.id, {name: 'Admins'});
			expect(renamed.icon).toBe(role.icon);

			const cleared = await updateRole(harness, owner.token, guild.id, role.id, {icon: null});
			expect(cleared.icon).toBeNull();
			expect((await drainDeletionQueue()).map((item) => item.s3Key)).toEqual([`role-icons/${role.id}/${role.icon}`]);

			const emoji = await updateRole(harness, owner.token, guild.id, role.id, {unicode_emoji: '🔥'});
			expect(emoji.unicode_emoji).toBe('🔥');
			const emojiCleared = await updateRole(harness, owner.token, guild.id, role.id, {unicode_emoji: null});
			expect(emojiCleared.unicode_emoji).toBeNull();

			const both = await updateRole(harness, owner.token, guild.id, role.id, {icon: null, unicode_emoji: null});
			expect(both).toMatchObject({icon: null, unicode_emoji: null});
		});

		test('dispatches both fields in GUILD_ROLE_UPDATE', async () => {
			const role = await createRole(harness, owner.token, guild.id, {name: 'Mods'});
			gateway.dispatches.length = 0;
			await updateRole(harness, owner.token, guild.id, role.id, {unicode_emoji: '🛡️'});
			const dispatch = gateway.dispatches.find((entry) => entry.event === 'GUILD_ROLE_UPDATE');
			assert(dispatch !== undefined);
			expect((dispatch.data as {role: GuildRoleResponse}).role).toMatchObject({
				id: role.id,
				icon: null,
				unicode_emoji: '🛡️',
			});
		});

		test('role list includes both keys', async () => {
			await createRole(harness, owner.token, guild.id, {name: 'Mods', unicode_emoji: '🛡️'});
			const roles = await getRoles(harness, owner.token, guild.id);
			for (const role of roles) {
				expect(role).toHaveProperty('icon');
				expect(role).toHaveProperty('unicode_emoji');
			}
			expect(roles.find((role) => role.name === 'Mods')?.unicode_emoji).toBe('🛡️');
		});
	});

	describe('@everyone', () => {
		test('rejects an image without writing to storage', async () => {
			await expectRoleIconError(
				owner.token,
				{icon: await pngDataUrl(64, 64)},
				'icon',
				ValidationErrorCodes.CANNOT_SET_ICON_ON_EVERYONE_ROLE,
				guild.id,
			);
			expect(roleIconUploads(harness)).toEqual([]);
		});

		test('rejects an emoji', async () => {
			await expectRoleIconError(
				owner.token,
				{unicode_emoji: '🔥'},
				'unicode_emoji',
				ValidationErrorCodes.CANNOT_SET_ICON_ON_EVERYONE_ROLE,
				guild.id,
			);
		});

		test('accepts null for both fields', async () => {
			const role = await updateRole(harness, owner.token, guild.id, guild.id, {icon: null, unicode_emoji: null});
			expect(role).toMatchObject({icon: null, unicode_emoji: null});
		});
	});

	describe('permissions', () => {
		test('a member without manage roles gets 403 and nothing is uploaded', async () => {
			const plain = await createRole(harness, owner.token, guild.id, {name: 'Plain', permissions: '0'});
			const member = await addMemberWithRole(plain.id);
			await createBuilder(harness, member.token)
				.patch(`/guilds/${guild.id}/roles/${plain.id}`)
				.body({icon: await pngDataUrl(64, 64)})
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
			await createBuilder(harness, member.token)
				.post(`/guilds/${guild.id}/roles`)
				.body({name: 'Sneaky', icon: await pngDataUrl(64, 64)})
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
			expect(roleIconUploads(harness)).toEqual([]);
		});

		test('a manager below the role gets 403 and nothing is uploaded', async () => {
			const high = await createRole(harness, owner.token, guild.id, {name: 'High', permissions: '0'});
			const manager = await createRole(harness, owner.token, guild.id, {
				name: 'Manager',
				permissions: Permissions.MANAGE_ROLES.toString(),
			});
			await updateRolePositions(harness, owner.token, guild.id, [
				{id: high.id, position: 3},
				{id: manager.id, position: 2},
			]);
			const member = await addMemberWithRole(manager.id);
			await createBuilder(harness, member.token)
				.patch(`/guilds/${guild.id}/roles/${high.id}`)
				.body({icon: await pngDataUrl(64, 64)})
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
			expect(roleIconUploads(harness)).toEqual([]);
		});
	});

	describe('image limits', () => {
		test('rejects an image over 256 KiB', async () => {
			const image = Buffer.from((await pngDataUrl(64, 64)).split(',')[1], 'base64');
			const padded = Buffer.concat([image, Buffer.alloc(256 * 1024 + 1 - image.length)]);
			await expectRoleIconError(
				owner.token,
				{icon: dataUrl('image/png', padded)},
				'icon',
				ValidationErrorCodes.IMAGE_SIZE_EXCEEDS_LIMIT,
			);
		});

		test('rejects images outside 32 to 1024 pixels', async () => {
			for (const [width, height] of [
				[16, 16],
				[2048, 2048],
				[64, 16],
			]) {
				await expectRoleIconError(
					owner.token,
					{icon: await pngDataUrl(width, height)},
					'icon',
					ValidationErrorCodes.IMAGE_DIMENSIONS_OUT_OF_RANGE,
				);
			}
			expect(roleIconUploads(harness)).toEqual([]);
		});

		test('rejects animated WebP, SVG and GIF', async () => {
			const svg = dataUrl('image/svg+xml', Buffer.from('<svg xmlns="http://www.w3.org/2000/svg"></svg>'));
			const gif = dataUrl(
				'image/gif',
				Buffer.from('R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7', 'base64'),
			);
			for (const icon of [animatedWebpDataUrl(), svg, gif]) {
				await expectRoleIconError(owner.token, {icon}, 'icon', ValidationErrorCodes.INVALID_IMAGE_FORMAT);
			}
			expect(roleIconUploads(harness)).toEqual([]);
		});

		test('rejects a body over the role icon limit with 413', async () => {
			const limit = jsonAssetBodyLimit({role_icon: 1});
			const body = JSON.stringify({name: 'Huge', pad: 'a'.repeat(limit)});
			const response = await harness.requestJson({
				path: `/guilds/${guild.id}/roles`,
				method: 'POST',
				body,
				headers: {Authorization: owner.token},
			});
			expect(response.status).toBe(413);
		});
	});

	describe('copying the icon of another role', () => {
		async function createCopy(sourceRoleId: string): Promise<GuildRoleResponse> {
			return createBuilder<GuildRoleResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/roles`)
				.body({name: 'Copy', icon_source_role_id: sourceRoleId})
				.expect(HTTP_STATUS.OK)
				.execute();
		}

		test('stores its own copy of the image, which outlives the source role', async () => {
			const source = await createRole(harness, owner.token, guild.id, {name: 'Admin', icon: await pngDataUrl(64, 64)});
			await runDeletionWorker(harness);
			const copy = await createCopy(source.id);
			expect(copy).toMatchObject({icon: source.icon, unicode_emoji: null});
			const copyKey = `role-icons/${copy.id}/${copy.icon}`;
			expect(harness.storageService.hasObject(Config.s3.buckets.cdn, copyKey)).toBe(true);

			await createBuilder(harness, owner.token)
				.delete(`/guilds/${guild.id}/roles/${source.id}`)
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			await runDeletionWorker(harness);
			expect(harness.storageService.hasObject(Config.s3.buckets.cdn, `role-icons/${source.id}/${source.icon}`)).toBe(
				false,
			);
			expect(harness.storageService.hasObject(Config.s3.buckets.cdn, copyKey)).toBe(true);
		});

		test('changing the copy leaves the source image in place', async () => {
			const source = await createRole(harness, owner.token, guild.id, {name: 'Admin', icon: await pngDataUrl(64, 64)});
			const copy = await createCopy(source.id);
			await updateRole(harness, owner.token, guild.id, copy.id, {icon: null});
			await runDeletionWorker(harness);
			expect(storedRoleIconKeys(harness, copy.id)).toEqual([]);
			expect(storedRoleIconKeys(harness, source.id)).toEqual([`role-icons/${source.id}/${source.icon}`]);
		});

		test('copies a Unicode emoji', async () => {
			const source = await createRole(harness, owner.token, guild.id, {name: 'Mods', unicode_emoji: '🛡️'});
			expect(await createCopy(source.id)).toMatchObject({icon: null, unicode_emoji: '🛡️'});
			expect(harness.storageService.getCopiedObjects()).toEqual([]);
		});

		test('a source role without an icon gives a role without an icon', async () => {
			const source = await createRole(harness, owner.token, guild.id, {name: 'Plain'});
			expect(await createCopy(source.id)).toMatchObject({icon: null, unicode_emoji: null});
		});

		test('a source image missing from storage gives a role without an icon', async () => {
			const source = await createRole(harness, owner.token, guild.id, {name: 'Admin', icon: await pngDataUrl(64, 64)});
			await harness.storageService.deleteObject(Config.s3.buckets.cdn, `role-icons/${source.id}/${source.icon}`);
			expect(await createCopy(source.id)).toMatchObject({icon: null, unicode_emoji: null});
		});

		test('rejects a role from another community', async () => {
			const otherGuild = await createGuild(harness, owner.token, 'Other Guild');
			const foreign = await createRole(harness, owner.token, otherGuild.id, {name: 'Foreign', unicode_emoji: '🔥'});
			await createBuilder(harness, owner.token)
				.post(`/guilds/${guild.id}/roles`)
				.body({name: 'Copy', icon_source_role_id: foreign.id})
				.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_ROLE)
				.execute();
		});

		test('rejects a source role together with a new icon', async () => {
			const source = await createRole(harness, owner.token, guild.id, {name: 'Mods', unicode_emoji: '🛡️'});
			await expectRoleIconError(
				owner.token,
				{icon_source_role_id: source.id, unicode_emoji: '🔥'},
				'icon_source_role_id',
				ValidationErrorCodes.ROLE_ICON_SOURCE_AND_ICON_EXCLUSIVE,
			);
		});
	});

	test('deleting a role queues its icon', async () => {
		const role = await createRole(harness, owner.token, guild.id, {name: 'Doomed', icon: await pngDataUrl(64, 64)});
		await createBuilder(harness, owner.token)
			.delete(`/guilds/${guild.id}/roles/${role.id}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const queued = await drainDeletionQueue();
		expect(queued).toHaveLength(1);
		expect(queued[0]).toMatchObject({
			s3Key: `role-icons/${role.id}/${role.icon}`,
			staleReference: {entityType: 'role', assetType: 'role_icon', entityId: role.id, guildId: guild.id},
		});
	});

	test.each([
		{label: 'an image', initial: {icon: 'image'}},
		{label: 'no icon', initial: {}},
	])('an image saved while a role with $label is deleted is queued with the old one', async ({initial}) => {
		const role = await createRole(harness, owner.token, guild.id, {
			name: 'Doomed',
			...(initial.icon ? {icon: await pngDataUrl(64, 64, 1)} : {}),
		});
		let saved: GuildRoleResponse | undefined;
		const spy = vi.spyOn(gateway, 'getMembersWithRole').mockImplementationOnce(async () => {
			saved = await updateRole(harness, owner.token, guild.id, role.id, {icon: await pngDataUrl(64, 64, 2)});
			await runDeletionWorker(harness);
			return [];
		});
		try {
			await createBuilder(harness, owner.token)
				.delete(`/guilds/${guild.id}/roles/${role.id}`)
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
		} finally {
			spy.mockRestore();
		}
		assert(saved?.icon != null);
		const queued = await drainDeletionQueue();
		const expected = [role.icon, saved.icon]
			.filter((hash) => hash != null)
			.map((hash) => `role-icons/${role.id}/${hash}`);
		expect(new Set(queued.map((entry) => entry.s3Key))).toEqual(new Set(expected));
		await Promise.all(queued.map((entry) => getAssetDeletionQueue().requeueItem(entry)));
		await runDeletionWorker(harness);
		expect(storedRoleIconKeys(harness, role.id)).toEqual([]);
	});

	test('the guild roles RPC collection includes both keys', async () => {
		await createRole(harness, owner.token, guild.id, {name: 'Mods', unicode_emoji: '🛡️'});
		const response = await createBuilder<{data: {roles: Array<GuildRoleResponse>}}>(harness, '')
			.post('/test/rpc-session-init')
			.body({type: 'guild_collection', guild_id: guild.id, collection: 'roles'})
			.expect(HTTP_STATUS.OK)
			.execute();
		const mods = response.data.roles.find((role) => role.name === 'Mods');
		expect(mods).toMatchObject({icon: null, unicode_emoji: '🛡️'});
		for (const role of response.data.roles) {
			expect(role).toHaveProperty('icon');
		}
	});

	test('template import drops an invalid unicode_emoji', async () => {
		const imported = await createBuilder<GuildResponse>(harness, owner.token)
			.post('/guilds')
			.body({
				name: 'Template Icons',
				template: {
					name: 'Source',
					description: null,
					roles: [
						{id: 1, name: '@everyone', permissions: '0'},
						{id: 2, name: 'Valid', permissions: '0', unicode_emoji: '🔥'},
						{id: 3, name: 'Invalid', permissions: '0', unicode_emoji: 'not an emoji'},
					],
					channels: [],
				},
			})
			.execute();
		const roles = await getRoles(harness, owner.token, imported.id);
		expect(roles.find((role) => role.name === 'Valid')?.unicode_emoji).toBe('🔥');
		expect(roles.find((role) => role.name === 'Invalid')?.unicode_emoji).toBeNull();
	});

	test('staff can clear a role icon', async () => {
		const role = await createRole(harness, owner.token, guild.id, {name: 'Abusive', icon: await pngDataUrl(64, 64)});
		const admin = await setUserACLs(harness, owner, ['admin:authenticate', 'guild:update:settings']);
		gateway.dispatches.length = 0;
		await createBuilder(harness, admin.token)
			.delete(`/admin/guilds/${guild.id}/roles/${role.id}/icon`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const roles = await getRoles(harness, owner.token, guild.id);
		expect(roles.find((entry) => entry.id === role.id)).toMatchObject({icon: null, unicode_emoji: null});
		expect((await drainDeletionQueue()).map((item) => item.s3Key)).toEqual([`role-icons/${role.id}/${role.icon}`]);
		const dispatch = gateway.dispatches.find((entry) => entry.event === 'GUILD_ROLE_UPDATE');
		assert(dispatch !== undefined);
		expect((dispatch.data as {role: GuildRoleResponse}).role).toMatchObject({id: role.id, icon: null});
	});

	test('staff clearing a role icon keeps a rename made after the role was read', async () => {
		const role = await createRole(harness, owner.token, guild.id, {name: 'Before', icon: await pngDataUrl(64, 64)});
		const admin = await setUserACLs(harness, owner, ['admin:authenticate', 'guild:update:settings']);
		const readRole = GuildRepository.prototype.getRole;
		const getRole = vi.spyOn(GuildRepository.prototype, 'getRole').mockImplementationOnce(async function (
			this: GuildRepository,
			...args
		) {
			const stale = await readRole.apply(this, args);
			getRole.mockRestore();
			await updateRole(harness, owner.token, guild.id, role.id, {name: 'Renamed'});
			return stale;
		});
		try {
			await createBuilder(harness, admin.token)
				.delete(`/admin/guilds/${guild.id}/roles/${role.id}/icon`)
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
		} finally {
			getRole.mockRestore();
		}
		const roles = await getRoles(harness, owner.token, guild.id);
		expect(roles.find((entry) => entry.id === role.id)).toMatchObject({name: 'Renamed', icon: null});
	});

	test('clearing a role icon needs the guild settings ACL', async () => {
		const role = await createRole(harness, owner.token, guild.id, {name: 'Mods', unicode_emoji: '🛡️'});
		const admin = await setUserACLs(harness, owner, ['admin:authenticate']);
		await createBuilder(harness, admin.token)
			.delete(`/admin/guilds/${guild.id}/roles/${role.id}/icon`)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
});
