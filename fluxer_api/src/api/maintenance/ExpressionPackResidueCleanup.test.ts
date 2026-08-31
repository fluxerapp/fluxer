// SPDX-License-Identifier: AGPL-3.0-or-later

import {spawnSync} from 'node:child_process';
import {createServer} from 'node:net';
import {
	getDefaultPostgresClient,
	type IPostgresClient,
	initPostgres,
	shutdownPostgres,
} from '@pkgs/postgres/src/Client';
import {afterAll, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';
import type {EmojiID, GuildID, StickerID, UserID} from '../BrandedTypes';
import {createEmojiID, createGuildID, createStickerID, createUserID} from '../BrandedTypes';
import {setCassandraQueryExecutorForTesting, upsertOne} from '../database/CassandraQueryExecution';
import {defineTable} from '../database/CassandraTableDsl';
import {ensurePostgresKvSchema, PostgresKvQueryExecutor} from '../database/PostgresKvQueryExecutor';
import {GuildRepository} from '../guild/repositories/GuildRepository';
import {ExpressionAssetPurger} from '../guild/services/content/ExpressionAssetPurger';
import {AssetDeletionQueue} from '../infrastructure/AssetDeletionQueue';
import {getKVClient} from '../middleware/ServiceRegistry';
import {GuildEmoji} from '../models/GuildEmoji';
import {GuildSticker} from '../models/GuildSticker';
import {startDockerContainer} from '../test/DockerTestContainer';
import {InMemoryCassandraQueryExecutor} from '../test/InMemoryCassandraQueryExecutor';
import {
	ExpressionPackResidueCleanup,
	type ExpressionPackResidueRepository,
	formatExpressionPackResidueReport,
} from './ExpressionPackResidueCleanup';

interface ExpressionPackSeedRow {
	pack_id: GuildID;
	pack_type: string;
	creator_id: UserID;
	name: string;
	description: string | null;
	created_at: Date;
	updated_at: Date;
	version: number;
}

const ExpressionPacksSeed = defineTable<ExpressionPackSeedRow, 'pack_id'>({
	name: 'expression_packs',
	columns: ['pack_id', 'pack_type', 'creator_id', 'name', 'description', 'created_at', 'updated_at', 'version'],
	primaryKey: ['pack_id'],
});

const KV_TABLE = 'kv_pack_residue';
const CONTAINER = `fluxer-packresidue-${process.pid.toString(36)}-${Date.now().toString(36)}`;
const dockerAvailable = spawnSync('docker', ['version'], {stdio: 'ignore'}).status === 0;

const CREATOR_ID = createUserID(700000000000000001n);
const PACK_ID = createGuildID(900000000000000001n);
const SECOND_PACK_ID = createGuildID(900000000000000002n);
const GUILD_ID = createGuildID(100000000000000001n);
const PACK_ID_THAT_IS_A_GUILD = createGuildID(100000000000000002n);
const PACK_EMOJI_ID = createEmojiID(910000000000000001n);
const SECOND_PACK_EMOJI_ID = createEmojiID(910000000000000002n);
const PACK_STICKER_ID = createStickerID(920000000000000001n);
const GUILD_EMOJI_ID = createEmojiID(110000000000000001n);
const GUILD_STICKER_ID = createStickerID(120000000000000001n);

async function sleep(ms: number): Promise<void> {
	await new Promise((resolve) => setTimeout(resolve, ms));
}

async function freePort(): Promise<number> {
	return new Promise((resolve, reject) => {
		const server = createServer();
		server.on('error', reject);
		server.listen(0, '127.0.0.1', () => {
			const address = server.address();
			if (typeof address === 'string' || address === null) {
				reject(new Error('no port'));
				return;
			}
			const port = address.port;
			server.close(() => resolve(port));
		});
	});
}

function assetQueue(): AssetDeletionQueue {
	return new AssetDeletionQueue(getKVClient());
}

function createCleanup(repository?: ExpressionPackResidueRepository): ExpressionPackResidueCleanup {
	return new ExpressionPackResidueCleanup(repository ?? new GuildRepository(), new ExpressionAssetPurger(assetQueue()));
}

async function seedPack(packId: GuildID): Promise<void> {
	await upsertOne(
		ExpressionPacksSeed.insert({
			pack_id: packId,
			pack_type: 'emoji',
			creator_id: CREATOR_ID,
			name: `pack ${packId}`,
			description: null,
			created_at: new Date('2026-01-01T00:00:00.000Z'),
			updated_at: new Date('2026-01-01T00:00:00.000Z'),
			version: 1,
		}),
	);
}

async function seedGuild(guildId: GuildID): Promise<void> {
	await new GuildRepository().upsertPartial(guildId, {name: `guild ${guildId}`, owner_id: CREATOR_ID});
}

async function seedEmoji(guildId: GuildID, emojiId: EmojiID, name: string): Promise<void> {
	await new GuildRepository().upsertEmoji({
		guild_id: guildId,
		emoji_id: emojiId,
		name,
		creator_id: CREATOR_ID,
		animated: false,
		nsfw: null,
		version: 1,
	});
}

async function seedSticker(guildId: GuildID, stickerId: StickerID, name: string): Promise<void> {
	await new GuildRepository().upsertSticker({
		guild_id: guildId,
		sticker_id: stickerId,
		name,
		description: null,
		animated: false,
		nsfw: null,
		tags: null,
		creator_id: CREATOR_ID,
		version: 1,
	});
}

async function emojiNames(guildId: GuildID): Promise<Array<string>> {
	const emojis = await new GuildRepository().listEmojis(guildId);
	return emojis.map((emoji) => emoji.name).sort();
}

async function stickerNames(guildId: GuildID): Promise<Array<string>> {
	const stickers = await new GuildRepository().listStickers(guildId);
	return stickers.map((sticker) => sticker.name).sort();
}

async function queuedS3Keys(): Promise<Array<string>> {
	const queue = assetQueue();
	const items = await queue.getBatch(100);
	return items.map((item) => item.s3Key).filter((key) => key.length > 0);
}

function defineResidueSuite(resetData: () => Promise<void>): void {
	beforeEach(async () => {
		await resetData();
		await assetQueue().clear();
	});

	it('deletes every emoji and sticker owned by a pack id and queues their assets', async () => {
		await seedPack(PACK_ID);
		await seedEmoji(PACK_ID, PACK_EMOJI_ID, 'packblob');
		await seedEmoji(PACK_ID, SECOND_PACK_EMOJI_ID, 'packwave');
		await seedSticker(PACK_ID, PACK_STICKER_ID, 'packsticker');

		const report = await createCleanup().run({apply: true});

		expect(report.emojiCount).toBe(2);
		expect(report.stickerCount).toBe(1);
		expect(await emojiNames(PACK_ID)).toEqual([]);
		expect(await stickerNames(PACK_ID)).toEqual([]);
		expect(await new GuildRepository().getEmojiById(PACK_EMOJI_ID)).toBeNull();
		expect(await new GuildRepository().getStickerById(PACK_STICKER_ID)).toBeNull();
		expect(await queuedS3Keys()).toEqual([
			`emojis/${PACK_EMOJI_ID}`,
			`emojis/${SECOND_PACK_EMOJI_ID}`,
			`stickers/${PACK_STICKER_ID}`,
		]);
	});

	it('leaves a real guild emoji and sticker completely untouched', async () => {
		await seedPack(PACK_ID);
		await seedGuild(GUILD_ID);
		await seedEmoji(GUILD_ID, GUILD_EMOJI_ID, 'guildblob');
		await seedSticker(GUILD_ID, GUILD_STICKER_ID, 'guildsticker');

		const report = await createCleanup().run({apply: true});

		expect(report.residues).toEqual([]);
		expect(report.emojiCount).toBe(0);
		expect(report.stickerCount).toBe(0);
		expect(await emojiNames(GUILD_ID)).toEqual(['guildblob']);
		expect(await stickerNames(GUILD_ID)).toEqual(['guildsticker']);
		expect(await new GuildRepository().getEmojiById(GUILD_EMOJI_ID)).not.toBeNull();
		expect(await new GuildRepository().getStickerById(GUILD_STICKER_ID)).not.toBeNull();
		expect(await queuedS3Keys()).toEqual([]);
	});

	it('deletes only the pack rows out of a mixed dataset', async () => {
		await seedPack(PACK_ID);
		await seedPack(SECOND_PACK_ID);
		await seedGuild(GUILD_ID);
		await seedEmoji(PACK_ID, PACK_EMOJI_ID, 'packblob');
		await seedSticker(PACK_ID, PACK_STICKER_ID, 'packsticker');
		await seedEmoji(GUILD_ID, GUILD_EMOJI_ID, 'guildblob');
		await seedSticker(GUILD_ID, GUILD_STICKER_ID, 'guildsticker');

		const report = await createCleanup().run({apply: true});

		expect(report.packIds).toEqual([PACK_ID, SECOND_PACK_ID]);
		expect(report.residues.map((residue) => residue.packId)).toEqual([PACK_ID]);
		expect(await emojiNames(PACK_ID)).toEqual([]);
		expect(await stickerNames(PACK_ID)).toEqual([]);
		expect(await emojiNames(GUILD_ID)).toEqual(['guildblob']);
		expect(await stickerNames(GUILD_ID)).toEqual(['guildsticker']);
		expect(await queuedS3Keys()).toEqual([`emojis/${PACK_EMOJI_ID}`, `stickers/${PACK_STICKER_ID}`]);
	});

	it('is a clean no-op on an empty dataset', async () => {
		const report = await createCleanup().run({apply: true});

		expect(report).toEqual({
			applied: true,
			packIds: [],
			liveGuildPackIds: [],
			residues: [],
			emojiCount: 0,
			stickerCount: 0,
		});
		expect(await queuedS3Keys()).toEqual([]);
	});

	it('reports the rows it would delete without deleting anything by default', async () => {
		await seedPack(PACK_ID);
		await seedEmoji(PACK_ID, PACK_EMOJI_ID, 'packblob');
		await seedSticker(PACK_ID, PACK_STICKER_ID, 'packsticker');

		const report = await createCleanup().run({apply: false});

		expect(formatExpressionPackResidueReport(report)).toBe(
			[
				'expression pack residue cleanup: DRY RUN, nothing deleted and nothing queued',
				'pack ids read from expression_packs: 1',
				`pack ${PACK_ID}: 1 emojis, 1 stickers`,
				`  guild_emojis guild_id=${PACK_ID} emoji_id=${PACK_EMOJI_ID} name="packblob"`,
				`  guild_stickers guild_id=${PACK_ID} sticker_id=${PACK_STICKER_ID} name="packsticker"`,
				'totals: 1 emojis, 1 stickers, 2 assets',
			].join('\n'),
		);
		expect(await emojiNames(PACK_ID)).toEqual(['packblob']);
		expect(await stickerNames(PACK_ID)).toEqual(['packsticker']);
		expect(await queuedS3Keys()).toEqual([]);
	});

	it('is a no-op when run again after a real run', async () => {
		await seedPack(PACK_ID);
		await seedEmoji(PACK_ID, PACK_EMOJI_ID, 'packblob');
		await seedSticker(PACK_ID, PACK_STICKER_ID, 'packsticker');

		await createCleanup().run({apply: true});
		await assetQueue().clear();
		const second = await createCleanup().run({apply: true});

		expect(second.packIds).toEqual([PACK_ID]);
		expect(second.residues).toEqual([]);
		expect(second.emojiCount).toBe(0);
		expect(second.stickerCount).toBe(0);
		expect(await queuedS3Keys()).toEqual([]);
	});

	it('skips a pack id that also names a live guild and leaves its expressions alone', async () => {
		await seedPack(PACK_ID_THAT_IS_A_GUILD);
		await seedGuild(PACK_ID_THAT_IS_A_GUILD);
		await seedEmoji(PACK_ID_THAT_IS_A_GUILD, GUILD_EMOJI_ID, 'guildblob');
		await seedSticker(PACK_ID_THAT_IS_A_GUILD, GUILD_STICKER_ID, 'guildsticker');

		const report = await createCleanup().run({apply: true});

		expect(report.liveGuildPackIds).toEqual([PACK_ID_THAT_IS_A_GUILD]);
		expect(report.residues).toEqual([]);
		expect(await emojiNames(PACK_ID_THAT_IS_A_GUILD)).toEqual(['guildblob']);
		expect(await stickerNames(PACK_ID_THAT_IS_A_GUILD)).toEqual(['guildsticker']);
		expect(await queuedS3Keys()).toEqual([]);
		expect(formatExpressionPackResidueReport(report)).toContain(
			`skipped pack ${PACK_ID_THAT_IS_A_GUILD}: that id is a live guild, its expressions were never read`,
		);
	});

	it('refuses to delete when a pack partition hands back a row owned by another guild', async () => {
		await seedPack(PACK_ID);
		const deleteEmoji = vi.fn(async () => {});
		const deleteSticker = vi.fn(async () => {});
		const repository: ExpressionPackResidueRepository = {
			listGuilds: async () => [],
			listEmojis: async () => [
				new GuildEmoji({
					guild_id: GUILD_ID,
					emoji_id: GUILD_EMOJI_ID,
					name: 'guildblob',
					creator_id: CREATOR_ID,
					animated: false,
					nsfw: null,
					version: 1,
				}),
			],
			listStickers: async () => [],
			deleteEmoji,
			deleteSticker,
		};

		await expect(createCleanup(repository).run({apply: true})).rejects.toThrow(
			`guild_emojis partition ${PACK_ID} returned a row owned by guild ${GUILD_ID}`,
		);
		expect(deleteEmoji).not.toHaveBeenCalled();
		expect(deleteSticker).not.toHaveBeenCalled();
		expect(await queuedS3Keys()).toEqual([]);
	});

	it('refuses to delete when a pack partition hands back a sticker owned by another guild', async () => {
		await seedPack(PACK_ID);
		const deleteSticker = vi.fn(async () => {});
		const repository: ExpressionPackResidueRepository = {
			listGuilds: async () => [],
			listEmojis: async () => [],
			listStickers: async () => [
				new GuildSticker({
					guild_id: GUILD_ID,
					sticker_id: GUILD_STICKER_ID,
					name: 'guildsticker',
					description: null,
					animated: false,
					nsfw: null,
					tags: null,
					creator_id: CREATOR_ID,
					version: 1,
				}),
			],
			deleteEmoji: async () => {},
			deleteSticker,
		};

		await expect(createCleanup(repository).run({apply: true})).rejects.toThrow(
			`guild_stickers partition ${PACK_ID} returned a row owned by guild ${GUILD_ID}`,
		);
		expect(deleteSticker).not.toHaveBeenCalled();
	});
}

describe('ExpressionPackResidueCleanup against the cassandra query shapes', () => {
	const executor = new InMemoryCassandraQueryExecutor();

	beforeAll(() => {
		setCassandraQueryExecutorForTesting(executor);
	});

	afterAll(() => {
		setCassandraQueryExecutorForTesting(new InMemoryCassandraQueryExecutor());
	});

	defineResidueSuite(async () => {
		executor.reset();
	});
});

describe.skipIf(!dockerAvailable)('ExpressionPackResidueCleanup against the postgres kv backend', () => {
	let raw: IPostgresClient;

	beforeAll(async () => {
		const port = await freePort();
		startDockerContainer([
			'run',
			'-d',
			'--name',
			CONTAINER,
			'-e',
			'POSTGRES_USER=fluxer',
			'-e',
			'POSTGRES_PASSWORD=fluxer',
			'-e',
			'POSTGRES_DB=fluxer',
			'-p',
			`127.0.0.1:${port}:5432`,
			'postgres:16-alpine',
			'-c',
			'fsync=off',
		]);
		let ready = false;
		for (let attempt = 0; attempt < 180 && !ready; attempt += 1) {
			await sleep(500);
			const probe = spawnSync('docker', ['exec', CONTAINER, 'pg_isready', '-U', 'fluxer', '-d', 'fluxer'], {
				stdio: 'ignore',
			});
			if (probe.status !== 0) continue;
			try {
				await initPostgres({
					url: `postgres://fluxer:fluxer@127.0.0.1:${port}/fluxer`,
					maxConnections: 1,
					kvTable: KV_TABLE,
				});
				await getDefaultPostgresClient().query('SELECT 1');
				ready = true;
			} catch {
				await shutdownPostgres().catch(() => {});
			}
		}
		if (!ready) throw new Error('postgres never came up');
		raw = getDefaultPostgresClient();
		await ensurePostgresKvSchema(raw);
		setCassandraQueryExecutorForTesting(new PostgresKvQueryExecutor(raw));
	}, 900_000);

	afterAll(async () => {
		setCassandraQueryExecutorForTesting(new InMemoryCassandraQueryExecutor());
		await shutdownPostgres().catch(() => {});
		spawnSync('docker', ['rm', '-f', CONTAINER], {stdio: 'ignore'});
	});

	defineResidueSuite(async () => {
		await raw.query(`DELETE FROM ${KV_TABLE}`);
	});
});

describe('ExpressionPackResidueCleanup on a deployment without the pack tables', () => {
	afterAll(() => {
		setCassandraQueryExecutorForTesting(new InMemoryCassandraQueryExecutor());
	});

	it('treats a missing expression_packs table as nothing to clean', async () => {
		setCassandraQueryExecutorForTesting({
			executeQuery: async () => {
				throw new Error('unconfigured table expression_packs');
			},
			executeBatch: async () => {},
		});

		const report = await createCleanup().run({apply: true});

		expect(report.packIds).toEqual([]);
		expect(report.residues).toEqual([]);
		expect(await queuedS3Keys()).toEqual([]);
	});

	it('rethrows any other failure while reading expression_packs', async () => {
		setCassandraQueryExecutorForTesting({
			executeQuery: async () => {
				throw new Error('Operation timed out');
			},
			executeBatch: async () => {},
		});

		await expect(createCleanup().run({apply: true})).rejects.toThrow('Operation timed out');
	});
});
