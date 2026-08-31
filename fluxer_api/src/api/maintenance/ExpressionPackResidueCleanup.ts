// SPDX-License-Identifier: AGPL-3.0-or-later

import type {EmojiID, GuildID, StickerID} from '../BrandedTypes';
import {fetchPage, type PagedQueryResult} from '../database/CassandraQueryExecution';
import {defineTable} from '../database/CassandraTableDsl';
import type {IGuildRepositoryAggregate} from '../guild/repositories/IGuildRepositoryAggregate';
import type {ExpressionAssetPurger} from '../guild/services/content/ExpressionAssetPurger';

const PACK_ID_PAGE_SIZE = 500;
const LIVE_GUILD_CHUNK_SIZE = 100;
const EXPRESSION_PACKS_TABLE = 'expression_packs';
const MISSING_TABLE_PATTERN = /unconfigured|undefined table|does not exist/i;

interface ExpressionPackIdRow {
	pack_id: GuildID;
}

const ExpressionPacks = defineTable<ExpressionPackIdRow, 'pack_id'>({
	name: EXPRESSION_PACKS_TABLE,
	columns: ['pack_id'],
	primaryKey: ['pack_id'],
});

const SCAN_EXPRESSION_PACK_IDS_QUERY = ExpressionPacks.selectCql();

export type ExpressionPackResidueRepository = Pick<
	IGuildRepositoryAggregate,
	'listGuilds' | 'listEmojis' | 'listStickers' | 'deleteEmoji' | 'deleteSticker'
>;

interface ExpressionPackResidueEmoji {
	id: EmojiID;
	name: string;
}

interface ExpressionPackResidueSticker {
	id: StickerID;
	name: string;
}

interface ExpressionPackResidue {
	packId: GuildID;
	emojis: Array<ExpressionPackResidueEmoji>;
	stickers: Array<ExpressionPackResidueSticker>;
}

interface ExpressionPackResidueReport {
	applied: boolean;
	packIds: Array<GuildID>;
	liveGuildPackIds: Array<GuildID>;
	residues: Array<ExpressionPackResidue>;
	emojiCount: number;
	stickerCount: number;
}

function compareIds(left: bigint, right: bigint): number {
	if (left === right) return 0;
	return left < right ? -1 : 1;
}

function isMissingPackTableError(error: unknown): boolean {
	const message = error instanceof Error ? error.message : String(error);
	return message.includes(EXPRESSION_PACKS_TABLE) && MISSING_TABLE_PATTERN.test(message);
}

export class ExpressionPackResidueCleanup {
	constructor(
		private readonly guildRepository: ExpressionPackResidueRepository,
		private readonly assetPurger: ExpressionAssetPurger,
	) {}

	async run(options: {apply: boolean}): Promise<ExpressionPackResidueReport> {
		const packIds = await this.scanPackIds();
		const report: ExpressionPackResidueReport = {
			applied: options.apply,
			packIds,
			liveGuildPackIds: [],
			residues: [],
			emojiCount: 0,
			stickerCount: 0,
		};
		if (packIds.length === 0) {
			return report;
		}
		report.liveGuildPackIds = await this.findLiveGuildIds(packIds);
		const liveGuildIds = new Set(report.liveGuildPackIds);
		for (const packId of packIds) {
			if (liveGuildIds.has(packId)) {
				continue;
			}
			const residue = await this.collectResidue(packId);
			if (residue.emojis.length === 0 && residue.stickers.length === 0) {
				continue;
			}
			report.residues.push(residue);
			report.emojiCount += residue.emojis.length;
			report.stickerCount += residue.stickers.length;
			if (options.apply) {
				await this.deleteResidue(residue);
			}
		}
		return report;
	}

	private async scanPackIds(): Promise<Array<GuildID>> {
		const packIds = new Set<GuildID>();
		let pageState: string | null = null;
		try {
			do {
				const page: PagedQueryResult<ExpressionPackIdRow> = await fetchPage<ExpressionPackIdRow>(
					SCAN_EXPRESSION_PACK_IDS_QUERY,
					{},
					{pageSize: PACK_ID_PAGE_SIZE, pageState},
				);
				for (const row of page.rows) {
					packIds.add(row.pack_id);
				}
				pageState = page.pageState;
			} while (pageState !== null);
		} catch (error) {
			if (!isMissingPackTableError(error)) {
				throw error;
			}
			return [];
		}
		return [...packIds].sort(compareIds);
	}

	private async findLiveGuildIds(packIds: ReadonlyArray<GuildID>): Promise<Array<GuildID>> {
		const liveGuildIds: Array<GuildID> = [];
		for (let index = 0; index < packIds.length; index += LIVE_GUILD_CHUNK_SIZE) {
			const guilds = await this.guildRepository.listGuilds(packIds.slice(index, index + LIVE_GUILD_CHUNK_SIZE));
			for (const guild of guilds) {
				liveGuildIds.push(guild.id);
			}
		}
		return liveGuildIds.sort(compareIds);
	}

	private async collectResidue(packId: GuildID): Promise<ExpressionPackResidue> {
		const [emojis, stickers] = await Promise.all([
			this.guildRepository.listEmojis(packId),
			this.guildRepository.listStickers(packId),
		]);
		for (const emoji of emojis) {
			if (emoji.guildId !== packId) {
				throw new Error(`guild_emojis partition ${packId} returned a row owned by guild ${emoji.guildId}`);
			}
		}
		for (const sticker of stickers) {
			if (sticker.guildId !== packId) {
				throw new Error(`guild_stickers partition ${packId} returned a row owned by guild ${sticker.guildId}`);
			}
		}
		return {
			packId,
			emojis: emojis
				.map((emoji) => ({id: emoji.id, name: emoji.name}))
				.sort((left, right) => compareIds(left.id, right.id)),
			stickers: stickers
				.map((sticker) => ({id: sticker.id, name: sticker.name}))
				.sort((left, right) => compareIds(left.id, right.id)),
		};
	}

	private async deleteResidue(residue: ExpressionPackResidue): Promise<void> {
		for (const emoji of residue.emojis) {
			await this.guildRepository.deleteEmoji(residue.packId, emoji.id);
			await this.assetPurger.purgeEmoji(emoji.id.toString());
		}
		for (const sticker of residue.stickers) {
			await this.guildRepository.deleteSticker(residue.packId, sticker.id);
			await this.assetPurger.purgeSticker(sticker.id.toString());
		}
	}
}

export function formatExpressionPackResidueReport(report: ExpressionPackResidueReport): string {
	const lines: Array<string> = [
		report.applied
			? 'expression pack residue cleanup: APPLIED, rows deleted and assets queued for purge'
			: 'expression pack residue cleanup: DRY RUN, nothing deleted and nothing queued',
		`pack ids read from expression_packs: ${report.packIds.length}`,
	];
	for (const packId of report.liveGuildPackIds) {
		lines.push(`skipped pack ${packId}: that id is a live guild, its expressions were never read`);
	}
	if (report.residues.length === 0) {
		lines.push('no guild_emojis or guild_stickers rows are owned by a pack id');
	}
	for (const residue of report.residues) {
		lines.push(`pack ${residue.packId}: ${residue.emojis.length} emojis, ${residue.stickers.length} stickers`);
		for (const emoji of residue.emojis) {
			lines.push(`  guild_emojis guild_id=${residue.packId} emoji_id=${emoji.id} name=${JSON.stringify(emoji.name)}`);
		}
		for (const sticker of residue.stickers) {
			lines.push(
				`  guild_stickers guild_id=${residue.packId} sticker_id=${sticker.id} name=${JSON.stringify(sticker.name)}`,
			);
		}
	}
	lines.push(
		`totals: ${report.emojiCount} emojis, ${report.stickerCount} stickers, ${report.emojiCount + report.stickerCount} assets`,
	);
	return lines.join('\n');
}
