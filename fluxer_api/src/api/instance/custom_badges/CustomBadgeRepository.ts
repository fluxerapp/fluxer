// SPDX-License-Identifier: AGPL-3.0-or-later

import {BatchBuilder, executeConditional, fetchMany} from '@app/api/database/CassandraQueryExecution';
import type {PreparedQuery} from '@app/api/database/CassandraTypes';
import type {CustomBadgeRow} from '@app/api/database/types/CustomBadgeTypes';
import {CustomBadges, CustomBadgesByPosition} from '@app/api/Tables';
import type {CustomBadgeType} from '@fluxer/constants/src/BadgeConstants';

export interface CustomBadge {
	readonly id: string;
	readonly type: CustomBadgeType;
	readonly position: bigint;
	readonly name: string;
	readonly description: string | null;
	readonly asset: string;
	readonly href: string | null;
	readonly updatedAt: Date;
}

export interface CustomBadgeChange {
	readonly before: CustomBadge | null;
	readonly after: CustomBadge | null;
}

const LOOKUP_CHUNK_SIZE = 100;
const QUORUM = {consistency: 'localQuorum'} as const;
const SERIAL = {consistency: 'serial'} as const;

const FETCH_BY_ID = CustomBadges.select({where: CustomBadges.where.eq('badge_id'), limit: 1});
const FETCH_BY_IDS = CustomBadges.select({where: CustomBadges.where.in('badge_id', 'badge_ids')});

function customBadgeFromRow(row: CustomBadgeRow): CustomBadge {
	return {
		id: row.badge_id.toString(),
		type: row.type === 'guild' ? 'guild' : 'user',
		position: row.position,
		name: row.name,
		description: row.description,
		asset: row.asset,
		href: row.href,
		updatedAt: row.updated_at,
	};
}

function customBadgeToRow(badge: CustomBadge): CustomBadgeRow {
	return {
		badge_id: BigInt(badge.id),
		type: badge.type,
		position: badge.position,
		name: badge.name,
		description: badge.description,
		asset: badge.asset,
		href: badge.href,
		updated_at: badge.updatedAt,
	};
}

export function compareCustomBadges(left: CustomBadge, right: CustomBadge): number {
	if (left.type !== right.type) return left.type === 'user' ? -1 : 1;
	if (left.position !== right.position) return left.position < right.position ? -1 : 1;
	return compareBigInts(BigInt(left.id), BigInt(right.id));
}

export function compareBigInts(left: bigint, right: bigint): number {
	if (left === right) return 0;
	return left < right ? -1 : 1;
}

export async function findCustomBadge(badgeId: bigint, serial = false): Promise<CustomBadge | null> {
	const [row] = await fetchMany<CustomBadgeRow>(
		FETCH_BY_ID.bind({badge_id: badgeId}),
		undefined,
		serial ? SERIAL : QUORUM,
	);
	return row ? customBadgeFromRow(row) : null;
}

export async function findCustomBadges(badgeIds: ReadonlyArray<bigint>): Promise<Array<CustomBadge>> {
	const unique = [...new Set(badgeIds)];
	const chunks: Array<Array<bigint>> = [];
	for (let index = 0; index < unique.length; index += LOOKUP_CHUNK_SIZE) {
		chunks.push(unique.slice(index, index + LOOKUP_CHUNK_SIZE));
	}
	const rows = await Promise.all(
		chunks.map((chunk) => fetchMany<CustomBadgeRow>(FETCH_BY_IDS.bind({badge_ids: chunk}), undefined, QUORUM)),
	);
	return rows.flat().map(customBadgeFromRow);
}

export interface CustomBadgeRangeQuery {
	readonly type: CustomBadgeType;
	readonly after?: bigint;
	readonly before?: bigint;
	readonly descending?: boolean;
	readonly limit: number;
	readonly serial?: boolean;
}

export async function listCustomBadgeRange(query: CustomBadgeRangeQuery): Promise<Array<CustomBadge>> {
	const where = [CustomBadgesByPosition.where.eq('type')];
	const params: Record<string, string | bigint> = {type: query.type};
	if (query.after !== undefined) {
		where.push(CustomBadgesByPosition.where.gt('position', 'after_position'));
		params.after_position = query.after;
	}
	if (query.before !== undefined) {
		where.push(CustomBadgesByPosition.where.lt('position', 'before_position'));
		params.before_position = query.before;
	}
	const template = CustomBadgesByPosition.select({
		where,
		limit: query.limit,
		...(query.descending ? {orderBy: {col: 'position', direction: 'DESC'}} : {}),
	});
	const rows = await fetchMany<CustomBadgeRow>(template.bind(params), undefined, query.serial ? SERIAL : QUORUM);
	return rows.map(customBadgeFromRow);
}

export async function insertCustomBadgeIfMissing(badge: CustomBadge): Promise<boolean> {
	return executeConditional(CustomBadges.insertIfNotExists(customBadgeToRow(badge)));
}

export async function writeCustomBadgePositionRow(badge: CustomBadge): Promise<void> {
	await new BatchBuilder()
		.addPrepared(CustomBadgesByPosition.upsertAll(customBadgeToRow(badge)))
		.execute(true, 'localQuorum');
}

function positionKey(badge: CustomBadge): string {
	return `${badge.type}:${badge.position}`;
}

const BATCH_BYTES_MAX = 32 * 1024;
const STATEMENT_OVERHEAD_BYTES = 128;

function rowBytes(badge: CustomBadge): number {
	return (
		STATEMENT_OVERHEAD_BYTES +
		Buffer.byteLength(badge.name) +
		Buffer.byteLength(badge.description ?? '') +
		Buffer.byteLength(badge.asset) +
		Buffer.byteLength(badge.href ?? '')
	);
}

interface CustomBadgeBatch {
	readonly queries: ReadonlyArray<PreparedQuery>;
	readonly bytes: number;
}

function planCustomBadgeBatches(changes: ReadonlyArray<CustomBadgeChange>): Array<CustomBadgeBatch> {
	const written = new Set<string>();
	for (const {after} of changes) {
		if (!after) continue;
		const key = positionKey(after);
		if (written.has(key)) {
			throw new Error(`Two custom badges would share position ${key}`);
		}
		written.add(key);
	}
	const batches: Array<CustomBadgeBatch> = [];
	let queries: Array<PreparedQuery> = [];
	let bytes = 0;
	for (const {before, after} of changes) {
		const group: Array<PreparedQuery> = [];
		let size = 0;
		if (before && !written.has(positionKey(before))) {
			group.push(CustomBadgesByPosition.deleteByPk({type: before.type, position: before.position}));
			size += STATEMENT_OVERHEAD_BYTES;
		}
		if (after) {
			const row = customBadgeToRow(after);
			group.push(CustomBadges.upsertAll(row), CustomBadgesByPosition.upsertAll(row));
			size += 2 * rowBytes(after);
		} else if (before) {
			group.push(CustomBadges.deleteByPk({badge_id: BigInt(before.id)}));
			size += STATEMENT_OVERHEAD_BYTES;
		}
		if (group.length === 0) continue;
		if (queries.length > 0 && bytes + size > BATCH_BYTES_MAX) {
			batches.push({queries, bytes});
			queries = [];
			bytes = 0;
		}
		queries.push(...group);
		bytes += size;
	}
	if (queries.length > 0) batches.push({queries, bytes});
	return batches;
}

export async function applyCustomBadgeChanges(changes: ReadonlyArray<CustomBadgeChange>): Promise<void> {
	for (const {queries} of planCustomBadgeBatches(changes)) {
		const batch = new BatchBuilder();
		for (const query of queries) batch.addPrepared(query);
		await batch.execute(true, 'localQuorum');
	}
}
