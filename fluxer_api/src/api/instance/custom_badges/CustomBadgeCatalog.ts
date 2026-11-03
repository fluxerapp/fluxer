// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import {
	applyCustomBadgeChanges,
	type CustomBadge,
	type CustomBadgeChange,
	compareCustomBadges,
	findCustomBadge,
	findCustomBadges,
	insertCustomBadgeIfMissing,
	listCustomBadgeRange,
	writeCustomBadgePositionRow,
} from '@app/api/instance/custom_badges/CustomBadgeRepository';
import {assetStorageKey} from '@app/api/instance/InstanceAssetReferences';
import type {CustomBadgeCatalogState, InstanceConfigRepository} from '@app/api/instance/InstanceConfigRepository';
import {Logger} from '@app/api/Logger';
import {
	CUSTOM_BADGE_REORDER_IDS_MAX,
	type CustomBadgeType,
	DISCOVERY_GUILD_BADGE_LIMIT,
} from '@fluxer/constants/src/BadgeConstants';
import {UnknownBadgeError} from '@fluxer/errors/src/domains/badge/UnknownBadgeError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {ResourceLockedError} from '@fluxer/errors/src/domains/core/ResourceLockedError';
import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';

const CUSTOM_BADGE_POSITION_STEP = 1n << 32n;
const INT64_MIN = -(1n << 63n);
const INT64_MAX = (1n << 63n) - 1n;
const BADGE_CACHE_MAX = 5000;
const MISSING_CACHE_MAX = 5000;
const CACHE_TTL_MS = 5 * 60_000;
const LOCK_KEY = 'custom-badges:catalog-write';
const PENDING_VERSION_KEY = 'custom-badges:pending-version';
const LOCK_TTL_SECONDS = 15;
const LOCK_WAIT_MS = 10_000;
const LOCK_RETRY_MS = 50;
const PUBLISH_ATTEMPTS = 3;
const SEARCH_SCAN_BUDGET = 5000;
const SEARCH_SCAN_PAGE = 1000;
const REBALANCE_WINDOW = 16;
const REBALANCE_WINDOW_MAX = 1024;
const BADGE_TYPES: ReadonlyArray<CustomBadgeType> = ['user', 'guild'];

type CustomBadgeMoveDirection = 'up' | 'down' | 'first' | 'last';

export type CustomBadgeMove = {direction: CustomBadgeMoveDirection} | {afterId: bigint | null};

export interface CustomBadgeCursor {
	readonly type: CustomBadgeType;
	readonly position: bigint;
}

export interface CustomBadgePage {
	readonly badges: ReadonlyArray<CustomBadge>;
	readonly next: string | null;
}

export interface StableRead<T> {
	readonly value: T;
	readonly version: string;
	readonly stable: boolean;
}

export interface CustomBadgeDiscoveryHead {
	readonly badges: ReadonlyArray<CustomBadge>;
	readonly partial: boolean;
}

export interface NewCustomBadge {
	readonly id: bigint;
	readonly type: CustomBadgeType;
	readonly name: string;
	readonly description: string | null;
	readonly asset: string;
	readonly href: string | null;
}

export type CustomBadgeEdit = Partial<Pick<CustomBadge, 'name' | 'description' | 'href' | 'asset'>>;

interface CacheGeneration {
	readonly version: string;
	readonly createdAt: number;
	readonly badges: Map<string, CustomBadge>;
	readonly missing: Map<string, true>;
	head: Promise<CustomBadgeDiscoveryHead> | null;
}

interface CustomBadgeCatalogDeps {
	readonly instanceConfigRepository: Pick<
		InstanceConfigRepository,
		| 'getCustomBadgeCatalogState'
		| 'readCustomBadgeCatalogStateForWrite'
		| 'updateCustomBadgeCatalogState'
		| 'readLegacyCustomBadges'
	>;
	readonly cacheService: Pick<ICacheService, 'acquireLock' | 'extendLock' | 'releaseLock' | 'get' | 'set' | 'delete'>;
	readonly snowflakeService: Pick<ISnowflakeService, 'generate'>;
	readonly mediaEndpoint: () => string;
	readonly now?: () => Date;
}

function encodeCustomBadgeCursor(badge: Pick<CustomBadge, 'type' | 'position'>): string {
	return `${badge.type === 'guild' ? 'g' : 'u'}${badge.position}`;
}

function decodeCustomBadgeCursor(cursor: string): CustomBadgeCursor {
	const match = /^([gu])(-?[0-9]{1,19})$/.exec(cursor);
	const position = match ? BigInt(match[2]!) : null;
	if (!match || position === null || position < INT64_MIN || position > INT64_MAX) {
		throw InputValidationError.create('after', 'This cursor is not valid');
	}
	return {type: match[1] === 'g' ? 'guild' : 'user', position};
}

export class CustomBadgeCatalogUnpublishedError extends ServiceUnavailableError {
	constructor() {
		super({
			message:
				'The badge change was saved but clients have not been told yet. The next badge change or api restart tells them',
		});
	}
}

function sleep(ms: number): Promise<void> {
	return new Promise((resolve) => setTimeout(resolve, ms));
}

function touch<K, V>(map: Map<K, V>, key: K, value: V, max: number): void {
	map.delete(key);
	map.set(key, value);
	while (map.size > max) {
		const oldest = map.keys().next();
		if (oldest.done) break;
		map.delete(oldest.value);
	}
}

function assertPosition(position: bigint): bigint {
	if (position <= INT64_MIN || position >= INT64_MAX) {
		throw InputValidationError.create('position', 'The badge order has no room left. Reorder the badges of this type');
	}
	return position;
}

function nextVersionAfter(current: string, candidate: bigint): string {
	const stored = BigInt(current);
	return (candidate > stored ? candidate : stored + 1n).toString();
}

export class CustomBadgeCatalog {
	private generation: CacheGeneration | null = null;
	private queue: Promise<void> = Promise.resolve();

	constructor(private readonly deps: CustomBadgeCatalogDeps) {}

	private now(): Date {
		return this.deps.now?.() ?? new Date();
	}

	async state(): Promise<CustomBadgeCatalogState> {
		return this.deps.instanceConfigRepository.getCustomBadgeCatalogState();
	}

	clientVersion(version: string): string {
		const media = createHash('sha256').update(this.deps.mediaEndpoint()).digest('hex').slice(0, 8);
		return `${version}.${media}`;
	}

	private cacheFor(version: string): CacheGeneration {
		const current = this.generation;
		if (current && current.version === version && Date.now() - current.createdAt < CACHE_TTL_MS) {
			return current;
		}
		const fresh: CacheGeneration = {
			version,
			createdAt: Date.now(),
			badges: new Map(),
			missing: new Map(),
			head: null,
		};
		this.generation = fresh;
		return fresh;
	}

	private async stableRead<T>(read: (generation: CacheGeneration) => Promise<T>): Promise<StableRead<T>> {
		const before = (await this.state()).version;
		const generation = this.cacheFor(before);
		const value = await read(generation);
		const after = (await this.state()).version;
		return {value, version: before, stable: before === after};
	}

	private async stillCurrent(generation: CacheGeneration): Promise<boolean> {
		return (await this.state()).version === generation.version && this.generation === generation;
	}

	async readByIds(ids: Iterable<bigint | string>): Promise<StableRead<Array<CustomBadge>>> {
		const wanted = [...new Set([...ids].map((id) => id.toString()))];
		return this.stableRead(async (generation) => {
			const found: Array<CustomBadge> = [];
			const misses: Array<bigint> = [];
			for (const id of wanted) {
				const cached = generation.badges.get(id);
				if (cached) {
					touch(generation.badges, id, cached, BADGE_CACHE_MAX);
					found.push(cached);
				} else if (!generation.missing.has(id)) {
					misses.push(BigInt(id));
				}
			}
			if (misses.length > 0) {
				const loaded = await findCustomBadges(misses);
				found.push(...loaded);
				if (await this.stillCurrent(generation)) {
					const loadedIds = new Set(loaded.map((badge) => badge.id));
					for (const badge of loaded) touch(generation.badges, badge.id, badge, BADGE_CACHE_MAX);
					for (const id of misses) {
						if (!loadedIds.has(id.toString())) touch(generation.missing, id.toString(), true, MISSING_CACHE_MAX);
					}
				}
			}
			return found.sort(compareCustomBadges);
		});
	}

	async getByIds(ids: Iterable<bigint | string>, type?: CustomBadgeType): Promise<Array<CustomBadge>> {
		const {value} = await this.readByIds(ids);
		return type ? value.filter((badge) => badge.type === type) : value;
	}

	async getById(id: bigint | string): Promise<CustomBadge | null> {
		const [badge] = await this.getByIds([id]);
		return badge ?? null;
	}

	async readPage(params: {
		type?: CustomBadgeType;
		after?: string;
		limit: number;
	}): Promise<StableRead<CustomBadgePage>> {
		return this.stableRead(() => this.listPage(params));
	}

	private orderedTypes(type: CustomBadgeType | undefined, cursor: CustomBadgeCursor | null): Array<CustomBadgeType> {
		if (cursor && type && cursor.type !== type) {
			throw InputValidationError.create('after', 'This cursor belongs to the other badge type');
		}
		const types = type ? [type] : [...BADGE_TYPES];
		return cursor ? types.slice(types.indexOf(cursor.type)) : types;
	}

	private async listPage(params: {type?: CustomBadgeType; after?: string; limit: number}): Promise<CustomBadgePage> {
		const cursor = params.after ? decodeCustomBadgeCursor(params.after) : null;
		const badges: Array<CustomBadge> = [];
		for (const [index, type] of this.orderedTypes(params.type, cursor).entries()) {
			const rows = await listCustomBadgeRange({
				type,
				after: index === 0 ? cursor?.position : undefined,
				limit: params.limit - badges.length + 1,
			});
			badges.push(...rows);
			if (badges.length > params.limit) break;
		}
		const page = badges.slice(0, params.limit);
		const last = page.at(-1);
		return {badges: page, next: badges.length > params.limit && last ? encodeCustomBadgeCursor(last) : null};
	}

	async readDiscoveryHead(): Promise<StableRead<CustomBadgeDiscoveryHead>> {
		return this.stableRead(async (generation) => {
			if (generation.head) return generation.head;
			const head = listCustomBadgeRange({type: 'guild', limit: DISCOVERY_GUILD_BADGE_LIMIT + 1}).then((rows) => ({
				badges: rows.slice(0, DISCOVERY_GUILD_BADGE_LIMIT),
				partial: rows.length > DISCOVERY_GUILD_BADGE_LIMIT,
			}));
			const result = await head;
			if (await this.stillCurrent(generation)) generation.head = head;
			return result;
		});
	}

	async search(params: {
		type?: CustomBadgeType;
		query: string;
		after?: string;
		limit: number;
	}): Promise<CustomBadgePage> {
		const cursor = params.after ? decodeCustomBadgeCursor(params.after) : null;
		const needle = params.query.toLowerCase();
		const matches: Array<CustomBadge> = [];
		const seen = new Set<string>();
		if (!cursor && /^[0-9]{1,20}$/.test(params.query) && BigInt(params.query) <= INT64_MAX) {
			for (const badge of await this.getByIds([params.query], params.type)) {
				matches.push(badge);
				seen.add(badge.id);
			}
		}
		let scanned = 0;
		let lastScanned: CustomBadge | null = null;
		for (const [index, type] of this.orderedTypes(params.type, cursor).entries()) {
			let after = index === 0 ? cursor?.position : undefined;
			for (;;) {
				const budget = Math.min(SEARCH_SCAN_PAGE, SEARCH_SCAN_BUDGET - scanned);
				if (budget <= 0) return {badges: matches, next: lastScanned ? encodeCustomBadgeCursor(lastScanned) : null};
				const rows = await listCustomBadgeRange({type, after, limit: budget});
				for (const row of rows) {
					scanned += 1;
					lastScanned = row;
					if (!seen.has(row.id) && row.name.toLowerCase().includes(needle)) {
						matches.push(row);
						seen.add(row.id);
						if (matches.length >= params.limit) {
							return {badges: matches, next: encodeCustomBadgeCursor(row)};
						}
					}
				}
				if (rows.length < budget) break;
				after = rows.at(-1)!.position;
			}
		}
		return {badges: matches, next: null};
	}

	async create(input: NewCustomBadge): Promise<CustomBadge> {
		return this.write(async (commit) => {
			const [last] = await listCustomBadgeRange({type: input.type, descending: true, limit: 1, serial: true});
			const badge: CustomBadge = {
				id: input.id.toString(),
				type: input.type,
				position: assertPosition(last ? last.position + CUSTOM_BADGE_POSITION_STEP : CUSTOM_BADGE_POSITION_STEP),
				name: input.name,
				description: input.description,
				asset: input.asset,
				href: input.href,
				updatedAt: this.now(),
			};
			await commit([{before: null, after: badge}]);
			return badge;
		});
	}

	async update(
		badgeId: bigint,
		edit: CustomBadgeEdit,
		settle?: (change: {before: CustomBadge; after: CustomBadge}) => Promise<void>,
	): Promise<{before: CustomBadge; after: CustomBadge}> {
		return this.write(async (commit) => {
			const before = await this.requireBadge(badgeId);
			const after: CustomBadge = {...before, ...edit, updatedAt: this.now()};
			await commit([{before, after}]);
			return {before, after};
		}, settle);
	}

	async delete(badgeId: bigint, settle?: (removed: CustomBadge) => Promise<void>): Promise<CustomBadge> {
		return this.write(async (commit) => {
			const before = await this.requireBadge(badgeId);
			await commit([{before, after: null}]);
			return before;
		}, settle);
	}

	async move(badgeId: bigint, move: CustomBadgeMove): Promise<CustomBadge> {
		return this.write(async (commit) => {
			const current = await this.requireBadge(badgeId);
			const changes = await this.planMove(current, move);
			if (changes.length > 0) await commit(changes);
			return changes.find((change) => change.after?.id === current.id)?.after ?? current;
		});
	}

	async reorderAll(order: ReadonlyArray<bigint>): Promise<void> {
		const ids = order.map(String);
		if (new Set(ids).size !== ids.length) {
			throw InputValidationError.create('badge_ids', 'Each badge can only be listed once');
		}
		await this.write(async (commit) => {
			const slots = new Map<CustomBadgeType, Array<CustomBadge>>();
			let total = 0;
			for (const type of BADGE_TYPES) {
				const rows = await listCustomBadgeRange({type, limit: CUSTOM_BADGE_REORDER_IDS_MAX + 1, serial: true});
				slots.set(type, rows);
				total += rows.length;
			}
			if (total > CUSTOM_BADGE_REORDER_IDS_MAX) {
				throw InputValidationError.create(
					'badge_ids',
					`This instance has more than ${CUSTOM_BADGE_REORDER_IDS_MAX} badges. Move badges one at a time with PATCH /admin/badges/order`,
				);
			}
			const stored = new Map((await findCustomBadges(order)).map((badge) => [badge.id, badge]));
			if (ids.some((id) => !stored.has(id))) throw new UnknownBadgeError();
			const listed = [...slots.values()].flat();
			if (ids.length !== total || listed.some((row) => !stored.has(row.id))) {
				throw InputValidationError.create('badge_ids', 'List every badge on this instance');
			}
			const now = this.now();
			const changes: Array<CustomBadgeChange> = [];
			for (const type of BADGE_TYPES) {
				const rows = slots.get(type)!;
				const ordered = ids.map((id) => stored.get(id)!).filter((badge) => badge.type === type);
				if (ordered.length !== rows.length) {
					throw InputValidationError.create('badge_ids', 'List every badge on this instance');
				}
				for (const [index, badge] of ordered.entries()) {
					const slot = rows[index]!;
					if (slot.id !== badge.id || slot.position !== badge.position) {
						changes.push({before: null, after: {...badge, position: slot.position, updatedAt: now}});
					}
				}
			}
			if (changes.length > 0) await commit(changes);
		});
	}

	async updateSettings(patch: {max_per_user?: number; max_per_guild?: number}): Promise<CustomBadgeCatalogState> {
		return this.deps.instanceConfigRepository.updateCustomBadgeCatalogState((current) => ({...current, ...patch}));
	}

	async initialize(): Promise<void> {
		await this.migrate();
		const pending = await this.deps.cacheService.get<string>(PENDING_VERSION_KEY);
		if (pending !== null) {
			await this.withLock(() => this.publishPending());
		}
	}

	async migrate(): Promise<void> {
		const {instanceConfigRepository} = this.deps;
		if ((await instanceConfigRepository.readCustomBadgeCatalogStateForWrite()).migrated) return;
		await this.withLock(async () => {
			if ((await instanceConfigRepository.readCustomBadgeCatalogStateForWrite()).migrated) return;
			const legacy = await instanceConfigRepository.readLegacyCustomBadges();
			const existing = new Map(
				(await findCustomBadges(legacy.map((badge) => BigInt(badge.id)))).map((badge) => [badge.id, badge]),
			);
			const used = new Map<CustomBadgeType, Set<bigint>>(BADGE_TYPES.map((type) => [type, new Set()]));
			for (const badge of existing.values()) used.get(badge.type)!.add(badge.position);
			const counters = new Map<CustomBadgeType, bigint>(BADGE_TYPES.map((type) => [type, 0n]));
			const now = this.now();
			for (const entry of legacy) {
				const type: CustomBadgeType = entry.type;
				const taken = used.get(type)!;
				let position = (counters.get(type)! + 1n) * CUSTOM_BADGE_POSITION_STEP;
				counters.set(type, counters.get(type)! + 1n);
				const stored = existing.get(entry.id);
				if (stored) {
					await writeCustomBadgePositionRow(stored);
					continue;
				}
				while (taken.has(position)) position += CUSTOM_BADGE_POSITION_STEP;
				taken.add(position);
				const badge: CustomBadge = {
					id: entry.id,
					type,
					position,
					name: entry.name,
					description: entry.description,
					asset: entry.asset,
					href: entry.href,
					updatedAt: now,
				};
				const kept = (await insertCustomBadgeIfMissing(badge)) ? badge : await findCustomBadge(BigInt(entry.id), true);
				if (kept) await writeCustomBadgePositionRow(kept);
			}
			const version = await this.deps.snowflakeService.generate();
			await instanceConfigRepository.updateCustomBadgeCatalogState((current) => ({
				...current,
				version: nextVersionAfter(current.version, version),
				migrated: true,
			}));
			Logger.info({badges: legacy.length}, 'Moved custom badges into their own tables');
		});
	}

	private async requireBadge(badgeId: bigint): Promise<CustomBadge> {
		const badge = await findCustomBadge(badgeId, true);
		if (!badge) throw new UnknownBadgeError();
		return badge;
	}

	private moved(badge: CustomBadge, position: bigint): CustomBadge {
		return {...badge, position: assertPosition(position), updatedAt: this.now()};
	}

	private async planMove(current: CustomBadge, move: CustomBadgeMove): Promise<Array<CustomBadgeChange>> {
		const type = current.type;
		if ('direction' in move) {
			switch (move.direction) {
				case 'up':
				case 'down': {
					const [neighbour] = await listCustomBadgeRange(
						move.direction === 'up'
							? {type, before: current.position, descending: true, limit: 1, serial: true}
							: {type, after: current.position, limit: 1, serial: true},
					);
					if (!neighbour) return [];
					return [
						{before: current, after: this.moved(current, neighbour.position)},
						{before: neighbour, after: this.moved(neighbour, current.position)},
					];
				}
				case 'first':
					return this.planMoveAfter(current, null);
				case 'last': {
					const [last] = await listCustomBadgeRange({type, descending: true, limit: 1, serial: true});
					if (!last || last.id === current.id) return [];
					return [{before: current, after: this.moved(current, last.position + CUSTOM_BADGE_POSITION_STEP)}];
				}
			}
		}
		return this.planMoveAfter(current, move.afterId);
	}

	private async planMoveAfter(current: CustomBadge, afterId: bigint | null): Promise<Array<CustomBadgeChange>> {
		const type = current.type;
		if (afterId === null) {
			const [first] = await listCustomBadgeRange({type, limit: 1, serial: true});
			if (!first || first.id === current.id) return [];
			return [{before: current, after: this.moved(current, first.position - CUSTOM_BADGE_POSITION_STEP)}];
		}
		if (afterId.toString() === current.id) {
			throw InputValidationError.create('after_id', 'A badge cannot be moved after itself');
		}
		const anchor = await findCustomBadge(afterId, true);
		if (!anchor || anchor.type !== type) throw new UnknownBadgeError();
		const [following] = await listCustomBadgeRange({type, after: anchor.position, limit: 1, serial: true});
		if (following?.id === current.id) return [];
		if (!following) {
			return [{before: current, after: this.moved(current, anchor.position + CUSTOM_BADGE_POSITION_STEP)}];
		}
		if (following.position - anchor.position > 1n) {
			return [{before: current, after: this.moved(current, (anchor.position + following.position) / 2n)}];
		}
		return this.planRebalance(current, anchor);
	}

	private async planRebalance(current: CustomBadge, anchor: CustomBadge): Promise<Array<CustomBadgeChange>> {
		const type = current.type;
		for (let window = REBALANCE_WINDOW; window <= REBALANCE_WINDOW_MAX; window *= 2) {
			const others = (rows: Array<CustomBadge>) => rows.filter((row) => row.id !== current.id);
			const earlier = others(
				await listCustomBadgeRange({
					type,
					before: anchor.position + 1n,
					descending: true,
					limit: window + 2,
					serial: true,
				}),
			);
			const later = others(await listCustomBadgeRange({type, after: anchor.position, limit: window + 2, serial: true}));
			const lower = earlier[window]?.position ?? null;
			const upper = later[window]?.position ?? null;
			const members = [...earlier.slice(0, window).reverse(), current, ...later.slice(0, window)];
			const count = BigInt(members.length);
			let start: bigint;
			let spacing: bigint;
			if (lower !== null && upper !== null) {
				spacing = (upper - lower) / (count + 1n);
				start = lower + spacing;
			} else if (lower !== null) {
				spacing = CUSTOM_BADGE_POSITION_STEP;
				start = lower + spacing;
			} else if (upper !== null) {
				spacing = CUSTOM_BADGE_POSITION_STEP;
				start = upper - spacing * count;
			} else {
				spacing = CUSTOM_BADGE_POSITION_STEP;
				start = members[0]!.position;
			}
			if (spacing < 1n) continue;
			const changes: Array<CustomBadgeChange> = [];
			for (const [index, member] of members.entries()) {
				const position = start + spacing * BigInt(index);
				if (member.id === current.id || member.position !== position) {
					changes.push({before: member, after: this.moved(member, position)});
				}
			}
			return changes;
		}
		throw InputValidationError.create(
			'after_id',
			'The badge order has no room left here. Reorder the badges of this type',
		);
	}

	private async withLock<T>(fn: () => Promise<T>): Promise<T> {
		const run = this.queue.then(() => this.withDistributedLock(fn));
		this.queue = run.then(
			() => undefined,
			() => undefined,
		);
		return run;
	}

	private async withDistributedLock<T>(fn: () => Promise<T>): Promise<T> {
		const {cacheService} = this.deps;
		const deadline = Date.now() + LOCK_WAIT_MS;
		let token = await cacheService.acquireLock(LOCK_KEY, LOCK_TTL_SECONDS);
		while (!token) {
			if (Date.now() > deadline) throw new ResourceLockedError();
			await sleep(LOCK_RETRY_MS);
			token = await cacheService.acquireLock(LOCK_KEY, LOCK_TTL_SECONDS);
		}
		const held = token;
		const keepAlive = setInterval(
			() => {
				cacheService.extendLock(LOCK_KEY, held, LOCK_TTL_SECONDS).catch((error: unknown) => {
					Logger.warn({error}, 'Could not extend the custom badge catalog lock');
				});
			},
			(LOCK_TTL_SECONDS * 1000) / 3,
		);
		try {
			return await fn();
		} finally {
			clearInterval(keepAlive);
			await cacheService.releaseLock(LOCK_KEY, held).catch(() => false);
		}
	}

	private async write<T>(
		fn: (commit: (changes: ReadonlyArray<CustomBadgeChange>) => Promise<void>) => Promise<T>,
		settle?: (result: T) => Promise<void>,
	): Promise<T> {
		return this.withLock(async () => {
			await this.publishPending();
			const version = await this.deps.snowflakeService.generate();
			let committed = false;
			const result = await fn(async (changes) => {
				if (!committed) {
					await this.deps.cacheService.set(PENDING_VERSION_KEY, version.toString());
					committed = true;
				}
				await applyCustomBadgeChanges(changes);
			});
			if (!committed) return result;
			let unpublished: unknown = null;
			await this.publish(version).catch((error: unknown) => {
				unpublished = error;
			});
			if (settle) await settle(result);
			if (unpublished !== null) throw unpublished;
			return result;
		});
	}

	private async publishPending(): Promise<void> {
		const pending = await this.deps.cacheService.get<string>(PENDING_VERSION_KEY);
		if (pending === null) return;
		const state = await this.deps.instanceConfigRepository.readCustomBadgeCatalogStateForWrite();
		if (/^[0-9]{1,20}$/.test(pending) && BigInt(pending) > BigInt(state.version)) {
			await this.publish(BigInt(pending));
			return;
		}
		await this.deps.cacheService.delete(PENDING_VERSION_KEY);
	}

	private async publish(version: bigint): Promise<void> {
		for (let attempt = 1; attempt <= PUBLISH_ATTEMPTS; attempt++) {
			try {
				await this.deps.instanceConfigRepository.updateCustomBadgeCatalogState((current) => ({
					...current,
					version: nextVersionAfter(current.version, version),
				}));
				await this.deps.cacheService.delete(PENDING_VERSION_KEY);
				return;
			} catch (error) {
				Logger.warn({error, attempt}, 'Could not publish a new custom badge catalog version');
				if (attempt < PUBLISH_ATTEMPTS) await sleep(100 * attempt);
			}
		}
		throw new CustomBadgeCatalogUnpublishedError();
	}
}

export async function isCustomBadgeAssetReferenced(
	instanceConfigRepository: Pick<InstanceConfigRepository, 'readStoredCustomBadgeReferences'>,
	s3Key: string,
): Promise<boolean> {
	const references = await instanceConfigRepository.readStoredCustomBadgeReferences();
	if (references.builtInAssetKeys.has(s3Key)) return true;
	if (references.legacyAssetKeys.has(s3Key)) return true;
	const badgeId = /^custom-badges\/([0-9]{1,20})\//.exec(s3Key)?.[1];
	if (!badgeId) return false;
	const badge = await findCustomBadge(BigInt(badgeId));
	return badge !== null && assetStorageKey(badge.asset) === s3Key;
}
