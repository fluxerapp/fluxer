// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {Db} from '@app/api/database/CassandraTypes';
import type {OperatorPassWallEntryRow, OperatorPassWallStateRow} from '@app/api/database/types/OperatorPassTypes';
import {getOperatorPassWallInfrastructure} from '@app/api/middleware/ServiceSingletons';
import {
	loadOperatorPassEntitlementState,
	resolveOperatorPassHolder,
} from '@app/api/operator_pass/OperatorPassEntitlementService';
import {getOperatorPassWallTier} from '@app/api/operator_pass/OperatorPassPrices';
import type {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import {operatorPassNow} from '@app/api/operator_pass/OperatorPassSchedule';
import {
	OPERATOR_PASS_WALL_AVATAR_SIZES,
	OPERATOR_PASS_WALL_TIER_SCALES,
	operatorPassWallSnapshotUrl,
} from '@app/api/operator_pass/OperatorPassWallAvatarStore';
import {OPERATOR_PASS_WALL_UPDATE_ATTEMPTS} from '@app/api/operator_pass/OperatorPassWallConstants';
import type {OperatorPassWallEntryStore} from '@app/api/operator_pass/OperatorPassWallEntryStore';
import {
	isOperatorPassWallListed,
	isOperatorPassWallStanding,
	operatorPassWallShownName,
} from '@app/api/operator_pass/OperatorPassWallPresentation';
import {
	type OperatorPassWallBlobEntry,
	operatorPassWallPublicId,
} from '@app/api/operator_pass/OperatorPassWallPublisher';
import {isOperatorPassCountHidden} from '@app/api/user/profile_badges/PremiumBadgeMasking';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import type {OperatorPassWallResponse} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';

const PUBLIC_WALL_CACHE_MS = 60_000;

const WALL_STATE_ID = 'wall';

let publicWallCache: {expiresAt: number; value: OperatorPassWallSnapshot} | null = null;

export function invalidateOperatorPassPublicWall(): void {
	publicWallCache = null;
}

interface OperatorPassWallSnapshot {
	version: number;
	frozen: boolean;
	generated_at: string;
	avatar_sizes: Array<number>;
	tier_scales: Array<number>;
	entries: Array<OperatorPassWallBlobEntry>;
}

interface RankedWallEntry {
	entry: OperatorPassWallBlobEntry;
	since: number;
}

export class OperatorPassWallPublicationService {
	constructor(
		private readonly repository: OperatorPassRepository,
		private readonly userRepository: UserRepository,
		private readonly entries: OperatorPassWallEntryStore,
	) {}

	private async buildPublicEntry(
		entry: OperatorPassWallEntryRow,
		now: Date = operatorPassNow(),
		{includePreview = false}: {includePreview?: boolean} = {},
	): Promise<OperatorPassWallBlobEntry | null> {
		return (await this.buildRankedEntry(entry, now, {includePreview}))?.entry ?? null;
	}

	private async buildRankedEntry(
		entry: OperatorPassWallEntryRow,
		now: Date,
		{includePreview = false}: {includePreview?: boolean} = {},
	): Promise<RankedWallEntry | null> {
		if (!isOperatorPassWallListed(entry) || entry.hidden) {
			return null;
		}
		const [user, state] = await Promise.all([
			this.userRepository.findUnique(entry.user_id),
			loadOperatorPassEntitlementState(this.repository, entry.user_id),
		]);
		if (!user || !isOperatorPassWallStanding(user, now.getTime())) {
			return null;
		}
		const holder = resolveOperatorPassHolder(state, now);
		if (!holder || (!includePreview && (await this.holdsOnlyPreviewPassesBeforeSale(entry.user_id, now)))) {
			return null;
		}
		return {
			entry: {
				id: operatorPassWallPublicId(entry.user_id),
				name: operatorPassWallShownName(entry),
				avatar: entry.live_avatar ?? null,
				tier: getOperatorPassWallTier(entry.live_business ? 1 : holder.count, Config.operatorPass.wallTierMinimums),
				...(isOperatorPassCountHidden(user.premiumFlags) ? {} : {count: holder.count}),
				...(entry.live_business ? {business: true as const} : {}),
			},
			since: holder.since.getTime(),
		};
	}

	private async holdsOnlyPreviewPassesBeforeSale(userId: UserID, now: Date): Promise<boolean> {
		const {saleOpensAt} = Config.operatorPass;
		if (!saleOpensAt || now.getTime() >= saleOpensAt.getTime()) {
			return false;
		}
		const held = (await this.repository.listUnitsByHolder(userId)).filter((unit) => unit.status === 'held');
		if (held.length === 0) {
			return false;
		}
		for (const unit of held) {
			const purchase = unit.purchase_id ? await this.repository.findPurchase(unit.purchase_id) : null;
			if (purchase?.preview_label == null) {
				return false;
			}
		}
		return true;
	}

	async snapshot(): Promise<OperatorPassWallSnapshot> {
		for (let attempt = 0; attempt < OPERATOR_PASS_WALL_UPDATE_ATTEMPTS; attempt++) {
			const state = await this.loadState();
			if (state.frozen && state.frozen_snapshot) {
				return JSON.parse(state.frozen_snapshot) as OperatorPassWallSnapshot;
			}
			if (
				publicWallCache &&
				publicWallCache.expiresAt > Date.now() &&
				publicWallCache.value.version === Number(state.wall_version)
			) {
				return publicWallCache.value;
			}
			const value = await this.computeSnapshot(Number(state.wall_version));
			const after = await this.loadState();
			if (after.wall_version === state.wall_version && after.frozen === state.frozen) {
				publicWallCache = {expiresAt: Date.now() + PUBLIC_WALL_CACHE_MS, value};
				return value;
			}
		}
		throw new Error("Operators' wall version changed too often to snapshot");
	}

	async listPublicWall(): Promise<OperatorPassWallResponse> {
		const snapshot = await this.snapshot();
		const size = Math.max(...OPERATOR_PASS_WALL_AVATAR_SIZES);
		return {
			version: snapshot.version,
			operators: snapshot.entries.map((entry) => ({
				id: entry.id,
				name: entry.name,
				avatar: entry.avatar,
				avatar_url: entry.avatar ? operatorPassWallSnapshotUrl(entry.avatar, size) : null,
				...(entry.count !== undefined ? {count: entry.count} : {}),
				tier: entry.tier,
				...(entry.business ? {business: true as const} : {}),
			})),
		};
	}

	async loadState(): Promise<OperatorPassWallStateRow> {
		const existing = await this.repository.findWallState(WALL_STATE_ID);
		if (existing) {
			return existing;
		}
		const created: OperatorPassWallStateRow = {
			id: WALL_STATE_ID,
			wall_version: 0n,
			frozen: false,
			frozen_at: null,
			frozen_by: null,
			frozen_snapshot: null,
			published_at: null,
			version: 1,
		};
		await this.repository.createWallState(created);
		return (await this.repository.findWallState(WALL_STATE_ID)) ?? created;
	}

	async setFrozen(frozen: boolean, adminId: UserID): Promise<OperatorPassWallStateRow> {
		for (let attempt = 0; attempt < OPERATOR_PASS_WALL_UPDATE_ATTEMPTS; attempt++) {
			const state = await this.loadState();
			if (state.frozen === frozen) {
				return state;
			}
			const now = operatorPassNow();
			const frozenSnapshot = frozen
				? JSON.stringify(await this.computeSnapshot(Number(state.wall_version), 'frozen'))
				: null;
			const next = await this.repository.updateWallState(state, {
				frozen: Db.set(frozen),
				frozen_at: Db.set(frozen ? now : null),
				frozen_by: Db.set(frozen ? adminId : null),
				frozen_snapshot: Db.set(frozenSnapshot),
			});
			if (next) {
				publicWallCache = null;
				if (!frozen) {
					await this.republish();
					await this.releaseFrozenAvatars(state.frozen_snapshot ?? null);
				}
				return next;
			}
		}
		throw new Error("Operators' wall state changed too often to update");
	}

	async republish(): Promise<number> {
		const state = await this.loadState();
		if (state.frozen) {
			return Number(state.wall_version);
		}
		const bumped = await this.bumpVersion();
		if (bumped === null) {
			return Number(state.wall_version);
		}
		await getOperatorPassWallInfrastructure().publisher.publish({
			type: 'operator_pass_wall_delta',
			version: bumped.version,
			previous_version: bumped.previous,
			published_at: new Date().toISOString(),
			resync: true,
			upserts: [],
			removals: [],
		});
		return bumped.version;
	}

	async publishChanges(userIds: ReadonlyArray<UserID>, ordering: 'reorder' | 'preserve' = 'reorder'): Promise<void> {
		publicWallCache = null;
		const state = await this.loadState();
		if (state.frozen) {
			await this.dropFromFrozenSnapshot(userIds);
			return;
		}
		const bumped = await this.bumpVersion();
		if (bumped === null) {
			await this.dropFromFrozenSnapshot(userIds);
			return;
		}
		const upserts: Array<OperatorPassWallBlobEntry> = [];
		const removals: Array<string> = [];
		for (const userId of new Set(userIds)) {
			const entry = await this.repository.findWallEntry(userId);
			const item = entry ? await this.buildPublicEntry(entry) : null;
			if (item) upserts.push(item);
			else removals.push(operatorPassWallPublicId(userId));
		}
		await getOperatorPassWallInfrastructure().publisher.publish({
			type: 'operator_pass_wall_delta',
			version: bumped.version,
			previous_version: bumped.previous,
			published_at: new Date().toISOString(),
			resync: ordering === 'reorder' && upserts.length > 0,
			upserts,
			removals,
		});
	}

	private async computeSnapshot(version: number, mode: 'live' | 'frozen' = 'live'): Promise<OperatorPassWallSnapshot> {
		const now = operatorPassNow();
		const ranked: Array<RankedWallEntry> = [];
		for (const entry of await this.entries.scanAll()) {
			const item = await this.buildRankedEntry(entry, now);
			if (item) ranked.push(item);
		}
		const entries = ranked
			.sort(
				(left, right) =>
					Number(left.entry.business === true) - Number(right.entry.business === true) ||
					(left.entry.business ? 0 : right.entry.tier - left.entry.tier) ||
					left.since - right.since ||
					left.entry.id.localeCompare(right.entry.id),
			)
			.map((item) => item.entry);
		return {
			version,
			frozen: mode === 'frozen',
			generated_at: new Date().toISOString(),
			avatar_sizes: [...OPERATOR_PASS_WALL_AVATAR_SIZES],
			tier_scales: [...OPERATOR_PASS_WALL_TIER_SCALES],
			entries,
		};
	}

	private async bumpVersion(): Promise<{version: number; previous: number} | null> {
		for (let attempt = 0; attempt < OPERATOR_PASS_WALL_UPDATE_ATTEMPTS * 4; attempt++) {
			const state = await this.loadState();
			if (state.frozen) {
				return null;
			}
			const next = state.wall_version + 1n;
			const updated = await this.repository.updateWallState(state, {
				wall_version: Db.set(next),
				published_at: Db.set(new Date()),
			});
			if (updated) {
				return {version: Number(next), previous: Number(state.wall_version)};
			}
		}
		throw new Error("Operators' wall version changed too often to update");
	}

	async isAvatarServed(sha256: string): Promise<boolean> {
		for (const userId of await this.repository.listWallAvatarRefs(sha256)) {
			const entry = await this.repository.findWallEntry(userId);
			if (
				entry?.live_avatar === sha256 &&
				(await this.buildPublicEntry(entry, operatorPassNow(), {includePreview: true})) !== null
			) {
				return true;
			}
		}
		const state = await this.loadState();
		if (state.frozen && state.frozen_snapshot) {
			const frozen = JSON.parse(state.frozen_snapshot) as OperatorPassWallSnapshot;
			return frozen.entries.some((item) => item.avatar === sha256);
		}
		return false;
	}

	private async releaseFrozenAvatars(frozenSnapshot: string | null): Promise<void> {
		if (!frozenSnapshot) {
			return;
		}
		const frozen = JSON.parse(frozenSnapshot) as OperatorPassWallSnapshot;
		const avatars = new Set(frozen.entries.flatMap((item) => (item.avatar ? [item.avatar] : [])));
		for (const avatar of avatars) {
			await this.releaseAvatar(avatar);
		}
	}

	async releaseAvatar(sha256: string): Promise<void> {
		if (!(await this.isAvatarServed(sha256))) {
			await getOperatorPassWallInfrastructure().avatarStore.unpublish(sha256);
		}
	}

	async publishAvatarIfServed(entry: OperatorPassWallEntryRow): Promise<void> {
		if (entry.live_avatar && (await this.buildPublicEntry(entry, operatorPassNow(), {includePreview: true})) !== null) {
			await getOperatorPassWallInfrastructure().avatarStore.publish(entry.live_avatar);
		}
	}

	private async dropFromFrozenSnapshot(userIds: ReadonlyArray<UserID>): Promise<void> {
		const gone: Array<string> = [];
		for (const userId of new Set(userIds)) {
			const entry = await this.repository.findWallEntry(userId);
			if (!entry || (await this.buildPublicEntry(entry)) === null) {
				gone.push(operatorPassWallPublicId(userId));
			}
		}
		if (gone.length === 0) {
			return;
		}
		for (let attempt = 0; attempt < OPERATOR_PASS_WALL_UPDATE_ATTEMPTS; attempt++) {
			const state = await this.loadState();
			if (!state.frozen || !state.frozen_snapshot) {
				return;
			}
			const frozen = JSON.parse(state.frozen_snapshot) as OperatorPassWallSnapshot;
			const entries = frozen.entries.filter((item) => !gone.includes(item.id));
			if (entries.length === frozen.entries.length) {
				return;
			}
			const updated = await this.repository.updateWallState(state, {
				frozen_snapshot: Db.set(JSON.stringify({...frozen, entries})),
			});
			if (updated) {
				publicWallCache = null;
				return;
			}
		}
		throw new Error("Operators' wall state changed too often to update");
	}
}
