// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {http} from '@app/features/platform/transport/RestTransport';
import {CUSTOM_BADGE_LOOKUP_IDS_MAX} from '@fluxer/constants/src/BadgeConstants';
import type {InstanceGuildBadge} from '@fluxer/instance_bootstrap/src/Types';
import {makeAutoObservable, observable, reaction, runInAction} from 'mobx';

const LOOKUP_DELAY_MS = 50;
const FETCHED_MAX = 2000;
const MISSING_MAX = 5000;
const REVALIDATE_COOLDOWN_MS = 30_000;
const RETRY_AFTER_MS = 30_000;
const LEGACY_REVALIDATE_COOLDOWN_MS = 5 * 60_000;

interface VersionedBadge {
	readonly badge: InstanceGuildBadge;
	readonly version: string;
}

interface BadgeLookupResponse {
	readonly version?: unknown;
	readonly badges?: unknown;
}

export interface ResolvedGuildBadges {
	readonly badges: ReadonlyArray<InstanceGuildBadge>;
	readonly settled: boolean;
}

function versionSequence(version: string): bigint | null {
	const match = /^([0-9]{1,20})/.exec(version);
	return match ? BigInt(match[1]!) : null;
}

function versionSuffix(version: string): string {
	const dot = version.indexOf('.');
	return dot === -1 ? '' : version.slice(dot + 1);
}

function isNewerBadgeVersion(candidate: string, current: string | null): boolean {
	if (current === null) return true;
	const next = versionSequence(candidate);
	const now = versionSequence(current);
	if (next === null || now === null || versionSuffix(candidate) !== versionSuffix(current)) return false;
	return next > now;
}

function readBadge(value: unknown): InstanceGuildBadge | null {
	if (typeof value !== 'object' || value === null) return null;
	const source = value as Record<string, unknown>;
	const {id, name, description, asset_url: assetUrl, href, position} = source;
	if (typeof id !== 'string' || typeof name !== 'string' || typeof assetUrl !== 'string') return null;
	return {
		id,
		name,
		description: typeof description === 'string' && description.length > 0 ? description : null,
		asset_url: assetUrl,
		href: typeof href === 'string' && /^https?:\/\//.test(href) ? href : null,
		...(typeof position === 'string' && /^-?[0-9]{1,19}$/.test(position) ? {position} : {}),
	};
}

function compareIds(left: string, right: string): number {
	const a = BigInt(left);
	const b = BigInt(right);
	if (a === b) return 0;
	return a < b ? -1 : 1;
}

function setCapped<V>(map: Map<string, V>, key: string, value: V, max: number, keep: (key: string) => boolean): void {
	map.delete(key);
	map.set(key, value);
	if (map.size <= max) return;
	for (const candidate of map.keys()) {
		if (map.size <= max) break;
		if (!keep(candidate)) map.delete(candidate);
	}
}

class GuildBadgeDefinitions {
	private readonly fetched = observable.map<string, VersionedBadge>(undefined, {deep: false});
	private readonly missing = observable.map<string, string>(undefined, {deep: false});
	private responseVersion: string | null = null;
	private readonly pending = new Set<string>();
	private readonly inFlight = new Set<string>();
	private readonly retryAt = new Map<string, number>();
	private readonly retained = new Map<string, number>();
	private readonly legacyRevalidatedAt = new Map<string, number>();
	private timer: ReturnType<typeof setTimeout> | null = null;
	private retryTimer: ReturnType<typeof setTimeout> | null = null;
	private lastRevalidateAt = 0;

	constructor() {
		makeAutoObservable<
			GuildBadgeDefinitions,
			| 'pending'
			| 'inFlight'
			| 'retryAt'
			| 'retained'
			| 'legacyRevalidatedAt'
			| 'timer'
			| 'retryTimer'
			| 'lastRevalidateAt'
		>(
			this,
			{
				pending: false,
				inFlight: false,
				retryAt: false,
				retained: false,
				legacyRevalidatedAt: false,
				timer: false,
				retryTimer: false,
				lastRevalidateAt: false,
			},
			{autoBind: true},
		);
		reaction(
			() => this.activeVersion,
			() => this.requestRetained(),
		);
		if (typeof window !== 'undefined') {
			window.addEventListener('online', () => this.retryNow());
		}
	}

	get activeVersion(): string | null {
		const discovery = RuntimeConfig.customBadgesVersion;
		if (discovery === null) return null;
		const response = this.responseVersion;
		return response !== null && isNewerBadgeVersion(response, discovery) ? response : discovery;
	}

	private get seeded(): ReadonlyMap<string, {badge: InstanceGuildBadge; index: number}> {
		return new Map(RuntimeConfig.guildBadges.map((badge, index) => [badge.id, {badge, index}]));
	}

	private current(id: string): InstanceGuildBadge | null | undefined {
		const version = this.activeVersion;
		const fetched = this.fetched.get(id);
		if (fetched && fetched.version === version) return fetched.badge;
		const seeded = this.seeded.get(id);
		if (seeded) return seeded.badge;
		if (version === null) return undefined;
		return this.missing.get(id) === version ? null : undefined;
	}

	private lookup(id: string): InstanceGuildBadge | null | undefined {
		const badge = this.current(id);
		return badge === undefined ? this.fetched.get(id)?.badge : badge;
	}

	resolve(ids: ReadonlyArray<string>): ResolvedGuildBadges {
		const found: Array<InstanceGuildBadge> = [];
		let settled = true;
		const seen = new Set<string>();
		for (const id of ids) {
			if (seen.has(id)) continue;
			seen.add(id);
			const badge = this.lookup(id);
			if (badge) found.push(badge);
			else if (badge === undefined) settled = false;
		}
		const seeded = this.seeded;
		found.sort((left, right) => {
			if (left.position !== undefined && right.position !== undefined) {
				const a = BigInt(left.position);
				const b = BigInt(right.position);
				if (a !== b) return a < b ? -1 : 1;
				return compareIds(left.id, right.id);
			}
			const leftIndex = seeded.get(left.id)?.index ?? Number.MAX_SAFE_INTEGER;
			const rightIndex = seeded.get(right.id)?.index ?? Number.MAX_SAFE_INTEGER;
			return leftIndex !== rightIndex ? leftIndex - rightIndex : compareIds(left.id, right.id);
		});
		return {badges: found, settled: settled || this.activeVersion === null};
	}

	retain(ids: ReadonlyArray<string>): () => void {
		const unique = [...new Set(ids)];
		for (const id of unique) this.retained.set(id, (this.retained.get(id) ?? 0) + 1);
		this.request(unique);
		return () => {
			for (const id of unique) {
				const count = (this.retained.get(id) ?? 1) - 1;
				if (count <= 0) this.retained.delete(id);
				else this.retained.set(id, count);
			}
		};
	}

	request(ids: ReadonlyArray<string>): void {
		if (this.activeVersion === null) {
			this.revalidateLegacy(ids);
			return;
		}
		const now = Date.now();
		for (const id of ids) {
			if (!/^[0-9]{1,20}$/.test(id) || this.current(id) !== undefined) continue;
			if (this.pending.has(id) || this.inFlight.has(id) || (this.retryAt.get(id) ?? 0) > now) continue;
			this.pending.add(id);
		}
		if (this.pending.size > 0 && this.timer === null) {
			this.timer = setTimeout(() => this.flush(), LOOKUP_DELAY_MS);
		}
		this.scheduleRetry();
	}

	private requestRetained(): void {
		if (this.retained.size > 0) this.request([...this.retained.keys()]);
	}

	private retryNow(): void {
		this.retryAt.clear();
		this.requestRetained();
	}

	private revalidateLegacy(ids: ReadonlyArray<string>): void {
		const now = Date.now();
		const unknown = ids.filter(
			(id) =>
				this.current(id) === undefined &&
				now - (this.legacyRevalidatedAt.get(id) ?? 0) >= LEGACY_REVALIDATE_COOLDOWN_MS,
		);
		if (unknown.length === 0) return;
		for (const id of unknown) this.legacyRevalidatedAt.set(id, now);
		void RuntimeConfig.revalidateDiscovery().catch(() => undefined);
	}

	private flush(): void {
		this.timer = null;
		const version = this.activeVersion;
		const ids = [...this.pending].sort(compareIds);
		this.pending.clear();
		if (version === null || ids.length === 0) return;
		for (let index = 0; index < ids.length; index += CUSTOM_BADGE_LOOKUP_IDS_MAX) {
			const chunk = ids.slice(index, index + CUSTOM_BADGE_LOOKUP_IDS_MAX);
			for (const id of chunk) this.inFlight.add(id);
			void http
				.get<BadgeLookupResponse>(Endpoints.INSTANCE_BADGES, {
					query: {ids: chunk.join(','), version},
					mode: 'silent',
				})
				.then(
					(response) => this.applyResponse(chunk, response.ok ? response.body : null),
					() => this.applyResponse(chunk, null),
				);
		}
	}

	private applyResponse(ids: ReadonlyArray<string>, body: BadgeLookupResponse | null): void {
		runInAction(() => {
			for (const id of ids) this.inFlight.delete(id);
			const responseVersion = typeof body?.version === 'string' ? body.version : null;
			const active = this.activeVersion;
			if (body === null || responseVersion === null || active === null || !Array.isArray(body.badges)) {
				this.deferRetry(ids);
				return;
			}
			if (isNewerBadgeVersion(responseVersion, active)) {
				this.responseVersion = responseVersion;
				this.revalidateDiscovery();
			}
			const current = responseVersion === this.activeVersion;
			const returned = new Set<string>();
			for (const entry of body.badges) {
				const badge = readBadge(entry);
				if (!badge) continue;
				returned.add(badge.id);
				const previous = this.fetched.get(badge.id);
				if (!current && previous?.version === this.activeVersion) continue;
				setCapped(this.fetched, badge.id, {badge, version: responseVersion}, FETCHED_MAX, (key) =>
					this.retained.has(key),
				);
			}
			if (current) {
				for (const id of ids) {
					if (!returned.has(id)) setCapped(this.missing, id, responseVersion, MISSING_MAX, () => false);
				}
			} else {
				this.deferRetry(ids);
			}
		});
	}

	private deferRetry(ids: ReadonlyArray<string>): void {
		const at = Date.now() + RETRY_AFTER_MS;
		for (const id of ids) this.retryAt.set(id, at);
		while (this.retryAt.size > MISSING_MAX) {
			const oldest = this.retryAt.keys().next();
			if (oldest.done) break;
			this.retryAt.delete(oldest.value);
		}
		this.scheduleRetry();
	}

	private scheduleRetry(): void {
		if (this.retryTimer !== null) return;
		const now = Date.now();
		let next: number | null = null;
		for (const id of this.retained.keys()) {
			const at = this.retryAt.get(id);
			if (at !== undefined && at > now && (next === null || at < next)) next = at;
		}
		if (next === null) return;
		this.retryTimer = setTimeout(
			() => {
				this.retryTimer = null;
				this.requestRetained();
				this.scheduleRetry();
			},
			next - now + LOOKUP_DELAY_MS,
		);
	}

	private revalidateDiscovery(): void {
		const now = Date.now();
		if (now - this.lastRevalidateAt < REVALIDATE_COOLDOWN_MS) return;
		this.lastRevalidateAt = now;
		void RuntimeConfig.revalidateDiscovery().catch(() => undefined);
	}
}

export default new GuildBadgeDefinitions();
