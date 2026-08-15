// SPDX-License-Identifier: AGPL-3.0-or-later

import {makePersistent} from '@app/features/platform/utils/MobXPersistence';
import {CHANNEL_RATE_LIMIT_PER_USER_MAX} from '@fluxer/constants/src/LimitConstants';
import {makeAutoObservable} from 'mobx';

const MAX_TRACKED_CHANNELS = 512;
const PRUNE_INTERVAL_MS = 60 * 1000;
const RETENTION_MS = CHANNEL_RATE_LIMIT_PER_USER_MAX * 1000;
const SNOWFLAKE_PATTERN = /^[1-9][0-9]*$/;

interface SlowmodeEntry {
	readonly lastSendTimestamp: number | null;
	readonly explicitExpiresAt: number | null;
}

class InvalidSlowmodeDurationError extends RangeError {
	constructor(reason: string) {
		super(`Invalid slowmode duration: ${reason}`);
		this.name = 'InvalidSlowmodeDurationError';
	}
}

class InvalidSlowmodeRateLimitError extends RangeError {
	constructor(rateLimitPerUser: number) {
		super(`Invalid slowmode rate limit: ${rateLimitPerUser} seconds`);
		this.name = 'InvalidSlowmodeRateLimitError';
	}
}

function isRecord(value: unknown): value is Record<string, unknown> {
	return typeof value === 'object' && value !== null && !Array.isArray(value);
}

function isValidTimestamp(value: unknown, now: number): value is number {
	return typeof value === 'number' && Number.isSafeInteger(value) && value > 0 && value <= now + RETENTION_MS;
}

class Slowmode {
	lastSendTimestamps: Record<string, number> = {};
	cooldownExpiresAt: Record<string, number> = {};
	private nextPruneAt = 0;

	constructor() {
		makeAutoObservable<this, 'nextPruneAt'>(this, {nextPruneAt: false}, {autoBind: true});
		void this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'Slowmode', ['lastSendTimestamps', 'cooldownExpiresAt']);
		this.nextPruneAt = 0;
		this.pruneExpired(Date.now());
	}

	recordMessageSend(channelId: string): void {
		const now = Date.now();
		this.pruneExpired(now);
		const current = this.getEntry(channelId);
		this.setEntry(channelId, {
			explicitExpiresAt: current.explicitExpiresAt,
			lastSendTimestamp: now,
		});
	}

	updateSlowmodeTimestamp(channelId: string, timestamp: number): void {
		const now = Date.now();
		if (!isValidTimestamp(timestamp, now)) return;
		this.pruneExpired(now);
		const current = this.getEntry(channelId);
		if (current.lastSendTimestamp === timestamp) return;
		this.setEntry(channelId, {
			explicitExpiresAt: current.explicitExpiresAt,
			lastSendTimestamp: timestamp,
		});
	}

	updateSlowmodeRemaining(channelId: string, retryAfterMs: number): void {
		if (!Number.isSafeInteger(retryAfterMs) || retryAfterMs < 0 || retryAfterMs > RETENTION_MS) {
			throw new InvalidSlowmodeDurationError(`${retryAfterMs} milliseconds`);
		}
		const now = Date.now();
		this.pruneExpired(now);
		if (retryAfterMs === 0) {
			this.clearChannel(channelId);
			return;
		}
		const current = this.getEntry(channelId);
		this.setEntry(channelId, {
			explicitExpiresAt: now + retryAfterMs,
			lastSendTimestamp: current.lastSendTimestamp,
		});
	}

	clearChannel(channelId: string): void {
		const current = this.getEntry(channelId);
		if (current.lastSendTimestamp === null && current.explicitExpiresAt === null) return;
		this.writeEntry(channelId, {lastSendTimestamp: null, explicitExpiresAt: null});
	}

	deleteChannel(channelId: string): void {
		this.clearChannel(channelId);
	}

	getLastSendTimestamp(channelId: string): number | null {
		const entry = this.getEntry(channelId);
		return entry.lastSendTimestamp;
	}

	getSlowmodeRemaining(channelId: string, rateLimitPerUser: number): number {
		if (
			!Number.isSafeInteger(rateLimitPerUser) ||
			rateLimitPerUser < 0 ||
			rateLimitPerUser > CHANNEL_RATE_LIMIT_PER_USER_MAX
		) {
			throw new InvalidSlowmodeRateLimitError(rateLimitPerUser);
		}
		const entry = this.getEntry(channelId);
		const lastSentTime = entry.lastSendTimestamp;
		const now = Date.now();
		const explicitExpiresAt = entry.explicitExpiresAt;
		let explicitRemaining = 0;
		if (explicitExpiresAt !== null) {
			explicitRemaining = Math.max(0, explicitExpiresAt - now);
		}
		if (lastSentTime === null) return explicitRemaining;
		const timeSinceLastMessage = Math.max(0, now - lastSentTime);
		const localRemaining = Math.max(0, rateLimitPerUser * 1000 - timeSinceLastMessage);
		return Math.max(localRemaining, explicitRemaining);
	}

	private getEntry(channelId: string): SlowmodeEntry {
		const now = Date.now();
		let lastSendTimestamp: number | undefined;
		if (isRecord(this.lastSendTimestamps)) {
			const candidate = this.lastSendTimestamps[channelId];
			if (isValidTimestamp(candidate, now)) lastSendTimestamp = candidate;
		}
		let explicitExpiresAt: number | undefined;
		if (isRecord(this.cooldownExpiresAt)) {
			const candidate = this.cooldownExpiresAt[channelId];
			if (isValidTimestamp(candidate, now)) explicitExpiresAt = candidate;
		}
		return {
			lastSendTimestamp: lastSendTimestamp === undefined ? null : lastSendTimestamp,
			explicitExpiresAt: explicitExpiresAt === undefined ? null : explicitExpiresAt,
		};
	}

	private setEntry(channelId: string, entry: SlowmodeEntry): void {
		const channelIsTracked = this.getTrackedChannelIds().has(channelId);
		if (!channelIsTracked && this.countTrackedChannels() >= MAX_TRACKED_CHANNELS) {
			this.evictOldestEntry();
		}
		this.writeEntry(channelId, entry);
	}

	private writeEntry(channelId: string, entry: SlowmodeEntry): void {
		const now = Date.now();
		const nextLastSendTimestamps: Record<string, number> = {};
		const nextCooldownExpiresAt: Record<string, number> = {};
		if (isRecord(this.lastSendTimestamps)) {
			for (const key of Object.keys(this.lastSendTimestamps)) {
				const value = this.lastSendTimestamps[key];
				if (SNOWFLAKE_PATTERN.test(key) && isValidTimestamp(value, now)) nextLastSendTimestamps[key] = value;
			}
		}
		if (isRecord(this.cooldownExpiresAt)) {
			for (const key of Object.keys(this.cooldownExpiresAt)) {
				const value = this.cooldownExpiresAt[key];
				if (SNOWFLAKE_PATTERN.test(key) && isValidTimestamp(value, now)) nextCooldownExpiresAt[key] = value;
			}
		}
		if (entry.lastSendTimestamp === null) {
			delete nextLastSendTimestamps[channelId];
		} else {
			nextLastSendTimestamps[channelId] = entry.lastSendTimestamp;
		}
		if (entry.explicitExpiresAt === null) {
			delete nextCooldownExpiresAt[channelId];
		} else {
			nextCooldownExpiresAt[channelId] = entry.explicitExpiresAt;
		}
		this.lastSendTimestamps = nextLastSendTimestamps;
		this.cooldownExpiresAt = nextCooldownExpiresAt;
	}

	private countTrackedChannels(): number {
		return this.getTrackedChannelIds().size;
	}

	private evictOldestEntry(): void {
		const ids = this.getTrackedChannelIds();
		let oldestId: string | null = null;
		let oldestActivityAt = Number.MAX_SAFE_INTEGER;
		for (const id of ids) {
			const entry = this.getEntry(id);
			let activityAt = 0;
			if (entry.lastSendTimestamp !== null) activityAt = Math.max(activityAt, entry.lastSendTimestamp);
			if (entry.explicitExpiresAt !== null) activityAt = Math.max(activityAt, entry.explicitExpiresAt);
			if (activityAt >= oldestActivityAt) continue;
			oldestActivityAt = activityAt;
			oldestId = id;
		}
		if (oldestId === null) {
			throw new Error('Slowmode capacity was reached without an eviction candidate');
		}
		this.clearChannel(oldestId);
	}

	private pruneExpired(now: number): void {
		if (now < this.nextPruneAt) return;
		this.nextPruneAt = now + PRUNE_INTERVAL_MS;
		const nextLastSendTimestamps: Record<string, number> = {};
		const nextCooldownExpiresAt: Record<string, number> = {};
		const channelIds = this.getTrackedChannelIds();
		for (const channelId of channelIds) {
			const current = this.getEntry(channelId);
			let lastSendTimestamp = current.lastSendTimestamp;
			if (
				lastSendTimestamp !== null &&
				(!isValidTimestamp(lastSendTimestamp, now) || now - lastSendTimestamp > RETENTION_MS)
			) {
				lastSendTimestamp = null;
			}
			let explicitExpiresAt = current.explicitExpiresAt;
			if (explicitExpiresAt !== null && (!isValidTimestamp(explicitExpiresAt, now) || explicitExpiresAt <= now)) {
				explicitExpiresAt = null;
			}
			if (lastSendTimestamp !== null) nextLastSendTimestamps[channelId] = lastSendTimestamp;
			if (explicitExpiresAt !== null) nextCooldownExpiresAt[channelId] = explicitExpiresAt;
		}
		if (!this.timestampMapsEqual(this.lastSendTimestamps, nextLastSendTimestamps)) {
			this.lastSendTimestamps = nextLastSendTimestamps;
		}
		if (!this.timestampMapsEqual(this.cooldownExpiresAt, nextCooldownExpiresAt)) {
			this.cooldownExpiresAt = nextCooldownExpiresAt;
		}
		this.trimToCapacity();
	}

	private getTrackedChannelIds(): Set<string> {
		const channelIds = new Set<string>();
		if (isRecord(this.lastSendTimestamps)) {
			for (const channelId of Object.keys(this.lastSendTimestamps)) {
				if (SNOWFLAKE_PATTERN.test(channelId)) channelIds.add(channelId);
			}
		}
		if (isRecord(this.cooldownExpiresAt)) {
			for (const channelId of Object.keys(this.cooldownExpiresAt)) {
				if (SNOWFLAKE_PATTERN.test(channelId)) channelIds.add(channelId);
			}
		}
		return channelIds;
	}

	private timestampMapsEqual(first: unknown, second: Record<string, number>): boolean {
		if (!isRecord(first)) return false;
		const firstKeys = Object.keys(first);
		const secondKeys = Object.keys(second);
		if (firstKeys.length !== secondKeys.length) return false;
		for (const channelId of firstKeys) {
			if (first[channelId] !== second[channelId]) return false;
		}
		return true;
	}

	private trimToCapacity(): void {
		while (this.countTrackedChannels() > MAX_TRACKED_CHANNELS) {
			this.evictOldestEntry();
		}
	}
}

export default new Slowmode();
