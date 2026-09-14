// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomInt, randomUUID} from 'node:crypto';
import {setTimeout as delay} from 'node:timers/promises';
import {Logger} from '@app/api/Logger';
import {streamToStringWithLimit} from '@app/api/utils/FetchUtils';
import {parseIpAddress} from '@fluxer/ip_utils/src/IpAddress';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {z} from 'zod';

const ONIONOO_URL =
	'https://onionoo.torproject.org/details?type=relay&running=true&flag=Exit&fields=exit_addresses,or_addresses';
const FETCH_TIMEOUT_MS = 30000;
const MAX_RESPONSE_BYTES = 8 * 1024 * 1024;
const REFRESH_INTERVAL_MS = 30 * 60 * 1000;
const REFRESH_JITTER_MS = 2 * 60 * 1000;
const OPPORTUNISTIC_REFRESH_MIN_MS = 60000;
const OPPORTUNISTIC_REFRESH_MAX_MS = 3 * 60000;
const KV_PAYLOAD_KEY = 'fluxer:tor_exit_list:payload';
const KV_LOCK_KEY = 'fluxer:tor_exit_list:lock';
const LOCK_TTL_SECONDS = 5 * 60;
const PAYLOAD_TTL_SECONDS = 60 * 60;
const HYDRATE_WAIT_MS = 60000;
const HYDRATE_POLL_MIN_MS = 500;
const HYDRATE_POLL_MAX_MS = 5000;

const OnionooDetailsSchema = z.object({
	relays: z.array(
		z.object({
			or_addresses: z.array(z.string()).optional(),
			exit_addresses: z.array(z.string()).optional(),
		}),
	),
});

interface TorExitSnapshot {
	readonly payload: string;
	readonly lastModified: string | null;
}

class TorExitListCache {
	private ipv4Exits: ReadonlySet<string> = new Set();
	private ipv6Exits: ReadonlySet<string> = new Set();
	private kvClient: IKVProvider | null = null;
	private refreshTimer: NodeJS.Timeout | null = null;
	private initPromise: Promise<void> | null = null;
	private refreshPromise: Promise<void> | null = null;
	private shutdownPromise: Promise<void> | null = null;
	private lifecycle = new AbortController();
	private fetchedSnapshot: TorExitSnapshot | null = null;

	setKvClient(kv: IKVProvider | null): void {
		this.kvClient = kv;
	}

	initialize(): Promise<void> {
		if (this.shutdownPromise) return Promise.reject(new Error('Tor exit list cache is shutting down'));
		if (this.lifecycle.signal.aborted) this.lifecycle = new AbortController();
		if (!this.initPromise) {
			this.initPromise = Promise.resolve(this.refreshPromise)
				.then(() => this.doInitialize())
				.catch((err) => {
					this.initPromise = null;
					throw err;
				});
		}
		return this.initPromise;
	}

	shutdown(): Promise<void> {
		if (this.shutdownPromise) return this.shutdownPromise;
		this.lifecycle.abort(new Error('Tor exit list cache is shut down'));
		if (this.refreshTimer) {
			clearTimeout(this.refreshTimer);
			this.refreshTimer = null;
		}
		this.shutdownPromise = Promise.allSettled([this.initPromise, this.refreshPromise]).then(() => {
			this.initPromise = null;
			this.shutdownPromise = null;
		});
		return this.shutdownPromise;
	}

	isTorExit(ip: string): boolean {
		const parsed = parseIpAddress(ip);
		if (!parsed) return false;
		return parsed.family === 'ipv4' ? this.ipv4Exits.has(parsed.normalized) : this.ipv6Exits.has(parsed.normalized);
	}

	async forceRefresh(): Promise<void> {
		await this.refresh();
	}

	seedForTesting(ips: Iterable<string>): void {
		this.applyPayload(Array.from(ips).join('\n'));
	}

	clearForTesting(): void {
		this.ipv4Exits = new Set();
		this.ipv6Exits = new Set();
		this.fetchedSnapshot = null;
	}

	private async doInitialize(): Promise<void> {
		this.lifecycle.signal.throwIfAborted();
		const cached = await this.readPayloadFromKv();
		if (cached !== null) {
			this.applyPayload(cached);
			Logger.info({ipv4Count: this.ipv4Exits.size, ipv6Count: this.ipv6Exits.size}, 'Tor exit list hydrated from KV');
			this.scheduleNextRefresh(this.opportunisticRefreshDelayMs());
			return;
		}
		await this.hydrateFromTorOrWait();
		this.scheduleNextRefresh(REFRESH_INTERVAL_MS);
	}

	private scheduleNextRefresh(baseDelayMs: number): void {
		const signal = this.lifecycle.signal;
		if (signal.aborted) return;
		if (this.refreshTimer) {
			clearTimeout(this.refreshTimer);
			this.refreshTimer = null;
		}
		const jitter = randomInt(REFRESH_JITTER_MS * 2) - REFRESH_JITTER_MS;
		const delay = Math.max(1000, baseDelayMs + jitter);
		this.refreshTimer = setTimeout(() => {
			this.refreshTimer = null;
			this.refresh()
				.catch((err) => {
					if (!signal.aborted) {
						Logger.warn({error: err instanceof Error ? err.message : String(err)}, 'Tor exit list refresh failed');
					}
				})
				.finally(() => {
					if (!signal.aborted) this.scheduleNextRefresh(REFRESH_INTERVAL_MS);
				});
		}, delay);
		this.refreshTimer.unref?.();
	}

	private opportunisticRefreshDelayMs(): number {
		const range = OPPORTUNISTIC_REFRESH_MAX_MS - OPPORTUNISTIC_REFRESH_MIN_MS;
		return OPPORTUNISTIC_REFRESH_MIN_MS + randomInt(range);
	}

	private async refresh(): Promise<void> {
		this.lifecycle.signal.throwIfAborted();
		this.refreshPromise ??= Promise.resolve(this.initPromise)
			.then(() => this.tryRefresh())
			.finally(() => {
				this.refreshPromise = null;
			});
		await this.refreshPromise;
	}

	private async tryRefresh(): Promise<void> {
		this.lifecycle.signal.throwIfAborted();
		const kv = this.kvClient;
		if (!kv) {
			const payload = await this.fetchFromTor();
			if (payload !== null) this.applyPayload(payload);
			return;
		}
		await this.withKvLock(kv, async (acquired) => {
			if (!acquired) {
				const cached = await this.readPayloadFromKv();
				if (cached !== null) this.applyPayload(cached);
				return;
			}
			const payload = await this.fetchFromTor();
			this.lifecycle.signal.throwIfAborted();
			if (payload === null) return;
			await this.writePayloadToKv(kv, payload);
			this.applyPayload(payload);
			Logger.info({ipv4Count: this.ipv4Exits.size, ipv6Count: this.ipv6Exits.size}, 'Tor exit list refreshed');
		});
	}

	private async hydrateFromTorOrWait(): Promise<void> {
		const kv = this.kvClient;
		if (!kv) {
			const payload = await this.fetchFromTor();
			if (payload !== null) {
				this.applyPayload(payload);
				return;
			}
			Logger.warn('Tor exit list bootstrap could not fetch; continuing with empty list');
			return;
		}
		const fetchedAsWinner = await this.withKvLock(kv, async (acquired) => {
			if (!acquired) return false;
			const payload = await this.fetchFromTor();
			this.lifecycle.signal.throwIfAborted();
			if (payload === null) {
				Logger.warn('Tor exit list bootstrap could not fetch; continuing with empty list');
				return true;
			}
			await this.writePayloadToKv(kv, payload);
			this.applyPayload(payload);
			Logger.info(
				{ipv4Count: this.ipv4Exits.size, ipv6Count: this.ipv6Exits.size},
				'Tor exit list fetched and hydrated on startup',
			);
			return true;
		});
		if (fetchedAsWinner) return;
		const cached = await this.pollForPeerPayload();
		if (cached !== null) {
			this.applyPayload(cached);
			Logger.info(
				{ipv4Count: this.ipv4Exits.size, ipv6Count: this.ipv6Exits.size},
				'Tor exit list hydrated from KV after peer fetch',
			);
			return;
		}
		Logger.warn('Tor exit list not available via KV after wait; fetching directly');
		const payload = await this.fetchFromTor();
		if (payload === null) {
			Logger.warn('Tor exit list bootstrap could not fetch; continuing with empty list');
			return;
		}
		this.applyPayload(payload);
	}

	private async pollForPeerPayload(): Promise<string | null> {
		const deadline = Date.now() + HYDRATE_WAIT_MS;
		let backoff = HYDRATE_POLL_MIN_MS;
		while (Date.now() < deadline) {
			await delay(backoff, undefined, {signal: this.lifecycle.signal});
			const cached = await this.readPayloadFromKv();
			if (cached !== null) return cached;
			backoff = Math.min(backoff * 2, HYDRATE_POLL_MAX_MS);
		}
		return null;
	}

	private async withKvLock<T>(kv: IKVProvider, fn: (acquired: boolean) => Promise<T>): Promise<T> {
		this.lifecycle.signal.throwIfAborted();
		const lockToken = randomUUID();
		const acquired = await kv.acquireLock(KV_LOCK_KEY, lockToken, LOCK_TTL_SECONDS).catch(() => false);
		try {
			this.lifecycle.signal.throwIfAborted();
			return await fn(acquired);
		} finally {
			if (acquired) {
				await kv.releaseLock(KV_LOCK_KEY, lockToken).catch((err) => {
					Logger.warn(
						{error: err instanceof Error ? err.message : String(err)},
						'Failed to release Tor exit list KV lock (will expire on TTL)',
					);
				});
			}
		}
	}

	private async readPayloadFromKv(): Promise<string | null> {
		this.lifecycle.signal.throwIfAborted();
		const kv = this.kvClient;
		if (!kv) return null;
		try {
			const payload = await kv.get(KV_PAYLOAD_KEY);
			this.lifecycle.signal.throwIfAborted();
			return payload;
		} catch (err) {
			this.lifecycle.signal.throwIfAborted();
			Logger.warn({error: err instanceof Error ? err.message : String(err)}, 'Failed reading Tor exit list from KV');
			return null;
		}
	}

	private async writePayloadToKv(kv: IKVProvider, payload: string): Promise<void> {
		this.lifecycle.signal.throwIfAborted();
		try {
			await kv.setex(KV_PAYLOAD_KEY, PAYLOAD_TTL_SECONDS, payload);
		} catch (err) {
			Logger.warn(
				{error: err instanceof Error ? err.message : String(err)},
				'Failed to write Tor exit list payload to KV',
			);
		}
	}

	private async fetchFromTor(): Promise<string | null> {
		const signal = this.lifecycle.signal;
		signal.throwIfAborted();
		const requestSignal = AbortSignal.any([AbortSignal.timeout(FETCH_TIMEOUT_MS), signal]);
		const snapshot = this.fetchedSnapshot;
		try {
			const headers: Record<string, string> = {
				Accept: 'application/json',
				'Accept-Encoding': 'gzip',
			};
			if (snapshot?.lastModified) {
				headers['If-Modified-Since'] = snapshot.lastModified;
			}
			const res = await fetch(ONIONOO_URL, {
				signal: requestSignal,
				headers,
			});
			if (res.status === 304) {
				if (!snapshot?.lastModified) throw new Error('Onionoo returned 304 without a cached response');
				return snapshot.payload;
			}
			if (res.status !== 200) {
				await res.body?.cancel();
				Logger.warn({status: res.status}, 'Onionoo fetch returned non-OK status');
				return null;
			}
			const text = await streamToStringWithLimit(res.body, {
				maxBytes: MAX_RESPONSE_BYTES,
				headers: res.headers,
				description: 'Onionoo response',
				signal: requestSignal,
			});
			const parsed = OnionooDetailsSchema.safeParse(JSON.parse(text));
			if (!parsed.success) throw new Error('Onionoo returned invalid relay data');
			signal.throwIfAborted();
			const ips = new Set<string>();
			let invalidCount = 0;
			for (const relay of parsed.data.relays) {
				for (const address of [...(relay.or_addresses ?? []), ...(relay.exit_addresses ?? [])]) {
					const ip = parseIpAddress(stripHostPort(address) ?? '');
					if (!ip) {
						invalidCount++;
						continue;
					}
					ips.add(ip.normalized);
				}
			}
			if (invalidCount > 0) {
				Logger.warn({invalidCount}, 'Onionoo returned relay addresses that could not be parsed');
			}
			if (ips.size === 0 && parsed.data.relays.length > 0) {
				Logger.error(
					{relays: parsed.data.relays.length},
					'Onionoo returned no usable relay addresses, keeping the current Tor exit list',
				);
				return null;
			}
			const payload = Array.from(ips).join('\n');
			this.fetchedSnapshot = {payload, lastModified: res.headers.get('last-modified')};
			return payload;
		} catch (err) {
			signal.throwIfAborted();
			if (requestSignal.aborted) {
				Logger.warn('Onionoo fetch timed out');
			} else {
				Logger.warn({error: err instanceof Error ? err.message : String(err)}, 'Onionoo fetch failed');
			}
			return null;
		}
	}

	private applyPayload(payload: string): void {
		const ipv4 = new Set<string>();
		const ipv6 = new Set<string>();
		let invalidCount = 0;
		for (const rawLine of payload.split('\n')) {
			const value = rawLine.trim();
			if (!value) continue;
			const parsed = parseIpAddress(value);
			if (!parsed) {
				invalidCount++;
				continue;
			}
			if (parsed.family === 'ipv4') {
				ipv4.add(parsed.normalized);
			} else {
				ipv6.add(parsed.normalized);
			}
		}
		if (invalidCount > 0) {
			Logger.warn({invalidCount}, 'Tor exit list payload contained lines that could not be parsed');
		}
		this.ipv4Exits = ipv4;
		this.ipv6Exits = ipv6;
	}
}

function stripHostPort(value: string): string | null {
	const trimmed = value.trim();
	if (!trimmed) return null;
	if (trimmed.startsWith('[')) {
		const end = trimmed.indexOf(']');
		if (end <= 1) return null;
		return trimmed.slice(1, end);
	}
	const firstColon = trimmed.indexOf(':');
	if (firstColon > 0 && firstColon === trimmed.lastIndexOf(':')) {
		return trimmed.slice(0, firstColon);
	}
	return trimmed || null;
}

export const torExitListCache = new TorExitListCache();
