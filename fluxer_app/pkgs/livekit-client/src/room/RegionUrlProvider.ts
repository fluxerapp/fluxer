// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {Mutex} from '@livekit/mutex';
import type {RegionInfo, RegionSettings} from '@livekit/protocol';
import {getLogger, LoggerNames} from '../logger.ts';
import {ConnectionError, ConnectionErrorReason} from './errors.ts';
import CriticalTimers, {type TimerHandle} from './timers.ts';
import {extractMaxAgeFromRequestHeaders, isCloud} from './utils.ts';

const log = getLogger(LoggerNames.Region);

export const DEFAULT_MAX_AGE_MS = 5_000;
const STOP_REFETCH_DELAY_MS = 30_000;

type CachedRegionSettings = {
	regionSettings: RegionSettings;
	updatedAtInMs: number;
	maxAgeInMs: number;
};

type ConnectionTracker = {
	connectionCount: number;
	cleanupTimeout?: TimerHandle;
};

export class RegionUrlProvider {
	private static readonly cache: Map<string, CachedRegionSettings> = new Map();

	private static settingsTimeouts: Map<string, TimerHandle> = new Map();

	private static connectionTrackers: Map<string, ConnectionTracker> = new Map();

	private static fetchLock = new Mutex();

	private static async fetchRegionSettings(
		serverUrl: URL,
		token: string,
		signal?: AbortSignal,
	): Promise<CachedRegionSettings> {
		const unlock = await RegionUrlProvider.fetchLock.lock();
		try {
			const regionSettingsResponse = await fetch(`${getCloudConfigUrl(serverUrl)}/regions`, {
				headers: {authorization: `Bearer ${token}`},
				signal,
			});
			if (regionSettingsResponse.ok) {
				const maxAge = extractMaxAgeFromRequestHeaders(regionSettingsResponse.headers);
				const maxAgeInMs = maxAge ? maxAge * 1000 : DEFAULT_MAX_AGE_MS;

				const regionSettings = (await regionSettingsResponse.json()) as RegionSettings;
				return {regionSettings, updatedAtInMs: Date.now(), maxAgeInMs};
			} else {
				if (regionSettingsResponse.status === 401) {
					throw ConnectionError.notAllowed(
						`Could not fetch region settings: ${regionSettingsResponse.statusText}`,
						regionSettingsResponse.status,
					);
				} else {
					throw ConnectionError.internal(`Could not fetch region settings: ${regionSettingsResponse.statusText}`);
				}
			}
		} catch (e: unknown) {
			if (e instanceof ConnectionError) {
				throw e;
			} else if (signal?.aborted) {
				throw ConnectionError.cancelled(`Region fetching was aborted`);
			} else {
				throw ConnectionError.serverUnreachable(
					`Could not fetch region settings, ${e instanceof Error ? `${e.name}: ${e.message}` : e}`,
				);
			}
		} finally {
			unlock();
		}
	}

	private static async scheduleRefetch(url: URL, token: string, maxAgeInMs: number) {
		const timeout = RegionUrlProvider.settingsTimeouts.get(url.hostname);
		CriticalTimers.clearTimeout(timeout);
		RegionUrlProvider.settingsTimeouts.set(
			url.hostname,
			CriticalTimers.setTimeout(async () => {
				try {
					const newSettings = await RegionUrlProvider.fetchRegionSettings(url, token);
					RegionUrlProvider.updateCachedRegionSettings(url, token, newSettings);
				} catch (error: unknown) {
					if (error instanceof ConnectionError && error.reason === ConnectionErrorReason.NotAllowed) {
						log.debug('token is not valid, cancelling auto region refresh');
						return;
					}
					log.debug('auto refetching of region settings failed', {error});
					RegionUrlProvider.scheduleRefetch(url, token, maxAgeInMs);
				}
			}, maxAgeInMs),
		);
	}

	private static updateCachedRegionSettings(url: URL, token: string, settings: CachedRegionSettings) {
		RegionUrlProvider.cache.set(url.hostname, settings);
		RegionUrlProvider.scheduleRefetch(url, token, settings.maxAgeInMs);
	}

	private static stopRefetch(hostname: string) {
		const timeout = RegionUrlProvider.settingsTimeouts.get(hostname);
		if (timeout) {
			CriticalTimers.clearTimeout(timeout);
			RegionUrlProvider.settingsTimeouts.delete(hostname);
		}
	}

	private static scheduleCleanup(hostname: string) {
		const tracker = RegionUrlProvider.connectionTrackers.get(hostname);
		if (!tracker) {
			return;
		}

		if (tracker.cleanupTimeout) {
			CriticalTimers.clearTimeout(tracker.cleanupTimeout);
		}

		tracker.cleanupTimeout = CriticalTimers.setTimeout(() => {
			const currentTracker = RegionUrlProvider.connectionTrackers.get(hostname);
			if (currentTracker && currentTracker.connectionCount === 0) {
				log.debug('stopping region refetch after disconnect delay', {hostname});
				RegionUrlProvider.stopRefetch(hostname);
			}
			if (currentTracker) {
				currentTracker.cleanupTimeout = undefined;
			}
		}, STOP_REFETCH_DELAY_MS);
	}

	private static cancelCleanup(hostname: string) {
		const tracker = RegionUrlProvider.connectionTrackers.get(hostname);
		if (tracker?.cleanupTimeout) {
			CriticalTimers.clearTimeout(tracker.cleanupTimeout);
			tracker.cleanupTimeout = undefined;
		}
	}

	notifyConnected() {
		const hostname = this.serverUrl.hostname;
		let tracker = RegionUrlProvider.connectionTrackers.get(hostname);
		if (!tracker) {
			tracker = {connectionCount: 0};
			RegionUrlProvider.connectionTrackers.set(hostname, tracker);
		}

		tracker.connectionCount++;

		RegionUrlProvider.cancelCleanup(hostname);
	}

	notifyDisconnected() {
		const hostname = this.serverUrl.hostname;
		const tracker = RegionUrlProvider.connectionTrackers.get(hostname);
		if (!tracker) {
			return;
		}

		tracker.connectionCount = Math.max(0, tracker.connectionCount - 1);

		if (tracker.connectionCount === 0) {
			RegionUrlProvider.scheduleCleanup(hostname);
		}
	}

	private serverUrl: URL;

	private token: string;

	private attemptedRegions: Array<RegionInfo> = [];

	constructor(url: string, token: string) {
		this.serverUrl = new URL(url);
		this.token = token;
	}

	updateToken(token: string) {
		this.token = token;
		const url = this.getServerUrl();
		const settings = RegionUrlProvider.cache.get(url.hostname);
		RegionUrlProvider.scheduleRefetch(this.serverUrl, this.token, settings?.maxAgeInMs ?? DEFAULT_MAX_AGE_MS);
	}

	isCloud() {
		return isCloud(this.serverUrl);
	}

	getServerUrl() {
		return this.serverUrl;
	}

	async fetchRegionSettings(abortSignal?: AbortSignal): Promise<CachedRegionSettings> {
		return RegionUrlProvider.fetchRegionSettings(this.serverUrl, this.token, abortSignal);
	}

	async getNextBestRegionUrl(abortSignal?: AbortSignal) {
		if (!this.isCloud()) {
			throw Error('region availability is only supported for LiveKit Cloud domains');
		}

		let cachedSettings = RegionUrlProvider.cache.get(this.serverUrl.hostname);

		if (!cachedSettings || Date.now() - cachedSettings.updatedAtInMs > cachedSettings.maxAgeInMs) {
			cachedSettings = await this.fetchRegionSettings(abortSignal);
			RegionUrlProvider.updateCachedRegionSettings(this.serverUrl, this.token, cachedSettings);
		}

		const regionsLeft = cachedSettings.regionSettings.regions.filter(
			(region) => !this.attemptedRegions.find((attempted) => attempted.url === region.url),
		);
		if (regionsLeft.length > 0) {
			const nextRegion = regionsLeft[0];
			this.attemptedRegions.push(nextRegion);
			log.info(`switching to region: ${nextRegion.region}`, {region: nextRegion.region});
			return nextRegion.url;
		} else {
			return null;
		}
	}

	resetAttempts() {
		this.attemptedRegions = [];
	}

	setServerReportedRegions(settings: CachedRegionSettings) {
		RegionUrlProvider.updateCachedRegionSettings(this.serverUrl, this.token, settings);
	}
}

function getCloudConfigUrl(serverUrl: URL) {
	return `${serverUrl.protocol.replace('ws', 'http')}//${serverUrl.host}/settings`;
}
