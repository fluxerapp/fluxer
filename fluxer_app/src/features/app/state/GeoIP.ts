// SPDX-License-Identifier: AGPL-3.0-or-later

import {runtimeInstanceKey} from '@app/features/app/state/InstanceSnapshotStore';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {AGE_BLOCKED_GEOS, AGE_RESTRICTED_GEOS} from '@fluxer/instance_bootstrap/src/AgeGeos';
import type {GeoEntry, GeolocationResponse, InstanceAgePolicy} from '@fluxer/instance_bootstrap/src/Types';
import {compareStructural, makeAutoObservable, reaction, runInAction} from 'mobx';

const logger = new Logger('GeoIP');
const RESOLVE_TIMEOUT_MS = 5000;

export type GeoIPResolution = 'pending' | 'resolved' | 'unavailable';

interface ConnectionGeo {
	countryCode: string | null;
	regionCode: string | null;
	latitude: string | null;
	longitude: string | null;
}

interface AgeTables {
	restricted: ReadonlyArray<GeoEntry>;
	blocked: ReadonlyArray<GeoEntry>;
}

interface ActiveRuntimeContext {
	readonly instanceKey: string;
	readonly agePolicy: InstanceAgePolicy | null;
	readonly transportApiEndpoint: string;
}

function ageTablesFor(policy: InstanceAgePolicy | null): AgeTables {
	if (policy == null) {
		return {restricted: AGE_RESTRICTED_GEOS, blocked: AGE_BLOCKED_GEOS};
	}
	const restricted: Array<GeoEntry> = [];
	const blocked: Array<GeoEntry> = [];
	for (const geo of policy.geos) {
		const entry: GeoEntry = {countryCode: geo.country_code, regionCode: geo.region_code};
		if (geo.action === 'block') {
			blocked.push(entry);
		} else {
			restricted.push(entry);
		}
	}
	return {restricted, blocked};
}

function activeRuntimeContext(): ActiveRuntimeContext | null {
	const snapshot = RuntimeConfig.getSnapshotOrNull();
	if (snapshot === null) {
		return null;
	}
	const instanceKey = runtimeInstanceKey(snapshot);
	if (instanceKey === null) {
		throw new Error(`Active runtime has an unusable API endpoint: ${snapshot.apiEndpoint}`);
	}
	return {
		instanceKey,
		agePolicy: snapshot.agePolicy ?? null,
		transportApiEndpoint: RuntimeConfig.transportApiEndpoint,
	};
}

function nonEmptyString(value: unknown): string | null {
	return typeof value === 'string' && value.length > 0 ? value : null;
}

function parseGeolocationResponse(payload: unknown): ConnectionGeo | null {
	if (typeof payload !== 'object' || payload === null) {
		return null;
	}
	const source = payload as Partial<GeolocationResponse>;
	return {
		countryCode: nonEmptyString(source.countryCode),
		regionCode: nonEmptyString(source.regionCode),
		latitude: nonEmptyString(source.latitude),
		longitude: nonEmptyString(source.longitude),
	};
}

class GeoIP {
	countryCode: string | null = null;
	regionCode: string | null = null;
	latitude: string | null = null;
	longitude: string | null = null;
	ageRestrictedGeos: ReadonlyArray<GeoEntry> = AGE_RESTRICTED_GEOS;
	ageBlockedGeos: ReadonlyArray<GeoEntry> = AGE_BLOCKED_GEOS;
	resolution: GeoIPResolution = 'pending';
	private generation = 0;
	private instanceKey: string | null = null;
	private transportApiEndpoint: string | null = null;
	private readyWaiters: Array<() => void> = [];

	constructor() {
		const runtime = activeRuntimeContext();
		this.instanceKey = runtime?.instanceKey ?? null;
		this.transportApiEndpoint = runtime?.transportApiEndpoint ?? null;
		this.resolution = runtime === null ? 'unavailable' : 'pending';
		this.applyAgeTables(ageTablesFor(runtime?.agePolicy ?? null));
		makeAutoObservable<GeoIP, 'readyWaiters'>(this, {readyWaiters: false}, {autoBind: true});
		reaction(
			() => activeRuntimeContext(),
			(nextRuntime) => {
				this.adoptRuntime(nextRuntime);
			},
			{equals: compareStructural},
		);
		if (runtime !== null) {
			void this.resolve();
		}
	}

	ready(): Promise<void> {
		if (this.resolution !== 'pending') {
			return Promise.resolve();
		}
		return new Promise<void>((settle) => {
			this.readyWaiters.push(settle);
		});
	}

	applyConnectionFallbackGeo(data: ConnectionGeo): void {
		runInAction(() => {
			if (this.countryCode === null) {
				this.countryCode = data.countryCode;
			}
			if (this.regionCode === null) {
				this.regionCode = data.regionCode;
			}
			if (data.latitude === null || data.longitude === null) return;
			if (this.latitude === null) {
				this.latitude = data.latitude;
			}
			if (this.longitude === null) {
				this.longitude = data.longitude;
			}
		});
	}

	isBlocked(): boolean {
		if (!this.countryCode) return false;
		return this.ageBlockedGeos.some((geo) => {
			if (geo.countryCode !== this.countryCode) return false;
			if (geo.regionCode === null) return true;
			return geo.regionCode === this.regionCode;
		});
	}

	private adoptRuntime(runtime: ActiveRuntimeContext | null): void {
		const instanceKey = runtime?.instanceKey ?? null;
		const transportApiEndpoint = runtime?.transportApiEndpoint ?? null;
		const switchedInstance = instanceKey !== this.instanceKey;
		const endpointChanged = transportApiEndpoint !== this.transportApiEndpoint;
		const retryUnresolved = this.resolution === 'unavailable' && transportApiEndpoint !== null;
		const restartResolution = transportApiEndpoint !== null && (switchedInstance || endpointChanged || retryUnresolved);
		runInAction(() => {
			this.instanceKey = instanceKey;
			this.transportApiEndpoint = transportApiEndpoint;
			this.applyAgeTables(ageTablesFor(runtime?.agePolicy ?? null));
			if (switchedInstance) {
				this.countryCode = null;
				this.regionCode = null;
				this.latitude = null;
				this.longitude = null;
			}
			if (restartResolution) {
				this.generation += 1;
				this.resolution = 'pending';
			} else if (transportApiEndpoint === null) {
				this.generation += 1;
				this.resolution = 'unavailable';
			}
		});
		if (transportApiEndpoint === null) {
			this.settle('unavailable', this.generation);
		} else if (restartResolution) {
			void this.resolve();
		}
	}

	private applyAgeTables(tables: AgeTables): void {
		this.ageRestrictedGeos = tables.restricted;
		this.ageBlockedGeos = tables.blocked;
	}

	private async resolve(): Promise<void> {
		const endpoint = this.transportApiEndpoint;
		if (endpoint === null) {
			this.settle('unavailable', this.generation);
			return;
		}
		const generation = this.generation;
		try {
			const response = await fetch(`${endpoint.replace(/\/$/u, '')}/ip`, {
				credentials: 'omit',
				cache: 'no-store',
				signal: AbortSignal.timeout(RESOLVE_TIMEOUT_MS),
			});
			if (!response.ok) {
				throw new Error(`Location lookup failed with status ${response.status}`);
			}
			const location = parseGeolocationResponse(await response.json());
			if (location === null) {
				throw new Error('Location lookup returned an unreadable body');
			}
			if (generation !== this.generation) return;
			runInAction(() => {
				if (location.countryCode !== null) {
					this.countryCode = location.countryCode;
					this.regionCode = location.regionCode;
				}
				if (location.latitude !== null && location.longitude !== null) {
					this.latitude = location.latitude;
					this.longitude = location.longitude;
				}
			});
			this.settle('resolved', generation);
		} catch (error) {
			logger.warn('Failed to resolve the client location', error);
			this.settle('unavailable', generation);
		}
	}

	private settle(resolution: GeoIPResolution, generation: number): void {
		if (generation !== this.generation) return;
		runInAction(() => {
			this.resolution = resolution;
		});
		const waiters = this.readyWaiters;
		this.readyWaiters = [];
		for (const waiter of waiters) {
			waiter();
		}
	}
}

export default new GeoIP();
