// SPDX-License-Identifier: AGPL-3.0-or-later

import Config from '@app/features/app/config/Config';
import {
	type RuntimeConfigIdentity,
	type RuntimeConfigSnapshot,
	requireRuntimeConfigSnapshot,
} from '@app/features/app/state/RuntimeConfigSnapshot';
import {isDesktopLocalAppDocument} from '@app/features/platform/DesktopLocalAppRuntime';
import {takePrebootDiscoveryResponse} from '@app/features/platform/state/PrebootNetworkHandoff';
import {randomUuid} from '@app/features/platform/utils/RandomUuid';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import {Headers as HttpHeader} from '@fluxer/constants/src/Headers';
import {MS_PER_HOUR} from '@fluxer/date_utils/src/DateConstants';
import type {DesktopRuntimeConfigAPI, DesktopRuntimePlan} from '@fluxer/desktop_ipc/src/LocalAppRuntimeContract';
import {projectClientRuntimeDiscovery} from '@fluxer/instance_bootstrap/src/ClientRuntimeDiscovery';
import {
	fetchInstanceDiscovery,
	type InstanceDiscoveryDocument,
	type InstanceDiscoveryFetch,
	parseInstanceDiscoveryDocument,
} from '@fluxer/instance_bootstrap/src/Discovery';
import {InstanceEndpointKind, normalizeInstanceEndpoint} from '@fluxer/instance_bootstrap/src/EndpointNormalization';
import {
	isOfficialInstanceHost,
	OFFICIAL_CLIENT_API_ENDPOINTS,
	type OfficialReleaseChannel,
	officialClientApiEndpointForAlias,
} from '@fluxer/instance_bootstrap/src/OfficialInstance';
import {expandWireFormat} from '@fluxer/limits/src/LimitDiffer';
import type {LimitConfigSnapshot, LimitConfigWireFormat} from '@fluxer/limits/src/LimitTypes';
import {makeAutoObservable, observableShallow, runInAction} from 'mobx';

const SNAPSHOT_CACHE_TTL_MS = MS_PER_HOUR;
const SNAPSHOT_CACHE_MAX_ENTRIES = 256;
const SNAPSHOT_RESOLUTION_MAX_PENDING = 8;

export type {RuntimeConfigIdentity, RuntimeConfigSnapshot} from '@app/features/app/state/RuntimeConfigSnapshot';

export interface InstanceSnapshotResolution {
	snapshot: RuntimeConfigSnapshot;
	instanceKey: string;
	productName: string;
}

export interface InstanceSnapshotResolveRequest {
	input: string;
	signal: AbortSignal | null;
}

export function runtimeInstanceKey(snapshot: RuntimeConfigIdentity): string | null {
	try {
		const endpoint = snapshot.apiEndpoint.trim();
		const url = new URL(endpoint);
		if (
			(url.protocol !== 'https:' && url.protocol !== 'http:') ||
			url.username ||
			url.password ||
			url.search ||
			url.hash
		) {
			return null;
		}
		const path = url.pathname.replace(/\/+$/u, '');
		return `${url.origin.toLowerCase()}${path}`;
	} catch {
		return null;
	}
}

export function runtimeConfigSnapshotsAreSameInstance(
	left: RuntimeConfigSnapshot | null | undefined,
	right: RuntimeConfigSnapshot | null | undefined,
): boolean {
	if (left == null || right == null) {
		return false;
	}
	const leftKey = runtimeInstanceKey(left);
	const rightKey = runtimeInstanceKey(right);
	return leftKey !== null && leftKey === rightKey;
}

function cloneLimitConfig(limits: LimitConfigSnapshot): LimitConfigSnapshot {
	return JSON.parse(JSON.stringify(limits));
}

function readInstanceLimits(limits: LimitConfigSnapshot | LimitConfigWireFormat): LimitConfigSnapshot {
	const expanded =
		'defaultsHash' in limits && limits.version === 2 ? expandWireFormat(limits) : (limits as LimitConfigSnapshot);
	return cloneLimitConfig(expanded);
}

export function runtimeSnapshotFromDiscovery(document: InstanceDiscoveryDocument): RuntimeConfigSnapshot {
	const discovery = projectClientRuntimeDiscovery(document);
	const endpoints = discovery.endpoints;
	return requireRuntimeConfigSnapshot({
		apiEndpoint: endpoints.apiEndpoint,
		apiPublicEndpoint: endpoints.apiPublicEndpoint,
		gatewayEndpoint: endpoints.gatewayEndpoint,
		mediaEndpoint: endpoints.mediaEndpoint,
		staticCdnEndpoint: endpoints.staticCdnEndpoint,
		marketingEndpoint: endpoints.marketingEndpoint,
		adminEndpoint: endpoints.adminEndpoint,
		inviteEndpoint: endpoints.inviteEndpoint,
		giftEndpoint: endpoints.giftEndpoint,
		webAppEndpoint: endpoints.webAppEndpoint,
		uploadRelayEndpoint: endpoints.uploadRelayEndpoint,
		gifProvider: discovery.gif.provider,
		gifProviderDisplayName: discovery.gif.display_name,
		gifAttributionRequired: discovery.gif.attribution_required,
		apiCodeVersion: discovery.apiCodeVersion,
		features: discovery.features,
		sso: discovery.sso,
		registration: discovery.registration,
		community: discovery.community,
		services: discovery.services,
		publicPushVapidKey: discovery.push.public_vapid_key,
		limits: readInstanceLimits(discovery.limits),
		appPublic: discovery.appPublic,
		agePolicy: discovery.agePolicy,
		domainMigration: discovery.domainMigration,
	});
}

function snapshotProductName(snapshot: RuntimeConfigSnapshot): string {
	return snapshot.appPublic.branding.product_name;
}

function endpointHostname(endpoint: string): string | null {
	try {
		return new URL(endpoint).hostname.toLowerCase();
	} catch {
		return null;
	}
}

function bootstrapReleaseChannel(): OfficialReleaseChannel {
	return Config.PUBLIC_RELEASE_CHANNEL === 'stable' ? 'stable' : 'canary';
}

const SAME_ORIGIN_API_META_SELECTOR = 'meta[name="fluxer-api-origin"][content="self"]';
const SAME_ORIGIN_API_PATH = '/api';

function browserDocumentOrigin(): string | null {
	if (typeof window === 'undefined' || isDesktopLocalAppDocument()) {
		return null;
	}
	return window.location.origin;
}

function documentServesItsOwnApi(): boolean {
	return typeof document !== 'undefined' && document.querySelector(SAME_ORIGIN_API_META_SELECTOR) !== null;
}

function sameOriginApiEndpoint(origin: string): string {
	return `${origin}${SAME_ORIGIN_API_PATH}`;
}

function adoptsDocumentOrigin(apiEndpoint: string): boolean {
	const origin = browserDocumentOrigin();
	return origin !== null && documentServesItsOwnApi() && apiEndpoint === sameOriginApiEndpoint(origin);
}

export function resolveDiscoveryApiEndpoint(input: string): string {
	const normalized = normalizeInstanceEndpoint(input, InstanceEndpointKind.API);
	if (normalized == null || normalized.startsWith('/')) {
		throw new Error(`"${input}" is not a valid instance address`);
	}
	const url = new URL(normalized);
	const documentOrigin = browserDocumentOrigin();
	const officialClientApiEndpoint = officialClientApiEndpointForAlias(normalized);
	const targetsOfficialOrigin = url.protocol === 'https:' && url.pathname === '/' && isOfficialInstanceHost(url.host);
	if (documentOrigin !== null) {
		const targetsDocumentOrigin = url.origin === documentOrigin && url.pathname === '/';
		if (
			((targetsOfficialOrigin || officialClientApiEndpoint !== null) && isOfficialInstanceHost(documentOrigin)) ||
			(targetsDocumentOrigin && documentServesItsOwnApi())
		) {
			return sameOriginApiEndpoint(documentOrigin);
		}
	}
	if (officialClientApiEndpoint !== null) {
		return officialClientApiEndpoint;
	}
	return targetsOfficialOrigin ? OFFICIAL_CLIENT_API_ENDPOINTS[bootstrapReleaseChannel()] : normalized;
}

export function storedInstanceKey(apiEndpoint: string): string | null {
	const instanceKey = runtimeInstanceKey({apiEndpoint});
	if (instanceKey === null || officialClientApiEndpointForAlias(instanceKey) === null) {
		return instanceKey;
	}
	return runtimeInstanceKey({apiEndpoint: resolveDiscoveryApiEndpoint(instanceKey)});
}

function isReachableThroughDocumentTransport(endpoint: string): boolean {
	if (typeof window === 'undefined') {
		return false;
	}
	try {
		return new URL(endpoint).origin === window.location.origin;
	} catch {
		return false;
	}
}

function getDesktopRuntimeConfigAPI(): DesktopRuntimeConfigAPI | null {
	if (!isDesktopLocalAppDocument()) {
		return null;
	}
	const api = getElectronAPI()?.desktopRuntimeConfig;
	if (api === undefined) {
		throw new Error('Desktop instance discovery API is unavailable');
	}
	return api;
}

type DesktopDiscoveryCancellationOutcome =
	| {readonly kind: 'completed'}
	| {readonly kind: 'failed'; readonly error: unknown};

async function cancelDesktopDiscovery(
	api: DesktopRuntimeConfigAPI,
	requestId: string,
): Promise<DesktopDiscoveryCancellationOutcome> {
	try {
		await api.cancelResolution({requestId});
		return {kind: 'completed'};
	} catch (error) {
		return {kind: 'failed', error};
	}
}

function settleWithSignal<T>(promise: Promise<T>, signal: AbortSignal | null): Promise<T> {
	if (signal == null) {
		return promise;
	}
	signal.throwIfAborted();
	return new Promise<T>((resolve, reject) => {
		const onAbort = (): void => {
			reject(signal.reason);
		};
		const detach = (): void => {
			signal.removeEventListener('abort', onAbort);
		};
		signal.addEventListener('abort', onAbort, {once: true});
		promise.then(
			(value) => {
				detach();
				resolve(value);
			},
			(error: unknown) => {
				detach();
				reject(error);
			},
		);
	});
}

interface InstanceSnapshotEntry {
	snapshot: RuntimeConfigSnapshot;
	discoveryUrl: string | null;
	etag: string | null;
	lastModified: string | null;
	fetchedAt: number;
}

class PendingInstanceSnapshotResolution {
	readonly controller = new AbortController();
	readonly promise: Promise<InstanceSnapshotResolution>;
	consumers = 0;
	settled = false;

	constructor(
		resolve: (signal: AbortSignal) => Promise<InstanceSnapshotResolution>,
		onSettled: (pending: PendingInstanceSnapshotResolution) => void,
	) {
		this.promise = resolve(this.controller.signal).finally(() => {
			this.settled = true;
			onSettled(this);
		});
	}
}

class InstanceSnapshotCacheInvariantError extends Error {
	constructor() {
		super('Instance snapshot cache reached its limit without an oldest entry');
		this.name = 'InstanceSnapshotCacheInvariantError';
	}
}

class PendingInstanceSnapshotInvariantError extends Error {
	constructor() {
		super('Instance snapshot resolution was released without an active consumer');
		this.name = 'PendingInstanceSnapshotInvariantError';
	}
}

class InstanceSnapshotResolutionCapacityError extends Error {
	constructor() {
		super('Instance snapshot resolution capacity is exhausted');
		this.name = 'InstanceSnapshotResolutionCapacityError';
	}
}

const fetchDiscoveryWithPrebootHandoff: InstanceDiscoveryFetch = (url, init) => {
	const preboot = takePrebootDiscoveryResponse(url, init.signal);
	if (preboot === null) {
		return globalThis.fetch(url, init);
	}
	return preboot.catch(() => {
		init.signal.throwIfAborted();
		return globalThis.fetch(url, init);
	});
};

class InstanceSnapshotStore {
	entries: Map<string, InstanceSnapshotEntry> = new Map();
	private pending: Map<string, PendingInstanceSnapshotResolution> = new Map();

	constructor() {
		makeAutoObservable<InstanceSnapshotStore, 'pending'>(
			this,
			{entries: observableShallow, pending: false},
			{autoBind: true},
		);
	}

	get(instanceKey: string): RuntimeConfigSnapshot | null {
		return this.entries.get(instanceKey)?.snapshot ?? null;
	}

	getForInstanceDomain(domain: string): RuntimeConfigSnapshot | null {
		return this.findByDomain(domain)?.snapshot ?? null;
	}

	getForApiEndpoint(apiEndpoint: string): RuntimeConfigSnapshot | null {
		return this.findBy((entry) => entry.snapshot.apiEndpoint === apiEndpoint)?.entry.snapshot ?? null;
	}

	discoveryUrlFor(instanceKey: string): string | null {
		return this.entries.get(instanceKey)?.discoveryUrl ?? null;
	}

	getLimitsForInstance(domain: string): LimitConfigSnapshot | null {
		return this.getForInstanceDomain(domain)?.limits ?? null;
	}

	async resolve({input, signal}: InstanceSnapshotResolveRequest): Promise<InstanceSnapshotResolution> {
		signal?.throwIfAborted();
		const apiEndpoint = resolveDiscoveryApiEndpoint(input);
		const cached = this.findFreshResolution(apiEndpoint);
		if (cached !== null) {
			return cached;
		}
		return await this.awaitResolution(apiEndpoint, signal);
	}

	async refresh({input, signal}: InstanceSnapshotResolveRequest): Promise<InstanceSnapshotResolution> {
		signal?.throwIfAborted();
		return await this.awaitResolution(resolveDiscoveryApiEndpoint(input), signal);
	}

	private async awaitResolution(apiEndpoint: string, signal: AbortSignal | null): Promise<InstanceSnapshotResolution> {
		const pending = this.acquirePendingResolution(apiEndpoint);
		try {
			return await settleWithSignal(pending.promise, signal);
		} finally {
			this.releasePendingResolution(apiEndpoint, pending);
		}
	}

	private acquirePendingResolution(apiEndpoint: string): PendingInstanceSnapshotResolution {
		let pending = this.pending.get(apiEndpoint);
		if (pending === undefined) {
			if (this.pending.size >= SNAPSHOT_RESOLUTION_MAX_PENDING) {
				throw new InstanceSnapshotResolutionCapacityError();
			}
			pending = new PendingInstanceSnapshotResolution(
				(signal) => this.discover(apiEndpoint, signal),
				(settled) => {
					if (this.pending.get(apiEndpoint) === settled) {
						this.pending.delete(apiEndpoint);
					}
				},
			);
			this.pending.set(apiEndpoint, pending);
		}
		pending.consumers += 1;
		return pending;
	}

	private releasePendingResolution(apiEndpoint: string, pending: PendingInstanceSnapshotResolution): void {
		if (pending.consumers <= 0) {
			throw new PendingInstanceSnapshotInvariantError();
		}
		pending.consumers -= 1;
		if (pending.consumers !== 0 || pending.settled) {
			return;
		}
		if (this.pending.get(apiEndpoint) === pending) {
			this.pending.delete(apiEndpoint);
		}
		pending.controller.abort();
	}

	private findFreshResolution(apiEndpoint: string): InstanceSnapshotResolution | null {
		const now = Date.now();
		const found = this.findBy((entry) => {
			const age = now - entry.fetchedAt;
			return entry.snapshot.apiEndpoint === apiEndpoint && age >= 0 && age <= SNAPSHOT_CACHE_TTL_MS;
		});
		if (found === null) {
			return null;
		}
		return this.toResolution(found.instanceKey, found.entry);
	}

	private findByDomain(domain: string): InstanceSnapshotEntry | null {
		const wanted = domain.trim().toLowerCase();
		if (wanted.length === 0) {
			return null;
		}
		let match: InstanceSnapshotEntry | null = null;
		for (const entry of this.entries.values()) {
			if (endpointHostname(entry.snapshot.apiEndpoint) === wanted) {
				if (match !== null && runtimeInstanceKey(match.snapshot) !== runtimeInstanceKey(entry.snapshot)) {
					return null;
				}
				match = entry;
			}
		}
		return match;
	}

	private findBy(
		matches: (entry: InstanceSnapshotEntry) => boolean,
	): {instanceKey: string; entry: InstanceSnapshotEntry} | null {
		for (const [instanceKey, entry] of this.entries.entries()) {
			if (matches(entry)) {
				return {instanceKey, entry};
			}
		}
		return null;
	}

	private conditionalHeaders(apiEndpoint: string): Record<string, string> | undefined {
		if (!isReachableThroughDocumentTransport(apiEndpoint)) {
			return undefined;
		}
		const found = this.findBy((entry) => entry.snapshot.apiEndpoint === apiEndpoint);
		if (found == null) {
			return undefined;
		}
		const headers: Record<string, string> = {};
		if (found.entry.etag != null) {
			headers[HttpHeader.IF_NONE_MATCH] = found.entry.etag;
		}
		if (found.entry.lastModified != null) {
			headers[HttpHeader.IF_MODIFIED_SINCE] = found.entry.lastModified;
		}
		return Object.keys(headers).length > 0 ? headers : undefined;
	}

	private setEntry(instanceKey: string, entry: InstanceSnapshotEntry): void {
		this.entries.delete(instanceKey);
		if (this.entries.size >= SNAPSHOT_CACHE_MAX_ENTRIES) {
			const oldestInstanceKey = this.entries.keys().next().value;
			if (oldestInstanceKey === undefined) {
				throw new InstanceSnapshotCacheInvariantError();
			}
			this.entries.delete(oldestInstanceKey);
		}
		this.entries.set(instanceKey, entry);
	}

	private toResolution(instanceKey: string, entry: InstanceSnapshotEntry): InstanceSnapshotResolution {
		return {snapshot: entry.snapshot, instanceKey, productName: snapshotProductName(entry.snapshot)};
	}

	private storeSnapshot(
		snapshot: RuntimeConfigSnapshot,
		source: {discoveryUrl: string | null; etag: string | null; lastModified: string | null; advertisedBy: string},
	): InstanceSnapshotResolution {
		const instanceKey = runtimeInstanceKey(snapshot);
		if (instanceKey === null) {
			throw new Error(`${source.advertisedBy} advertised an unusable API endpoint ("${snapshot.apiEndpoint}")`);
		}
		const entry: InstanceSnapshotEntry = {
			snapshot,
			discoveryUrl: source.discoveryUrl,
			etag: source.etag,
			lastModified: source.lastModified,
			fetchedAt: Date.now(),
		};
		runInAction(() => {
			this.setEntry(instanceKey, entry);
		});
		return this.toResolution(instanceKey, entry);
	}

	private async discover(apiEndpoint: string, signal: AbortSignal | null): Promise<InstanceSnapshotResolution> {
		const desktopApi = getDesktopRuntimeConfigAPI();
		if (desktopApi != null) {
			return await this.discoverThroughDesktopRuntime(desktopApi, apiEndpoint, signal);
		}
		const result = await fetchInstanceDiscovery({
			input: apiEndpoint,
			fetch: fetchDiscoveryWithPrebootHandoff,
			signal: signal ?? undefined,
			conditionalHeaders: this.conditionalHeaders(apiEndpoint),
			adoptServingOrigin: adoptsDocumentOrigin(apiEndpoint),
		});
		if (result.kind === 'not-modified') {
			const found = this.findBy((entry) => entry.discoveryUrl === result.url);
			if (found == null) {
				throw new Error(`${result.url} responded 304 with no cached discovery document`);
			}
			runInAction(() => {
				this.setEntry(found.instanceKey, {...found.entry, fetchedAt: Date.now()});
			});
			return this.toResolution(found.instanceKey, found.entry);
		}
		return this.storeSnapshot(runtimeSnapshotFromDiscovery(result.document), {
			discoveryUrl: result.url,
			etag: result.etag,
			lastModified: result.lastModified,
			advertisedBy: result.url,
		});
	}

	private async discoverThroughDesktopRuntime(
		api: DesktopRuntimeConfigAPI,
		apiEndpoint: string,
		signal: AbortSignal | null,
	): Promise<InstanceSnapshotResolution> {
		const requestId = randomUuid();
		const cancellation: {current: Promise<DesktopDiscoveryCancellationOutcome> | null} = {current: null};
		const cancel = (): void => {
			cancellation.current ??= cancelDesktopDiscovery(api, requestId);
		};
		signal?.throwIfAborted();
		signal?.addEventListener('abort', cancel, {once: true});
		let plan: DesktopRuntimePlan;
		try {
			plan = await api.resolve({input: apiEndpoint, requestId});
		} catch (error) {
			if (cancellation.current !== null) {
				const outcome = await cancellation.current;
				if (outcome.kind === 'failed') {
					throw new AggregateError(
						[error, outcome.error],
						'Instance discovery failed and its desktop cancellation was rejected',
					);
				}
			}
			throw error;
		} finally {
			signal?.removeEventListener('abort', cancel);
		}
		if (cancellation.current !== null) {
			const outcome = await cancellation.current;
			if (outcome.kind === 'failed') {
				throw new Error('Desktop instance discovery cancellation was rejected', {cause: outcome.error});
			}
		}
		signal?.throwIfAborted();
		return this.storeSnapshot(runtimeSnapshotFromDiscovery(parseInstanceDiscoveryDocument(plan.document)), {
			discoveryUrl: plan.remoteApiEndpoint,
			etag: null,
			lastModified: null,
			advertisedBy: apiEndpoint,
		});
	}
}

export default new InstanceSnapshotStore();
