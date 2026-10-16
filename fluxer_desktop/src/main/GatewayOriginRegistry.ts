// SPDX-License-Identifier: AGPL-3.0-or-later

import {getDesktopAppStorage} from '@electron/main/DesktopAppStorage';
import {getDesktopOutboundHTTP} from '@electron/main/DesktopOutboundHTTP';
import {fetchValidatedFluxerInstance} from '@electron/main/FluxerInstanceValidation';
import {getDesktopSelectedInstanceClient} from '@electron/main/SelectedInstanceFetch';
import {websocketHTTPOrigin, websocketOrigin} from '@electron/main/WebSocketOrigin';
import {normalizeHTTPNetworkOrigin} from '@fluxer/instance_bootstrap/src/NetworkOrigin';

const GATEWAY_ORIGIN_ADMISSION_TTL_MS = 6 * 60 * 60 * 1000;
const GATEWAY_ORIGIN_DISCOVERY_FAILURE_TTL_MS = 60 * 1000;
const GATEWAY_ORIGIN_REGISTRATIONS_PER_WINDOW = 8;
const GATEWAY_ORIGIN_REGISTRATION_WINDOW_MS = 60 * 1000;
const GATEWAY_ORIGIN_REGISTRATIONS_PER_DOCUMENT = 64;
const GATEWAY_ORIGIN_REGISTRATION_MIN_INTERVAL_MS = 1000;
const GATEWAY_ORIGIN_ANCHOR_LIMIT = 32;
const GATEWAY_ORIGIN_RESOLUTION_CACHE_LIMIT = 256;

type GatewayOriginResolver = (anchorOrigin: string, signal: AbortSignal | null) => Promise<string | null>;

interface GatewayOriginRegistryDependencies {
	readonly resolveAnchorGatewayOrigin: GatewayOriginResolver;
	readonly additionalAnchorOrigins?: () => Promise<ReadonlyArray<string>>;
	readonly now?: () => number;
}

interface GatewayOriginAdmissionRequest {
	readonly documentKey: object;
	readonly url: string;
	readonly signal?: AbortSignal | null;
}

function anchorOrigin(value: string): string | null {
	return normalizeHTTPNetworkOrigin(value) ?? websocketHTTPOrigin(value);
}

class GatewayOriginNotAdmittedError extends Error {
	public readonly reason: string;

	public constructor(reason: string) {
		super(`Gateway origin is not admitted for this renderer document: ${reason}`);
		this.name = 'GatewayOriginNotAdmittedError';
		this.reason = reason;
	}
}

interface AnchorResolution {
	readonly expiresAt: number;
	readonly gatewayOrigin: string | null;
}

interface DocumentState {
	readonly admitted: Map<string, number>;
	readonly inFlight: Map<string, Promise<string>>;
	readonly registrationTimes: Array<number>;
	readonly lastAttemptPerOrigin: Map<string, number>;
	registrations: number;
}

function createDocumentState(): DocumentState {
	return {
		admitted: new Map(),
		inFlight: new Map(),
		registrationTimes: [],
		lastAttemptPerOrigin: new Map(),
		registrations: 0,
	};
}

export class GatewayOriginRegistry {
	private readonly anchorOrigins = new Set<string>();
	private readonly anchorResolutions = new Map<string, AnchorResolution>();
	private readonly anchorInFlight = new Map<string, Promise<string | null>>();
	private readonly documents = new WeakMap<object, DocumentState>();
	private readonly now: () => number;

	private readonly dependencies: GatewayOriginRegistryDependencies;

	public constructor(dependencies: GatewayOriginRegistryDependencies) {
		this.dependencies = dependencies;
		this.now = dependencies.now ?? Date.now;
	}

	public seedAnchorOrigin(value: string): void {
		const origin = anchorOrigin(value);
		if (origin == null) return;
		this.anchorOrigins.delete(origin);
		this.anchorOrigins.add(origin);
		while (this.anchorOrigins.size > GATEWAY_ORIGIN_ANCHOR_LIMIT) {
			const oldest = this.anchorOrigins.values().next().value;
			if (oldest == null) break;
			this.anchorOrigins.delete(oldest);
		}
	}

	public recordAnchorGateway(anchorValue: string, gatewayEndpoint: string | null): void {
		const origin = anchorOrigin(anchorValue);
		if (origin == null) return;
		this.seedAnchorOrigin(origin);
		const gatewayOrigin = gatewayEndpoint == null ? null : websocketOrigin(gatewayEndpoint);
		if (gatewayOrigin == null) return;
		this.storeAnchorResolution(origin, gatewayOrigin);
	}

	public releaseDocument(documentKey: object): void {
		this.documents.delete(documentKey);
	}

	public cleanup(): void {
		this.anchorResolutions.clear();
		this.anchorInFlight.clear();
	}

	public async admit({documentKey, url, signal}: GatewayOriginAdmissionRequest): Promise<string> {
		const targetOrigin = websocketOrigin(url);
		if (targetOrigin == null) {
			throw new GatewayOriginNotAdmittedError('the target has no WebSocket origin');
		}
		const state = this.stateFor(documentKey);
		const admittedUntil = state.admitted.get(targetOrigin);
		const now = this.now();
		if (admittedUntil != null && admittedUntil > now) {
			return targetOrigin;
		}
		const inFlight = state.inFlight.get(targetOrigin);
		if (inFlight != null) {
			return await inFlight;
		}
		state.admitted.delete(targetOrigin);
		this.consumeRegistrationBudget(state, targetOrigin, now);
		const admission = this.resolveAdmission(state, targetOrigin, url, signal ?? null).finally(() => {
			state.inFlight.delete(targetOrigin);
		});
		state.inFlight.set(targetOrigin, admission);
		return await admission;
	}

	private async resolveAdmission(
		state: DocumentState,
		targetOrigin: string,
		url: string,
		signal: AbortSignal | null,
	): Promise<string> {
		if (this.hasCachedAnchorFor(targetOrigin)) {
			state.admitted.set(targetOrigin, this.now() + GATEWAY_ORIGIN_ADMISSION_TTL_MS);
			return targetOrigin;
		}
		for (const candidate of await this.anchorCandidates(url)) {
			const gatewayOrigin = await this.resolveAnchor(candidate, signal);
			if (gatewayOrigin === targetOrigin) {
				state.admitted.set(targetOrigin, this.now() + GATEWAY_ORIGIN_ADMISSION_TTL_MS);
				return targetOrigin;
			}
		}
		throw new GatewayOriginNotAdmittedError('no reachable instance advertises it as its gateway');
	}

	private hasCachedAnchorFor(targetOrigin: string): boolean {
		const now = this.now();
		for (const candidate of this.anchorOrigins) {
			const cached = this.anchorResolutions.get(candidate);
			if (cached != null && cached.expiresAt > now && cached.gatewayOrigin === targetOrigin) {
				return true;
			}
		}
		return false;
	}

	private stateFor(documentKey: object): DocumentState {
		const existing = this.documents.get(documentKey);
		if (existing != null) return existing;
		const created = createDocumentState();
		this.documents.set(documentKey, created);
		return created;
	}

	private async anchorCandidates(url: string): Promise<ReadonlyArray<string>> {
		const candidates = new Set(this.anchorOrigins);
		for (const value of await this.readAdditionalAnchorOrigins()) {
			const origin = anchorOrigin(value);
			if (origin != null) candidates.add(origin);
			if (candidates.size >= GATEWAY_ORIGIN_ANCHOR_LIMIT) break;
		}
		const targetHTTPOrigin = websocketHTTPOrigin(url);
		if (targetHTTPOrigin != null) candidates.add(targetHTTPOrigin);
		return [...candidates];
	}

	private async readAdditionalAnchorOrigins(): Promise<ReadonlyArray<string>> {
		const read = this.dependencies.additionalAnchorOrigins;
		if (read == null) return [];
		try {
			return await read();
		} catch {
			return [];
		}
	}

	private consumeRegistrationBudget(state: DocumentState, targetOrigin: string, now: number): void {
		if (state.registrations >= GATEWAY_ORIGIN_REGISTRATIONS_PER_DOCUMENT) {
			throw new GatewayOriginNotAdmittedError('this document exhausted its origin registration budget');
		}
		const lastAttempt = state.lastAttemptPerOrigin.get(targetOrigin);
		if (lastAttempt != null && now - lastAttempt < GATEWAY_ORIGIN_REGISTRATION_MIN_INTERVAL_MS) {
			throw new GatewayOriginNotAdmittedError('this origin was attempted too recently');
		}
		while (
			state.registrationTimes.length > 0 &&
			now - state.registrationTimes[0] >= GATEWAY_ORIGIN_REGISTRATION_WINDOW_MS
		) {
			state.registrationTimes.shift();
		}
		if (state.registrationTimes.length >= GATEWAY_ORIGIN_REGISTRATIONS_PER_WINDOW) {
			throw new GatewayOriginNotAdmittedError('this document registered too many origins in the last minute');
		}
		state.registrationTimes.push(now);
		state.lastAttemptPerOrigin.set(targetOrigin, now);
		state.registrations += 1;
	}

	private async resolveAnchor(candidate: string, signal: AbortSignal | null): Promise<string | null> {
		const cached = this.anchorResolutions.get(candidate);
		if (cached != null && cached.expiresAt > this.now()) {
			return cached.gatewayOrigin;
		}
		const inFlight = this.anchorInFlight.get(candidate);
		if (inFlight != null) return inFlight;
		const resolution = this.dependencies
			.resolveAnchorGatewayOrigin(candidate, signal)
			.then((endpoint) => (endpoint == null ? null : websocketOrigin(endpoint)))
			.catch(() => null)
			.then((gatewayOrigin) => {
				this.storeAnchorResolution(candidate, gatewayOrigin);
				return gatewayOrigin;
			})
			.finally(() => {
				this.anchorInFlight.delete(candidate);
			});
		this.anchorInFlight.set(candidate, resolution);
		return resolution;
	}

	private storeAnchorResolution(candidate: string, gatewayOrigin: string | null): void {
		this.anchorResolutions.delete(candidate);
		this.anchorResolutions.set(candidate, {
			gatewayOrigin,
			expiresAt:
				this.now() +
				(gatewayOrigin == null ? GATEWAY_ORIGIN_DISCOVERY_FAILURE_TTL_MS : GATEWAY_ORIGIN_ADMISSION_TTL_MS),
		});
		while (this.anchorResolutions.size > GATEWAY_ORIGIN_RESOLUTION_CACHE_LIMIT) {
			const oldest = this.anchorResolutions.keys().next().value;
			if (oldest == null) break;
			this.anchorResolutions.delete(oldest);
		}
	}
}

let sharedRegistry: GatewayOriginRegistry | null = null;

async function readKnownInstanceAnchorOrigins(): Promise<ReadonlyArray<string>> {
	const storage = getDesktopAppStorage();
	if (storage === null) return [];
	const records = await storage.getAllKnownInstances();
	return records.map((record) => record.domain);
}

async function readAnchorGatewayEndpoint(candidate: string, signal: AbortSignal | null): Promise<string | null> {
	const instance = await fetchValidatedFluxerInstance({
		origin: candidate,
		outboundHTTP: getDesktopOutboundHTTP(),
		selectedInstanceClient: getDesktopSelectedInstanceClient(),
		signal: signal ?? undefined,
	});
	return instance.gatewayEndpoint;
}

export function getGatewayOriginRegistry(): GatewayOriginRegistry {
	sharedRegistry ??= new GatewayOriginRegistry({
		additionalAnchorOrigins: readKnownInstanceAnchorOrigins,
		resolveAnchorGatewayOrigin: readAnchorGatewayEndpoint,
	});
	return sharedRegistry;
}

export function recordGatewayOriginAnchor(value: string, gatewayEndpoint: string | null): void {
	getGatewayOriginRegistry().recordAnchorGateway(value, gatewayEndpoint);
}
