// SPDX-License-Identifier: AGPL-3.0-or-later

import {AccountIdentityModes, TagStyles} from '@fluxer/constants/src/AccountIdentityConstants';
import {Headers as HttpHeader} from '@fluxer/constants/src/Headers';
import {HttpStatus, MimeType} from '@fluxer/constants/src/HttpConstants';
import type {LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';
import {InstanceEndpointKind, normalizeInstanceEndpoint} from '@fluxer/instance_bootstrap/src/EndpointNormalization';
import {CanonicalNetworkProtocol} from '@fluxer/instance_bootstrap/src/NetworkOrigin';
import {
	AGE_POLICY_ACTIONS,
	CURRENT_INSTANCE_CODENAME,
	type InstanceAgePolicy,
	type InstanceAgePolicyGeo,
	type InstanceAppRegistration,
	type InstanceBranding,
	type InstanceCommunity,
	type InstanceDomainMigration,
	type InstanceFeatures,
	type InstanceGif,
	type InstanceLegal,
	type InstancePush,
	type InstanceRegistration,
	type InstanceServices,
	type InstanceSetup,
	type InstanceSso,
	PUSH_DELIVERY_MODES,
	REGISTRATION_MODES,
} from '@fluxer/instance_bootstrap/src/Types';
import type {LimitConfigSnapshot, LimitConfigWireFormat, LimitFilter, LimitRule} from '@fluxer/limits/src/LimitTypes';
import {EXPERIMENT_BUCKET_RESOLUTION} from '@fluxer/schema/src/domains/experiment/ExperimentBucket';

export const INSTANCE_DISCOVERY_PATH = '/.well-known/fluxer';
const INSTANCE_DISCOVERY_CONVENTIONAL_ENDPOINT_PATH = '/api';
export const INSTANCE_DISCOVERY_MAX_BYTES = 1024 * 1024;
export const INSTANCE_DISCOVERY_MAX_CHUNKS = 1024;
export const INSTANCE_DISCOVERY_TIMEOUT_MS = 15_000;

export const ACCOUNT_DELETION_GRACE_PERIOD_HOURS_MIN = 1;
export const ACCOUNT_DELETION_GRACE_PERIOD_HOURS_MAX = 8760;
export const MAX_BACKGROUND_GATEWAY_CONNECTIONS_MIN = 0;
export const MAX_BACKGROUND_GATEWAY_CONNECTIONS_MAX = 64;
export const LIMIT_RULES_MAX = 100;
export const AGE_POLICY_GEOS_MAX = 512;
export const DOMAIN_MIGRATION_ROLLOUT_BASIS_POINTS_MAX = EXPERIMENT_BUCKET_RESOLUTION;
export const DOMAIN_MIGRATION_ROLLOUT_SALT_MAX_LENGTH = 64;
const DOMAIN_MIGRATION_ROLLOUT_SALT_PATTERN = /^[\x20-\x7e]+$/u;

export const InstanceGeneration = Object.freeze({
	TUNGSTEN: CURRENT_INSTANCE_CODENAME,
	LEGACY: 'legacy',
} as const);

export type InstanceGeneration = (typeof InstanceGeneration)[keyof typeof InstanceGeneration];

const FEATURE_FLAG_KEYS = Object.freeze([
	'voice_enabled',
	'stripe_enabled',
	'self_hosted',
	'presigned_attachment_uploads',
	'emails_enabled',
	'premium_enabled',
	'stripe_serviceable',
	'phone_verification_enabled',
	'desktop_modules_enabled',
] as const) satisfies ReadonlyArray<keyof InstanceFeatures>;

const BRANDING_URL_KEYS = Object.freeze([
	'icon_url',
	'symbol_url',
	'logo_url',
	'wordmark_url',
	'favicon_url',
	'status_page_url',
	'status_page_incident_history_url',
	'premium_info_url',
] as const) satisfies ReadonlyArray<keyof InstanceBranding>;

interface InstanceDiscoveryEndpoints {
	readonly api: string;
	readonly gateway: string;
	readonly api_client: string | null;
	readonly api_public: string | null;
	readonly media: string | null;
	readonly upload_relay: string | null;
	readonly static_cdn: string | null;
	readonly marketing: string | null;
	readonly docs: string | null;
	readonly admin: string | null;
	readonly invite: string | null;
	readonly gift: string | null;
	readonly webapp: string | null;
}

interface InstanceDiscoveryAppPublic {
	readonly branding: Readonly<Partial<InstanceBranding>>;
	readonly setup: Readonly<Partial<InstanceSetup>>;
	readonly legal: Readonly<Partial<InstanceLegal>>;
	readonly registration: Readonly<Partial<InstanceAppRegistration>>;
}

export interface InstanceDiscoveryDocument {
	readonly generation: InstanceGeneration;
	readonly apiCodeVersion: number | null;
	readonly endpoints: InstanceDiscoveryEndpoints;
	readonly features: Readonly<Partial<InstanceFeatures>>;
	readonly gif: InstanceGif | null;
	readonly sso: InstanceSso | null;
	readonly registration: InstanceRegistration | null;
	readonly community: InstanceCommunity | null;
	readonly services: InstanceServices | null;
	readonly limits: LimitConfigSnapshot | LimitConfigWireFormat | null;
	readonly push: Readonly<Partial<InstancePush>>;
	readonly appPublic: InstanceDiscoveryAppPublic | null;
	readonly agePolicy: InstanceAgePolicy | null;
	readonly domainMigration: InstanceDomainMigration | null;
}

interface InstanceDiscoveryAttempt {
	readonly url: string;
	readonly reason: string;
}

export class InvalidInstanceDiscoveryDocumentError extends Error {
	public readonly reason: string;

	public constructor(reason: string) {
		super(`Instance discovery document is unusable: ${reason}`);
		this.name = 'InvalidInstanceDiscoveryDocumentError';
		this.reason = reason;
	}
}

export class InstanceRequiresNewerClientError extends Error {
	public readonly reportedGeneration: string;

	public constructor(reportedGeneration: string) {
		super(`Instance speaks generation "${reportedGeneration}", which this client does not understand`);
		this.name = 'InstanceRequiresNewerClientError';
		this.reportedGeneration = reportedGeneration;
	}
}

export class InstanceDiscoveryOriginMismatchError extends Error {
	public readonly servedBy: string;
	public readonly declared: string;

	public constructor(servedBy: string, declared: string) {
		super(`Instance discovery served by ${servedBy} declared an API at ${declared}`);
		this.name = 'InstanceDiscoveryOriginMismatchError';
		this.servedBy = servedBy;
		this.declared = declared;
	}
}

export class InstanceDiscoveryUnreachableError extends Error {
	public readonly attempts: ReadonlyArray<InstanceDiscoveryAttempt>;

	public constructor(attempts: ReadonlyArray<InstanceDiscoveryAttempt>) {
		const described = attempts.map((attempt) => `${attempt.url} (${attempt.reason})`).join(', ');
		super(`No discovery document was served by any candidate: ${described}`);
		this.name = 'InstanceDiscoveryUnreachableError';
		this.attempts = attempts;
	}
}

export interface InstanceDiscoveryRequestInit {
	readonly method: 'GET';
	readonly headers: Record<string, string>;
	readonly redirect: 'error';
	readonly credentials: 'omit';
	readonly cache: 'no-store';
	readonly referrerPolicy: 'no-referrer';
	readonly signal: AbortSignal;
}

export interface InstanceDiscoveryBodyReader {
	read(): Promise<{done: boolean; value?: Uint8Array}>;
	cancel(): Promise<void>;
}

interface InstanceDiscoveryResponseLike {
	readonly status: number;
	readonly headers: {get(name: string): string | null};
	readonly body: {getReader(): InstanceDiscoveryBodyReader} | null;
	text(): Promise<string>;
}

export type InstanceDiscoveryFetch = (
	url: string,
	init: InstanceDiscoveryRequestInit,
) => Promise<InstanceDiscoveryResponseLike>;

interface InstanceDiscoveryCandidateRequest {
	readonly input: string;
	readonly knownEndpointPath?: string | null;
}

interface FetchInstanceDiscoveryRequest extends InstanceDiscoveryCandidateRequest {
	readonly fetch?: InstanceDiscoveryFetch;
	readonly signal?: AbortSignal;
	readonly conditionalHeaders?: Readonly<Record<string, string>>;
	readonly adoptServingOrigin?: boolean;
}

type InstanceDiscoveryResult =
	| {
			readonly kind: 'ok';
			readonly url: string;
			readonly document: InstanceDiscoveryDocument;
			readonly etag: string | null;
			readonly lastModified: string | null;
	  }
	| {readonly kind: 'not-modified'; readonly url: string};

export function buildInstanceDiscoveryCandidates({
	input,
	knownEndpointPath,
}: InstanceDiscoveryCandidateRequest): ReadonlyArray<string> {
	const base = normalizeInstanceEndpoint(input, InstanceEndpointKind.SERVICE);
	if (base == null) {
		return [];
	}
	const url = new URL(base);
	const origin = `${url.protocol}//${url.host}`;
	const carriedPath = url.pathname === '/' ? null : url.pathname;
	const endpointPath = normalizeDiscoveryPath(knownEndpointPath) ?? carriedPath;
	const candidates: Array<string> = [];
	appendCandidate(candidates, `${origin}${INSTANCE_DISCOVERY_PATH}`);
	if (endpointPath != null) {
		appendCandidate(candidates, `${origin}${endpointPath}${INSTANCE_DISCOVERY_PATH}`);
	}
	appendCandidate(candidates, `${origin}${INSTANCE_DISCOVERY_CONVENTIONAL_ENDPOINT_PATH}${INSTANCE_DISCOVERY_PATH}`);
	return candidates;
}

export async function fetchInstanceDiscovery(request: FetchInstanceDiscoveryRequest): Promise<InstanceDiscoveryResult> {
	const candidates = buildInstanceDiscoveryCandidates(request);
	if (candidates.length === 0) {
		throw new InstanceDiscoveryUnreachableError([{url: request.input, reason: 'not an absolute http(s) endpoint'}]);
	}
	const attempts: Array<InstanceDiscoveryAttempt> = [];
	for (const url of candidates) {
		try {
			return await requestInstanceDiscovery(url, request);
		} catch (error) {
			request.signal?.throwIfAborted();
			if (error instanceof InstanceRequiresNewerClientError) {
				throw error;
			}
			attempts.push({url, reason: error instanceof Error ? error.message : String(error)});
		}
	}
	throw new InstanceDiscoveryUnreachableError(attempts);
}

export function parseInstanceDiscoveryDocument(payload: unknown): InstanceDiscoveryDocument {
	const source = asRecord(payload);
	if (source == null) {
		throw new InvalidInstanceDiscoveryDocumentError('the response body is not a JSON object');
	}
	const apiCodeVersion = readInteger(source, 'api_code_version', 1, Number.MAX_SAFE_INTEGER);
	return {
		generation: resolveGeneration(source, apiCodeVersion),
		apiCodeVersion,
		endpoints: parseEndpoints(source.endpoints),
		features: parseFeatures(source.features),
		gif: parseGif(source.gif),
		sso: parseSso(source.sso),
		registration: parseRegistration(source.registration),
		community: parseCommunity(source.community),
		services: parseServices(source.services),
		limits: parseLimits(source.limits),
		push: parsePush(source.push),
		appPublic: parseAppPublic(source.app_public),
		agePolicy: parseAgePolicy(source.age_policy),
		domainMigration: parseInstanceDomainMigration(source.domain_migration),
	};
}

async function requestInstanceDiscovery(
	url: string,
	request: FetchInstanceDiscoveryRequest,
): Promise<InstanceDiscoveryResult> {
	const deadline = AbortSignal.timeout(INSTANCE_DISCOVERY_TIMEOUT_MS);
	const fetchInstance = request.fetch ?? globalThis.fetch;
	const response = await fetchInstance(url, {
		method: 'GET',
		headers: {[HttpHeader.ACCEPT]: MimeType.JSON, ...request.conditionalHeaders},
		redirect: 'error',
		credentials: 'omit',
		cache: 'no-store',
		referrerPolicy: 'no-referrer',
		signal: request.signal == null ? deadline : AbortSignal.any([deadline, request.signal]),
	});
	if (response.status === HttpStatus.NOT_MODIFIED) {
		return {kind: 'not-modified', url};
	}
	if (response.status !== HttpStatus.OK) {
		throw new Error(`responded HTTP ${response.status.toString()}`);
	}
	if (!isJSONContentType(response.headers.get(HttpHeader.CONTENT_TYPE))) {
		throw new Error('responded without a JSON content type');
	}
	const parsed = parseInstanceDiscoveryDocument(JSON.parse(await readBoundedBody(response)));
	const document = request.adoptServingOrigin === true ? withEndpointsOnServingOrigin(url, parsed) : parsed;
	requireDiscoveryDeclaresItsOwnOrigin(url, document);
	return {
		kind: 'ok',
		url,
		document,
		etag: response.headers.get(HttpHeader.ETAG),
		lastModified: response.headers.get(HttpHeader.LAST_MODIFIED),
	};
}

function withEndpointsOnServingOrigin(url: string, document: InstanceDiscoveryDocument): InstanceDiscoveryDocument {
	const servedBy = originOfEndpoint(url);
	if (servedBy == null) {
		return document;
	}
	const api = `${servedBy}${INSTANCE_DISCOVERY_CONVENTIONAL_ENDPOINT_PATH}`;
	return {
		...document,
		endpoints: {
			...document.endpoints,
			api,
			api_client: document.endpoints.api_client == null ? null : api,
			webapp: document.endpoints.webapp == null ? null : servedBy,
		},
	};
}

export function requireDiscoveryDeclaresItsOwnOrigin(url: string, document: InstanceDiscoveryDocument): void {
	const servedBy = originOfEndpoint(url);
	if (servedBy == null) {
		throw new InstanceDiscoveryOriginMismatchError(url, document.endpoints.api);
	}
	for (const declared of [document.endpoints.api, document.endpoints.api_client]) {
		if (declared == null) {
			continue;
		}
		if (originOfEndpoint(declared) !== servedBy) {
			throw new InstanceDiscoveryOriginMismatchError(servedBy, declared);
		}
	}
}

function originOfEndpoint(value: string): string | null {
	try {
		return new URL(value).origin;
	} catch {
		return null;
	}
}

async function readBoundedBody(response: InstanceDiscoveryResponseLike): Promise<string> {
	const body = response.body;
	if (body == null) {
		const text = await response.text();
		if (new TextEncoder().encode(text).length > INSTANCE_DISCOVERY_MAX_BYTES) {
			throw new Error('exceeded the discovery document byte cap');
		}
		return text;
	}
	const reader = body.getReader();
	const decoder = new TextDecoder();
	let text = '';
	let bytes = 0;
	let chunks = 0;
	try {
		for (;;) {
			const {done, value} = await reader.read();
			if (done) {
				break;
			}
			if (value == null) {
				continue;
			}
			chunks += 1;
			bytes += value.byteLength;
			if (chunks > INSTANCE_DISCOVERY_MAX_CHUNKS) {
				throw new Error('exceeded the discovery document chunk cap');
			}
			if (bytes > INSTANCE_DISCOVERY_MAX_BYTES) {
				throw new Error('exceeded the discovery document byte cap');
			}
			text += decoder.decode(value, {stream: true});
		}
	} finally {
		await reader.cancel();
	}
	return `${text}${decoder.decode()}`;
}

function isJSONContentType(value: string | null): boolean {
	if (value == null) {
		return false;
	}
	const essence = value.split(';')[0]?.trim().toLowerCase() ?? '';
	return essence === MimeType.JSON || essence.endsWith('+json');
}

function appendCandidate(candidates: Array<string>, value: string): void {
	if (!candidates.includes(value)) {
		candidates.push(value);
	}
}

function normalizeDiscoveryPath(value: string | null | undefined): string | null {
	if (typeof value !== 'string') {
		return null;
	}
	const trimmed = value.trim();
	if (!trimmed.startsWith('/') || trimmed.includes('?') || trimmed.includes('#')) {
		return null;
	}
	const stripped = trimmed.replace(/\/+$/u, '');
	return stripped.length === 0 ? null : stripped;
}

function resolveGeneration(source: Record<string, unknown>, apiCodeVersion: number | null): InstanceGeneration {
	const reported = source.codename;
	if (typeof reported === 'string' && reported.length > 0) {
		if (reported === InstanceGeneration.TUNGSTEN) {
			return InstanceGeneration.TUNGSTEN;
		}
		throw new InstanceRequiresNewerClientError(reported);
	}
	if (source.api_code_version != null && apiCodeVersion == null) {
		throw new InvalidInstanceDiscoveryDocumentError('api_code_version is not a positive integer');
	}
	return InstanceGeneration.LEGACY;
}

function parseEndpoints(value: unknown): InstanceDiscoveryEndpoints {
	const source = asRecord(value);
	if (source == null) {
		throw new InvalidInstanceDiscoveryDocumentError('endpoints is missing or not an object');
	}
	const api = normalizeInstanceEndpoint(source.api, InstanceEndpointKind.SERVICE);
	if (api == null) {
		throw new InvalidInstanceDiscoveryDocumentError('endpoints.api is missing or not an absolute http(s) endpoint');
	}
	const gateway = normalizeInstanceEndpoint(source.gateway, InstanceEndpointKind.GATEWAY);
	if (gateway == null) {
		throw new InvalidInstanceDiscoveryDocumentError('endpoints.gateway is missing or not a ws(s) endpoint');
	}
	return {
		api,
		gateway,
		api_client: normalizeInstanceEndpoint(source.api_client, InstanceEndpointKind.SERVICE),
		api_public: normalizeInstanceEndpoint(source.api_public, InstanceEndpointKind.SERVICE),
		media: normalizeInstanceEndpoint(source.media, InstanceEndpointKind.SERVICE),
		upload_relay: normalizeInstanceEndpoint(source.upload_relay, InstanceEndpointKind.SERVICE),
		static_cdn: normalizeInstanceEndpoint(source.static_cdn, InstanceEndpointKind.SERVICE),
		marketing: normalizeInstanceEndpoint(source.marketing, InstanceEndpointKind.SERVICE),
		docs: normalizeInstanceEndpoint(source.docs, InstanceEndpointKind.SERVICE),
		admin: normalizeInstanceEndpoint(source.admin, InstanceEndpointKind.SERVICE),
		invite: normalizeInstanceEndpoint(source.invite, InstanceEndpointKind.SERVICE),
		gift: normalizeInstanceEndpoint(source.gift, InstanceEndpointKind.SERVICE),
		webapp: normalizeInstanceEndpoint(source.webapp, InstanceEndpointKind.WEBAPP),
	};
}

function parseFeatures(value: unknown): Readonly<Partial<InstanceFeatures>> {
	const source = asRecord(value);
	if (source == null) {
		return {};
	}
	const features: Partial<InstanceFeatures> = {};
	for (const key of FEATURE_FLAG_KEYS) {
		const flag = source[key];
		if (typeof flag === 'boolean') {
			features[key] = flag;
		}
	}
	const accountIdentity = Object.values(AccountIdentityModes).find((mode) => mode === source.account_identity);
	if (accountIdentity !== undefined) {
		features.account_identity = accountIdentity;
	}
	const tagStyle = Object.values(TagStyles).find((style) => style === source.tag_style);
	if (tagStyle !== undefined) {
		features.tag_style = tagStyle;
	}
	const gracePeriodHours = readInteger(
		source,
		'account_deletion_grace_period_hours',
		ACCOUNT_DELETION_GRACE_PERIOD_HOURS_MIN,
		ACCOUNT_DELETION_GRACE_PERIOD_HOURS_MAX,
	);
	if (gracePeriodHours != null) {
		features.account_deletion_grace_period_hours = gracePeriodHours;
	}
	const maxBackgroundGatewayConnections = readInteger(
		source,
		'max_background_gateway_connections',
		MAX_BACKGROUND_GATEWAY_CONNECTIONS_MIN,
		MAX_BACKGROUND_GATEWAY_CONNECTIONS_MAX,
	);
	if (maxBackgroundGatewayConnections != null) {
		features.max_background_gateway_connections = maxBackgroundGatewayConnections;
	}
	return features;
}

function parseGif(value: unknown): InstanceGif | null {
	const source = asRecord(value);
	if (source == null) {
		return null;
	}
	const provider = readNonEmptyString(source, 'provider');
	const displayName = readNonEmptyString(source, 'display_name');
	const attributionRequired = readBoolean(source, 'attribution_required');
	if (provider == null || displayName == null || attributionRequired == null) {
		return null;
	}
	return {provider, display_name: displayName, attribution_required: attributionRequired};
}

function parseSso(value: unknown): InstanceSso | null {
	const source = asRecord(value);
	if (source == null) {
		return null;
	}
	const enabled = readBoolean(source, 'enabled');
	const enforced = readBoolean(source, 'enforced');
	const redirectUri = readAbsoluteHTTPURL(source, 'redirect_uri');
	if (enabled == null || enforced == null || redirectUri == null) {
		return null;
	}
	const sso: InstanceSso = {
		enabled,
		enforced,
		display_name: readNonEmptyString(source, 'display_name'),
		redirect_uri: redirectUri,
	};
	const available = readBoolean(source, 'available');
	if (available != null) {
		sso.available = available;
	}
	return sso;
}

function parseRegistration(value: unknown): InstanceRegistration | null {
	const source = asRecord(value);
	if (source == null) {
		return null;
	}
	const mode = readLiteral(source, 'mode', REGISTRATION_MODES);
	const adminRegistrationUrlsEnabled = readBoolean(source, 'admin_registration_urls_enabled');
	if (mode == null || adminRegistrationUrlsEnabled == null) {
		return null;
	}
	return {mode, admin_registration_urls_enabled: adminRegistrationUrlsEnabled};
}

function parseCommunity(value: unknown): InstanceCommunity | null {
	const source = asRecord(value);
	if (source == null) {
		return null;
	}
	const singleCommunity = readBoolean(source, 'single_community');
	const directMessagesDisabled = readBoolean(source, 'direct_messages_disabled');
	if (singleCommunity == null || directMessagesDisabled == null) {
		return null;
	}
	return {
		single_community: singleCommunity,
		single_community_guild_id: readNonEmptyString(source, 'single_community_guild_id'),
		direct_messages_disabled: directMessagesDisabled,
		guild_create_access: readBoolean(source, 'guild_create_access') ?? true,
	};
}

function parseServices(value: unknown): InstanceServices | null {
	const source = asRecord(value);
	if (source == null) {
		return null;
	}
	const gifEnabled = readBoolean(source, 'gif_enabled');
	const youtubeEnabled = readBoolean(source, 'youtube_enabled');
	const blueskyEnabled = readBoolean(source, 'bluesky_enabled');
	if (gifEnabled == null || youtubeEnabled == null || blueskyEnabled == null) {
		return null;
	}
	return {gif_enabled: gifEnabled, youtube_enabled: youtubeEnabled, bluesky_enabled: blueskyEnabled};
}

function parseLimits(value: unknown): LimitConfigSnapshot | LimitConfigWireFormat | null {
	const source = asRecord(value);
	if (source == null) {
		return null;
	}
	const traitDefinitions = readStringArray(source, 'traitDefinitions');
	const rules = source.rules;
	if (traitDefinitions == null || !Array.isArray(rules) || rules.length > LIMIT_RULES_MAX) {
		return null;
	}
	const wireRules: LimitConfigWireFormat['rules'] = [];
	const snapshotRules: Array<LimitRule> = [];
	for (const entry of rules) {
		const rule = asRecord(entry);
		const id = rule == null ? null : readNonEmptyString(rule, 'id');
		if (rule == null || id == null) {
			return null;
		}
		const filters = parseLimitFilter(rule.filters);
		if (Object.hasOwn(rule, 'filters') && filters == null) {
			return null;
		}
		const overrides = readLimitValues(rule.overrides);
		const limits = readLimitValues(rule.limits);
		if (overrides != null) {
			wireRules.push(filters == null ? {id, overrides} : {id, filters, overrides});
			continue;
		}
		if (limits == null) {
			return null;
		}
		snapshotRules.push(filters == null ? {id, limits} : {id, filters, limits});
	}
	if (wireRules.length > 0 && snapshotRules.length > 0) {
		return null;
	}
	const version = readInteger(source, 'version', 1, Number.MAX_SAFE_INTEGER);
	if (wireRules.length > 0 || (snapshotRules.length === 0 && version === 2)) {
		const defaultsHash = readNonEmptyString(source, 'defaultsHash');
		if (defaultsHash == null) {
			return null;
		}
		return {
			version: 2,
			traitDefinitions,
			rules: wireRules,
			defaultsHash,
		};
	}
	const snapshot: LimitConfigSnapshot = {traitDefinitions, rules: snapshotRules};
	if (version != null) {
		snapshot.version = version;
	}
	return snapshot;
}

function parseLimitFilter(value: unknown): LimitFilter | null {
	const source = asRecord(value);
	if (source == null) {
		return null;
	}
	const filter: LimitFilter = {};
	const traits = readStringArray(source, 'traits');
	if (traits != null) {
		filter.traits = traits;
	}
	const guildFeatures = readStringArray(source, 'guildFeatures');
	if (guildFeatures != null) {
		filter.guildFeatures = guildFeatures;
	}
	return filter;
}

function readLimitValues(value: unknown): Partial<Record<LimitKey, number>> | null {
	const source = asRecord(value);
	if (source == null) {
		return null;
	}
	const values: Record<string, number> = {};
	for (const [key, entry] of Object.entries(source)) {
		if (typeof entry !== 'number' || !Number.isFinite(entry)) {
			return null;
		}
		values[key] = entry;
	}
	return values as Partial<Record<LimitKey, number>>;
}

function parsePush(value: unknown): Readonly<Partial<InstancePush>> {
	const source = asRecord(value);
	if (source == null) {
		return {};
	}
	const push: Partial<InstancePush> = {};
	if (Object.hasOwn(source, 'public_vapid_key')) {
		push.public_vapid_key = readNonEmptyString(source, 'public_vapid_key');
	}
	const deliveryMode = readLiteral(source, 'delivery_mode', PUSH_DELIVERY_MODES);
	if (deliveryMode != null) {
		push.delivery_mode = deliveryMode;
	}
	const eventResolutionPath = readNonEmptyString(source, 'event_resolution_path');
	if (eventResolutionPath != null) {
		push.event_resolution_path = eventResolutionPath;
	}
	if (Object.hasOwn(source, 'external_relay_url')) {
		push.external_relay_url = readAbsoluteHTTPURL(source, 'external_relay_url');
	}
	if (Object.hasOwn(source, 'relay_signing_key_hashes')) {
		push.relay_signing_key_hashes = readStringArray(source, 'relay_signing_key_hashes');
	}
	return push;
}

function parseAppPublic(value: unknown): InstanceDiscoveryAppPublic | null {
	const source = asRecord(value);
	if (source == null) {
		return null;
	}
	return {
		branding: parseBranding(source.branding),
		setup: parseSetup(source.setup),
		legal: parseLegal(source.legal),
		registration: parseAppRegistration(source.registration),
	};
}

function parseBranding(value: unknown): Readonly<Partial<InstanceBranding>> {
	const source = asRecord(value);
	if (source == null) {
		return {};
	}
	const branding: Partial<InstanceBranding> = {};
	const productName = readNonEmptyString(source, 'product_name');
	if (productName != null) {
		branding.product_name = productName;
	}
	const premiumProductName = readNonEmptyString(source, 'premium_product_name');
	if (premiumProductName != null) {
		branding.premium_product_name = premiumProductName;
	}
	for (const key of BRANDING_URL_KEYS) {
		if (Object.hasOwn(source, key)) {
			branding[key] = readAbsoluteHTTPURL(source, key);
		}
	}
	if (Object.hasOwn(source, 'theme_color')) {
		branding.theme_color = readNonEmptyString(source, 'theme_color');
	}
	return branding;
}

function parseSetup(value: unknown): Readonly<Partial<InstanceSetup>> {
	const source = asRecord(value);
	if (source == null) {
		return {};
	}
	const setup: Partial<InstanceSetup> = {};
	const configured = readBoolean(source, 'configured');
	if (configured != null) {
		setup.configured = configured;
	}
	if (Object.hasOwn(source, 'admin_url')) {
		setup.admin_url = readAbsoluteHTTPURL(source, 'admin_url');
	}
	return setup;
}

function parseLegal(value: unknown): Readonly<Partial<InstanceLegal>> {
	const source = asRecord(value);
	if (source == null) {
		return {};
	}
	const legal: Partial<InstanceLegal> = {};
	if (Object.hasOwn(source, 'terms_url')) {
		legal.terms_url = readAbsoluteHTTPURL(source, 'terms_url');
	}
	if (Object.hasOwn(source, 'privacy_url')) {
		legal.privacy_url = readAbsoluteHTTPURL(source, 'privacy_url');
	}
	return legal;
}

function parseAppRegistration(value: unknown): Readonly<Partial<InstanceAppRegistration>> {
	const source = asRecord(value);
	if (source == null) {
		return {};
	}
	const collectDateOfBirth = readBoolean(source, 'collect_date_of_birth');
	return collectDateOfBirth == null ? {} : {collect_date_of_birth: collectDateOfBirth};
}

function parseAgePolicy(value: unknown): InstanceAgePolicy | null {
	const source = asRecord(value);
	if (source == null) {
		return null;
	}
	const entries = source.geos;
	if (!Array.isArray(entries) || entries.length > AGE_POLICY_GEOS_MAX) {
		return null;
	}
	const geos: Array<InstanceAgePolicyGeo> = [];
	for (const entry of entries) {
		const geo = asRecord(entry);
		const countryCode = geo == null ? null : readNonEmptyString(geo, 'country_code');
		if (geo == null || countryCode == null) {
			return null;
		}
		const action = readLiteral(geo, 'action', AGE_POLICY_ACTIONS);
		const cardVerificationAvailable = readBoolean(geo, 'card_verification_available');
		if (action == null || cardVerificationAvailable == null) {
			return null;
		}
		geos.push({
			country_code: countryCode,
			region_code: readNonEmptyString(geo, 'region_code'),
			action,
			card_verification_available: cardVerificationAvailable,
		});
	}
	return {geos};
}

export function parseInstanceDomainMigration(value: unknown): InstanceDomainMigration | null {
	const source = asRecord(value);
	if (source == null) {
		return null;
	}
	const enabled = readBoolean(source, 'enabled');
	const anonymousRolloutBasisPoints = readInteger(
		source,
		'anonymous_rollout_basis_points',
		0,
		DOMAIN_MIGRATION_ROLLOUT_BASIS_POINTS_MAX,
	);
	const rolloutSalt = readDomainMigrationRolloutSalt(source);
	const standaloneForwarding = readBoolean(source, 'standalone_forwarding');
	if (enabled == null || anonymousRolloutBasisPoints == null || rolloutSalt == null || standaloneForwarding == null) {
		return null;
	}
	return {
		enabled,
		anonymous_rollout_basis_points: anonymousRolloutBasisPoints,
		rollout_salt: rolloutSalt,
		standalone_forwarding: standaloneForwarding,
	};
}

function readDomainMigrationRolloutSalt(source: Record<string, unknown>): string | null {
	const raw = source.rollout_salt;
	if (typeof raw !== 'string') {
		return null;
	}
	const value = raw.trim();
	if (
		value.length === 0 ||
		value.length > DOMAIN_MIGRATION_ROLLOUT_SALT_MAX_LENGTH ||
		!DOMAIN_MIGRATION_ROLLOUT_SALT_PATTERN.test(value)
	) {
		return null;
	}
	return value;
}

function asRecord(value: unknown): Record<string, unknown> | null {
	if (typeof value !== 'object' || value == null || Array.isArray(value)) {
		return null;
	}
	return value as Record<string, unknown>;
}

function readBoolean(source: Record<string, unknown>, key: string): boolean | null {
	const value = source[key];
	return typeof value === 'boolean' ? value : null;
}

function readNonEmptyString(source: Record<string, unknown>, key: string): string | null {
	const value = source[key];
	if (typeof value !== 'string') {
		return null;
	}
	return value.length > 0 ? value : null;
}

function readInteger(source: Record<string, unknown>, key: string, minimum: number, maximum: number): number | null {
	const value = source[key];
	if (typeof value !== 'number' || !Number.isSafeInteger(value)) {
		return null;
	}
	return value >= minimum && value <= maximum ? value : null;
}

function readStringArray(source: Record<string, unknown>, key: string): Array<string> | null {
	const value = source[key];
	if (!Array.isArray(value) || value.some((entry) => typeof entry !== 'string')) {
		return null;
	}
	return [...(value as Array<string>)];
}

function readLiteral<TValue extends string>(
	source: Record<string, unknown>,
	key: string,
	allowed: ReadonlyArray<TValue>,
): TValue | null {
	const value = source[key];
	return allowed.find((entry) => entry === value) ?? null;
}

function readAbsoluteHTTPURL(source: Record<string, unknown>, key: string): string | null {
	const raw = readNonEmptyString(source, key);
	if (raw == null) {
		return null;
	}
	const value = raw.trim();
	let url: URL;
	try {
		url = new URL(value);
	} catch {
		return null;
	}
	if (url.protocol !== CanonicalNetworkProtocol.HTTP && url.protocol !== CanonicalNetworkProtocol.HTTPS) {
		return null;
	}
	return value;
}
