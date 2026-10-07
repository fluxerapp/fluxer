// SPDX-License-Identifier: AGPL-3.0-or-later

import {Buffer} from 'node:buffer';
import dns from 'node:dns';
import http from 'node:http';
import https from 'node:https';
import {isIPv4, isIPv6, type LookupFunction} from 'node:net';
import {Readable, Transform} from 'node:stream';
import type {ReadableStream as NodeReadableStream} from 'node:stream/web';
import {createChildLogger} from '@electron/common/Logger';
import {
	type DesktopProxyResolver,
	type DesktopSessionHTTPSender,
	isDirectProxyRoute,
	resolveDesktopSessionProxy,
	sendThroughDesktopSession,
} from '@electron/main/DesktopSessionHTTP';
import {normalizeHTTPNetworkOrigin} from '@fluxer/instance_bootstrap/src/NetworkOrigin';

const logger = createChildLogger('DesktopOutboundHTTP');

const DESKTOP_OUTBOUND_HTTP_MAX_IN_FLIGHT = 32;
const DESKTOP_OUTBOUND_HTTP_MAX_SOCKETS = 16;
const DESKTOP_OUTBOUND_HTTP_RESOLUTION_TIMEOUT_MS = 10_000;
const DESKTOP_OUTBOUND_HTTP_MAX_TARGET_URL_BYTES = 16 * 1024;
const DESKTOP_OUTBOUND_HTTP_MAX_REDIRECT_LOCATION_BYTES = 4096;
const DESKTOP_OUTBOUND_HTTP_MAX_ORIGINS = 2048;
const DESKTOP_OUTBOUND_HTTP_MAX_ORIGIN_REQUESTS_IN_FLIGHT = 128;
const DESKTOP_OUTBOUND_HTTP_MAX_IN_FLIGHT_PER_SERVICE = 64;
const DESKTOP_OUTBOUND_HTTP_MAX_PENDING_RESOLUTIONS = 64;
const DESKTOP_OUTBOUND_HTTP_MAX_REQUEST_BODY_BYTES = 1024 * 1024;
const DESKTOP_OUTBOUND_HTTP_STALE_SOCKET_ATTEMPTS = 1;

const DESKTOP_OUTBOUND_HTTP_UNREACHABLE_ADDRESS_CODES: ReadonlySet<string> = new Set([
	'ECONNREFUSED',
	'EHOSTUNREACH',
	'ENETUNREACH',
	'ETIMEDOUT',
]);
const DESKTOP_OUTBOUND_HTTP_STALE_SOCKET_CODES: ReadonlySet<string> = new Set(['ECONNRESET', 'EPIPE']);
const DESKTOP_OUTBOUND_HTTP_IDEMPOTENT_METHODS: ReadonlySet<string> = new Set([
	'DELETE',
	'GET',
	'HEAD',
	'OPTIONS',
	'PUT',
	'TRACE',
]);

const DESKTOP_OUTBOUND_HTTP_BLOCKED_MESSAGE = 'The requested address could not be reached';
const DESKTOP_OUTBOUND_HTTP_TRANSPORT_MESSAGE = 'The request could not be completed';
const DESKTOP_OUTBOUND_HTTP_TIMEOUT_MESSAGE = 'The request timed out';
const DESKTOP_OUTBOUND_HTTP_CAPACITY_MESSAGE = 'Too many downloads are already in progress';

const IPV6_GROUP_COUNT = 8;

const DesktopOutboundBlockReason = Object.freeze({
	INSECURE_TRANSPORT: 'insecure-transport',
	INVALID_TARGET: 'invalid-target',
	LOOKUP_HOSTNAME_MISMATCH: 'lookup-hostname-mismatch',
	NON_PUBLIC_ADDRESS: 'non-public-address',
	NO_USABLE_ADDRESS: 'no-usable-address',
	RESOLUTION_FAILED: 'resolution-failed',
	RESOLUTION_TIMEOUT: 'resolution-timeout',
} as const);

type DesktopOutboundBlockReason = (typeof DesktopOutboundBlockReason)[keyof typeof DesktopOutboundBlockReason];

export const DesktopAddressRequirement = Object.freeze({
	ANY: 'any',
	PUBLIC: 'public',
} as const);

export type DesktopAddressRequirement = (typeof DesktopAddressRequirement)[keyof typeof DesktopAddressRequirement];

const DesktopOriginAddressScope = Object.freeze({
	NON_PUBLIC: 'non-public',
	PUBLIC: 'public',
} as const);

type DesktopOriginAddressScope = (typeof DesktopOriginAddressScope)[keyof typeof DesktopOriginAddressScope];

export const DesktopOriginTrust = Object.freeze({
	BOUND: 'bound',
	REGISTERED: 'registered',
} as const);

export type DesktopOriginTrust = (typeof DesktopOriginTrust)[keyof typeof DesktopOriginTrust];

interface PinnedAddress {
	readonly address: string;
	readonly family: 4 | 6;
}

type DesktopHostAddressResolver = (hostname: string) => Promise<ReadonlyArray<string>>;

interface DesktopOriginAddressBinding {
	readonly address: PinnedAddress;
	readonly origin: string;
	readonly scope: DesktopOriginAddressScope;
	readonly unreachable: boolean;
}

interface DesktopPendingOriginBinding {
	readonly operation: Promise<DesktopOriginAddressBinding>;
	requirement: DesktopAddressRequirement;
}

export interface DesktopOutboundHTTPRequest {
	readonly body: Uint8Array | ReadableStream<Uint8Array> | null;
	readonly expectedOrigin: string;
	readonly headers: Readonly<Record<string, string>> | null;
	readonly maximumRequestBodyBytes?: number;
	readonly method: string;
	readonly originTrust: DesktopOriginTrust;
	readonly serviceName: string;
	readonly signal: AbortSignal | null;
	readonly timeoutMs: number;
	readonly url: string;
}

interface DesktopAnchoredOriginRegistration {
	readonly anchorOrigin: string;
	readonly origins: ReadonlyArray<string>;
	readonly unresolvedAnchorRequirement?: DesktopAddressRequirement | null;
}

interface DesktopOutboundHTTPOptions {
	readonly resolveHostAddresses?: DesktopHostAddressResolver;
	readonly resolveProxy?: DesktopProxyResolver;
	readonly sendThroughSession?: DesktopSessionHTTPSender;
}

interface DesktopOutboundGETRequest {
	readonly context: string;
	readonly timeoutMs: number;
	readonly url: URL;
}

export interface DesktopOutboundHTTPMessage {
	readonly headers: http.IncomingHttpHeaders;
	readonly message: Readable;
	readonly status: number;
	readonly statusText: string;
	readonly url: URL;
}

interface BoundedMessageRead {
	readonly declaredBytes: number | null;
	readonly description: string;
	readonly maxBytes: number;
	readonly maxChunks: number;
	readonly message: Readable;
}

class DesktopOutboundHTTPBlockedError extends Error {
	public constructor() {
		super(DESKTOP_OUTBOUND_HTTP_BLOCKED_MESSAGE);
		this.name = 'DesktopOutboundHTTPBlockedError';
	}
}

class DesktopOutboundHTTPTransportError extends Error {
	public constructor() {
		super(DESKTOP_OUTBOUND_HTTP_TRANSPORT_MESSAGE);
		this.name = 'DesktopOutboundHTTPTransportError';
	}
}

class DesktopOutboundHTTPTimeoutError extends Error {
	public constructor() {
		super(DESKTOP_OUTBOUND_HTTP_TIMEOUT_MESSAGE);
		this.name = 'DesktopOutboundHTTPTimeoutError';
	}
}

class DesktopOutboundHTTPCapacityError extends Error {
	public constructor(limit?: string) {
		super(limit == null ? DESKTOP_OUTBOUND_HTTP_CAPACITY_MESSAGE : `Desktop outbound HTTP exceeded its ${limit} limit`);
		this.name = 'DesktopOutboundHTTPCapacityError';
	}
}

class DesktopOutboundHTTPClosedError extends Error {
	public constructor() {
		super('Desktop outbound HTTP is closed');
		this.name = 'DesktopOutboundHTTPClosedError';
	}
}

class DesktopOutboundHTTPInvalidOriginError extends TypeError {
	public constructor(origin: string) {
		super(`Desktop outbound HTTP origin is not a canonical http(s) origin: ${origin}`);
		this.name = 'DesktopOutboundHTTPInvalidOriginError';
	}
}

class DesktopOutboundHTTPInvalidTargetError extends TypeError {
	public constructor(url: string) {
		super(`Desktop outbound HTTP target is not a canonical URL under its declared origin: ${url}`);
		this.name = 'DesktopOutboundHTTPInvalidTargetError';
	}
}

class DesktopOutboundHTTPInsecureTransportError extends Error {
	public constructor(origin: string) {
		super(`Desktop outbound HTTP requires https for the publicly routable origin ${origin}`);
		this.name = 'DesktopOutboundHTTPInsecureTransportError';
	}
}

class DesktopOutboundHTTPPublicAddressRequiredError extends Error {
	public constructor(origin: string) {
		super(`A publicly reachable instance cannot authorize the privately resolving origin ${origin}`);
		this.name = 'DesktopOutboundHTTPPublicAddressRequiredError';
	}
}

class DesktopOutboundHTTPOriginNotRegisteredError extends Error {
	public constructor(origin: string) {
		super(`Desktop outbound HTTP origin is not registered to a discovered instance: ${origin}`);
		this.name = 'DesktopOutboundHTTPOriginNotRegisteredError';
	}
}

class DesktopOutboundHTTPMixedAddressScopeError extends Error {
	public constructor(origin: string) {
		super(`Desktop outbound HTTP origin ${origin} resolved to both public and non-public addresses`);
		this.name = 'DesktopOutboundHTTPMixedAddressScopeError';
	}
}

class DesktopOutboundHTTPEmptyResolutionError extends Error {
	public constructor(origin: string) {
		super(`Desktop outbound HTTP origin ${origin} resolved to no usable address`);
		this.name = 'DesktopOutboundHTTPEmptyResolutionError';
	}
}

class DesktopOutboundHTTPResolutionTimeoutError extends Error {
	public constructor(hostname: string) {
		super(`Desktop outbound HTTP resolution of ${hostname} exceeded ${DESKTOP_OUTBOUND_HTTP_RESOLUTION_TIMEOUT_MS} ms`);
		this.name = 'DesktopOutboundHTTPResolutionTimeoutError';
	}
}

class DesktopOutboundHTTPRequestTimeoutError extends Error {
	public constructor(url: string, timeoutMs: number) {
		super(`Desktop outbound HTTP request to ${url} timed out after ${timeoutMs} ms`);
		this.name = 'DesktopOutboundHTTPRequestTimeoutError';
	}
}

class DesktopOutboundHTTPRequestAbortedError extends Error {
	public constructor(url: string) {
		super(`Desktop outbound HTTP request to ${url} was aborted by its caller`);
		this.name = 'DesktopOutboundHTTPRequestAbortedError';
	}
}

class DesktopOutboundHTTPRequestBodyLimitError extends Error {
	public constructor(maximumBytes: number) {
		super(`Desktop outbound HTTP request body exceeds ${maximumBytes} bytes`);
		this.name = 'DesktopOutboundHTTPRequestBodyLimitError';
	}
}

class BoundedMessageByteLimitError extends RangeError {
	public constructor(description: string, maxBytes: number) {
		super(`${description} exceeds ${maxBytes} bytes`);
		this.name = 'BoundedMessageByteLimitError';
	}
}

class BoundedMessageChunkLimitError extends RangeError {
	public constructor(description: string, maxChunks: number) {
		super(`${description} exceeds ${maxChunks} response chunks`);
		this.name = 'BoundedMessageChunkLimitError';
	}
}

class InvalidContentLengthError extends TypeError {
	public constructor(description: string) {
		super(`${description} has an invalid Content-Length header`);
		this.name = 'InvalidContentLengthError';
	}
}

class PinnedLookupHostnameMismatchError extends Error {
	public constructor() {
		super(DESKTOP_OUTBOUND_HTTP_BLOCKED_MESSAGE);
		this.name = 'PinnedLookupHostnameMismatchError';
	}
}

function blocked(
	reason: DesktopOutboundBlockReason,
	context: string,
	hostname: string,
): DesktopOutboundHTTPBlockedError {
	logger.warn('Blocked outbound request', {context, hostname, reason});
	return new DesktopOutboundHTTPBlockedError();
}

function stripIPv6ZoneIdentifier(value: string): string {
	const zoneIndex = value.indexOf('%');
	if (zoneIndex === -1) {
		return value;
	}
	const addressPart = value.slice(0, zoneIndex);
	return addressPart.includes(':') ? addressPart : value;
}

function normalizeIPv6(value: string): string {
	try {
		const hostname = new URL(`http://[${value}]`).hostname;
		return hostname.startsWith('[') && hostname.endsWith(']') ? hostname.slice(1, -1) : hostname;
	} catch {
		return value;
	}
}

function parseIPAddress(value: string): PinnedAddress | null {
	const trimmed = value.trim();
	const unbracketed = trimmed.startsWith('[') && trimmed.endsWith(']') ? trimmed.slice(1, -1) : trimmed;
	const unzoned = stripIPv6ZoneIdentifier(unbracketed);
	if (isIPv4(unzoned)) {
		return {address: unzoned, family: 4};
	}
	if (isIPv6(unzoned)) {
		return {address: normalizeIPv6(unzoned), family: 6};
	}
	return null;
}

function parseIPv4Octets(address: string): Array<number> | null {
	const parts = address.split('.');
	if (parts.length !== 4) {
		return null;
	}
	const octets = parts.map((part) => (/^\d{1,3}$/u.test(part) ? Number.parseInt(part, 10) : Number.NaN));
	if (octets.some((octet) => !Number.isInteger(octet) || octet < 0 || octet > 255)) {
		return null;
	}
	return octets;
}

function ipv4Value(octets: ReadonlyArray<number>): number {
	return ((octets[0] << 24) | (octets[1] << 16) | (octets[2] << 8) | octets[3]) >>> 0;
}

function isIPv4InCIDR(value: number, base: number, prefixLength: number): boolean {
	const mask = prefixLength === 0 ? 0 : (0xffffffff << (32 - prefixLength)) >>> 0;
	return (value & mask) >>> 0 === (base & mask) >>> 0;
}

const IPV4_NON_PUBLIC_RANGES: ReadonlyArray<readonly [base: number, prefixLength: number]> = Object.freeze([
	[0x00000000, 8],
	[0x0a000000, 8],
	[0x64400000, 10],
	[0x7f000000, 8],
	[0xa9fe0000, 16],
	[0xac100000, 12],
	[0xc0000000, 24],
	[0xc0000200, 24],
	[0xc0a80000, 16],
	[0xc6120000, 15],
	[0xc6336400, 24],
	[0xcb007100, 24],
	[0xe0000000, 4],
	[0xf0000000, 4],
]);

function isPublicIPv4Address(address: string): boolean {
	const octets = parseIPv4Octets(address);
	if (octets == null) {
		return false;
	}
	const value = ipv4Value(octets);
	return !IPV4_NON_PUBLIC_RANGES.some(([base, prefixLength]) => isIPv4InCIDR(value, base, prefixLength));
}

function expandIPv6Groups(address: string): Array<string> {
	const halves = address.split('::');
	if (halves.length === 2) {
		const left = halves[0].length > 0 ? halves[0].split(':') : [];
		const right = halves[1].length > 0 ? halves[1].split(':') : [];
		const missing = Math.max(IPV6_GROUP_COUNT - left.length - right.length, 0);
		return [...left, ...Array<string>(missing).fill('0'), ...right].map((group) => group.padStart(4, '0'));
	}
	return address.split(':').map((group) => group.padStart(4, '0'));
}

function ipv4FromMappedIPv6(groups: ReadonlyArray<string>): string | null {
	const isMapped =
		groups[0] === '0000' &&
		groups[1] === '0000' &&
		groups[2] === '0000' &&
		groups[3] === '0000' &&
		groups[4] === '0000' &&
		groups[5] === 'ffff';
	if (!isMapped) {
		return null;
	}
	const high = Number.parseInt(groups[6], 16);
	const low = Number.parseInt(groups[7], 16);
	return `${(high >> 8) & 0xff}.${high & 0xff}.${(low >> 8) & 0xff}.${low & 0xff}`;
}

function isPublicIPv6Address(address: string): boolean {
	const groups = expandIPv6Groups(address);
	if (groups.length !== IPV6_GROUP_COUNT) {
		return false;
	}
	const mapped = ipv4FromMappedIPv6(groups);
	if (mapped != null) {
		return isPublicIPv4Address(mapped);
	}
	const first = Number.parseInt(groups[0], 16);
	const second = Number.parseInt(groups[1], 16);
	const last = Number.parseInt(groups[7], 16);
	if (groups.slice(0, 7).every((group) => group === '0000') && (last === 0 || last === 1)) {
		return false;
	}
	if ((first & 0xe000) !== 0x2000) {
		return false;
	}
	if ((first & 0xffc0) === 0xfe80) {
		return false;
	}
	if ((first & 0xfe00) === 0xfc00) {
		return false;
	}
	if ((first & 0xff00) === 0xff00) {
		return false;
	}
	return !(first === 0x2001 && second === 0x0db8);
}

function isPublicPinnedAddress(pinned: PinnedAddress): boolean {
	return pinned.family === 4 ? isPublicIPv4Address(pinned.address) : isPublicIPv6Address(pinned.address);
}

function proxiedHostScope(hostname: string): DesktopOriginAddressScope {
	const literal = parseIPAddress(hostname);
	return literal == null ? DesktopOriginAddressScope.PUBLIC : addressScope(literal);
}

export function requireDesktopHTTPOrigin(value: string): string {
	const normalized = normalizeHTTPNetworkOrigin(value);
	if (normalized == null) {
		throw new DesktopOutboundHTTPInvalidOriginError(value);
	}
	return new URL(normalized).origin;
}

export function parseDesktopHTTPTarget(value: string): URL | null {
	if (Buffer.byteLength(value, 'utf8') > DESKTOP_OUTBOUND_HTTP_MAX_TARGET_URL_BYTES) {
		return null;
	}
	let url: URL;
	try {
		url = new URL(value);
	} catch {
		return null;
	}
	if (url.protocol !== 'http:' && url.protocol !== 'https:') {
		return null;
	}
	if (url.username.length > 0 || url.password.length > 0 || url.hostname.length === 0 || url.port === '0') {
		return null;
	}
	return url;
}

export function parseDesktopRedirectTarget(base: URL, location: string | Array<string> | undefined): URL | null {
	if (typeof location !== 'string' || location.length === 0) {
		return null;
	}
	if (Buffer.byteLength(location, 'utf8') > DESKTOP_OUTBOUND_HTTP_MAX_REDIRECT_LOCATION_BYTES) {
		return null;
	}
	let resolved: string;
	try {
		resolved = new URL(location, base).toString();
	} catch {
		return null;
	}
	return parseDesktopHTTPTarget(resolved);
}

function requireDesktopHTTPTarget(value: string, expectedOrigin: string): URL {
	const url = parseDesktopHTTPTarget(value);
	if (url == null || url.origin !== expectedOrigin) {
		throw new DesktopOutboundHTTPInvalidTargetError(value);
	}
	return url;
}

function normalizeLookupHostname(hostname: string): string {
	let value = hostname.trim().toLowerCase();
	if (value.startsWith('[') && value.endsWith(']')) {
		value = value.slice(1, -1);
	}
	if (value.length > 1 && value.endsWith('.')) {
		value = value.slice(0, -1);
	}
	return value;
}

function absoluteLookupHostname(hostname: string): string {
	if (hostname.includes('.') && !hostname.endsWith('.')) {
		return `${hostname}.`;
	}
	return hostname;
}

export function createPinnedHostLookup(hostname: string, pinned: PinnedAddress): LookupFunction {
	const expected = normalizeLookupHostname(hostname);
	return (requestedHostname, options, callback) => {
		process.nextTick(() => {
			if (normalizeLookupHostname(requestedHostname) !== expected) {
				callback(new PinnedLookupHostnameMismatchError(), '', undefined);
				return;
			}
			if (options.all === true) {
				(callback as unknown as (error: null, addresses: ReadonlyArray<PinnedAddress>) => void)(null, [pinned]);
				return;
			}
			callback(null, pinned.address, pinned.family);
		});
	};
}

function parseContentLengthHeader(value: string | undefined, description: string): number | null {
	if (value == null) {
		return null;
	}
	const normalized = value.trim();
	if (!/^\d+$/u.test(normalized)) {
		throw new InvalidContentLengthError(description);
	}
	const parsed = Number.parseInt(normalized, 10);
	if (!Number.isSafeInteger(parsed)) {
		throw new InvalidContentLengthError(description);
	}
	return parsed;
}

export function readMessageContentLength(message: DesktopOutboundHTTPMessage, description: string): number | null {
	const raw = message.headers['content-length'];
	if (Array.isArray(raw)) {
		throw new InvalidContentLengthError(description);
	}
	return parseContentLengthHeader(raw, description);
}

export async function readBoundedMessage({
	declaredBytes,
	description,
	maxBytes,
	maxChunks,
	message,
}: BoundedMessageRead): Promise<Buffer> {
	if (declaredBytes != null && declaredBytes > maxBytes) {
		message.destroy();
		throw new BoundedMessageByteLimitError(description, maxBytes);
	}
	const chunks: Array<Buffer> = [];
	let totalBytes = 0;
	let chunkCount = 0;
	try {
		for await (const chunk of message) {
			const buffer = Buffer.isBuffer(chunk) ? chunk : Buffer.from(chunk as Uint8Array);
			chunkCount += 1;
			if (chunkCount > maxChunks) {
				throw new BoundedMessageChunkLimitError(description, maxChunks);
			}
			totalBytes += buffer.byteLength;
			if (totalBytes > maxBytes) {
				throw new BoundedMessageByteLimitError(description, maxBytes);
			}
			chunks.push(buffer);
		}
	} catch (error) {
		message.destroy();
		throw error;
	}
	return Buffer.concat(chunks, totalBytes);
}

async function lookupAllAddresses(hostname: string): Promise<ReadonlyArray<string>> {
	const records = await dns.promises.lookup(hostname, {all: true, order: 'verbatim'});
	return records.map((record) => record.address);
}

function selectPinnedAddress(
	candidates: ReadonlyArray<PinnedAddress>,
	scope: DesktopOriginAddressScope | null,
): PinnedAddress | null {
	if (candidates.length === 0) {
		return null;
	}
	if (scope === DesktopOriginAddressScope.NON_PUBLIC) {
		return candidates.find((candidate) => candidate.family === 4) ?? candidates[0];
	}
	return candidates[0];
}

function addressScope(address: PinnedAddress): DesktopOriginAddressScope {
	return isPublicPinnedAddress(address) ? DesktopOriginAddressScope.PUBLIC : DesktopOriginAddressScope.NON_PUBLIC;
}

function strictestRequirement(
	left: DesktopAddressRequirement,
	right: DesktopAddressRequirement,
): DesktopAddressRequirement {
	if (left === DesktopAddressRequirement.PUBLIC || right === DesktopAddressRequirement.PUBLIC) {
		return DesktopAddressRequirement.PUBLIC;
	}
	return DesktopAddressRequirement.ANY;
}

function requireScope(binding: DesktopOriginAddressBinding, requirement: DesktopAddressRequirement): void {
	if (requirement === DesktopAddressRequirement.PUBLIC && binding.scope !== DesktopOriginAddressScope.PUBLIC) {
		throw new DesktopOutboundHTTPPublicAddressRequiredError(binding.origin);
	}
}

function requireStableScope(
	previous: DesktopOriginAddressBinding | undefined,
	next: DesktopOriginAddressBinding,
): void {
	if (previous != null && previous.scope !== next.scope) {
		throw new DesktopOutboundHTTPMixedAddressScopeError(next.origin);
	}
}

function isStaleKeepAliveSocketError(error: unknown): boolean {
	if (error == null || typeof error !== 'object') {
		return false;
	}
	const code = (error as NodeJS.ErrnoException).code;
	return code != null && DESKTOP_OUTBOUND_HTTP_STALE_SOCKET_CODES.has(code);
}

function isReplayableRequest(request: DesktopOutboundHTTPRequest): boolean {
	if (!DESKTOP_OUTBOUND_HTTP_IDEMPOTENT_METHODS.has(request.method.toUpperCase())) {
		return false;
	}
	return request.body == null || request.body instanceof Uint8Array;
}

export function isDesktopHostResolutionFailure(error: unknown): boolean {
	if (error == null || typeof error !== 'object') {
		return false;
	}
	if (error instanceof DesktopOutboundHTTPResolutionTimeoutError) {
		return true;
	}
	return (error as NodeJS.ErrnoException).syscall === 'getaddrinfo';
}

function isUnreachableAddressError(error: unknown): boolean {
	if (error == null || typeof error !== 'object') {
		return false;
	}
	const code = (error as NodeJS.ErrnoException).code;
	if (code != null && DESKTOP_OUTBOUND_HTTP_UNREACHABLE_ADDRESS_CODES.has(code)) {
		return true;
	}
	const causes = (error as AggregateError).errors;
	return Array.isArray(causes) && causes.some((cause) => isUnreachableAddressError(cause));
}

function requestHeaders(headers: Readonly<Record<string, string>> | null): Record<string, string> {
	const normalized: Record<string, string> = {};
	if (headers == null) {
		return normalized;
	}
	for (const [name, value] of Object.entries(headers)) {
		if (name.toLowerCase() === 'host') {
			continue;
		}
		normalized[name] = value;
	}
	return normalized;
}

function createRequestBodyLimit(maximumBytes: number): Transform {
	let totalBytes = 0;
	return new Transform({
		transform(chunk: Buffer, _encoding, callback) {
			totalBytes += chunk.byteLength;
			if (totalBytes > maximumBytes) {
				callback(new DesktopOutboundHTTPRequestBodyLimitError(maximumBytes));
				return;
			}
			callback(null, chunk);
		},
	});
}

interface DesktopProxiedSend {
	readonly body: Uint8Array | Readable | null;
	readonly headers: Readonly<Record<string, string>>;
	readonly method: string;
	readonly release: () => void;
	readonly signal: AbortSignal | null;
	readonly target: URL;
	readonly timeout: () => Error;
	readonly timeoutMs: number;
}

export class DesktopOutboundHTTP {
	private readonly httpAgent = new http.Agent({keepAlive: true, maxSockets: DESKTOP_OUTBOUND_HTTP_MAX_SOCKETS});
	private readonly httpsAgent = new https.Agent({keepAlive: true, maxSockets: DESKTOP_OUTBOUND_HTTP_MAX_SOCKETS});
	private readonly originRequestHttpAgent = new http.Agent({
		keepAlive: true,
		maxSockets: DESKTOP_OUTBOUND_HTTP_MAX_IN_FLIGHT_PER_SERVICE,
	});
	private readonly originRequestHttpsAgent = new https.Agent({
		keepAlive: true,
		maxSockets: DESKTOP_OUTBOUND_HTTP_MAX_IN_FLIGHT_PER_SERVICE,
	});
	private readonly bindings = new Map<string, DesktopOriginAddressBinding>();
	private readonly pendingBindings = new Map<string, DesktopPendingOriginBinding>();
	private readonly registeredRequirements = new Map<string, DesktopAddressRequirement>();
	private readonly originRequestsInFlightByService = new Map<string, number>();
	private readonly resolveHostAddresses: DesktopHostAddressResolver;
	private readonly resolveProxy: DesktopProxyResolver;
	private readonly sendThroughSession: DesktopSessionHTTPSender;
	private inFlight = 0;
	private originRequestsInFlight = 0;
	private activeResolutions = 0;
	private acceptingRequests = true;

	public constructor(options: DesktopOutboundHTTPOptions = {}) {
		this.resolveHostAddresses = options.resolveHostAddresses ?? lookupAllAddresses;
		this.resolveProxy = options.resolveProxy ?? resolveDesktopSessionProxy;
		this.sendThroughSession = options.sendThroughSession ?? sendThroughDesktopSession;
	}

	public async get(request: DesktopOutboundGETRequest): Promise<DesktopOutboundHTTPMessage> {
		if (this.inFlight >= DESKTOP_OUTBOUND_HTTP_MAX_IN_FLIGHT) {
			throw new DesktopOutboundHTTPCapacityError();
		}
		this.inFlight += 1;
		let released = false;
		const release = (): void => {
			if (released) {
				return;
			}
			released = true;
			this.inFlight -= 1;
		};
		try {
			const requirement = this.registeredRequirements.get(request.url.origin) ?? DesktopAddressRequirement.PUBLIC;
			if (requirement === DesktopAddressRequirement.PUBLIC && request.url.protocol !== 'https:') {
				throw blocked(DesktopOutboundBlockReason.INSECURE_TRANSPORT, request.context, request.url.hostname);
			}
			if (!(await this.routesDirect(request.url))) {
				if (
					requirement === DesktopAddressRequirement.PUBLIC &&
					proxiedHostScope(request.url.hostname) !== DesktopOriginAddressScope.PUBLIC
				) {
					throw blocked(DesktopOutboundBlockReason.NON_PUBLIC_ADDRESS, request.context, request.url.hostname);
				}
				return await this.issueProxied(request, release);
			}
			const pinned = await this.pinAddress(request.url.hostname, requirement, request.context);
			return await this.issue(request, pinned, release);
		} catch (error) {
			release();
			throw error;
		}
	}

	public async registerAnchoredOrigins({
		anchorOrigin,
		origins,
		unresolvedAnchorRequirement = null,
	}: DesktopAnchoredOriginRegistration): Promise<DesktopAddressRequirement> {
		this.requireAdmission();
		const canonicalAnchor = requireDesktopHTTPOrigin(anchorOrigin);
		const requirement = await this.anchorRequirement(canonicalAnchor, unresolvedAnchorRequirement);
		const canonicalOrigins = new Set<string>([canonicalAnchor]);
		for (const origin of origins) {
			canonicalOrigins.add(requireDesktopHTTPOrigin(origin));
		}
		let additions = 0;
		for (const origin of canonicalOrigins) {
			if (!this.registeredRequirements.has(origin)) {
				additions += 1;
			}
			const binding = this.bindings.get(origin);
			if (binding != null) {
				requireScope(binding, requirement);
			}
		}
		if (this.registeredRequirements.size + additions > DESKTOP_OUTBOUND_HTTP_MAX_ORIGINS) {
			throw new DesktopOutboundHTTPCapacityError('registered origin');
		}
		for (const origin of canonicalOrigins) {
			const current = this.registeredRequirements.get(origin);
			const next = current == null ? requirement : strictestRequirement(current, requirement);
			this.registeredRequirements.set(origin, next);
			const pending = this.pendingBindings.get(origin);
			if (pending != null) {
				pending.requirement = strictestRequirement(pending.requirement, next);
			}
		}
		return requirement;
	}

	private async anchorRequirement(
		canonicalAnchor: string,
		unresolvedAnchorRequirement: DesktopAddressRequirement | null,
	): Promise<DesktopAddressRequirement> {
		if (!(await this.routesDirect(new URL(canonicalAnchor)))) {
			return proxiedHostScope(new URL(canonicalAnchor).hostname) === DesktopOriginAddressScope.PUBLIC
				? DesktopAddressRequirement.PUBLIC
				: DesktopAddressRequirement.ANY;
		}
		let anchor: DesktopOriginAddressBinding;
		try {
			anchor = await this.ensureBinding(canonicalAnchor, DesktopAddressRequirement.ANY);
		} catch (error) {
			if (unresolvedAnchorRequirement === null || !isDesktopHostResolutionFailure(error)) {
				throw error;
			}
			return unresolvedAnchorRequirement;
		}
		return anchor.scope === DesktopOriginAddressScope.PUBLIC
			? DesktopAddressRequirement.PUBLIC
			: DesktopAddressRequirement.ANY;
	}

	public isRegisteredOrigin(origin: string): boolean {
		return this.registeredRequirements.has(requireDesktopHTTPOrigin(origin));
	}

	public registeredCleartextOrigins(): Array<string> {
		const origins: Array<string> = [];
		for (const [origin, requirement] of this.registeredRequirements) {
			if (requirement === DesktopAddressRequirement.ANY && origin.startsWith('http://')) {
				origins.push(origin);
			}
		}
		return origins.sort();
	}

	public requireRegisteredOrigins(origins: ReadonlyArray<string>): void {
		for (const origin of origins) {
			const canonical = requireDesktopHTTPOrigin(origin);
			if (!this.registeredRequirements.has(canonical)) {
				throw new DesktopOutboundHTTPOriginNotRegisteredError(canonical);
			}
		}
	}

	public async request(request: DesktopOutboundHTTPRequest): Promise<DesktopOutboundHTTPMessage> {
		this.requireAdmission();
		const origin = requireDesktopHTTPOrigin(request.expectedOrigin);
		const target = requireDesktopHTTPTarget(request.url, origin);
		const maximumRequestBodyBytes = request.maximumRequestBodyBytes ?? DESKTOP_OUTBOUND_HTTP_MAX_REQUEST_BODY_BYTES;
		if (request.body instanceof Uint8Array && request.body.byteLength > maximumRequestBodyBytes) {
			throw new DesktopOutboundHTTPRequestBodyLimitError(maximumRequestBodyBytes);
		}
		const requirement = this.requirementFor(origin, request.originTrust);
		if (!(await this.routesDirect(target))) {
			const scope = proxiedHostScope(target.hostname);
			if (requirement === DesktopAddressRequirement.PUBLIC && scope !== DesktopOriginAddressScope.PUBLIC) {
				throw new DesktopOutboundHTTPPublicAddressRequiredError(origin);
			}
			if (scope === DesktopOriginAddressScope.PUBLIC && target.protocol !== 'https:') {
				throw new DesktopOutboundHTTPInsecureTransportError(origin);
			}
			this.requireAdmission();
			return await this.issueProxiedOriginRequest(request, target);
		}
		const binding = await this.ensureBinding(origin, requirement);
		if (binding.scope === DesktopOriginAddressScope.PUBLIC && target.protocol !== 'https:') {
			throw new DesktopOutboundHTTPInsecureTransportError(origin);
		}
		this.requireAdmission();
		return await this.issueOriginRequest(request, target, binding);
	}

	public async requireCleartextTransportAddress(origin: string): Promise<string> {
		this.requireAdmission();
		const canonical = requireDesktopHTTPOrigin(origin);
		const binding = await this.ensureBinding(
			canonical,
			this.registeredRequirements.get(canonical) ?? DesktopAddressRequirement.ANY,
		);
		if (binding.scope === DesktopOriginAddressScope.PUBLIC) {
			throw new DesktopOutboundHTTPInsecureTransportError(canonical);
		}
		return binding.address.address;
	}

	public cleanup(): void {
		if (!this.acceptingRequests) {
			return;
		}
		this.acceptingRequests = false;
		this.httpAgent.destroy();
		this.httpsAgent.destroy();
		this.originRequestHttpAgent.destroy();
		this.originRequestHttpsAgent.destroy();
		this.bindings.clear();
		this.registeredRequirements.clear();
		this.originRequestsInFlightByService.clear();
	}

	private async routesDirect(url: URL): Promise<boolean> {
		return isDirectProxyRoute(await this.resolveProxy(url.href));
	}

	private issueProxied(request: DesktopOutboundGETRequest, release: () => void): Promise<DesktopOutboundHTTPMessage> {
		return this.sendProxied({
			body: null,
			headers: {},
			method: 'GET',
			signal: null,
			target: request.url,
			timeout: () => new DesktopOutboundHTTPTimeoutError(),
			timeoutMs: request.timeoutMs,
			release,
		}).catch((error: unknown) => {
			if (error instanceof DesktopOutboundHTTPTimeoutError) {
				throw error;
			}
			logger.warn('Outbound request failed', {context: request.context, hostname: request.url.hostname, error});
			throw new DesktopOutboundHTTPTransportError();
		});
	}

	private issueProxiedOriginRequest(
		request: DesktopOutboundHTTPRequest,
		target: URL,
	): Promise<DesktopOutboundHTTPMessage> {
		this.acquireSlot(request.serviceName);
		let released = false;
		const release = (): void => {
			if (released) {
				return;
			}
			released = true;
			this.releaseSlot(request.serviceName);
		};
		return this.sendProxied({
			body: this.proxiedRequestBody(request),
			headers: requestHeaders(request.headers),
			method: request.method,
			signal: request.signal,
			target,
			timeout: () => new DesktopOutboundHTTPRequestTimeoutError(target.toString(), request.timeoutMs),
			timeoutMs: request.timeoutMs,
			release,
		}).catch((error: unknown) => {
			if (request.signal?.aborted === true) {
				throw new DesktopOutboundHTTPRequestAbortedError(target.toString());
			}
			throw error;
		});
	}

	private async sendProxied({
		body,
		headers,
		method,
		signal,
		target,
		timeout,
		timeoutMs,
		release,
	}: DesktopProxiedSend): Promise<DesktopOutboundHTTPMessage> {
		const deadline = new AbortController();
		const timer = setTimeout(() => deadline.abort(timeout()), timeoutMs);
		timer.unref();
		const settle = (): void => {
			clearTimeout(timer);
			release();
		};
		try {
			const response = await this.sendThroughSession({
				body,
				headers,
				method,
				signal: signal == null ? deadline.signal : AbortSignal.any([deadline.signal, signal]),
				url: target,
			});
			response.message.on('end', settle);
			response.message.on('close', settle);
			response.message.on('error', settle);
			return {...response, url: target};
		} catch (error) {
			settle();
			throw error;
		}
	}

	private proxiedRequestBody(request: DesktopOutboundHTTPRequest): Uint8Array | Readable | null {
		const body = request.body;
		if (body == null || body instanceof Uint8Array) {
			return body;
		}
		const limit = createRequestBodyLimit(
			request.maximumRequestBodyBytes ?? DESKTOP_OUTBOUND_HTTP_MAX_REQUEST_BODY_BYTES,
		);
		const source = Readable.fromWeb(body as unknown as NodeReadableStream<Uint8Array>);
		source.on('error', (error: Error) => limit.destroy(error));
		return source.pipe(limit);
	}

	private async pinAddress(
		hostname: string,
		requirement: DesktopAddressRequirement,
		context: string,
	): Promise<PinnedAddress> {
		const literal = parseIPAddress(hostname);
		if (literal != null) {
			if (requirement === DesktopAddressRequirement.PUBLIC && !isPublicPinnedAddress(literal)) {
				throw blocked(DesktopOutboundBlockReason.NON_PUBLIC_ADDRESS, context, hostname);
			}
			return literal;
		}
		let addresses: ReadonlyArray<string>;
		try {
			addresses = await this.resolveHost(hostname);
		} catch (error) {
			const reason =
				error instanceof DesktopOutboundHTTPTimeoutError
					? DesktopOutboundBlockReason.RESOLUTION_TIMEOUT
					: DesktopOutboundBlockReason.RESOLUTION_FAILED;
			throw blocked(reason, context, hostname);
		}
		const candidates = addresses.map((address) => parseIPAddress(address)).filter((value) => value != null);
		if (candidates.length === 0) {
			throw blocked(DesktopOutboundBlockReason.NO_USABLE_ADDRESS, context, hostname);
		}
		if (
			requirement === DesktopAddressRequirement.PUBLIC &&
			candidates.some((candidate) => !isPublicPinnedAddress(candidate))
		) {
			throw blocked(DesktopOutboundBlockReason.NON_PUBLIC_ADDRESS, context, hostname);
		}
		return candidates[0];
	}

	private async resolveHost(hostname: string): Promise<ReadonlyArray<string>> {
		return await Promise.race([
			this.resolveHostAddresses(hostname),
			new Promise<never>((_resolve, reject) => {
				const timer = setTimeout(
					() => reject(new DesktopOutboundHTTPTimeoutError()),
					DESKTOP_OUTBOUND_HTTP_RESOLUTION_TIMEOUT_MS,
				);
				timer.unref();
			}),
		]);
	}

	private issue(
		request: DesktopOutboundGETRequest,
		pinned: PinnedAddress,
		release: () => void,
	): Promise<DesktopOutboundHTTPMessage> {
		return new Promise<DesktopOutboundHTTPMessage>((resolve, reject) => {
			const secure = request.url.protocol === 'https:';
			const transport = secure ? https : http;
			const clientRequest = transport.request(request.url, {
				agent: secure ? this.httpsAgent : this.httpAgent,
				lookup: createPinnedHostLookup(request.url.hostname, pinned),
				method: 'GET',
			});
			const deadline = setTimeout(() => {
				clientRequest.destroy(new DesktopOutboundHTTPTimeoutError());
			}, request.timeoutMs);
			deadline.unref();
			let settled = false;
			const settle = (): void => {
				if (settled) {
					return;
				}
				settled = true;
				clearTimeout(deadline);
				release();
			};
			clientRequest.on('error', (error) => {
				settle();
				if (error instanceof DesktopOutboundHTTPTimeoutError) {
					reject(error);
					return;
				}
				logger.warn('Outbound request failed', {context: request.context, hostname: request.url.hostname, error});
				reject(new DesktopOutboundHTTPTransportError());
			});
			clientRequest.on('response', (message) => {
				message.on('end', settle);
				message.on('close', settle);
				message.on('error', settle);
				resolve({
					headers: message.headers,
					message,
					status: message.statusCode ?? 0,
					statusText: message.statusMessage ?? '',
					url: request.url,
				});
			});
			clientRequest.end();
		});
	}

	private requirementFor(origin: string, trust: DesktopOriginTrust): DesktopAddressRequirement {
		const registered = this.registeredRequirements.get(origin);
		if (registered != null) {
			return registered;
		}
		if (trust === DesktopOriginTrust.REGISTERED) {
			throw new DesktopOutboundHTTPOriginNotRegisteredError(origin);
		}
		return DesktopAddressRequirement.ANY;
	}

	private async ensureBinding(
		origin: string,
		requirement: DesktopAddressRequirement,
	): Promise<DesktopOriginAddressBinding> {
		this.requireAdmission();
		const existing = this.bindings.get(origin);
		if (existing != null && !existing.unreachable) {
			requireScope(existing, requirement);
			return existing;
		}
		const pending = this.pendingBindings.get(origin);
		if (pending != null) {
			pending.requirement = strictestRequirement(pending.requirement, requirement);
			const binding = await pending.operation;
			requireScope(binding, pending.requirement);
			return binding;
		}
		if (this.pendingBindings.size >= DESKTOP_OUTBOUND_HTTP_MAX_PENDING_RESOLUTIONS) {
			throw new DesktopOutboundHTTPCapacityError('pending resolution');
		}
		if (existing == null && this.bindings.size + this.pendingBindings.size >= DESKTOP_OUTBOUND_HTTP_MAX_ORIGINS) {
			throw new DesktopOutboundHTTPCapacityError('origin');
		}
		const record: DesktopPendingOriginBinding = {
			operation: this.resolveBinding(origin).then((binding) => {
				this.requireAdmission();
				requireStableScope(existing, binding);
				requireScope(binding, record.requirement);
				this.bindings.set(origin, binding);
				return binding;
			}),
			requirement,
		};
		this.pendingBindings.set(origin, record);
		try {
			const binding = await record.operation;
			requireScope(binding, record.requirement);
			return binding;
		} finally {
			if (this.pendingBindings.get(origin) === record) {
				this.pendingBindings.delete(origin);
			}
		}
	}

	private async resolveBinding(origin: string): Promise<DesktopOriginAddressBinding> {
		const hostname = new URL(origin).hostname;
		const literal = parseIPAddress(hostname);
		if (literal != null) {
			return {address: literal, origin, scope: addressScope(literal), unreachable: false};
		}
		const rawAddresses = await this.resolveOriginHost(absoluteLookupHostname(hostname), origin);
		const candidates: Array<PinnedAddress> = [];
		let scope: DesktopOriginAddressScope | null = null;
		for (const rawAddress of rawAddresses) {
			const address = parseIPAddress(rawAddress);
			if (address == null) {
				continue;
			}
			const currentScope = addressScope(address);
			if (scope != null && scope !== currentScope) {
				throw new DesktopOutboundHTTPMixedAddressScopeError(origin);
			}
			scope = currentScope;
			candidates.push(address);
		}
		const pinned = selectPinnedAddress(candidates, scope);
		if (pinned == null || scope == null) {
			throw new DesktopOutboundHTTPEmptyResolutionError(origin);
		}
		return {address: pinned, origin, scope, unreachable: false};
	}

	private markBindingUnreachable(binding: DesktopOriginAddressBinding): void {
		if (this.bindings.get(binding.origin) !== binding) {
			return;
		}
		this.bindings.set(binding.origin, {...binding, unreachable: true});
	}

	private async resolveOriginHost(hostname: string, origin: string): Promise<ReadonlyArray<string>> {
		if (this.activeResolutions >= DESKTOP_OUTBOUND_HTTP_MAX_PENDING_RESOLUTIONS) {
			throw new DesktopOutboundHTTPCapacityError('pending resolution');
		}
		this.activeResolutions += 1;
		try {
			return await Promise.race([
				this.resolveHostAddresses(hostname),
				new Promise<never>((_resolve, reject) => {
					const timeout = setTimeout(
						() => reject(new DesktopOutboundHTTPResolutionTimeoutError(origin)),
						DESKTOP_OUTBOUND_HTTP_RESOLUTION_TIMEOUT_MS,
					);
					timeout.unref();
				}),
			]);
		} finally {
			this.activeResolutions -= 1;
		}
	}

	private acquireSlot(serviceName: string): void {
		if (this.originRequestsInFlight >= DESKTOP_OUTBOUND_HTTP_MAX_ORIGIN_REQUESTS_IN_FLIGHT) {
			throw new DesktopOutboundHTTPCapacityError('in-flight request');
		}
		const perService = this.originRequestsInFlightByService.get(serviceName) ?? 0;
		if (perService >= DESKTOP_OUTBOUND_HTTP_MAX_IN_FLIGHT_PER_SERVICE) {
			throw new DesktopOutboundHTTPCapacityError(`in-flight request for ${serviceName}`);
		}
		this.originRequestsInFlight += 1;
		this.originRequestsInFlightByService.set(serviceName, perService + 1);
	}

	private releaseSlot(serviceName: string): void {
		this.originRequestsInFlight -= 1;
		const perService = (this.originRequestsInFlightByService.get(serviceName) ?? 1) - 1;
		if (perService <= 0) {
			this.originRequestsInFlightByService.delete(serviceName);
		} else {
			this.originRequestsInFlightByService.set(serviceName, perService);
		}
	}

	private issueOriginRequest(
		request: DesktopOutboundHTTPRequest,
		target: URL,
		binding: DesktopOriginAddressBinding,
	): Promise<DesktopOutboundHTTPMessage> {
		this.acquireSlot(request.serviceName);
		let released = false;
		const release = (): void => {
			if (released) {
				return;
			}
			released = true;
			this.releaseSlot(request.serviceName);
		};
		return new Promise<DesktopOutboundHTTPMessage>((resolve, reject) => {
			let attemptsRemaining = isReplayableRequest(request) ? DESKTOP_OUTBOUND_HTTP_STALE_SOCKET_ATTEMPTS : 0;
			let active: http.ClientRequest | null = null;
			let terminated = false;
			const timeout = setTimeout(() => {
				terminated = true;
				active?.destroy(new DesktopOutboundHTTPRequestTimeoutError(target.toString(), request.timeoutMs));
			}, request.timeoutMs);
			timeout.unref();
			const onAbort = (): void => {
				terminated = true;
				active?.destroy(new DesktopOutboundHTTPRequestAbortedError(target.toString()));
			};
			request.signal?.addEventListener('abort', onAbort, {once: true});
			const settle = (): void => {
				clearTimeout(timeout);
				request.signal?.removeEventListener('abort', onAbort);
				release();
			};
			const destroyActive = (error: Error): void => {
				active?.destroy(error);
			};
			if (request.signal?.aborted === true) {
				settle();
				reject(new DesktopOutboundHTTPRequestAbortedError(target.toString()));
				return;
			}
			const attempt = (): void => {
				const transport = target.protocol === 'https:' ? https : http;
				const clientRequest = transport.request(target, {
					agent: target.protocol === 'https:' ? this.originRequestHttpsAgent : this.originRequestHttpAgent,
					headers: requestHeaders(request.headers),
					lookup: createPinnedHostLookup(target.hostname, binding.address),
					method: request.method,
				});
				active = clientRequest;
				clientRequest.on('error', (error) => {
					if (
						!terminated &&
						attemptsRemaining > 0 &&
						clientRequest.reusedSocket === true &&
						isStaleKeepAliveSocketError(error)
					) {
						attemptsRemaining -= 1;
						attempt();
						return;
					}
					settle();
					if (isUnreachableAddressError(error)) {
						this.markBindingUnreachable(binding);
					}
					reject(error);
				});
				clientRequest.on('response', (message) => {
					attemptsRemaining = 0;
					message.on('end', settle);
					message.on('close', settle);
					message.on('error', settle);
					resolve({
						headers: message.headers,
						message,
						status: message.statusCode ?? 0,
						statusText: message.statusMessage ?? '',
						url: target,
					});
				});
				this.sendRequestBody(clientRequest, request);
			};
			try {
				attempt();
			} catch (error) {
				destroyActive(error instanceof Error ? error : new Error(String(error)));
				settle();
				reject(error);
			}
		});
	}

	private sendRequestBody(clientRequest: http.ClientRequest, request: DesktopOutboundHTTPRequest): void {
		const body = request.body;
		if (body == null) {
			clientRequest.end();
			return;
		}
		if (body instanceof Uint8Array) {
			clientRequest.end(Buffer.from(body));
			return;
		}
		const source = Readable.fromWeb(body as unknown as NodeReadableStream<Uint8Array>);
		const limit = createRequestBodyLimit(
			request.maximumRequestBodyBytes ?? DESKTOP_OUTBOUND_HTTP_MAX_REQUEST_BODY_BYTES,
		);
		source.on('error', (error: Error) => {
			limit.destroy(error);
			clientRequest.destroy(error);
		});
		limit.on('error', (error: Error) => {
			source.destroy(error);
			clientRequest.destroy(error);
		});
		clientRequest.once('close', () => {
			source.destroy();
			limit.destroy();
		});
		source.pipe(limit).pipe(clientRequest);
	}

	private requireAdmission(): void {
		if (!this.acceptingRequests) {
			throw new DesktopOutboundHTTPClosedError();
		}
	}
}

let sharedOutboundHTTP: DesktopOutboundHTTP | null = null;

export function getDesktopOutboundHTTP(): DesktopOutboundHTTP {
	sharedOutboundHTTP ??= new DesktopOutboundHTTP();
	return sharedOutboundHTTP;
}

export function cleanupDesktopOutboundHTTP(): void {
	sharedOutboundHTTP?.cleanup();
	sharedOutboundHTTP = null;
}
