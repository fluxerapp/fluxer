// SPDX-License-Identifier: AGPL-3.0-or-later

import {Buffer} from 'node:buffer';
import dns from 'node:dns';
import http from 'node:http';
import https from 'node:https';
import {isIPv4, isIPv6, type LookupFunction} from 'node:net';
import type {Readable} from 'node:stream';
import {getAppUrl} from '@electron/common/DesktopConfig';
import {createChildLogger} from '@electron/common/Logger';

const logger = createChildLogger('DesktopOutboundHTTP');

const DESKTOP_OUTBOUND_HTTP_MAX_IN_FLIGHT = 32;
const DESKTOP_OUTBOUND_HTTP_MAX_SOCKETS = 16;
const DESKTOP_OUTBOUND_HTTP_RESOLUTION_TIMEOUT_MS = 10_000;
const DESKTOP_OUTBOUND_HTTP_MAX_TARGET_URL_BYTES = 16 * 1024;
const DESKTOP_OUTBOUND_HTTP_MAX_REDIRECT_LOCATION_BYTES = 4096;

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

const DesktopAddressRequirement = Object.freeze({
	ANY: 'any',
	PUBLIC: 'public',
} as const);

type DesktopAddressRequirement = (typeof DesktopAddressRequirement)[keyof typeof DesktopAddressRequirement];

interface PinnedAddress {
	readonly address: string;
	readonly family: 4 | 6;
}

interface DesktopOutboundGETRequest {
	readonly context: string;
	readonly timeoutMs: number;
	readonly url: URL;
}

export interface DesktopOutboundHTTPMessage {
	readonly headers: http.IncomingHttpHeaders;
	readonly message: http.IncomingMessage;
	readonly status: number;
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
	public constructor() {
		super(DESKTOP_OUTBOUND_HTTP_CAPACITY_MESSAGE);
		this.name = 'DesktopOutboundHTTPCapacityError';
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

function createPinnedHostLookup(hostname: string, pinned: PinnedAddress): LookupFunction {
	const expected = normalizeLookupHostname(hostname);
	return (requestedHostname, options, callback) => {
		if (normalizeLookupHostname(requestedHostname) !== expected) {
			callback(new PinnedLookupHostnameMismatchError(), '', undefined);
			return;
		}
		if (options.all === true) {
			(callback as unknown as (error: null, addresses: ReadonlyArray<PinnedAddress>) => void)(null, [pinned]);
			return;
		}
		callback(null, pinned.address, pinned.family);
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
	const records = await dns.promises.lookup(hostname, {all: true, verbatim: true});
	return records.map((record) => record.address);
}

class DesktopOutboundHTTP {
	private readonly httpAgent = new http.Agent({keepAlive: true, maxSockets: DESKTOP_OUTBOUND_HTTP_MAX_SOCKETS});
	private readonly httpsAgent = new https.Agent({keepAlive: true, maxSockets: DESKTOP_OUTBOUND_HTTP_MAX_SOCKETS});
	private addressRequirementOperation: Promise<DesktopAddressRequirement> | null = null;
	private inFlight = 0;

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
			const requirement = await this.addressRequirement();
			if (requirement === DesktopAddressRequirement.PUBLIC && request.url.protocol !== 'https:') {
				throw blocked(DesktopOutboundBlockReason.INSECURE_TRANSPORT, request.context, request.url.hostname);
			}
			const pinned = await this.pinAddress(request.url.hostname, requirement, request.context);
			return await this.issue(request, pinned, release);
		} catch (error) {
			release();
			throw error;
		}
	}

	private addressRequirement(): Promise<DesktopAddressRequirement> {
		this.addressRequirementOperation ??= this.resolveAddressRequirement();
		return this.addressRequirementOperation;
	}

	private async resolveAddressRequirement(): Promise<DesktopAddressRequirement> {
		const appOrigin = parseDesktopHTTPTarget(getAppUrl());
		if (appOrigin == null) {
			return DesktopAddressRequirement.PUBLIC;
		}
		const literal = parseIPAddress(appOrigin.hostname);
		if (literal != null) {
			return isPublicPinnedAddress(literal) ? DesktopAddressRequirement.PUBLIC : DesktopAddressRequirement.ANY;
		}
		let addresses: ReadonlyArray<string>;
		try {
			addresses = await this.resolveHost(appOrigin.hostname);
		} catch {
			logger.warn('App origin did not resolve; requiring publicly routable outbound addresses');
			return DesktopAddressRequirement.PUBLIC;
		}
		const candidates = addresses.map((address) => parseIPAddress(address)).filter((value) => value != null);
		if (candidates.length === 0 || candidates.every((candidate) => isPublicPinnedAddress(candidate))) {
			return DesktopAddressRequirement.PUBLIC;
		}
		return DesktopAddressRequirement.ANY;
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
			lookupAllAddresses(hostname),
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
					url: request.url,
				});
			});
			clientRequest.end();
		});
	}
}

let sharedOutboundHTTP: DesktopOutboundHTTP | null = null;

export function getDesktopOutboundHTTP(): DesktopOutboundHTTP {
	sharedOutboundHTTP ??= new DesktopOutboundHTTP();
	return sharedOutboundHTTP;
}
