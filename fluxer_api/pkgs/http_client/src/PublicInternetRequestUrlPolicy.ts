// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import dns from 'node:dns';
import type {LookupFunction} from 'node:net';
import {BlockList, isIP} from 'node:net';
import {ipToBytes} from '@fluxer/ip_utils/src/IpAddress';
import {isFqdnHostname} from '@fluxer/schema/src/primitives/UrlValidators';
import {formatUrlForDiagnostics} from '@pkgs/http_client/src/HttpClientDiagnostics';
import type {
	FetchDispatcher,
	RequestUrlPolicy,
	RequestUrlValidationContext,
} from '@pkgs/http_client/src/HttpClientTypes';
import {HttpError} from '@pkgs/http_client/src/HttpError';
import {Agent, Dispatcher1Wrapper} from 'undici';

const DEFAULT_DNS_CACHE_TTL_MS = 60000;
const DNS_CACHE_MAX_ENTRIES = 10000;
const ALLOWED_PROTOCOLS = new Set(['http:', 'https:']);
interface BlockedSubnet {
	address: string;
	prefixLength: number;
	family: 'ipv4' | 'ipv6';
}

interface CachedLookupResult {
	addresses: Array<string>;
	expiresAt: number;
}

interface PublicInternetRequestUrlPolicyOptions {
	dnsCacheTtlMs?: number;
	lookupHost?: (hostname: string) => Promise<Array<string>>;
	allowPrivateAddresses?: boolean;
}

const BLOCKED_IPV4_SUBNETS: Array<BlockedSubnet> = [
	{address: '0.0.0.0', prefixLength: 8, family: 'ipv4'},
	{address: '10.0.0.0', prefixLength: 8, family: 'ipv4'},
	{address: '100.64.0.0', prefixLength: 10, family: 'ipv4'},
	{address: '127.0.0.0', prefixLength: 8, family: 'ipv4'},
	{address: '169.254.0.0', prefixLength: 16, family: 'ipv4'},
	{address: '172.16.0.0', prefixLength: 12, family: 'ipv4'},
	{address: '192.0.0.0', prefixLength: 24, family: 'ipv4'},
	{address: '192.0.2.0', prefixLength: 24, family: 'ipv4'},
	{address: '192.88.99.0', prefixLength: 24, family: 'ipv4'},
	{address: '192.168.0.0', prefixLength: 16, family: 'ipv4'},
	{address: '198.18.0.0', prefixLength: 15, family: 'ipv4'},
	{address: '198.51.100.0', prefixLength: 24, family: 'ipv4'},
	{address: '203.0.113.0', prefixLength: 24, family: 'ipv4'},
	{address: '224.0.0.0', prefixLength: 4, family: 'ipv4'},
	{address: '240.0.0.0', prefixLength: 4, family: 'ipv4'},
	{address: '255.255.255.255', prefixLength: 32, family: 'ipv4'},
];
const BLOCKED_IPV6_SUBNETS: Array<BlockedSubnet> = [
	{address: '::', prefixLength: 128, family: 'ipv6'},
	{address: '::1', prefixLength: 128, family: 'ipv6'},
	{address: '2001:db8::', prefixLength: 32, family: 'ipv6'},
	{address: 'fc00::', prefixLength: 7, family: 'ipv6'},
	{address: 'fe80::', prefixLength: 10, family: 'ipv6'},
	{address: 'ff00::', prefixLength: 8, family: 'ipv6'},
];
const blockedIpv4List = createBlockList(BLOCKED_IPV4_SUBNETS);
const blockedIpv6List = createBlockList(BLOCKED_IPV6_SUBNETS);

function createBlockList(subnets: Array<BlockedSubnet>): BlockList {
	const blockList = new BlockList();
	for (const subnet of subnets) {
		blockList.addSubnet(subnet.address, subnet.prefixLength, subnet.family);
	}
	return blockList;
}

function stripIpv6Brackets(value: string): string {
	if (value.startsWith('[') && value.endsWith(']')) {
		return value.slice(1, -1);
	}
	return value;
}

function normalizeHostname(hostname: string): string {
	const trimmed = stripIpv6Brackets(hostname.trim().toLowerCase());
	return trimmed.endsWith('.') ? trimmed.slice(0, -1) : trimmed;
}

function parseEmbeddedIpv4Address(ipv6Address: string): string | null {
	const parsed = ipToBytes(ipv6Address);
	const bytes = parsed?.family === 'ipv6' ? parsed.bytes : null;
	if (!bytes) {
		return null;
	}
	const hasPrefix = (prefix: Array<number>): boolean => prefix.every((byte, index) => bytes[index] === byte);
	const dottedQuadAt = (start: number): string =>
		`${bytes[start]}.${bytes[start + 1]}.${bytes[start + 2]}.${bytes[start + 3]}`;
	if (hasPrefix([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0xff, 0xff])) {
		return dottedQuadAt(12);
	}
	if (hasPrefix([0x00, 0x64, 0xff, 0x9b, 0, 0, 0, 0, 0, 0, 0, 0])) {
		return dottedQuadAt(12);
	}
	if (hasPrefix([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0])) {
		return dottedQuadAt(12);
	}
	if (hasPrefix([0x20, 0x02])) {
		return dottedQuadAt(2);
	}
	return null;
}

function isBlockedIpAddress(address: string): boolean {
	const normalizedAddress = stripIpv6Brackets(address.trim().toLowerCase());
	const family = isIP(normalizedAddress);
	if (family === 4) {
		return blockedIpv4List.check(normalizedAddress, 'ipv4');
	}
	if (family === 6) {
		const embeddedIpv4 = parseEmbeddedIpv4Address(normalizedAddress);
		if (embeddedIpv4) {
			return blockedIpv4List.check(embeddedIpv4, 'ipv4');
		}
		return blockedIpv6List.check(normalizedAddress, 'ipv6');
	}
	return true;
}

export function isPubliclyRoutableUrlShape(url: URL): boolean {
	if (!ALLOWED_PROTOCOLS.has(url.protocol)) {
		return false;
	}
	const normalizedHostname = normalizeHostname(url.hostname);
	if (!normalizedHostname) {
		return false;
	}
	if (isIP(normalizedHostname)) {
		return !isBlockedIpAddress(normalizedHostname);
	}
	return isFqdnHostname(normalizedHostname);
}

function getPolicyErrorContext(context: RequestUrlValidationContext): string {
	if (context.phase === 'redirect') {
		const previous = context.previousUrl === undefined ? 'unknown' : formatUrlForDiagnostics(context.previousUrl);
		return `redirect #${context.redirectCount} from ${previous}`;
	}
	return 'initial request';
}

function createBlockedRequestError(url: URL, context: RequestUrlValidationContext, reason: string): HttpError {
	const message = `Blocked outbound ${getPolicyErrorContext(context)} to ${formatUrlForDiagnostics(url)}: ${reason}`;
	return new HttpError(message, undefined, undefined, true, 'network_error');
}

async function defaultLookupHost(hostname: string): Promise<Array<string>> {
	const addresses = await dns.promises.lookup(hostname, {all: true, order: 'verbatim'});
	return addresses.map((addressEntry) => addressEntry.address);
}

function createBlocklistDispatcher(allowPrivateAddresses: boolean): FetchDispatcher {
	const lookup: LookupFunction = (hostname, options, callback) => {
		dns.lookup(hostname, {...options, all: true, order: options.order ?? 'verbatim'}, (error, addresses) => {
			if (error) {
				callback(error, []);
				return;
			}
			if (!allowPrivateAddresses && addresses.some((entry) => isBlockedIpAddress(entry.address))) {
				callback(new Error(`Hostname ${hostname} resolved to a disallowed address`), []);
				return;
			}
			if (options.all) {
				callback(null, addresses);
				return;
			}
			const [primary] = addresses;
			if (!primary) {
				callback(new Error(`Hostname ${hostname} resolved to no IP addresses`), []);
				return;
			}
			callback(null, primary.address, primary.family);
		});
	};
	return new Dispatcher1Wrapper(
		new Agent({
			allowH2: false,
			connect: {
				lookup,
			},
		}),
	) as unknown as FetchDispatcher;
}

interface PublicInternetRequestUrlPolicy extends RequestUrlPolicy {
	readonly dispatcher: FetchDispatcher;
}

export function createPublicInternetRequestUrlPolicy(
	options?: PublicInternetRequestUrlPolicyOptions,
): PublicInternetRequestUrlPolicy {
	const dnsCacheTtlMs = options?.dnsCacheTtlMs ?? DEFAULT_DNS_CACHE_TTL_MS;
	if (!Number.isFinite(dnsCacheTtlMs) || dnsCacheTtlMs <= 0) {
		throw new RangeError('DNS cache TTL must be a positive finite number');
	}
	const lookupHost = options?.lookupHost ?? defaultLookupHost;
	const allowPrivateAddresses = options?.allowPrivateAddresses === true;
	const dnsCache = new Map<string, CachedLookupResult>();
	async function resolveHostname(hostname: string): Promise<Array<string>> {
		const now = performance.now();
		const cached = dnsCache.get(hostname);
		dnsCache.delete(hostname);
		if (cached && cached.expiresAt > now) {
			dnsCache.set(hostname, cached);
			return cached.addresses;
		}
		const expiresAt = now + dnsCacheTtlMs;
		const resolvedAddresses = [...new Set(await lookupHost(hostname))];
		const current = dnsCache.get(hostname);
		if (expiresAt <= performance.now() || (current && current.expiresAt > expiresAt)) {
			return resolvedAddresses;
		}
		dnsCache.delete(hostname);
		if (dnsCache.size >= DNS_CACHE_MAX_ENTRIES) {
			const oldest = dnsCache.keys().next();
			assert(!oldest.done, 'A full DNS cache must contain an eviction candidate');
			dnsCache.delete(oldest.value);
		}
		dnsCache.set(hostname, {
			addresses: resolvedAddresses,
			expiresAt,
		});
		return resolvedAddresses;
	}
	async function validate(url: URL, context: RequestUrlValidationContext): Promise<void> {
		if (!ALLOWED_PROTOCOLS.has(url.protocol)) {
			throw createBlockedRequestError(url, context, 'Only HTTP and HTTPS protocols are allowed');
		}
		const normalizedHostname = normalizeHostname(url.hostname);
		if (!normalizedHostname) {
			throw createBlockedRequestError(url, context, 'Hostname is empty');
		}
		if (isIP(normalizedHostname)) {
			if (!allowPrivateAddresses && isBlockedIpAddress(normalizedHostname)) {
				throw createBlockedRequestError(url, context, 'IP address is in an internal or special-use range');
			}
			return;
		}
		if (!isFqdnHostname(normalizedHostname)) {
			throw createBlockedRequestError(url, context, 'Hostname is not a valid FQDN');
		}
		const resolvedAddresses = await resolveHostname(normalizedHostname);
		if (resolvedAddresses.length === 0) {
			throw createBlockedRequestError(url, context, 'Hostname resolved to no IP addresses');
		}
		if (allowPrivateAddresses) {
			return;
		}
		for (const address of resolvedAddresses) {
			if (isBlockedIpAddress(address)) {
				throw createBlockedRequestError(url, context, `Hostname resolved to disallowed address ${address}`);
			}
		}
	}
	const dispatcher = createBlocklistDispatcher(allowPrivateAddresses);
	return {validate, dispatcher};
}
