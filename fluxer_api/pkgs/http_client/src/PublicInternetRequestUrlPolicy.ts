// SPDX-License-Identifier: AGPL-3.0-or-later

import dns from 'node:dns';
import {BlockList, isIP} from 'node:net';
import type {RequestUrlPolicy, RequestUrlValidationContext} from '@pkgs/http_client/src/HttpClientTypes';
import {HttpError} from '@pkgs/http_client/src/HttpError';

const DEFAULT_DNS_CACHE_TTL_MS = 60000;
const ALLOWED_PROTOCOLS = new Set(['http:', 'https:']);
const HOSTNAME_LABEL_REGEX = /^[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?$/;

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

function isFqdnHostname(hostname: string): boolean {
	if (!hostname || hostname.length > 253 || !hostname.includes('.')) {
		return false;
	}
	const labels = hostname.split('.');
	for (const label of labels) {
		if (!label || label.length > 63 || !HOSTNAME_LABEL_REGEX.test(label)) {
			return false;
		}
	}
	const topLevelDomain = labels[labels.length - 1];
	return !/^\d+$/.test(topLevelDomain);
}

function parseIpv4MappedIpv6Address(ipv6Address: string): string | null {
	const normalized = stripIpv6Brackets(ipv6Address.trim().toLowerCase());
	if (!normalized.startsWith('::ffff:')) {
		return null;
	}
	const suffix = normalized.slice('::ffff:'.length);
	if (isIP(suffix) === 4) {
		return suffix;
	}
	const groups = suffix.split(':');
	if (groups.length !== 2) {
		return null;
	}
	const high = parseHexGroup(groups[0]);
	const low = parseHexGroup(groups[1]);
	if (high === null || low === null) {
		return null;
	}
	const octet1 = (high >> 8) & 0xff;
	const octet2 = high & 0xff;
	const octet3 = (low >> 8) & 0xff;
	const octet4 = low & 0xff;
	return `${octet1}.${octet2}.${octet3}.${octet4}`;
}

function parseHexGroup(value: string | undefined): number | null {
	if (!value || !/^[0-9a-f]{1,4}$/.test(value)) {
		return null;
	}
	return Number.parseInt(value, 16);
}

function isBlockedIpAddress(address: string): boolean {
	const normalizedAddress = stripIpv6Brackets(address.trim().toLowerCase());
	const family = isIP(normalizedAddress);
	if (family === 4) {
		return blockedIpv4List.check(normalizedAddress, 'ipv4');
	}
	if (family === 6) {
		const mappedIpv4 = parseIpv4MappedIpv6Address(normalizedAddress);
		if (mappedIpv4) {
			return blockedIpv4List.check(mappedIpv4, 'ipv4');
		}
		return blockedIpv6List.check(normalizedAddress, 'ipv6');
	}
	return true;
}

function getPolicyErrorContext(context: RequestUrlValidationContext): string {
	if (context.phase === 'redirect') {
		const previous = context.previousUrl ?? 'unknown';
		return `redirect #${context.redirectCount} from ${previous}`;
	}
	return 'initial request';
}

function createBlockedRequestError(url: URL, context: RequestUrlValidationContext, reason: string): HttpError {
	const message = `Blocked outbound ${getPolicyErrorContext(context)} to ${url.href}: ${reason}`;
	return new HttpError(message, undefined, undefined, true, 'network_error');
}

async function defaultLookupHost(hostname: string): Promise<Array<string>> {
	const addresses = await dns.promises.lookup(hostname, {all: true, verbatim: true});
	return addresses.map((addressEntry) => addressEntry.address);
}

function deduplicateAddresses(addresses: Array<string>): Array<string> {
	const seen = new Set<string>();
	const deduplicated: Array<string> = [];
	for (const address of addresses) {
		if (seen.has(address)) {
			continue;
		}
		seen.add(address);
		deduplicated.push(address);
	}
	return deduplicated;
}

export function createPublicInternetRequestUrlPolicy(
	options?: PublicInternetRequestUrlPolicyOptions,
): RequestUrlPolicy {
	const dnsCacheTtlMs =
		typeof options?.dnsCacheTtlMs === 'number' && options.dnsCacheTtlMs > 0
			? options.dnsCacheTtlMs
			: DEFAULT_DNS_CACHE_TTL_MS;
	const lookupHost = options?.lookupHost ?? defaultLookupHost;
	const allowPrivateAddresses = options?.allowPrivateAddresses === true;
	const dnsCache = new Map<string, CachedLookupResult>();
	async function resolveHostname(hostname: string): Promise<Array<string>> {
		const now = Date.now();
		const cached = dnsCache.get(hostname);
		if (cached && cached.expiresAt > now) {
			return cached.addresses;
		}
		const resolvedAddresses = deduplicateAddresses(await lookupHost(hostname));
		dnsCache.set(hostname, {
			addresses: resolvedAddresses,
			expiresAt: now + dnsCacheTtlMs,
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
	return {validate};
}
