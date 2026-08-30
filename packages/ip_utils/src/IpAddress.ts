// SPDX-License-Identifier: AGPL-3.0-or-later

import {isIPv4, isIPv6} from 'node:net';

export type IpAddressFamily = 'ipv4' | 'ipv6';
type IpPrefixLength = number | 'exact';

export interface ParsedIpAddress {
	raw: string;
	normalized: string;
	family: IpAddressFamily;
}

interface IpNetworkKeyOptions {
	ipv4PrefixLength: IpPrefixLength;
	ipv6PrefixLength: IpPrefixLength;
}

const SAME_IP_DECISION_KEY_OPTIONS: Readonly<IpNetworkKeyOptions> = {
	ipv4PrefixLength: 'exact',
	ipv6PrefixLength: 64,
};

function stripIpv6Brackets(value: string): string {
	if (value.startsWith('[') && value.endsWith(']')) {
		return value.slice(1, -1);
	}
	return value;
}

function stripIpv6ZoneIdentifier(value: string): string {
	const zoneIndex = value.indexOf('%');
	if (zoneIndex === -1) {
		return value;
	}
	const addressPart = value.slice(0, zoneIndex);
	if (!addressPart.includes(':')) {
		return value;
	}
	return addressPart;
}

function normalizeIpv6(value: string): string {
	if (!isIPv6(value)) {
		return value;
	}
	try {
		const hostname = new URL(`http://[${value}]`).hostname;
		if (hostname.startsWith('[') && hostname.endsWith(']')) {
			return hostname.slice(1, -1);
		}
		return hostname;
	} catch {
		return value;
	}
}

function getAddressFamily(value: string): IpAddressFamily | null {
	if (isIPv4(value)) {
		return 'ipv4';
	}
	if (isIPv6(value)) {
		return 'ipv6';
	}
	return null;
}

export function normalizeIpString(value: string): string {
	const trimmed = value.trim();
	const withoutBrackets = stripIpv6Brackets(trimmed);
	const withoutZone = stripIpv6ZoneIdentifier(withoutBrackets);
	return normalizeIpv6(withoutZone);
}

export function parseIpAddress(value: string): ParsedIpAddress | null {
	const normalized = normalizeIpString(value);
	const family = getAddressFamily(normalized);
	if (!family) {
		return null;
	}
	return {
		raw: value,
		normalized,
		family,
	};
}

export function isValidIp(value: string): boolean {
	return parseIpAddress(value) !== null;
}

export function maskIpForDisplay(ip: string): string | null {
	const parsed = parseIpAddress(ip);
	if (!parsed) {
		return null;
	}
	if (parsed.family === 'ipv4') {
		return maskIpv4ForDisplay(parsed.normalized);
	}
	return maskIpv6ForDisplay(parsed.normalized);
}

function expandIpv6ToGroups(address: string): Array<string> {
	const halves = address.split('::');
	if (halves.length === 2) {
		const left = halves[0] ? halves[0].split(':') : [];
		const right = halves[1] ? halves[1].split(':') : [];
		const missing = 8 - left.length - right.length;
		const middle = Array<string>(missing).fill('0000');
		return [...left, ...middle, ...right].map((g) => g.padStart(4, '0'));
	}
	return address.split(':').map((g) => g.padStart(4, '0'));
}

function isIpv4MappedIpv6Groups(groups: Array<string>): boolean {
	return (
		groups[0] === '0000' &&
		groups[1] === '0000' &&
		groups[2] === '0000' &&
		groups[3] === '0000' &&
		groups[4] === '0000' &&
		groups[5] === 'ffff'
	);
}

function ipv4FromMappedIpv6Groups(groups: Array<string>): string {
	const hi = parseInt(groups[6], 16);
	const lo = parseInt(groups[7], 16);
	const a = (hi >> 8) & 0xff;
	const b = hi & 0xff;
	const c = (lo >> 8) & 0xff;
	const d = lo & 0xff;
	return `${a}.${b}.${c}.${d}`;
}

function maskIpv4ForDisplay(address: string): string | null {
	const octets = parseIpv4Octets(address);
	if (!octets) {
		return null;
	}
	return `${octets[0]}.${octets[1]}.${octets[2]}.x`;
}

function maskIpv6ForDisplay(address: string): string {
	const groups = expandIpv6ToGroups(address);
	const mappedIpv4 = isIpv4MappedIpv6Groups(groups) ? ipv4FromMappedIpv6Groups(groups) : null;
	if (mappedIpv4) {
		return maskIpv4ForDisplay(mappedIpv4) ?? 'x.x.x.x';
	}
	return getIpv6NetworkKey(address, 64);
}

function assertPrefixLength(prefixLength: number, maxBits: number): void {
	if (!Number.isInteger(prefixLength) || prefixLength < 0 || prefixLength > maxBits) {
		throw new RangeError(`Prefix length must be an integer between 0 and ${maxBits}`);
	}
}

function getIpv4NetworkKey(address: string, prefixLength: IpPrefixLength): string {
	if (prefixLength === 'exact') {
		return address;
	}
	assertPrefixLength(prefixLength, 32);
	const octets = address.split('.').map((octet) => Number.parseInt(octet, 10));
	let value = 0;
	for (const octet of octets) {
		value = (value << 8) | octet;
	}
	const shift = 32 - prefixLength;
	const mask = prefixLength === 0 ? 0 : (0xffffffff << shift) >>> 0;
	const network = value & mask;
	return `${(network >>> 24) & 0xff}.${(network >>> 16) & 0xff}.${(network >>> 8) & 0xff}.${network & 0xff}/${prefixLength}`;
}

function getIpv6NetworkKey(address: string, prefixLength: IpPrefixLength): string {
	if (prefixLength === 'exact') {
		return address;
	}
	assertPrefixLength(prefixLength, 128);
	const groups = expandIpv6ToGroups(address);
	let remainingBits = prefixLength;
	const maskedGroups = groups.map((group) => {
		if (remainingBits >= 16) {
			remainingBits -= 16;
			return group;
		}
		if (remainingBits <= 0) {
			return '0000';
		}
		const value = parseInt(group, 16);
		const mask = ((0xffff << (16 - remainingBits)) & 0xffff) >>> 0;
		remainingBits = 0;
		return (value & mask).toString(16).padStart(4, '0');
	});
	const normalized = normalizeIpv6(maskedGroups.join(':'));
	return `${normalized}/${prefixLength}`;
}

export function getIpNetworkKey(ip: string, options: IpNetworkKeyOptions): string | null {
	const parsed = parseIpAddress(ip);
	if (!parsed) {
		return null;
	}
	if (parsed.family === 'ipv4') {
		return getIpv4NetworkKey(parsed.normalized, options.ipv4PrefixLength);
	}
	const groups = expandIpv6ToGroups(parsed.normalized);
	if (isIpv4MappedIpv6Groups(groups)) {
		return getIpv4NetworkKey(ipv4FromMappedIpv6Groups(groups), options.ipv4PrefixLength);
	}
	return getIpv6NetworkKey(parsed.normalized, options.ipv6PrefixLength);
}

export function getSameIpDecisionKey(ip: string): string | null {
	return getIpNetworkKey(ip, SAME_IP_DECISION_KEY_OPTIONS);
}

function parseIpv4Octets(address: string): Array<number> | null {
	if (!isIPv4(address)) {
		return null;
	}
	const octets = address.split('.').map((octet) => Number.parseInt(octet, 10));
	return octets.length === 4 && octets.every((octet) => Number.isInteger(octet) && octet >= 0 && octet <= 255)
		? octets
		: null;
}

function isIpv4InCidr(octets: Array<number>, base: number, prefixLength: number): boolean {
	const value = ((octets[0] << 24) | (octets[1] << 16) | (octets[2] << 8) | octets[3]) >>> 0;
	const shift = 32 - prefixLength;
	const mask = prefixLength === 0 ? 0 : (0xffffffff << shift) >>> 0;
	return (value & mask) >>> 0 === (base & mask) >>> 0;
}

function isPublicIpv4Address(address: string): boolean {
	const octets = parseIpv4Octets(address);
	if (!octets) {
		return false;
	}
	const reservedRanges: Array<[base: number, prefixLength: number]> = [
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
	];
	return !reservedRanges.some(([base, prefixLength]) => isIpv4InCidr(octets, base, prefixLength));
}

function getIpv4MappedIpv6(address: string): string | null {
	const groups = expandIpv6ToGroups(address);
	if (!isIpv4MappedIpv6Groups(groups)) {
		return null;
	}
	return ipv4FromMappedIpv6Groups(groups);
}

function ipv6GroupValue(group: string): number {
	return Number.parseInt(group, 16);
}

function isPublicIpv6Address(address: string): boolean {
	const mappedIpv4 = getIpv4MappedIpv6(address);
	if (mappedIpv4) {
		return isPublicIpv4Address(mappedIpv4);
	}
	const groups = expandIpv6ToGroups(address);
	const first = ipv6GroupValue(groups[0]);
	const second = ipv6GroupValue(groups[1]);
	const last = ipv6GroupValue(groups[7]);
	const isUnspecifiedOrLoopback = groups.slice(0, 7).every((group) => group === '0000') && (last === 0 || last === 1);
	if (isUnspecifiedOrLoopback) {
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
	if (first === 0x2001 && second === 0x0db8) {
		return false;
	}
	if (first === 0x0064 && second === 0xff9b) {
		return false;
	}
	if (first === 0x0100 && second === 0x0000) {
		return false;
	}
	return true;
}

export function isPublicIpAddress(ip: string): boolean {
	const parsed = parseIpAddress(ip);
	if (!parsed) {
		return false;
	}
	if (parsed.family === 'ipv4') {
		return isPublicIpv4Address(parsed.normalized);
	}
	return isPublicIpv6Address(parsed.normalized);
}

function isLoopbackIpv4Address(address: string): boolean {
	const octets = parseIpv4Octets(address);
	if (!octets) {
		return false;
	}
	return isIpv4InCidr(octets, 0x7f000000, 8);
}

export function isLoopbackIpAddress(ip: string): boolean {
	const parsed = parseIpAddress(ip);
	if (!parsed) {
		return false;
	}
	if (parsed.family === 'ipv4') {
		return isLoopbackIpv4Address(parsed.normalized);
	}
	const mappedIpv4 = getIpv4MappedIpv6(parsed.normalized);
	if (mappedIpv4) {
		return isLoopbackIpv4Address(mappedIpv4);
	}
	const groups = expandIpv6ToGroups(parsed.normalized);
	return groups.length === 8 && groups.slice(0, 7).every((group) => group === '0000') && groups[7] === '0001';
}

export function isSameIpDecisionMatch(left: string | null | undefined, right: string | null | undefined): boolean {
	if (!left || !right) {
		return false;
	}
	const leftKey = getSameIpDecisionKey(left);
	const rightKey = getSameIpDecisionKey(right);
	if (leftKey && rightKey) {
		return leftKey === rightKey;
	}
	return normalizeIpString(left) === normalizeIpString(right);
}

export function getSubnet(ip: string): string | null {
	return getIpNetworkKey(ip, {
		ipv4PrefixLength: 24,
		ipv6PrefixLength: 48,
	});
}
