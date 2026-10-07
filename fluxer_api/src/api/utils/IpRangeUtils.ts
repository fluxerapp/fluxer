// SPDX-License-Identifier: AGPL-3.0-or-later

import {isIP} from 'node:net';
import type {IpAddressFamily} from '@fluxer/ip_utils/src/IpAddress';
import {normalizeIpString} from '@fluxer/ip_utils/src/IpAddress';

interface ParsedBase {
	canonical: string;
	family: IpAddressFamily;
}

interface ParsedIpSingle extends ParsedBase {
	type: 'single';
	value: bigint;
}

interface ParsedIpRange extends ParsedBase {
	type: 'range';
	prefixLength: number;
	start: bigint;
	end: bigint;
}

type IpBanEntry = ParsedIpSingle | ParsedIpRange;

const BIT_LENGTHS: Record<IpAddressFamily, number> = {
	ipv4: 32,
	ipv6: 128,
};

function bufferToBigInt(bytes: Buffer): bigint {
	let value = 0n;
	for (const byte of bytes) {
		value = (value << 8n) | BigInt(byte);
	}
	return value;
}

function formatNormalizedAddress(family: IpAddressFamily, bytes: Buffer): string {
	if (family === 'ipv4') {
		return Array.from(bytes.values()).join('.');
	}
	const groups: Array<string> = [];
	for (let i = 0; i < 16; i += 2) {
		const high = bytes[i];
		const low = bytes[i + 1];
		const value = (high << 8) | low;
		groups.push(value.toString(16).padStart(4, '0'));
	}
	return groups.join(':');
}

interface ParsedIP {
	family: IpAddressFamily;
	bytes: Buffer;
	mappedIpv4Bytes: Buffer | null;
}

interface ParsedCIDR {
	family: IpAddressFamily;
	address: Buffer;
	prefixLength: number;
}

function parseRange(value: string): ParsedIpRange | null {
	const normalized = normalizeIpString(value);
	if (!normalized) return null;
	const parsed = parseCIDR(normalized);
	if (!parsed) return null;
	const {family, address, prefixLength} = parsed;
	const totalBits = BIT_LENGTHS[family];
	const hostBits = totalBits - prefixLength;
	const baseValue = bufferToBigInt(address);
	const mask = hostBits === 0 ? 0n : (1n << BigInt(hostBits)) - 1n;
	const start = baseValue & ~mask;
	const end = baseValue | mask;
	return {
		type: 'range',
		family,
		canonical: `${formatNormalizedAddress(family, address)}/${prefixLength}`,
		prefixLength,
		start,
		end,
	};
}

function parseSingle(value: string): ParsedIpSingle | null {
	const normalized = normalizeIpString(value);
	if (!normalized || normalized.includes('/')) return null;
	const parsed = parseIP(normalized);
	if (!parsed) return null;
	const resolved = resolveMappedIpv4(parsed);
	return {
		type: 'single',
		family: resolved.family,
		canonical: formatNormalizedAddress(resolved.family, resolved.bytes),
		value: bufferToBigInt(resolved.bytes),
	};
}

function parseIP(value: string): ParsedIP | null {
	const ipWithoutZone = value.split('%', 1)[0].trim();
	if (!ipWithoutZone) return null;
	const family = isIP(ipWithoutZone);
	if (!family) return null;
	if (family === 4) {
		const bytes = parseIPv4(ipWithoutZone);
		if (!bytes) return null;
		return {family: 'ipv4', bytes, mappedIpv4Bytes: null};
	}
	let ipv6Address = ipWithoutZone;
	if (ipWithoutZone.includes('.')) {
		const converted = convertEmbeddedIpv4(ipWithoutZone);
		if (!converted) return null;
		ipv6Address = converted;
	}
	const bytes = parseIPv6(ipv6Address);
	if (!bytes) return null;
	return {
		family: 'ipv6',
		bytes,
		mappedIpv4Bytes: extractMappedIpv4(bytes),
	};
}

function resolveMappedIpv4(parsed: ParsedIP): {
	family: IpAddressFamily;
	bytes: Buffer;
} {
	if (parsed.family === 'ipv6' && parsed.mappedIpv4Bytes) {
		return {family: 'ipv4', bytes: parsed.mappedIpv4Bytes};
	}
	return {family: parsed.family, bytes: parsed.bytes};
}

function extractMappedIpv4(bytes: Buffer): Buffer | null {
	if (bytes.length !== 16) return null;
	for (let i = 0; i < 10; i++) {
		if (bytes[i] !== 0) return null;
	}
	if (bytes[10] !== 0xff || bytes[11] !== 0xff) return null;
	return Buffer.from(bytes.subarray(12, 16));
}

function convertEmbeddedIpv4(value: string): string | null {
	const lastColon = value.lastIndexOf(':');
	if (lastColon === -1) return null;
	const ipv4Part = value.slice(lastColon + 1);
	const ipv4Bytes = parseIPv4(ipv4Part);
	if (!ipv4Bytes) return null;
	const high = ((ipv4Bytes[0] << 8) | ipv4Bytes[1]).toString(16);
	const low = ((ipv4Bytes[2] << 8) | ipv4Bytes[3]).toString(16);
	return `${value.slice(0, lastColon + 1)}${high}:${low}`;
}

function parseIPv4(ip: string): Buffer | null {
	const parts = ip.split('.');
	if (parts.length !== 4) return null;
	const bytes = Buffer.alloc(4);
	for (let i = 0; i < 4; i++) {
		const part = parts[i];
		if (!/^\d+$/.test(part)) return null;
		const octet = Number(part);
		if (!Number.isInteger(octet) || octet < 0 || octet > 255) return null;
		bytes[i] = octet;
	}
	return bytes;
}

function parseIPv6(ip: string): Buffer | null {
	const addr = ip.trim();
	const doubleColonIndex = addr.indexOf('::');
	let headPart = '';
	let tailPart = '';
	if (doubleColonIndex !== -1) {
		const parts = addr.split('::');
		if (parts.length !== 2) return null;
		[headPart, tailPart] = parts;
	} else {
		headPart = addr;
		tailPart = '';
	}
	const headGroups = headPart ? headPart.split(':').filter((g) => g.length > 0) : [];
	const tailGroups = tailPart ? tailPart.split(':').filter((g) => g.length > 0) : [];
	if (doubleColonIndex === -1 && headGroups.length !== 8) return null;
	const totalGroups = headGroups.length + tailGroups.length;
	if (totalGroups > 8) return null;
	const zerosToInsert = 8 - totalGroups;
	const groups: Array<number> = [];
	for (const group of headGroups) {
		const value = parseInt(group, 16);
		if (!Number.isFinite(value) || value < 0 || value > 0xffff) return null;
		groups.push(value);
	}
	for (let i = 0; i < zerosToInsert; i++) {
		groups.push(0);
	}
	for (const group of tailGroups) {
		const value = parseInt(group, 16);
		if (!Number.isFinite(value) || value < 0 || value > 0xffff) return null;
		groups.push(value);
	}
	if (groups.length !== 8) return null;
	const bytes = Buffer.alloc(16);
	for (let i = 0; i < 8; i++) {
		bytes[i * 2] = (groups[i] >> 8) & 0xff;
		bytes[i * 2 + 1] = groups[i] & 0xff;
	}
	return bytes;
}

function parseCIDR(cidr: string): ParsedCIDR | null {
	const trimmed = cidr.trim();
	if (!trimmed) return null;
	const slashIdx = trimmed.indexOf('/');
	if (slashIdx === -1) return null;
	const ipPart = trimmed.slice(0, slashIdx).trim();
	const prefixPart = trimmed.slice(slashIdx + 1).trim();
	if (!ipPart || !prefixPart) return null;
	const prefixLength = Number(prefixPart);
	if (!Number.isInteger(prefixLength) || prefixLength < 0) return null;
	const parsed = parseIP(ipPart);
	if (!parsed) return null;
	let resolvedFamily = parsed.family;
	let resolvedAddress = Buffer.from(parsed.bytes);
	let resolvedPrefix = prefixLength;
	if (parsed.family === 'ipv6' && parsed.mappedIpv4Bytes && prefixLength >= 96) {
		resolvedFamily = 'ipv4';
		resolvedAddress = Buffer.from(parsed.mappedIpv4Bytes);
		resolvedPrefix = prefixLength - 96;
	}
	const maxPrefix = resolvedFamily === 'ipv4' ? 32 : 128;
	if (resolvedPrefix > maxPrefix) return null;
	const network = Buffer.from(resolvedAddress);
	const fullBytes = Math.floor(resolvedPrefix / 8);
	const remainingBits = resolvedPrefix % 8;
	if (remainingBits > 0 && fullBytes < network.length) {
		const mask = (0xff << (8 - remainingBits)) & 0xff;
		network[fullBytes] &= mask;
		for (let i = fullBytes + 1; i < network.length; i++) {
			network[i] = 0;
		}
	} else {
		for (let i = fullBytes; i < network.length; i++) {
			network[i] = 0;
		}
	}
	return {
		family: resolvedFamily,
		address: network,
		prefixLength: resolvedPrefix,
	};
}

export function parseIpBanEntry(value: string): IpBanEntry | null {
	return parseRange(value) ?? parseSingle(value);
}

export function tryParseSingleIp(value: string): ParsedIpSingle | null {
	const parsed = parseIpBanEntry(value);
	return parsed?.type === 'single' ? parsed : null;
}
