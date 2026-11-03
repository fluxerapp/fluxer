// SPDX-License-Identifier: AGPL-3.0-or-later

import {isIP} from 'node:net';
import {type IpAddressFamily, ipToBytes, normalizeIpString} from '@fluxer/ip_utils/src/IpAddress';

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
	if (!isIP(ipWithoutZone)) return null;
	const parsed = ipToBytes(ipWithoutZone);
	if (!parsed) return null;
	const bytes = Buffer.from(parsed.bytes);
	return {family: parsed.family, bytes, mappedIpv4Bytes: parsed.family === 'ipv6' ? extractMappedIpv4(bytes) : null};
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
