/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {isIP} from 'node:net';
import {normalizeIpString} from '~/utils/IpUtils';

export type IpFamily = 'ipv4' | 'ipv6';

interface ParsedBase {
	canonical: string;
	family: IpFamily;
}

export interface ParsedIpSingle extends ParsedBase {
	type: 'single';
	value: bigint;
}

export interface ParsedIpRange extends ParsedBase {
	type: 'range';
	prefixLength: number;
	start: bigint;
	end: bigint;
}

export type IpBanEntry = ParsedIpSingle | ParsedIpRange;

const BIT_LENGTHS: Record<IpFamily, number> = {
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

function formatNormalizedAddress(family: IpFamily, bytes: Buffer): string {
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
	family: IpFamily;
	bytes: Buffer;
}

interface ParsedCIDR {
	family: IpFamily;
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

	return {
		type: 'single',
		family: parsed.family,
		canonical: formatNormalizedAddress(parsed.family, parsed.bytes),
		value: bufferToBigInt(parsed.bytes),
	};
}

function parseIP(value: string): ParsedIP | null {
	const ipWithoutZone = value.split('%', 1)[0].trim();
	if (!ipWithoutZone) return null;

	const family = isIP(ipWithoutZone);
	if (!family) return null;

	if (family === 6 && ipWithoutZone.includes('.')) {
		const lastColon = ipWithoutZone.lastIndexOf(':');
		const ipv4Part = ipWithoutZone.slice(lastColon + 1);
		const bytes = parseIPv4(ipv4Part);
		if (!bytes) return null;
		return {family: 'ipv4', bytes};
	}

	if (family === 4) {
		const bytes = parseIPv4(ipWithoutZone);
		if (!bytes) return null;
		return {family: 'ipv4', bytes};
	}

	const bytes = parseIPv6(ipWithoutZone);
	if (!bytes) return null;
	return {family: 'ipv6', bytes};
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

	const maxPrefix = parsed.family === 'ipv4' ? 32 : 128;
	if (prefixLength > maxPrefix) return null;

	const network = Buffer.from(parsed.bytes);
	const fullBytes = Math.floor(prefixLength / 8);
	const remainingBits = prefixLength % 8;

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
		family: parsed.family,
		address: network,
		prefixLength,
	};
}

export function parseIpBanEntry(value: string): IpBanEntry | null {
	return parseRange(value) ?? parseSingle(value);
}

export function tryParseSingleIp(value: string): ParsedIpSingle | null {
	const parsed = parseIpBanEntry(value);
	return parsed?.type === 'single' ? parsed : null;
}

export function isValidIpOrRange(value: string): boolean {
	return parseIpBanEntry(value) !== null;
}
