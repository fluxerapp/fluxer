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
import {Logger} from '~/Logger';
import {sendRequest} from '~/utils/FetchUtils';

const CLOUDFLARE_IPV4_URL = 'https://www.cloudflare.com/ips-v4';
const CLOUDFLARE_IPV6_URL = 'https://www.cloudflare.com/ips-v6';
const REFRESH_INTERVAL_MS = 10 * 60 * 1000;

type IPFamily = 4 | 6;

interface ParsedIP {
	family: IPFamily;
	bytes: Buffer;
}

interface CidrEntry {
	family: IPFamily;
	network: Buffer;
	prefixLength: number;
}

export class CloudflareIPService {
	private cidrs: Array<CidrEntry> = [];
	private refreshTimer?: NodeJS.Timeout;

	async initialize(): Promise<void> {
		await this.refreshNow();

		this.refreshTimer = setInterval(() => {
			this.refreshNow().catch((error) => {
				Logger.error({error}, 'Failed to refresh Cloudflare IP ranges; keeping existing ranges');
			});
		}, REFRESH_INTERVAL_MS);
		this.refreshTimer.unref();
	}

	isFromCloudflare(ip: string | undefined | null): boolean {
		if (!ip) return false;
		if (this.cidrs.length === 0) return false;

		const parsed = parseIP(ip);
		if (!parsed) return false;

		for (const cidr of this.cidrs) {
			if (cidr.family !== parsed.family) continue;
			if (ipInCidr(parsed, cidr)) return true;
		}

		return false;
	}

	private async refreshNow(): Promise<void> {
		const [v4Ranges, v6Ranges] = await Promise.all([
			this.fetchRanges(CLOUDFLARE_IPV4_URL),
			this.fetchRanges(CLOUDFLARE_IPV6_URL),
		]);

		const nextCidrs: Array<CidrEntry> = [];
		for (const range of [...v4Ranges, ...v6Ranges]) {
			const parsed = parseCIDR(range);
			if (!parsed) continue;
			nextCidrs.push(parsed);
		}

		if (nextCidrs.length === 0) {
			throw new Error('Cloudflare IP list refresh returned no valid ranges');
		}

		this.cidrs = nextCidrs;
		Logger.info({count: this.cidrs.length}, 'Refreshed Cloudflare IP ranges');
	}

	private async fetchRanges(url: string): Promise<Array<string>> {
		const response = await sendRequest({url, method: 'GET'});

		if (response.status !== 200) {
			throw new Error(`Failed to download Cloudflare IPs from ${url} (status ${response.status})`);
		}

		const text = await CloudflareIPService.streamToString(response.stream);
		return text
			.split('\n')
			.map((l) => l.trim())
			.filter((l) => l.length > 0);
	}

	private static async streamToString(stream: NodeJS.ReadableStream): Promise<string> {
		const chunks: Array<Buffer> = [];
		for await (const chunk of stream) {
			chunks.push(Buffer.isBuffer(chunk) ? chunk : Buffer.from(chunk));
		}
		return Buffer.concat(chunks).toString('utf8');
	}
}

function parseIP(ip: string): ParsedIP | null {
	const ipWithoutZone = ip.split('%', 1)[0].trim();
	if (!ipWithoutZone) return null;

	const family = isIP(ipWithoutZone);
	if (!family) return null;

	if (family === 6 && ipWithoutZone.includes('.')) {
		const lastColon = ipWithoutZone.lastIndexOf(':');
		const ipv4Part = ipWithoutZone.slice(lastColon + 1);
		const bytes = parseIPv4(ipv4Part);
		if (!bytes) return null;
		return {family: 4, bytes};
	}

	if (family === 4) {
		const bytes = parseIPv4(ipWithoutZone);
		if (!bytes) return null;
		return {family: 4, bytes};
	}

	const bytes = parseIPv6(ipWithoutZone);
	if (!bytes) return null;
	return {family: 6, bytes};
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

	const hasDoubleColon = addr.includes('::');
	let headPart = '';
	let tailPart = '';

	if (hasDoubleColon) {
		const parts = addr.split('::');
		if (parts.length !== 2) return null;
		[headPart, tailPart] = parts;
	} else {
		headPart = addr;
		tailPart = '';
	}

	const headGroups = headPart ? headPart.split(':').filter((g) => g.length > 0) : [];
	const tailGroups = tailPart ? tailPart.split(':').filter((g) => g.length > 0) : [];

	if (!hasDoubleColon && headGroups.length !== 8) return null;

	const totalGroups = headGroups.length + tailGroups.length;
	if (totalGroups > 8) return null;

	const zerosToInsert = 8 - totalGroups;
	const groups: Array<number> = [];

	for (const g of headGroups) {
		const value = parseInt(g, 16);
		if (!Number.isFinite(value) || value < 0 || value > 0xffff) return null;
		groups.push(value);
	}

	for (let i = 0; i < zerosToInsert; i++) {
		groups.push(0);
	}

	for (const g of tailGroups) {
		const value = parseInt(g, 16);
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

function parseCIDR(cidr: string): CidrEntry | null {
	const trimmed = cidr.trim();
	if (!trimmed) return null;

	const slashIdx = trimmed.indexOf('/');
	if (slashIdx === -1) return null;

	const ipPart = trimmed.slice(0, slashIdx).trim();
	const prefixPart = trimmed.slice(slashIdx + 1).trim();
	if (!ipPart || !prefixPart) return null;

	const prefixLength = Number(prefixPart);
	if (!Number.isInteger(prefixLength) || prefixLength < 0) return null;

	const parsedIP = parseIP(ipPart);
	if (!parsedIP) return null;

	const maxPrefix = parsedIP.family === 4 ? 32 : 128;
	if (prefixLength > maxPrefix) return null;

	const network = Buffer.from(parsedIP.bytes);
	const fullBytes = Math.floor(prefixLength / 8);
	const remainingBits = prefixLength % 8;

	if (fullBytes < network.length) {
		if (remainingBits !== 0 && fullBytes < network.length) {
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
	}

	return {
		family: parsedIP.family,
		network,
		prefixLength,
	};
}

function ipInCidr(ip: ParsedIP, cidr: CidrEntry): boolean {
	if (ip.family !== cidr.family) return false;

	const prefixLength = cidr.prefixLength;
	const bytesToCheck = Math.floor(prefixLength / 8);
	const remainingBits = prefixLength % 8;

	for (let i = 0; i < bytesToCheck; i++) {
		if (ip.bytes[i] !== cidr.network[i]) return false;
	}

	if (remainingBits > 0) {
		const mask = (0xff << (8 - remainingBits)) & 0xff;
		const idx = bytesToCheck;
		if (((ip.bytes[idx] ?? 0) & mask) !== ((cidr.network[idx] ?? 0) & mask)) {
			return false;
		}
	}

	return true;
}
