// SPDX-License-Identifier: AGPL-3.0-or-later

const LOW_PERCENT = 10;
const FULL_PERCENT = 1;
const FULL_INODES_CAP = 10_000;
const FULL_BYTES_CAP = 1_000_000_000;

type StorageSpaceLevel = 'ok' | 'low' | 'full';

export interface StorageSpaceStats {
	bsize: number;
	blocks: number;
	bavail: number;
	files: number;
	ffree: number;
}

export interface StorageSpace {
	level: StorageSpaceLevel;
	detail: string;
	freeBytes: number;
	totalBytes: number;
	freeInodes: number;
	totalInodes: number;
}

function levelFor(total: number, free: number, fullCap: number): StorageSpaceLevel {
	if (total <= 0) return 'ok';
	if (free < Math.min((total * FULL_PERCENT) / 100, fullCap)) return 'full';
	if (free < (total * LOW_PERCENT) / 100) return 'low';
	return 'ok';
}

const RANK: Record<StorageSpaceLevel, number> = {ok: 0, low: 1, full: 2};

function percent(free: number, total: number): string {
	return `${((free * 100) / total).toFixed(1)}%`;
}

const UNITS = ['bytes', 'kB', 'MB', 'GB', 'TB', 'PB'];

function size(bytes: number): string {
	let value = bytes;
	let unit = 0;
	while (value >= 1000 && unit < UNITS.length - 1) {
		value /= 1000;
		unit++;
	}
	return unit === 0 ? `${value} bytes` : `${value.toFixed(1)} ${UNITS[unit]}`;
}

export function storageSpace(root: string, stats: StorageSpaceStats): StorageSpace {
	const totalBytes = stats.blocks * stats.bsize;
	const freeBytes = stats.bavail * stats.bsize;
	const inodes = levelFor(stats.files, stats.ffree, FULL_INODES_CAP);
	const bytes = levelFor(totalBytes, freeBytes, FULL_BYTES_CAP);
	const level = RANK[inodes] >= RANK[bytes] ? inodes : bytes;
	const parts: Array<string> = [];
	if (stats.files > 0) {
		parts.push(`${stats.ffree} of ${stats.files} inodes free (${percent(stats.ffree, stats.files)})`);
	}
	if (totalBytes > 0) {
		parts.push(`${size(freeBytes)} of ${size(totalBytes)} free (${percent(freeBytes, totalBytes)})`);
	}
	return {
		level,
		detail: `${root} has ${parts.join(' and ')}`,
		freeBytes,
		totalBytes,
		freeInodes: stats.ffree,
		totalInodes: stats.files,
	};
}
