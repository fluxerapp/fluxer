// SPDX-License-Identifier: AGPL-3.0-or-later

import {OUTBOUND_USER_AGENT} from '@/server/HTTPConstants';

const RELEASES_API = 'https://api.github.com/repos/fluxerapp/fluxer/releases';
const TAG_PREFIX = 'fluxer-release@';
const METADATA_ASSET = 'rexctl-release.json';
const CHECKSUMS_ASSET = 'checksums.txt';
const COMPOSE_ASSET_PREFIX = 'fluxer-compose-';
const CACHE_MS = 5 * 60 * 1000;
const RETRY_MS = 30 * 1000;
const FETCH_TIMEOUT_MS = 15 * 1000;
const PAGES = 3;
const CALVER = /^\d{4}\.\d{3,4}\.\d{1,6}$/;
const SHA256 = /^[0-9a-f]{64}$/;
const ENGINE_VERSION = /^\d+\.\d+\.\d+$/;
const DEFAULT_MIN_PODMAN = '5.4.0';
const DEFAULT_MIN_DOCKER = '24.0.0';
const DEFAULT_STATE_SCHEMA = 1;

export const REXCTL_TARGETS = [
	{os: 'linux', arch: 'amd64'},
	{os: 'linux', arch: 'arm64'},
	{os: 'darwin', arch: 'amd64'},
	{os: 'darwin', arch: 'arm64'},
	{os: 'windows', arch: 'amd64'},
	{os: 'windows', arch: 'arm64'},
] as const;

export interface RexctlAsset {
	readonly os: string;
	readonly arch: string;
	readonly url: string;
	readonly sha256: string;
}

export interface RexctlRelease {
	readonly version: string;
	readonly min_from: string;
	readonly schema_changed: boolean;
	readonly min_podman: string;
	readonly min_docker: string;
	readonly state_schema: number;
	readonly assets: ReadonlyArray<RexctlAsset>;
	readonly compose_url: string;
}

interface GitHubAsset {
	readonly name: string;
	readonly browser_download_url: string;
	readonly digest?: string | null;
}

interface GitHubRelease {
	readonly tag_name: string;
	readonly draft: boolean;
	readonly prerelease: boolean;
	readonly assets: ReadonlyArray<GitHubAsset>;
}

interface CacheEntry {
	value: RexctlRelease | null;
	fetchedAt: number;
	failedAt: number;
	pending: Promise<RexctlRelease | null> | null;
}

const cache = new Map<string, CacheEntry>();

export class ResolverUnavailableError extends Error {}

export function isCalVer(value: string): boolean {
	return CALVER.test(value);
}

export function compareCalVer(a: string, b: string): number {
	return a.localeCompare(b, 'en', {numeric: true});
}

function archiveName(version: string, os: string, arch: string): string {
	const extension = os === 'windows' ? 'zip' : 'tar.gz';
	return `rexctl_${version}_${os}_${arch}.${extension}`;
}

async function getJSON<T>(url: string): Promise<T | null> {
	const response = await fetch(url, {
		headers: {
			accept: 'application/vnd.github+json',
			'user-agent': OUTBOUND_USER_AGENT,
			'x-github-api-version': '2022-11-28',
		},
		signal: AbortSignal.timeout(FETCH_TIMEOUT_MS),
	});
	if (response.status === 404) {
		return null;
	}
	if (!response.ok) {
		throw new ResolverUnavailableError(`GitHub answered ${response.status} for ${url}`);
	}
	return (await response.json()) as T;
}

async function getText(url: string): Promise<string | null> {
	const response = await fetch(url, {
		headers: {'user-agent': OUTBOUND_USER_AGENT},
		redirect: 'follow',
		signal: AbortSignal.timeout(FETCH_TIMEOUT_MS),
	});
	if (!response.ok) {
		return null;
	}
	return response.text();
}

export function parseChecksums(text: string): Map<string, string> {
	const sums = new Map<string, string>();
	for (const line of text.split('\n')) {
		const match = /^([0-9a-f]{64})\s+\*?(\S+)\s*$/.exec(line.trim());
		if (match) {
			sums.set(match[2], match[1]);
		}
	}
	return sums;
}

function versionOf(release: GitHubRelease): string | null {
	if (!release.tag_name.startsWith(TAG_PREFIX)) {
		return null;
	}
	const version = release.tag_name.slice(TAG_PREFIX.length);
	return isCalVer(version) ? version : null;
}

function hasEveryArchive(release: GitHubRelease, version: string): boolean {
	const names = new Set(release.assets.map((asset) => asset.name));
	return REXCTL_TARGETS.every((t) => names.has(archiveName(version, t.os, t.arch)));
}

async function toRexctlRelease(release: GitHubRelease, version: string): Promise<RexctlRelease | null> {
	const byName = new Map(release.assets.map((asset) => [asset.name, asset]));
	let checksums: Map<string, string> | null = null;
	const assets: Array<RexctlAsset> = [];
	for (const target of REXCTL_TARGETS) {
		const asset = byName.get(archiveName(version, target.os, target.arch));
		if (!asset) {
			return null;
		}
		let sha256 = asset.digest?.startsWith('sha256:') ? asset.digest.slice('sha256:'.length) : '';
		if (!SHA256.test(sha256)) {
			if (checksums === null) {
				const sumsAsset = byName.get(CHECKSUMS_ASSET);
				const text = sumsAsset ? await getText(sumsAsset.browser_download_url) : null;
				checksums = parseChecksums(text ?? '');
			}
			sha256 = checksums.get(asset.name) ?? '';
		}
		if (!SHA256.test(sha256)) {
			return null;
		}
		assets.push({os: target.os, arch: target.arch, url: asset.browser_download_url, sha256});
	}
	let minFrom = '';
	let schemaChanged = true;
	let minPodman = DEFAULT_MIN_PODMAN;
	let minDocker = DEFAULT_MIN_DOCKER;
	let stateSchema = DEFAULT_STATE_SCHEMA;
	const metadataAsset = byName.get(METADATA_ASSET);
	if (metadataAsset) {
		const text = await getText(metadataAsset.browser_download_url);
		if (text === null) {
			throw new ResolverUnavailableError(`could not read ${METADATA_ASSET} for ${version}`);
		}
		const metadata = JSON.parse(text) as {
			min_from?: unknown;
			schema_changed?: unknown;
			min_podman?: unknown;
			min_docker?: unknown;
			state_schema?: unknown;
		};
		if (typeof metadata.min_from === 'string' && isCalVer(metadata.min_from)) {
			minFrom = metadata.min_from;
		}
		if (typeof metadata.schema_changed === 'boolean') {
			schemaChanged = metadata.schema_changed;
		}
		if (typeof metadata.min_podman === 'string' && ENGINE_VERSION.test(metadata.min_podman)) {
			minPodman = metadata.min_podman;
		}
		if (typeof metadata.min_docker === 'string' && ENGINE_VERSION.test(metadata.min_docker)) {
			minDocker = metadata.min_docker;
		}
		if (
			typeof metadata.state_schema === 'number' &&
			Number.isInteger(metadata.state_schema) &&
			metadata.state_schema > 0
		) {
			stateSchema = metadata.state_schema;
		}
	}
	return {
		version,
		min_from: minFrom,
		schema_changed: schemaChanged,
		min_podman: minPodman,
		min_docker: minDocker,
		state_schema: stateSchema,
		assets,
		compose_url: byName.get(`${COMPOSE_ASSET_PREFIX}${version}.tar.gz`)?.browser_download_url ?? '',
	};
}

async function resolveLatest(): Promise<RexctlRelease | null> {
	const candidates: Array<{release: GitHubRelease; version: string}> = [];
	for (let page = 1; page <= PAGES && candidates.length === 0; page++) {
		const releases = await getJSON<ReadonlyArray<GitHubRelease>>(`${RELEASES_API}?per_page=100&page=${page}`);
		if (!releases || releases.length === 0) {
			break;
		}
		for (const release of releases) {
			const version = versionOf(release);
			if (version && !release.draft && !release.prerelease && hasEveryArchive(release, version)) {
				candidates.push({release, version});
			}
		}
	}
	candidates.sort((a, b) => compareCalVer(b.version, a.version));
	for (const {release, version} of candidates) {
		const resolved = await toRexctlRelease(release, version);
		if (resolved) {
			return resolved;
		}
	}
	return null;
}

async function resolveVersion(version: string): Promise<RexctlRelease | null> {
	const release = await getJSON<GitHubRelease>(`${RELEASES_API}/tags/${encodeURIComponent(TAG_PREFIX + version)}`);
	if (!release || release.draft) {
		return null;
	}
	return toRexctlRelease(release, version);
}

export async function resolveRexctlRelease(version: string | null): Promise<RexctlRelease | null> {
	const key = version ?? 'latest';
	const now = Date.now();
	let entry = cache.get(key);
	if (!entry) {
		entry = {value: null, fetchedAt: 0, failedAt: 0, pending: null};
		cache.set(key, entry);
	}
	const current = entry;
	if (now - current.fetchedAt < CACHE_MS) {
		return current.value;
	}
	if (now - current.failedAt < RETRY_MS) {
		if (current.value) {
			return current.value;
		}
		throw new ResolverUnavailableError('GitHub was unreachable a moment ago');
	}
	if (!current.pending) {
		current.pending = (version ? resolveVersion(version) : resolveLatest())
			.then(
				(value) => {
					if (value || !current.value) {
						current.value = value;
					}
					current.fetchedAt = Date.now();
					return current.value;
				},
				(error: unknown) => {
					current.failedAt = Date.now();
					if (current.value) {
						return current.value;
					}
					throw error;
				},
			)
			.finally(() => {
				current.pending = null;
			});
	}
	return current.pending;
}
