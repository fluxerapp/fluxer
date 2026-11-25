// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	archiveName,
	CACHE_MS,
	compareCalVer,
	type GitHubAsset,
	type GitHubRelease,
	getJSON,
	getText,
	githubBlocked,
	isCalVer,
	isPublished,
	latestRelease,
	PAGES,
	quotaLow,
	RELEASES_API,
	RETRY_MS,
	REXCTL_TARGETS,
	ResolverUnavailableError,
	TAG_PREFIX,
} from '@/installer/GitHubReleases';

const METADATA_ASSET = 'rexctl-release.json';
const CHECKSUMS_ASSET = 'checksums.txt';
const COMPOSE_ASSET_PREFIX = 'fluxer-compose-';
const MAX_ENTRIES = 256;
const PINNED_RESERVE = 15;
const SHA256 = /^[0-9a-f]{64}$/;
const ENGINE_VERSION = /^\d+\.\d+\.\d+$/;
const DEFAULT_MIN_PODMAN = '5.4.0';
const DEFAULT_MIN_DOCKER = '24.0.0';
const DEFAULT_STATE_SCHEMA = 1;

const NATIVE_ARCHES = ['amd64', 'arm64'] as const;

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
	readonly dist: ReadonlyArray<RexctlAsset>;
	readonly compose_url: string;
	readonly compose_last: string;
}

interface CacheEntry {
	value: RexctlRelease | null;
	fetchedAt: number;
	failedAt: number;
	pending: Promise<RexctlRelease | null> | null;
}

const cache = new Map<string, CacheEntry>();

function nativeName(version: string, arch: string): string {
	return `fluxer-${version}-linux-${arch}.tar.zst`;
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

async function toRexctlRelease(release: GitHubRelease, version: string): Promise<RexctlRelease | null> {
	const byName = new Map(release.assets.map((asset) => [asset.name, asset]));
	let checksums: Map<string, string> | null = null;
	const sha256Of = async (asset: GitHubAsset): Promise<string | null> => {
		let sha256 = asset.digest?.startsWith('sha256:') ? asset.digest.slice('sha256:'.length) : '';
		if (!SHA256.test(sha256)) {
			if (checksums === null) {
				const sumsAsset = byName.get(CHECKSUMS_ASSET);
				const text = sumsAsset ? await getText(sumsAsset.browser_download_url) : null;
				checksums = parseChecksums(text ?? '');
			}
			sha256 = checksums.get(asset.name) ?? '';
		}
		return SHA256.test(sha256) ? sha256 : null;
	};
	const assets: Array<RexctlAsset> = [];
	for (const target of REXCTL_TARGETS) {
		const asset = byName.get(archiveName(version, target.os, target.arch));
		const sha256 = asset ? await sha256Of(asset) : null;
		if (!asset || sha256 === null) {
			return null;
		}
		assets.push({os: target.os, arch: target.arch, url: asset.browser_download_url, sha256});
	}
	const dist: Array<RexctlAsset> = [];
	for (const arch of NATIVE_ARCHES) {
		const asset = byName.get(nativeName(version, arch));
		const sha256 = asset ? await sha256Of(asset) : null;
		if (asset && sha256 !== null) {
			dist.push({os: 'linux', arch, url: asset.browser_download_url, sha256});
		}
	}
	let minFrom = '';
	let schemaChanged = true;
	let minPodman = DEFAULT_MIN_PODMAN;
	let minDocker = DEFAULT_MIN_DOCKER;
	let stateSchema = DEFAULT_STATE_SCHEMA;
	let composeLast = '';
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
			compose_last?: unknown;
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
		if (typeof metadata.compose_last === 'string' && isCalVer(metadata.compose_last)) {
			composeLast = metadata.compose_last;
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
		dist,
		compose_url: byName.get(`${COMPOSE_ASSET_PREFIX}${version}.tar.gz`)?.browser_download_url ?? '',
		compose_last: composeLast,
	};
}

async function resolveLatest(): Promise<RexctlRelease | null> {
	const marked = await latestRelease();
	const markedVersion = marked ? isPublished(marked) : null;
	if (marked && markedVersion) {
		const resolved = await toRexctlRelease(marked, markedVersion);
		if (resolved) {
			return resolved;
		}
	}
	const candidates: Array<{release: GitHubRelease; version: string}> = [];
	for (let page = 1; page <= PAGES && candidates.length === 0; page++) {
		const releases = await getJSON<ReadonlyArray<GitHubRelease>>(`${RELEASES_API}?per_page=100&page=${page}`);
		if (!releases || releases.length === 0) {
			break;
		}
		for (const release of releases) {
			const version = isPublished(release);
			if (version) {
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

function evictOldest(): void {
	if (cache.size < MAX_ENTRIES) {
		return;
	}
	for (const [key, entry] of cache) {
		if (key !== 'latest' && !entry.pending) {
			cache.delete(key);
			return;
		}
	}
}

export async function resolveRexctlRelease(version: string | null): Promise<RexctlRelease | null> {
	const key = version ?? 'latest';
	const now = Date.now();
	let entry = cache.get(key);
	if (!entry) {
		entry = {value: null, fetchedAt: 0, failedAt: 0, pending: null};
		evictOldest();
		cache.set(key, entry);
	}
	const current = entry;
	if (now - current.fetchedAt < CACHE_MS) {
		return current.value;
	}
	const quotaReserved = version !== null && quotaLow(PINNED_RESERVE, now);
	if (!current.pending && (now - current.failedAt < RETRY_MS || githubBlocked(now) || quotaReserved)) {
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
