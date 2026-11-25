// SPDX-License-Identifier: AGPL-3.0-or-later

import {OUTBOUND_USER_AGENT} from '@/server/HTTPConstants';

export const REPO_API = 'https://api.github.com/repos/fluxerapp/fluxer';
export const RELEASES_API = `${REPO_API}/releases`;
export const TAG_PREFIX = 'fluxer-release@';
export const CACHE_MS = 5 * 60 * 1000;
export const RETRY_MS = 30 * 1000;
export const PAGES = 3;
export const TAG_FILL = 5;
const FETCH_TIMEOUT_MS = 15 * 1000;
const CALVER = /^\d{4}\.\d{3,4}\.\d{1,6}$/;

export const REXCTL_TARGETS = [
	{os: 'linux', arch: 'amd64'},
	{os: 'linux', arch: 'arm64'},
	{os: 'darwin', arch: 'amd64'},
	{os: 'darwin', arch: 'arm64'},
	{os: 'windows', arch: 'amd64'},
	{os: 'windows', arch: 'arm64'},
] as const;

export interface GitHubAsset {
	readonly name: string;
	readonly browser_download_url: string;
	readonly digest?: string | null;
}

export interface GitHubRelease {
	readonly tag_name: string;
	readonly draft: boolean;
	readonly prerelease: boolean;
	readonly published_at?: string | null;
	readonly assets: ReadonlyArray<GitHubAsset>;
}

export class ResolverUnavailableError extends Error {}

let githubRetryAt = 0;
let rateLimitRemaining = Number.POSITIVE_INFINITY;
let rateLimitResetAt = 0;

export function githubBlocked(now: number): boolean {
	return now < githubRetryAt;
}

export function quotaLow(reserve: number, now: number): boolean {
	return rateLimitRemaining < reserve && now < rateLimitResetAt;
}

export function isCalVer(value: string): boolean {
	return CALVER.test(value);
}

export function compareCalVer(a: string, b: string): number {
	return a.localeCompare(b, 'en', {numeric: true});
}

export function archiveName(version: string, os: string, arch: string): string {
	const extension = os === 'windows' ? 'zip' : 'tar.gz';
	return `rexctl_${version}_${os}_${arch}.${extension}`;
}

export function versionOf(release: GitHubRelease): string | null {
	if (!release.tag_name.startsWith(TAG_PREFIX)) {
		return null;
	}
	const version = release.tag_name.slice(TAG_PREFIX.length);
	return isCalVer(version) ? version : null;
}

export function hasEveryArchive(release: GitHubRelease, version: string): boolean {
	const names = new Set(release.assets.map((asset) => asset.name));
	return REXCTL_TARGETS.every((t) => names.has(archiveName(version, t.os, t.arch)));
}

export function isPublished(release: GitHubRelease): string | null {
	const version = versionOf(release);
	return version && !release.draft && !release.prerelease && hasEveryArchive(release, version) ? version : null;
}

export async function getJSON<T>(url: string): Promise<T | null> {
	const response = await fetch(url, {
		headers: {
			accept: 'application/vnd.github+json',
			'user-agent': OUTBOUND_USER_AGENT,
			'x-github-api-version': '2022-11-28',
		},
		signal: AbortSignal.timeout(FETCH_TIMEOUT_MS),
	});
	const remaining = Number.parseInt(response.headers.get('x-ratelimit-remaining') ?? '', 10);
	const reset = Number.parseInt(response.headers.get('x-ratelimit-reset') ?? '', 10);
	if (Number.isFinite(remaining) && Number.isFinite(reset)) {
		rateLimitRemaining = remaining;
		rateLimitResetAt = reset * 1000;
	}
	if (response.status === 404) {
		return null;
	}
	if (!response.ok) {
		const now = Date.now();
		const exhausted = (response.status === 403 || response.status === 429) && remaining === 0;
		githubRetryAt = exhausted && rateLimitResetAt > now ? rateLimitResetAt : now + RETRY_MS;
		throw new ResolverUnavailableError(`GitHub returned ${response.status} for ${url}`);
	}
	return (await response.json()) as T;
}

export async function getText(url: string): Promise<string | null> {
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

interface GitRef {
	readonly ref: string;
}

export async function releaseTags(): Promise<Array<string>> {
	const refs = await getJSON<ReadonlyArray<GitRef>>(`${REPO_API}/git/matching-refs/tags/${TAG_PREFIX}`);
	const versions = (refs ?? [])
		.map((r) => r.ref.replace(/^refs\/tags\//, ''))
		.filter((tag) => tag.startsWith(TAG_PREFIX))
		.map((tag) => tag.slice(TAG_PREFIX.length))
		.filter(isCalVer);
	return [...new Set(versions)].sort((a, b) => compareCalVer(b, a));
}

export async function releaseByVersion(version: string): Promise<GitHubRelease | null> {
	return getJSON<GitHubRelease>(`${RELEASES_API}/tags/${encodeURIComponent(TAG_PREFIX + version)}`);
}

export async function latestRelease(): Promise<GitHubRelease | null> {
	return getJSON<GitHubRelease>(`${RELEASES_API}/latest`);
}

export async function collectReleases(
	known: Map<string, GitHubRelease | null>,
	wanted: number,
): Promise<Array<GitHubRelease>> {
	const page = (await getJSON<ReadonlyArray<GitHubRelease>>(`${RELEASES_API}?per_page=100&page=1`)) ?? [];
	for (const release of page) {
		const version = versionOf(release);
		if (version) {
			known.set(version, release);
		}
	}
	const tags = await releaseTags();
	let fill = TAG_FILL;
	for (const version of tags.slice(0, wanted)) {
		if (known.has(version) || fill === 0) {
			continue;
		}
		fill--;
		known.set(version, await releaseByVersion(version));
	}
	return [...known.values()].filter((r): r is GitHubRelease => r !== null);
}
