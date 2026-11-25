// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	CACHE_MS,
	collectReleases,
	compareCalVer,
	type GitHubRelease,
	githubBlocked,
	isPublished,
	RETRY_MS,
	ResolverUnavailableError,
} from '@/installer/GitHubReleases';

export const FEED_SCHEMA = 1;
const FEED_SIZE = 50;

export interface FeedRelease {
	readonly version: string;
	readonly date: string;
}

export interface Feed {
	readonly schema: typeof FEED_SCHEMA;
	readonly latest: string;
	readonly releases: ReadonlyArray<FeedRelease>;
}

function buildFeed(releases: ReadonlyArray<GitHubRelease>): Feed {
	const seen = new Set<string>();
	const out: Array<FeedRelease> = [];
	const published = releases
		.map((release) => ({release, version: isPublished(release)}))
		.filter((r): r is {release: GitHubRelease; version: string} => r.version !== null)
		.sort((a, b) => compareCalVer(b.version, a.version));
	for (const {release, version} of published) {
		if (seen.has(version) || out.length >= FEED_SIZE) {
			continue;
		}
		seen.add(version);
		out.push({version, date: release.published_at ?? ''});
	}
	return {schema: FEED_SCHEMA, latest: out[0]?.version ?? '', releases: out};
}

function escapeXML(value: string): string {
	return value.replace(/[<>&'"]/g, (c) => `&#${c.charCodeAt(0)};`);
}

export function feedAtom(feed: Feed, self: string): string {
	const updated = feed.releases[0]?.date || new Date(0).toISOString();
	const guide = new URL('operator/upgrades/', self).toString();
	const entries = feed.releases.map((r) =>
		[
			'  <entry>',
			`    <id>${escapeXML(`${self}#${r.version}`)}</id>`,
			`    <title>Fluxer ${escapeXML(r.version)}</title>`,
			`    <updated>${escapeXML(r.date || updated)}</updated>`,
			`    <link rel="alternate" href="${escapeXML(guide)}"/>`,
			'  </entry>',
		].join('\n'),
	);
	return [
		'<?xml version="1.0" encoding="utf-8"?>',
		'<feed xmlns="http://www.w3.org/2005/Atom">',
		`  <id>${escapeXML(self)}</id>`,
		'  <title>Fluxer releases</title>',
		`  <updated>${escapeXML(updated)}</updated>`,
		`  <link rel="self" href="${escapeXML(self)}"/>`,
		'  <link rel="alternate" href="https://github.com/fluxerapp/fluxer/releases"/>',
		'  <author><name>Fluxer</name></author>',
		...entries,
		'</feed>',
		'',
	].join('\n');
}

interface FeedCache {
	value: Feed | null;
	fetchedAt: number;
	failedAt: number;
	pending: Promise<Feed> | null;
}

const cache: FeedCache = {value: null, fetchedAt: 0, failedAt: 0, pending: null};
const known = new Map<string, GitHubRelease | null>();

function keepFeed(previous: Feed | null, next: Feed): Feed {
	if (previous && previous.releases.length > 0 && next.releases.length === 0) {
		return previous;
	}
	return next;
}

export async function releaseFeed(): Promise<Feed> {
	const now = Date.now();
	if (cache.value && now - cache.fetchedAt < CACHE_MS) {
		return cache.value;
	}
	if (!cache.pending && (now - cache.failedAt < RETRY_MS || githubBlocked(now))) {
		if (cache.value) {
			return cache.value;
		}
		throw new ResolverUnavailableError('GitHub was unreachable a moment ago');
	}
	if (!cache.pending) {
		cache.pending = collectReleases(known, FEED_SIZE)
			.then(
				(releases) => {
					cache.value = keepFeed(cache.value, buildFeed(releases));
					cache.fetchedAt = Date.now();
					return cache.value;
				},
				(error: unknown) => {
					cache.failedAt = Date.now();
					if (cache.value) {
						return cache.value;
					}
					throw error;
				},
			)
			.finally(() => {
				cache.pending = null;
			});
	}
	return cache.pending;
}
