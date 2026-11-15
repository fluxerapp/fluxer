// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	BLOCKLIST_FEED_CATEGORIES,
	type BlocklistFeedCategory,
	COMBINED_FEED_URLS_KEY,
	deleteBlocklistS3Object,
	feedUrlsKey,
	isMissingBlocklistObjectError,
	writeLinesToS3,
} from '@app/api/blocklist/BlocklistS3';
import {Config} from '@app/api/Config';
import {BANNED_URLS_REFRESH_CHANNEL} from '@app/api/constants/ContentModeration';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import {EXTERNAL_RESPONSE_LIMITS} from '@app/api/utils/ExternalResponseLimits';
import * as FetchUtils from '@app/api/utils/FetchUtils';
import {canonicalizeUrl} from '@app/api/utils/UrlNormalizer';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler, WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';

interface FeedSource {
	url: string;
	category: BlocklistFeedCategory;
	parse: (text: string) => Array<string>;
}

const FEED_SOURCES: Array<FeedSource> = [
	{
		url: 'https://urlhaus.abuse.ch/downloads/text/',
		category: 'urlhaus',
		parse(text: string): Array<string> {
			const out: Array<string> = [];
			for (const raw of text.split('\n')) {
				const l = raw.trim();
				if (!l || l.startsWith('#') || l.startsWith('"')) continue;
				out.push(l);
			}
			return out;
		},
	},
	{
		url: 'https://data.phishtank.com/data/online-valid.csv',
		category: 'phishtank',
		parse(text: string): Array<string> {
			const out: Array<string> = [];
			let first = true;
			for (const line of text.split('\n')) {
				if (first) {
					first = false;
					continue;
				}
				const match = line.match(/^"?\d+"?,"?([^"]+)"?,/);
				if (match?.[1]) out.push(match[1]);
			}
			return out;
		},
	},
];

async function fetchFeed(source: FeedSource): Promise<Array<string>> {
	const res = await fetch(source.url, {signal: AbortSignal.timeout(120000)});
	if (!res.ok) {
		FetchUtils.discardResponseBody(res.body, res.status);
		throw new Error(`HTTP ${res.status} fetching ${source.url}`);
	}
	const text = await FetchUtils.streamToStringWithLimit(res.body, {
		maxBytes: EXTERNAL_RESPONSE_LIMITS.urlBlocklistBytes,
		headers: res.headers,
		url: res.url,
		description: 'URL blocklist feed',
	});
	return source.parse(text);
}

async function deleteFeedFile(
	storageService: IStorageService,
	key: string,
	logger: WorkerTaskHelpers['logger'],
): Promise<void> {
	try {
		await deleteBlocklistS3Object(storageService, key);
	} catch (error) {
		if (!isMissingBlocklistObjectError(error)) {
			logger.warn({error, key}, 'Failed to delete the URL blocklist feed file');
		}
	}
}

const syncUrlBlocklists: WorkerTaskHandler = async (_payload, helpers) => {
	helpers.logger.info('Starting URL blocklist sync');
	await helpers.setContextLink('/url-domain-bans');
	const {storageService, kvClient} = getWorkerDependencies();
	if (!Config.blocklistFeeds.enabled) {
		for (const key of [...BLOCKLIST_FEED_CATEGORIES.map(feedUrlsKey), COMBINED_FEED_URLS_KEY]) {
			await deleteFeedFile(storageService, key, helpers.logger);
		}
		await kvClient.publish(BANNED_URLS_REFRESH_CHANNEL, 'refresh');
		return;
	}
	const results = await Promise.allSettled(
		FEED_SOURCES.map(async (source) => ({source, rawUrls: await fetchFeed(source)})),
	);
	let writtenFeeds = 0;
	let writtenUrls = 0;
	for (let i = 0; i < results.length; i++) {
		const result = results[i]!;
		if (result.status === 'rejected') {
			helpers.logger.warn({source: FEED_SOURCES[i]!.url, error: result.reason}, 'Failed to fetch URL feed');
			continue;
		}
		const {source, rawUrls} = result.value;
		const canonicalSet = new Set<string>();
		for (const raw of rawUrls) {
			const canonical = canonicalizeUrl(raw);
			if (canonical) canonicalSet.add(canonical);
		}
		helpers.logger.info({accepted: canonicalSet.size, raw: rawUrls.length, source: source.url}, 'Processed URL feed');
		if (canonicalSet.size === 0) {
			helpers.logger.warn({source: source.url}, 'URL feed returned zero URLs, keeping its previous feed file');
			continue;
		}
		writtenUrls += await writeLinesToS3(storageService, feedUrlsKey(source.category), canonicalSet);
		writtenFeeds++;
	}
	if (writtenFeeds === 0) {
		helpers.logger.warn('All feeds returned zero URLs — skipping S3 write to avoid wiping blocklist');
		return;
	}
	await deleteFeedFile(storageService, COMBINED_FEED_URLS_KEY, helpers.logger);
	await kvClient.publish(BANNED_URLS_REFRESH_CHANNEL, 'refresh');
	helpers.logger.info({feeds: writtenFeeds, urls: writtenUrls}, 'URL blocklist sync complete — wrote feed files to S3');
};

export default syncUrlBlocklists;
