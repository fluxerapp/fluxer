// SPDX-License-Identifier: AGPL-3.0-or-later

import {BANNED_URLS_REFRESH_CHANNEL} from '@app/api/constants/ContentModeration';
import {RISK_S3_KEYS, writeLinesToS3} from '@app/api/risk/RiskBlocklistS3';
import {EXTERNAL_RESPONSE_LIMITS} from '@app/api/utils/ExternalResponseLimits';
import * as FetchUtils from '@app/api/utils/FetchUtils';
import {canonicalizeUrl} from '@app/api/utils/UrlNormalizer';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

interface FeedSource {
	url: string;
	category: string;
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

const syncUrlBlocklists: WorkerTaskHandler = async (_payload, helpers) => {
	helpers.logger.info('Starting URL blocklist sync');
	await helpers.setContextLink('/url-domain-bans');
	const {storageService, kvClient} = getWorkerDependencies();
	const results = await Promise.allSettled(
		FEED_SOURCES.map(async (source) => ({source, rawUrls: await fetchFeed(source)})),
	);
	const canonicalSet = new Set<string>();
	for (let i = 0; i < results.length; i++) {
		const result = results[i]!;
		if (result.status === 'fulfilled') {
			const {source, rawUrls} = result.value;
			let accepted = 0;
			for (const raw of rawUrls) {
				const canonical = canonicalizeUrl(raw);
				if (canonical) {
					canonicalSet.add(canonical);
					accepted++;
				}
			}
			helpers.logger.info({accepted, raw: rawUrls.length, source: source.url}, 'Processed URL feed');
		} else {
			helpers.logger.warn({source: FEED_SOURCES[i]!.url, error: result.reason}, 'Failed to fetch URL feed');
		}
	}
	if (canonicalSet.size === 0) {
		helpers.logger.warn('All feeds returned zero URLs — skipping S3 write to avoid wiping blocklist');
		return;
	}
	const count = await writeLinesToS3(storageService, RISK_S3_KEYS.feedUrls, canonicalSet);
	await kvClient.publish(BANNED_URLS_REFRESH_CHANNEL, 'refresh');
	helpers.logger.info({urls: count}, 'URL blocklist sync complete — wrote feed file to S3');
};

export default syncUrlBlocklists;
