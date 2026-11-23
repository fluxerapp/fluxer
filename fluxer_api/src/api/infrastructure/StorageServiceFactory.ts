// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {ChangeFeedStorageService} from '@app/api/infrastructure/ChangeFeedStorageService';
import {type DualStoreCounters, DualStoreStorageService} from '@app/api/infrastructure/DualStoreStorageService';
import {FilesystemStorageService} from '@app/api/infrastructure/FilesystemStorageService';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import {JetStreamStorageChangeSink, StorageChangeFeed} from '@app/api/infrastructure/StorageChangeFeed';
import {StorageService} from '@app/api/infrastructure/StorageService';
import {parseBucketMap, StoreBucketResolver} from '@app/api/infrastructure/StoreBucketMap';
import {Logger} from '@app/api/Logger';
import {Counter, registerMetricsSection} from '@fluxer/hono/src/middleware/Metrics';

const CHANGE_FEED_SHUTDOWN_TIMEOUT_MS = 5000;

let changeFeed: StorageChangeFeed | null = null;

function withChangeFeed(service: IStorageService): IStorageService {
	const {enabled, stream, skipBuckets} = Config.storageChangeFeed;
	if (!enabled) {
		return service;
	}
	changeFeed ??= new StorageChangeFeed(
		new JetStreamStorageChangeSink({url: Config.nats.jetStreamUrl, token: Config.nats.authToken, stream}),
	);
	return new ChangeFeedStorageService(service, changeFeed, skipBuckets);
}

export function createStorageService(): IStorageService {
	if (Config.storage.backend === 'filesystem') {
		return withChangeFeed(
			new FilesystemStorageService({
				root: Config.storage.root,
				buckets: Config.s3.buckets,
				readOnly: Config.storage.readOnly,
				logger: Logger,
			}),
		);
	}
	return withChangeFeed(createS3StorageService());
}

const fallbackHits = new Counter();
const doubleMisses = new Counter();
const storeErrors = new Counter();
let metricsRegistered = false;

function labelValue(value: string): string {
	return value.replaceAll('\\', '\\\\').replaceAll('"', '\\"');
}

const dualStoreCounters: DualStoreCounters = {
	fallbackHit: (bucket, op, storeId) =>
		fallbackHits.inc(`bucket="${labelValue(bucket)}",op="${op}",store="${labelValue(storeId)}"`),
	doubleMiss: (bucket, op) => doubleMisses.inc(`bucket="${labelValue(bucket)}",op="${op}"`),
	storeError: (bucket, op, storeId) =>
		storeErrors.inc(`bucket="${labelValue(bucket)}",op="${op}",store="${labelValue(storeId)}"`),
};

function registerDualStoreMetrics(): void {
	if (metricsRegistered) return;
	metricsRegistered = true;
	registerMetricsSection(() =>
		[
			fallbackHits.render('fluxer_api_storage_fallback_hit_total', 'Reads served by the fallback object store'),
			doubleMisses.render('fluxer_api_storage_fallback_double_miss_total', 'Reads missing on both object stores'),
			storeErrors.render('fluxer_api_storage_store_errors_total', 'Object store errors seen by the dual store'),
		].join('\n'),
	);
}

function createS3StorageService(): IStorageService {
	const s3 = Config.s3;
	if (!s3.dualStoreEnabled || !s3.fallback) {
		return new StorageService();
	}
	const fallback = s3.fallback;
	const primaryMap = new Map<string, string>([
		['cdn', s3.buckets.cdn],
		['uploads', s3.buckets.uploads],
		['reports', s3.buckets.reports],
		['harvests', s3.buckets.harvests],
	]);
	registerDualStoreMetrics();
	Logger.info(
		{primary: s3.storeId, fallback: fallback.storeId, fallbackReads: s3.fallbackReadsEnabled},
		'Dual object store enabled',
	);
	return new DualStoreStorageService({
		primary: new StorageService(),
		primaryStoreId: s3.storeId,
		fallback: new StorageService({
			endpoint: fallback.endpoint,
			presignedUrlBase: fallback.presignedUrlBase,
			forcePathStyle: fallback.forcePathStyle,
			region: fallback.region,
			accessKeyId: fallback.accessKeyId,
			secretAccessKey: fallback.secretAccessKey,
		}),
		fallbackStoreId: fallback.storeId,
		buckets: new StoreBucketResolver(primaryMap, parseBucketMap(fallback.bucketMap)),
		cdnBucket: s3.buckets.cdn,
		fallbackReadsEnabled: s3.fallbackReadsEnabled,
		counters: dualStoreCounters,
	});
}

export async function shutdownStorageChangeFeed(): Promise<void> {
	const feed = changeFeed;
	changeFeed = null;
	await feed?.close(CHANGE_FEED_SHUTDOWN_TIMEOUT_MS);
}
