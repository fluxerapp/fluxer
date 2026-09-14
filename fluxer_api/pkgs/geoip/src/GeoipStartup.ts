// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {createWriteStream} from 'node:fs';
import fs from 'node:fs/promises';
import path from 'node:path';
import {Readable} from 'node:stream';
import {pipeline} from 'node:stream/promises';
import {GetObjectCommand, S3Client} from '@aws-sdk/client-s3';

const GEOIP_DOWNLOAD_PATH_QUERY_PARAM = 'download_path';
const GEOIP_ASN_DOWNLOAD_PATH_QUERY_PARAM = 'asn_download_path';
const GEOIP_ASN_KEY_QUERY_PARAM = 'asn_key';
const DEFAULT_GEOIP_TEMPORARY_DIRECTORY = '/tmp/fluxer/geoip';
const DEFAULT_GEOIP_ASN_DB_BASENAME = 'GeoLite2-ASN.mmdb';

type GeoipSourceMode = 'filesystem' | 's3';

interface GeoipFilesystemSourceConfig {
	mode: 'filesystem';
	maxmindDbPath?: string;
	maxmindAsnDbPath?: string;
}

interface GeoipS3SourceConfig {
	mode: 's3';
	maxmindDbPath: string;
	maxmindAsnDbPath?: string;
	s3Bucket: string;
	s3Key: string;
	s3AsnKey?: string;
}

type GeoipSourceConfig = GeoipFilesystemSourceConfig | GeoipS3SourceConfig;

interface GeoipS3ConnectionConfig {
	endpoint: string;
	region: string;
	accessKeyId: string;
	secretAccessKey: string;
}

interface GeoipDownloadedDatabase {
	readonly path: string;
	readonly sizeBytes: number;
	readonly upstream: string;
}

interface GeoipStartupResult {
	mode: GeoipSourceMode;
	downloaded: boolean;
	city?: GeoipDownloadedDatabase;
	asn?: GeoipDownloadedDatabase;
	maxmindDbPath?: string;
	bucket?: string;
	key?: string;
}

interface EnsureGeoipStartupOptions {
	geoip: GeoipSourceConfig;
	s3Config?: GeoipS3ConnectionConfig;
}

interface GeoipRuntimePathOptions {
	serviceName: string;
	temporaryDirectory?: string;
}

export function parseGeoipSourceConfig(rawValue: string | undefined): GeoipSourceConfig {
	if (!rawValue || !rawValue.startsWith('s3://')) {
		return createGeoipFilesystemSourceConfig(rawValue);
	}
	return parseGeoipS3SourceConfig(rawValue);
}

export function resolveGeoipRuntimeSourceConfig(
	sourceConfig: GeoipSourceConfig,
	options: GeoipRuntimePathOptions,
): GeoipSourceConfig {
	if (sourceConfig.mode !== 's3') {
		return sourceConfig;
	}
	const temporaryDirectory = options.temporaryDirectory ?? DEFAULT_GEOIP_TEMPORARY_DIRECTORY;
	requireGeoipRuntimePathOptions(options.serviceName, temporaryDirectory);
	const serviceDir = path.join(temporaryDirectory, options.serviceName);
	const resolvedCityPath = path.join(serviceDir, path.basename(sourceConfig.maxmindDbPath));
	const resolvedAsnPath = sourceConfig.s3AsnKey
		? path.join(serviceDir, path.basename(sourceConfig.maxmindAsnDbPath ?? sourceConfig.s3AsnKey))
		: sourceConfig.maxmindAsnDbPath;
	return {
		...sourceConfig,
		maxmindDbPath: resolvedCityPath,
		maxmindAsnDbPath: resolvedAsnPath,
	};
}

export async function ensureGeoipDatabaseOnStartup(options: EnsureGeoipStartupOptions): Promise<GeoipStartupResult> {
	const {geoip} = options;
	if (geoip.mode === 'filesystem') {
		return {
			mode: 'filesystem',
			downloaded: false,
			maxmindDbPath: geoip.maxmindDbPath,
		};
	}
	return ensureS3Startup(geoip, options.s3Config);
}

async function ensureS3Startup(
	geoip: GeoipS3SourceConfig,
	s3Config: GeoipS3ConnectionConfig | undefined,
): Promise<GeoipStartupResult> {
	const resolvedS3Config = requireGeoipS3ConnectionConfig(s3Config);
	await fs.mkdir(path.dirname(geoip.maxmindDbPath), {recursive: true});
	if (geoip.maxmindAsnDbPath) {
		await fs.mkdir(path.dirname(geoip.maxmindAsnDbPath), {recursive: true});
	}
	const client = new S3Client({
		endpoint: resolvedS3Config.endpoint,
		region: resolvedS3Config.region,
		forcePathStyle: true,
		credentials: {
			accessKeyId: resolvedS3Config.accessKeyId,
			secretAccessKey: resolvedS3Config.secretAccessKey,
		},
		requestChecksumCalculation: 'WHEN_REQUIRED',
		responseChecksumValidation: 'WHEN_REQUIRED',
	});
	try {
		const city = await downloadS3Object(client, geoip.s3Bucket, geoip.s3Key, geoip.maxmindDbPath);
		let asn: GeoipDownloadedDatabase | undefined;
		if (geoip.s3AsnKey && geoip.maxmindAsnDbPath) {
			asn = await downloadS3Object(client, geoip.s3Bucket, geoip.s3AsnKey, geoip.maxmindAsnDbPath);
		}
		return {
			mode: 's3',
			downloaded: true,
			city,
			asn,
			maxmindDbPath: geoip.maxmindDbPath,
			bucket: geoip.s3Bucket,
			key: geoip.s3Key,
		};
	} finally {
		client.destroy();
	}
}

async function downloadS3Object(
	client: S3Client,
	bucket: string,
	key: string,
	destination: string,
): Promise<GeoipDownloadedDatabase> {
	const tempPath = `${destination}.tmp-${process.pid}-${Date.now()}`;
	try {
		const response = await client.send(new GetObjectCommand({Bucket: bucket, Key: key}));
		const body = response.Body;
		assert(body != null && body instanceof Readable, 'GeoIP S3 response body is not a readable stream');
		await pipeline(body, createWriteStream(tempPath));
		await fs.rename(tempPath, destination);
	} catch (error) {
		await fs.rm(tempPath, {force: true}).catch(() => undefined);
		throw new Error(`Failed to download GeoIP database from s3://${bucket}/${key}`, {cause: error});
	}
	const stat = await fs.stat(destination);
	return {path: destination, sizeBytes: stat.size, upstream: `s3://${bucket}/${key}`};
}

function createGeoipFilesystemSourceConfig(rawValue: string | undefined): GeoipFilesystemSourceConfig {
	const maxmindDbPath = rawValue === '' ? undefined : rawValue;
	return {
		mode: 'filesystem',
		maxmindDbPath,
		maxmindAsnDbPath: maxmindDbPath ? path.join(path.dirname(maxmindDbPath), DEFAULT_GEOIP_ASN_DB_BASENAME) : undefined,
	};
}

function parseGeoipS3SourceConfig(rawValue: string): GeoipS3SourceConfig {
	const sourceUrl = parseGeoipS3Url(rawValue);
	const s3Bucket = sourceUrl.hostname;
	if (!s3Bucket) {
		throw new Error(`Invalid GeoIP S3 URL (missing bucket): ${rawValue}`);
	}
	const s3Key = decodeURIComponent(sourceUrl.pathname.replace(/^\/+/, ''));
	if (!s3Key) {
		throw new Error(`Invalid GeoIP S3 URL (missing object key): ${rawValue}`);
	}
	const maxmindDbPath = resolveGeoipDownloadPath(sourceUrl, rawValue);
	const {s3AsnKey, maxmindAsnDbPath} = resolveGeoipAsnPaths(sourceUrl, maxmindDbPath, rawValue);
	return {
		mode: 's3',
		maxmindDbPath,
		maxmindAsnDbPath,
		s3Bucket,
		s3Key,
		s3AsnKey,
	};
}

function resolveGeoipDownloadPath(sourceUrl: URL, rawValue: string): string {
	const configuredDownloadPath = sourceUrl.searchParams.get(GEOIP_DOWNLOAD_PATH_QUERY_PARAM);
	if (!configuredDownloadPath) {
		throw new Error(`Invalid GeoIP S3 URL (missing query parameter "${GEOIP_DOWNLOAD_PATH_QUERY_PARAM}"): ${rawValue}`);
	}
	if (!path.isAbsolute(configuredDownloadPath)) {
		throw new Error(
			`GeoIP S3 URL query parameter "${GEOIP_DOWNLOAD_PATH_QUERY_PARAM}" must be an absolute path: ${rawValue}`,
		);
	}
	return configuredDownloadPath;
}

function resolveGeoipAsnPaths(
	sourceUrl: URL,
	cityDownloadPath: string,
	rawValue: string,
): {
	s3AsnKey?: string;
	maxmindAsnDbPath?: string;
} {
	const asnKey = sourceUrl.searchParams.get(GEOIP_ASN_KEY_QUERY_PARAM) ?? undefined;
	if (!asnKey) return {};
	const explicitAsnDownloadPath = sourceUrl.searchParams.get(GEOIP_ASN_DOWNLOAD_PATH_QUERY_PARAM);
	if (explicitAsnDownloadPath && !path.isAbsolute(explicitAsnDownloadPath)) {
		throw new Error(
			`GeoIP S3 URL query parameter "${GEOIP_ASN_DOWNLOAD_PATH_QUERY_PARAM}" must be an absolute path: ${rawValue}`,
		);
	}
	const maxmindAsnDbPath = explicitAsnDownloadPath ?? path.join(path.dirname(cityDownloadPath), path.basename(asnKey));
	return {s3AsnKey: asnKey, maxmindAsnDbPath};
}

function requireGeoipS3ConnectionConfig(s3Config: GeoipS3ConnectionConfig | undefined): GeoipS3ConnectionConfig {
	if (!s3Config) {
		throw new Error('GeoIP is configured for S3 mode, but S3 configuration is missing.');
	}
	requireS3ConfigValue(s3Config.endpoint, 'GeoIP S3 startup mode requires s3.endpoint.');
	requireS3ConfigValue(s3Config.region, 'GeoIP S3 startup mode requires s3.region.');
	requireS3ConfigValue(s3Config.accessKeyId, 'GeoIP S3 startup mode requires s3.access_key_id.');
	requireS3ConfigValue(s3Config.secretAccessKey, 'GeoIP S3 startup mode requires s3.secret_access_key.');
	return s3Config;
}

function requireS3ConfigValue(value: string, errorMessage: string): void {
	if (!value) {
		throw new Error(errorMessage);
	}
}

function requireGeoipRuntimePathOptions(serviceName: string, temporaryDirectory: string): void {
	if (!serviceName) {
		throw new Error('GeoIP runtime path options must include a non-empty serviceName.');
	}
	if (!path.isAbsolute(temporaryDirectory)) {
		throw new Error(`GeoIP temporary directory must be an absolute path: ${temporaryDirectory}`);
	}
}

function parseGeoipS3Url(rawValue: string): URL {
	try {
		return new URL(rawValue);
	} catch {
		throw new Error(`Invalid GeoIP S3 URL: ${rawValue}`);
	}
}
