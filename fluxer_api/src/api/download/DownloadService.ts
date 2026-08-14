// SPDX-License-Identifier: AGPL-3.0-or-later

import {posix} from 'node:path';
import {Readable} from 'node:stream';
import {S3ServiceException} from '@aws-sdk/client-s3';
import type {
	DesktopArch,
	DesktopChannel,
	DesktopFormat,
	DesktopPlatform,
} from '@fluxer/schema/src/domains/download/DownloadSchemas';
import {Config} from '../Config';
import type {IStorageService} from '../infrastructure/IStorageService';
import {isJsonRecord, parseJsonUnknown} from '../utils/JsonBoundaryUtils';

export const DOWNLOAD_PREFIX = '/dl';
export const DESKTOP_REDIRECT_PREFIX = `${DOWNLOAD_PREFIX}/desktop`;

export class UnsatisfiableRangeError extends Error {
	constructor(public readonly totalSize: number) {
		super('Range Not Satisfiable');
		this.name = 'UnsatisfiableRangeError';
	}
}

export interface DownloadStreamResult {
	body: Readable;
	contentLength: number;
	contentRange?: string | null;
	contentType?: string | null;
	cacheControl?: string | null;
	contentDisposition?: string | null;
	etag?: string | null;
	lastModified?: Date | null;
}

function isStorageNotFoundError(error: unknown): boolean {
	return error instanceof S3ServiceException && (error.name === 'NoSuchKey' || error.name === 'NotFound');
}

function isUnsatisfiableRangeError(error: unknown): boolean {
	return (
		error instanceof S3ServiceException && (error.name === 'InvalidRange' || error.$metadata?.httpStatusCode === 416)
	);
}
const DESKTOP_BUCKET_PREFIX = 'desktop';
const DESKTOP_TEST_BUCKET_PREFIX = 'desktop-test';
const DEFAULT_API_CLIENT_BASE_URL = 'https://api.fluxer.app';

function desktopBucketPrefix(test?: boolean): string {
	return test ? DESKTOP_TEST_BUCKET_PREFIX : DESKTOP_BUCKET_PREFIX;
}

function desktopArtifactPrefix(params: {
	channel: DesktopChannel;
	plat: DesktopPlatform;
	arch: DesktopArch;
	test?: boolean;
}): string | null {
	return `${desktopBucketPrefix(params.test)}/${params.channel}/${params.plat}/${params.arch}`;
}

type DesktopManifestFileEntry =
	| string
	| {
			filename: string;
			sha256: string;
	  };
type DesktopManifest = {
	channel: DesktopChannel;
	platform: DesktopPlatform;
	arch: DesktopArch;
	version: string;
	pub_date: string;
	minimum_system_version?: string | null;
	files: Record<string, DesktopManifestFileEntry>;
};
type FormatMapping = {
	ext: string;
	arch: Record<'x64' | 'arm64', string | Array<string>>;
};

const FORMAT_MAPPINGS: Record<DesktopFormat, Partial<Record<DesktopPlatform, FormatMapping>>> = {
	setup: {win32: {ext: '.exe', arch: {x64: 'x64', arm64: 'arm64'}}},
	dmg: {darwin: {ext: '.dmg', arch: {x64: ['universal', 'x64'], arm64: ['universal', 'arm64']}}},
	zip: {darwin: {ext: '.zip', arch: {x64: ['universal', 'x64'], arm64: ['universal', 'arm64']}}},
	appimage: {linux: {ext: '.AppImage', arch: {x64: 'x86_64', arm64: ['aarch64', 'arm64']}}},
	deb: {linux: {ext: '.deb', arch: {x64: 'amd64', arm64: 'arm64'}}},
	rpm: {linux: {ext: '.rpm', arch: {x64: 'x86_64', arm64: 'aarch64'}}},
	tar_gz: {linux: {ext: '.tar.gz', arch: {x64: 'x64', arm64: 'arm64'}}},
	portable: {win32: {ext: '.zip', arch: {x64: 'x64', arm64: 'arm64'}}},
};
const MODERN_PLATFORM_TOKENS: Record<DesktopPlatform, 'win' | 'mac' | 'linux'> = {
	win32: 'win',
	darwin: 'mac',
	linux: 'linux',
};

type VersionFile = {
	url: string;
	sha256: string | null;
	checksum_url: string | null;
};
type VersionInfo = {
	version: string;
	pub_date: string;
	minimum_system_version?: string | null;
	files: Record<string, VersionFile>;
};
export type DesktopChecksumFile = {
	filename: string;
	sha256: string;
	body: string;
};
function isDesktopManifestFileEntry(value: unknown): value is DesktopManifestFileEntry {
	if (typeof value === 'string') {
		return true;
	}
	return isJsonRecord(value) && typeof value.filename === 'string' && typeof value.sha256 === 'string';
}

function isDesktopManifest(value: unknown): value is DesktopManifest {
	if (!isJsonRecord(value) || !isJsonRecord(value.files)) return false;
	return (
		(value.channel === 'stable' || value.channel === 'canary') &&
		(value.platform === 'win32' || value.platform === 'darwin' || value.platform === 'linux') &&
		(value.arch === 'x64' || value.arch === 'arm64') &&
		typeof value.version === 'string' &&
		typeof value.pub_date === 'string' &&
		(value.minimum_system_version === undefined ||
			value.minimum_system_version === null ||
			typeof value.minimum_system_version === 'string') &&
		Object.values(value.files).every(isDesktopManifestFileEntry)
	);
}

interface LatestFilenameLookupParams {
	channel: DesktopChannel;
	plat: DesktopPlatform;
	arch: DesktopArch;
	format: DesktopFormat;
	test?: boolean;
}

interface ManifestFilenameResolutionParams extends LatestFilenameLookupParams {
	filename: string;
}

export class DownloadService {
	constructor(private readonly storageService: IStorageService) {}

	async resolveLatestDesktopKey(params: {
		channel: DesktopChannel;
		plat: DesktopPlatform;
		arch: DesktopArch;
		format: DesktopFormat;
		test?: boolean;
	}): Promise<string | null> {
		const prefix = desktopArtifactPrefix(params);
		if (!prefix) {
			return null;
		}
		const manifestKey = `${prefix}/manifest.json`;
		try {
			const manifest = await this.readJsonObjectFromStorage(manifestKey);
			if (!isDesktopManifest(manifest)) {
				return this.resolveLatestDesktopKeyFromObjects(params);
			}
			const entry = manifest.files[params.format];
			if (!entry) {
				return this.resolveLatestDesktopKeyFromObjects(params);
			}
			const filename = this.extractFilename(entry);
			if (filename.trim().length === 0) {
				return this.resolveLatestDesktopKeyFromObjects(params);
			}
			const resolvedFilename = await this.resolveManifestFilename({
				channel: params.channel,
				plat: params.plat,
				arch: params.arch,
				format: params.format,
				filename,
				test: params.test,
			});
			if (!resolvedFilename) {
				return this.resolveLatestDesktopKeyFromObjects(params);
			}
			return this.buildDesktopArtifactKey({
				channel: params.channel,
				plat: params.plat,
				arch: params.arch,
				filename: resolvedFilename,
				test: params.test,
			});
		} catch (error) {
			if (error instanceof S3ServiceException && (error.name === 'NoSuchKey' || error.name === 'NotFound')) {
				return this.resolveLatestDesktopKeyFromObjects(params);
			}
			throw error;
		}
	}

	async getLatestDesktopVersion(params: {
		channel: DesktopChannel;
		plat: DesktopPlatform;
		arch: DesktopArch;
		baseUrl?: string;
		test?: boolean;
	}): Promise<VersionInfo | null> {
		const prefix = desktopArtifactPrefix(params);
		if (!prefix) {
			return null;
		}
		const manifestKey = `${prefix}/manifest.json`;
		try {
			const manifest = await this.readJsonObjectFromStorage(manifestKey);
			if (!isDesktopManifest(manifest)) {
				return this.getLatestDesktopVersionFromObjects(params);
			}
			const result = await this.getLatestDesktopVersionFromManifest(params, manifest);
			if (result) {
				return result;
			}
		} catch (error) {
			if (error instanceof S3ServiceException && (error.name === 'NoSuchKey' || error.name === 'NotFound')) {
				return this.getLatestDesktopVersionFromObjects(params);
			}
			throw error;
		}
		return this.getLatestDesktopVersionFromObjects(params);
	}

	async listDesktopVersions(params: {
		channel: DesktopChannel;
		plat: DesktopPlatform;
		arch: DesktopArch;
		limit: number;
		before?: string | null;
		after?: string | null;
		baseUrl?: string;
		test?: boolean;
	}): Promise<{
		versions: Array<VersionInfo>;
		hasMore: boolean;
	}> {
		const basePrefix = desktopArtifactPrefix(params);
		if (!basePrefix) {
			return {versions: [], hasMore: false};
		}
		const prefix = `${basePrefix}/`;
		try {
			const objects = await this.storageService.listObjects({
				bucket: Config.s3.buckets.downloads,
				prefix,
			});
			if (!objects || objects.length === 0) {
				return {versions: [], hasMore: false};
			}
			const versionMap = new Map<
				string,
				{
					pub_date: Date;
					files: Map<
						DesktopFormat,
						{
							filename: string;
							sha256Key: string | null;
						}
					>;
				}
			>();
			const sha256Files = new Set<string>();
			for (const obj of objects) {
				if (obj.key.endsWith('.sha256')) {
					sha256Files.add(obj.key);
				}
			}
			for (const obj of objects) {
				const filename = obj.key.slice(prefix.length);
				if (filename.includes('/') || filename.endsWith('.sha256') || filename === 'manifest.json') {
					continue;
				}
				const parsed = this.parseVersionFromFilename(filename, params.channel, params.plat, params.arch);
				if (!parsed) {
					continue;
				}
				const {version, format} = parsed;
				const sha256Key = sha256Files.has(`${obj.key}.sha256`) ? `${obj.key}.sha256` : null;
				if (!versionMap.has(version)) {
					versionMap.set(version, {
						pub_date: obj.lastModified ?? new Date(),
						files: new Map(),
					});
				}
				const entry = versionMap.get(version);
				if (entry) {
					if (!entry.files.has(format)) {
						entry.files.set(format, {filename, sha256Key});
					}
					if (obj.lastModified && obj.lastModified > entry.pub_date) {
						entry.pub_date = obj.lastModified;
					}
				}
			}
			const sortedVersions = Array.from(versionMap.keys()).sort(this.compareVersions);
			let filteredVersions = sortedVersions;
			if (params.before) {
				filteredVersions = filteredVersions.filter((v) => this.compareVersions(v, params.before ?? '') > 0);
			}
			if (params.after) {
				filteredVersions = filteredVersions.filter((v) => this.compareVersions(v, params.after ?? '') < 0);
			}
			const hasMore = filteredVersions.length > params.limit;
			const paginatedVersions = filteredVersions.slice(0, params.limit);
			const sha256Promises: Array<
				Promise<{
					key: string;
					hash: string | null;
				}>
			> = [];
			for (const version of paginatedVersions) {
				const entry = versionMap.get(version);
				if (!entry) {
					continue;
				}
				for (const [, fileInfo] of entry.files) {
					if (fileInfo.sha256Key) {
						sha256Promises.push(
							(async () => {
								try {
									const streamResult = await this.storageService.streamObject({
										bucket: Config.s3.buckets.downloads,
										key: fileInfo.sha256Key as string,
									});
									if (streamResult) {
										const body = Readable.toWeb(streamResult.body);
										const text = await new Response(body as ReadableStream).text();
										return {key: fileInfo.sha256Key as string, hash: text.trim().split(/\s+/u)[0]};
									}
								} catch {
									return {key: fileInfo.sha256Key as string, hash: null};
								}
								return {key: fileInfo.sha256Key as string, hash: null};
							})(),
						);
					}
				}
			}
			const sha256Results = await Promise.all(sha256Promises);
			const sha256Map = new Map<string, string | null>();
			for (const result of sha256Results) {
				sha256Map.set(result.key, result.hash);
			}
			const versions: Array<VersionInfo> = [];
			for (const version of paginatedVersions) {
				const entry = versionMap.get(version);
				if (!entry) {
					continue;
				}
				const files: Record<string, VersionFile> = {};
				for (const [format, fileInfo] of entry.files) {
					const sha256 = fileInfo.sha256Key ? (sha256Map.get(fileInfo.sha256Key) ?? null) : null;
					const validSha256 = sha256 && this.isValidSha256(sha256) ? sha256 : null;
					files[format] = {
						url: this.buildDesktopVersionUrl({
							channel: params.channel,
							plat: params.plat,
							arch: params.arch,
							version,
							format,
							baseUrl: params.baseUrl,
							test: params.test,
						}),
						sha256: validSha256,
						checksum_url: validSha256
							? this.buildDesktopVersionChecksumUrl({
									channel: params.channel,
									plat: params.plat,
									arch: params.arch,
									version,
									format,
									baseUrl: params.baseUrl,
									test: params.test,
								})
							: null,
					};
				}
				versions.push({
					version,
					pub_date: entry.pub_date.toISOString(),
					files,
				});
			}
			return {versions, hasMore};
		} catch (error) {
			if (error instanceof S3ServiceException && (error.name === 'NoSuchKey' || error.name === 'NotFound')) {
				return {versions: [], hasMore: false};
			}
			throw error;
		}
	}

	async resolveVersionedDesktopKey(params: {
		channel: DesktopChannel;
		plat: DesktopPlatform;
		arch: DesktopArch;
		version: string;
		format: DesktopFormat;
		test?: boolean;
	}): Promise<string | null> {
		const manifestFilename = await this.resolveVersionedDesktopKeyFromManifest(params);
		if (manifestFilename) {
			return manifestFilename;
		}
		const filenames = this.buildPossibleFilenames(
			params.channel,
			params.version,
			params.arch,
			params.format,
			params.plat,
		);
		if (filenames.length === 0) {
			return null;
		}
		const prefix = desktopArtifactPrefix(params);
		if (!prefix) {
			return null;
		}
		const s3Prefix = `${prefix}/`;
		for (const filename of filenames) {
			const key = `${s3Prefix}${filename}`;
			try {
				const metadata = await this.storageService.getObjectMetadata(Config.s3.buckets.downloads, key);
				if (metadata) {
					return key;
				}
			} catch (error) {
				if (error instanceof S3ServiceException && (error.name === 'NoSuchKey' || error.name === 'NotFound')) {
					continue;
				}
				throw error;
			}
		}
		return null;
	}

	async resolveLatestDesktopChecksumFile(params: {
		channel: DesktopChannel;
		plat: DesktopPlatform;
		arch: DesktopArch;
		format: DesktopFormat;
		test?: boolean;
	}): Promise<DesktopChecksumFile | null> {
		const version = await this.getLatestDesktopVersion(params);
		const file = version?.files[params.format];
		if (!file?.sha256 || !this.isValidSha256(file.sha256)) {
			return null;
		}
		const key = await this.resolveLatestDesktopKey(params);
		if (!key) {
			return null;
		}
		const filename = this.filenameFromKey(key);
		return this.buildDesktopChecksumFile(filename, file.sha256);
	}

	async resolveVersionedDesktopChecksumFile(params: {
		channel: DesktopChannel;
		plat: DesktopPlatform;
		arch: DesktopArch;
		version: string;
		format: DesktopFormat;
		test?: boolean;
	}): Promise<DesktopChecksumFile | null> {
		const key = await this.resolveVersionedDesktopKey(params);
		if (!key) {
			return null;
		}
		const filename = this.filenameFromKey(key);
		const objectSha256 = await this.readDesktopSha256ForArtifactKey(key);
		if (objectSha256) {
			return this.buildDesktopChecksumFile(filename, objectSha256);
		}
		const latest = await this.getLatestDesktopVersion(params);
		const file = latest?.version === params.version ? latest.files[params.format] : undefined;
		if (!file?.sha256 || !this.isValidSha256(file.sha256)) {
			return null;
		}
		return this.buildDesktopChecksumFile(filename, file.sha256);
	}

	async resolveDownloadKey(params: {path: string; test?: boolean}): Promise<string | null> {
		const key = this.buildKeyFromPath(params.path);
		if (!key) {
			return null;
		}
		const rewrittenKey = params.test ? this.rewriteToTestBucketKey(key) : key;
		const normalizedKey = this.normalizePlatformArchKey(rewrittenKey);
		const candidateKeys = [rewrittenKey, normalizedKey];
		const keysToTry = Array.from(new Set(candidateKeys.filter((candidate): candidate is string => candidate !== null)));
		for (const candidateKey of keysToTry) {
			try {
				const metadata = await this.storageService.getObjectMetadata(Config.s3.buckets.downloads, candidateKey);
				if (metadata) {
					return candidateKey;
				}
			} catch (error) {
				if (error instanceof S3ServiceException && (error.name === 'NoSuchKey' || error.name === 'NotFound')) {
					continue;
				}
				throw error;
			}
		}
		return null;
	}

	async streamDownload(params: {key: string; range?: string}): Promise<DownloadStreamResult | null> {
		try {
			return await this.storageService.streamObject({
				bucket: Config.s3.buckets.downloads,
				key: params.key,
				range: params.range,
			});
		} catch (error) {
			if (isStorageNotFoundError(error)) {
				return null;
			}
			if (isUnsatisfiableRangeError(error)) {
				const metadata = await this.getDownloadMetadata({key: params.key});
				throw new UnsatisfiableRangeError(metadata?.contentLength ?? 0);
			}
			throw error;
		}
	}

	async getDownloadMetadata(params: {key: string}): Promise<{
		contentLength: number;
		contentType?: string | null;
		etag?: string | null;
		lastModified?: Date | null;
	} | null> {
		try {
			const metadata = await this.storageService.getObjectMetadata(Config.s3.buckets.downloads, params.key);
			if (!metadata) {
				return null;
			}
			return {
				contentLength: metadata.contentLength,
				...(metadata.contentType === undefined ? {} : {contentType: metadata.contentType}),
				...(metadata.etag === undefined ? {} : {etag: metadata.etag}),
				...(metadata.lastModified === undefined ? {} : {lastModified: metadata.lastModified}),
			};
		} catch (error) {
			if (isStorageNotFoundError(error)) {
				return null;
			}
			throw error;
		}
	}

	private buildBaseUrl(baseUrl?: string): string {
		const configuredBaseUrl = (baseUrl ?? Config.endpoints.apiClient).trim();
		if (configuredBaseUrl.length > 0) {
			return configuredBaseUrl.replace(/\/+$/u, '');
		}
		return DEFAULT_API_CLIENT_BASE_URL;
	}

	private buildDesktopVersionUrl(params: {
		channel: DesktopChannel;
		plat: DesktopPlatform;
		arch: DesktopArch;
		version: string;
		format: DesktopFormat;
		baseUrl?: string;
		test?: boolean;
	}): string {
		const url = `${this.buildBaseUrl(params.baseUrl)}${DOWNLOAD_PREFIX}/desktop/${params.channel}/${params.plat}/${params.arch}/${params.version}/${params.format}`;
		return params.test ? `${url}?test=1` : url;
	}

	private buildDesktopVersionChecksumUrl(params: {
		channel: DesktopChannel;
		plat: DesktopPlatform;
		arch: DesktopArch;
		version: string;
		format: DesktopFormat;
		baseUrl?: string;
		test?: boolean;
	}): string {
		const url = `${this.buildBaseUrl(params.baseUrl)}${DOWNLOAD_PREFIX}/desktop/${params.channel}/${params.plat}/${params.arch}/${params.version}/${params.format}.sha256`;
		return params.test ? `${url}?test=1` : url;
	}

	private extractFilename(entry: DesktopManifestFileEntry): string {
		if (typeof entry === 'string') {
			return entry;
		}
		return entry.filename;
	}

	private extractEmbeddedSha256(entry: DesktopManifestFileEntry): string | null {
		if (typeof entry === 'string') {
			return null;
		}
		return entry.sha256 || null;
	}

	private async readJsonObjectFromStorage(key: string): Promise<unknown | null> {
		const streamResult = await this.storageService.streamObject({
			bucket: Config.s3.buckets.downloads,
			key,
		});
		if (!streamResult) {
			return null;
		}
		const body = Readable.toWeb(streamResult.body);
		const text = await new Response(body as ReadableStream).text();
		return parseJsonUnknown(text);
	}

	private isValidSha256(value: string): boolean {
		return /^[a-f0-9]{64}$/u.test(value);
	}

	private async resolveManifestFilename(params: ManifestFilenameResolutionParams): Promise<string | null> {
		const manifestFilename = params.filename.trim();
		if (manifestFilename.length === 0) {
			return null;
		}
		if (
			this.isManifestFilenameCompatibleWithRequestedFormat({...params, filename: manifestFilename}) &&
			(await this.desktopArtifactExists({
				channel: params.channel,
				plat: params.plat,
				arch: params.arch,
				filename: manifestFilename,
				test: params.test,
			}))
		) {
			return manifestFilename;
		}
		return this.findLatestFilenameForRequestedArch(params);
	}

	private isFilenameCompatibleWithRequestedArch(params: ManifestFilenameResolutionParams): boolean {
		const parsed = this.parseVersionFromFilename(params.filename, params.channel, params.plat, params.arch);
		if (!parsed) {
			return false;
		}
		return parsed.format === params.format;
	}

	private isManifestFilenameCompatibleWithRequestedFormat(params: ManifestFilenameResolutionParams): boolean {
		if (this.isFilenameCompatibleWithRequestedArch(params)) {
			return true;
		}
		if (params.plat !== 'win32' || params.format !== 'setup') {
			return false;
		}
		return params.filename.toLowerCase().endsWith('.exe');
	}

	private async findLatestFilenameForRequestedArch(params: LatestFilenameLookupParams): Promise<string | null> {
		const basePrefix = desktopArtifactPrefix(params);
		if (!basePrefix) {
			return null;
		}
		const prefix = `${basePrefix}/`;
		const objects = await this.storageService.listObjects({
			bucket: Config.s3.buckets.downloads,
			prefix,
		});
		if (!objects || objects.length === 0) {
			return null;
		}
		let latestFilename: string | null = null;
		let latestVersion: string | null = null;
		for (const obj of objects) {
			const filename = obj.key.slice(prefix.length);
			if (filename.length === 0) {
				continue;
			}
			if (
				filename.includes('/') ||
				filename.endsWith('.sha256') ||
				filename.endsWith('.blockmap') ||
				filename.endsWith('.yml') ||
				filename === 'manifest.json' ||
				filename === 'RELEASES.json' ||
				filename === 'releases.json'
			) {
				continue;
			}
			const parsed = this.parseVersionFromFilename(filename, params.channel, params.plat, params.arch);
			if (!parsed || parsed.format !== params.format) {
				continue;
			}
			if (!latestVersion || this.compareVersions(parsed.version, latestVersion) < 0) {
				latestVersion = parsed.version;
				latestFilename = filename;
			}
		}
		return latestFilename;
	}

	private escapeRegex(str: string): string {
		return str.replace(/[.*+?^${}()|[\]\\]/gu, '\\$&');
	}

	private buildPossibleFilenames(
		channel: DesktopChannel,
		version: string,
		arch: DesktopArch,
		format: DesktopFormat,
		plat: DesktopPlatform,
	): Array<string> {
		const mapping = FORMAT_MAPPINGS[format][plat];
		if (!mapping) {
			return [];
		}
		const {ext, arch: archMap} = mapping;
		const filenames = new Set<string>();
		for (const archSuffix of this.getArchTokens(archMap[arch as 'x64' | 'arm64'])) {
			const modernFilename = this.buildModernArtifactFilename(channel, version, plat, archSuffix, ext);
			if (modernFilename) {
				filenames.add(modernFilename);
			}
			if (format === 'setup') {
				filenames.add(`fluxer-${channel}-${version}-${archSuffix}-setup${ext}`);
				filenames.add(`Fluxer-${channel}-${version}-${archSuffix}-Setup${ext}`);
				filenames.add(`fluxer-${version}-${archSuffix}-setup${ext}`);
				filenames.add(`Fluxer-${version}-${archSuffix}-Setup${ext}`);
			} else if (format === 'portable') {
				filenames.add(
					`${this.getModernProductName(channel)}-${version}-portable-${MODERN_PLATFORM_TOKENS[plat]}-${archSuffix}${ext}`,
				);
				filenames.add(`fluxer-${channel}-${version}-portable-${archSuffix}${ext}`);
				filenames.add(`Fluxer-${version}-portable-${archSuffix}${ext}`);
			} else {
				filenames.add(`fluxer-${channel}-${version}-${archSuffix}${ext}`);
				filenames.add(`fluxer-${version}-${archSuffix}${ext}`);
				filenames.add(`Fluxer-${channel}-${version}-${archSuffix}${ext}`);
				filenames.add(`Fluxer-${version}-${archSuffix}${ext}`);
			}
		}
		return Array.from(filenames);
	}

	private parseVersionFromFilename(
		filename: string,
		channel: DesktopChannel,
		plat: DesktopPlatform,
		arch: DesktopArch,
	): {
		version: string;
		format: DesktopFormat;
	} | null {
		const formats = Object.keys(FORMAT_MAPPINGS) as Array<DesktopFormat>;
		for (const format of formats) {
			const mapping = FORMAT_MAPPINGS[format][plat];
			if (!mapping) {
				continue;
			}
			const {ext, arch: archMap} = mapping;
			const escapedExt = this.escapeRegex(ext);
			const escapedModernFilenamePrefix = this.escapeRegex(this.getModernProductName(channel));
			const modernPlatformToken = MODERN_PLATFORM_TOKENS[plat];
			for (const archSuffix of this.getArchTokens(archMap[arch as 'x64' | 'arm64'])) {
				const patterns = [
					new RegExp(
						`^[Ff]luxer-${this.escapeRegex(channel)}-(\\d+\\.\\d+\\.\\d+)-${this.escapeRegex(archSuffix)}(?:-[Ss]etup)?${escapedExt}$`,
						'u',
					),
					new RegExp(
						`^[Ff]luxer-(\\d+\\.\\d+\\.\\d+)-${this.escapeRegex(archSuffix)}(?:-[Ss]etup)?${escapedExt}$`,
						'u',
					),
					new RegExp(
						`^${escapedModernFilenamePrefix}-(\\d+\\.\\d+\\.\\d+)-${this.escapeRegex(modernPlatformToken)}-${this.escapeRegex(archSuffix)}${escapedExt}$`,
						'iu',
					),
				];
				if (format === 'portable') {
					patterns.push(
						new RegExp(
							`^${escapedModernFilenamePrefix}-(\\d+\\.\\d+\\.\\d+)-portable-${this.escapeRegex(modernPlatformToken)}-${this.escapeRegex(archSuffix)}${escapedExt}$`,
							'iu',
						),
					);
				}
				for (const pattern of patterns) {
					const match = filename.match(pattern);
					if (match) {
						return {version: match[1], format};
					}
				}
			}
		}
		return null;
	}

	private getModernProductName(channel: DesktopChannel): string {
		return channel === 'canary' ? 'Fluxer Canary' : 'Fluxer';
	}

	private buildModernArtifactFilename(
		channel: DesktopChannel,
		version: string,
		plat: DesktopPlatform,
		archToken: string,
		ext: string,
	): string {
		return `${this.getModernProductName(channel)}-${version}-${MODERN_PLATFORM_TOKENS[plat]}-${archToken}${ext}`;
	}

	private getArchTokens(archToken: string | Array<string>): Array<string> {
		return Array.isArray(archToken) ? archToken : [archToken];
	}

	private compareVersions(a: string, b: string): number {
		const partsA = a.split('.').map(Number);
		const partsB = b.split('.').map(Number);
		const len = Math.max(partsA.length, partsB.length);
		for (let i = 0; i < len; i++) {
			const numA = partsA[i] ?? 0;
			const numB = partsB[i] ?? 0;
			if (numA !== numB) {
				return numB - numA;
			}
		}
		return 0;
	}

	private buildKeyFromPath(path: string): string | null {
		if (!path.startsWith(DOWNLOAD_PREFIX)) {
			return null;
		}
		const stripped = path.slice(DOWNLOAD_PREFIX.length);
		const normalized = posix.normalize(stripped.replace(/^\/+/u, ''));
		if (normalized.startsWith('..') || normalized.startsWith('/')) {
			return null;
		}
		const segments = normalized.split('/');
		for (const segment of segments) {
			if (segment === '..' || segment === '.' || segment.includes('\0')) {
				return null;
			}
		}
		return normalized.length > 0 ? normalized : null;
	}

	private normalizePlatformArchKey(key: string): string | null {
		const match = key.match(/^(desktop(?:-test)?\/(stable|canary)\/(win32|darwin|linux))-(x64|arm64)(\/.*)$/u);
		if (!match) {
			return null;
		}
		const [, prefix, , , arch, suffix] = match;
		return `${prefix}/${arch}${suffix}`;
	}

	private rewriteToTestBucketKey(key: string): string {
		if (key.startsWith(`${DESKTOP_TEST_BUCKET_PREFIX}/`)) {
			return key;
		}
		if (key.startsWith(`${DESKTOP_BUCKET_PREFIX}/`)) {
			return `${DESKTOP_TEST_BUCKET_PREFIX}/${key.slice(DESKTOP_BUCKET_PREFIX.length + 1)}`;
		}
		return key;
	}

	private async resolveLatestDesktopKeyFromObjects(params: LatestFilenameLookupParams): Promise<string | null> {
		const filename = await this.findLatestFilenameForRequestedArch(params);
		if (!filename) {
			return null;
		}
		return this.buildDesktopArtifactKey({
			channel: params.channel,
			plat: params.plat,
			arch: params.arch,
			filename,
			test: params.test,
		});
	}

	private async resolveVersionedDesktopKeyFromManifest(params: {
		channel: DesktopChannel;
		plat: DesktopPlatform;
		arch: DesktopArch;
		version: string;
		format: DesktopFormat;
		test?: boolean;
	}): Promise<string | null> {
		const prefix = desktopArtifactPrefix(params);
		if (!prefix) {
			return null;
		}
		const manifestKey = `${prefix}/manifest.json`;
		try {
			const manifest = await this.readJsonObjectFromStorage(manifestKey);
			if (!isDesktopManifest(manifest)) {
				return null;
			}
			if (manifest.version !== params.version) {
				return null;
			}
			const entry = manifest.files[params.format];
			if (!entry) {
				return null;
			}
			const filename = this.extractFilename(entry).trim();
			if (filename.length === 0) {
				return null;
			}
			const resolvedFilename = await this.resolveManifestFilename({
				channel: params.channel,
				plat: params.plat,
				arch: params.arch,
				format: params.format,
				filename,
				test: params.test,
			});
			if (!resolvedFilename) {
				return null;
			}
			return this.buildDesktopArtifactKey({
				channel: params.channel,
				plat: params.plat,
				arch: params.arch,
				filename: resolvedFilename,
				test: params.test,
			});
		} catch (error) {
			if (error instanceof S3ServiceException && (error.name === 'NoSuchKey' || error.name === 'NotFound')) {
				return null;
			}
			throw error;
		}
	}

	private async getLatestDesktopVersionFromManifest(
		params: {
			channel: DesktopChannel;
			plat: DesktopPlatform;
			arch: DesktopArch;
			baseUrl?: string;
			test?: boolean;
		},
		manifest: DesktopManifest,
	): Promise<VersionInfo | null> {
		const files: Record<string, VersionFile> = {};
		for (const [formatKey, entry] of Object.entries(manifest.files)) {
			const format = formatKey as DesktopFormat;
			const manifestFilename = this.extractFilename(entry).trim();
			if (manifestFilename.length === 0) {
				continue;
			}
			const resolvedFilename = await this.resolveManifestFilename({
				channel: params.channel,
				plat: params.plat,
				arch: params.arch,
				format,
				filename: manifestFilename,
				test: params.test,
			});
			if (!resolvedFilename) {
				return null;
			}
			const parsed = this.parseVersionFromFilename(resolvedFilename, params.channel, params.plat, params.arch);
			if (parsed && (parsed.format !== format || parsed.version !== manifest.version)) {
				return null;
			}
			if (
				!parsed &&
				!this.isManifestFilenameCompatibleWithRequestedFormat({
					channel: params.channel,
					plat: params.plat,
					arch: params.arch,
					format,
					filename: resolvedFilename,
					test: params.test,
				})
			) {
				return null;
			}
			const sha256 = await this.resolveDesktopFileSha256({
				channel: params.channel,
				plat: params.plat,
				arch: params.arch,
				entry,
				manifestFilename,
				resolvedFilename,
				test: params.test,
			});
			files[format] = {
				url: this.buildDesktopVersionUrl({
					channel: params.channel,
					plat: params.plat,
					arch: params.arch,
					version: manifest.version,
					format,
					baseUrl: params.baseUrl,
					test: params.test,
				}),
				sha256,
				checksum_url: sha256
					? this.buildDesktopVersionChecksumUrl({
							channel: params.channel,
							plat: params.plat,
							arch: params.arch,
							version: manifest.version,
							format,
							baseUrl: params.baseUrl,
							test: params.test,
						})
					: null,
			};
		}
		if (Object.keys(files).length === 0) {
			return null;
		}
		const minimumSystemVersion = manifest.minimum_system_version ?? null;
		return {
			version: manifest.version,
			pub_date: manifest.pub_date,
			...(minimumSystemVersion ? {minimum_system_version: minimumSystemVersion} : {}),
			files,
		};
	}

	private async getLatestDesktopVersionFromObjects(params: {
		channel: DesktopChannel;
		plat: DesktopPlatform;
		arch: DesktopArch;
		baseUrl?: string;
		test?: boolean;
	}): Promise<VersionInfo | null> {
		const {versions} = await this.listDesktopVersions({
			channel: params.channel,
			plat: params.plat,
			arch: params.arch,
			limit: 1,
			baseUrl: params.baseUrl,
			test: params.test,
		});
		return versions[0] ?? null;
	}

	private async resolveDesktopFileSha256(params: {
		channel: DesktopChannel;
		plat: DesktopPlatform;
		arch: DesktopArch;
		entry: DesktopManifestFileEntry;
		manifestFilename: string;
		resolvedFilename: string;
		test?: boolean;
	}): Promise<string | null> {
		if (params.manifestFilename === params.resolvedFilename) {
			const embeddedSha256 = this.extractEmbeddedSha256(params.entry);
			if (embeddedSha256 && this.isValidSha256(embeddedSha256)) {
				return embeddedSha256;
			}
		}
		const key = this.buildDesktopArtifactKey({
			channel: params.channel,
			plat: params.plat,
			arch: params.arch,
			filename: params.resolvedFilename,
			test: params.test,
		});
		if (!key) {
			return null;
		}
		return this.readDesktopSha256ForArtifactKey(key);
	}

	private async readDesktopSha256ForArtifactKey(key: string): Promise<string | null> {
		try {
			const streamResult = await this.storageService.streamObject({
				bucket: Config.s3.buckets.downloads,
				key: `${key}.sha256`,
			});
			if (!streamResult) {
				return null;
			}
			const body = Readable.toWeb(streamResult.body);
			const text = await new Response(body as ReadableStream).text();
			const sha256 = text.trim().split(/\s+/u)[0] ?? null;
			return sha256 && this.isValidSha256(sha256) ? sha256 : null;
		} catch {
			return null;
		}
	}

	private async desktopArtifactExists(params: {
		channel: DesktopChannel;
		plat: DesktopPlatform;
		arch: DesktopArch;
		filename: string;
		test?: boolean;
	}): Promise<boolean> {
		const key = this.buildDesktopArtifactKey(params);
		if (!key) {
			return false;
		}
		try {
			const metadata = await this.storageService.getObjectMetadata(Config.s3.buckets.downloads, key);
			return metadata != null;
		} catch (error) {
			if (error instanceof S3ServiceException && (error.name === 'NoSuchKey' || error.name === 'NotFound')) {
				return false;
			}
			throw error;
		}
	}

	private buildDesktopArtifactKey(params: {
		channel: DesktopChannel;
		plat: DesktopPlatform;
		arch: DesktopArch;
		filename: string;
		test?: boolean;
	}): string | null {
		const prefix = desktopArtifactPrefix(params);
		return prefix ? `${prefix}/${params.filename}` : null;
	}

	private filenameFromKey(key: string): string {
		return key.split('/').pop() ?? 'download';
	}

	private buildDesktopChecksumFile(filename: string, sha256: string): DesktopChecksumFile {
		return {
			filename,
			sha256,
			body: `${sha256}  ${filename}\n`,
		};
	}
}
