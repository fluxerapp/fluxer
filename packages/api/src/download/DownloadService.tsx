/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {posix} from 'node:path';
import {Readable} from 'node:stream';
import {S3ServiceException} from '@aws-sdk/client-s3';
import {Config} from '@fluxer/api/src/Config';
import type {IStorageService} from '@fluxer/api/src/infrastructure/IStorageService';
import type {
	DesktopArch,
	DesktopChannel,
	DesktopFormat,
	DesktopPlatform,
} from '@fluxer/schema/src/domains/download/DownloadSchemas';

export const DOWNLOAD_PREFIX = '/dl';
export const DESKTOP_REDIRECT_PREFIX = `${DOWNLOAD_PREFIX}/desktop`;

type DesktopManifestFileEntry = string | {filename: string; sha256: string};

type DesktopManifest = {
	channel: DesktopChannel;
	platform: DesktopPlatform;
	arch: DesktopArch;
	version: string;
	pub_date: string;
	files: Record<string, DesktopManifestFileEntry>;
};

type FormatMapping = {
	ext: string;
	arch: Record<'x64' | 'arm64', string>;
};

const FORMAT_MAPPINGS: Record<DesktopFormat, Partial<Record<DesktopPlatform, FormatMapping>>> = {
	setup: {win32: {ext: '.exe', arch: {x64: 'x64', arm64: 'arm64'}}},
	dmg: {darwin: {ext: '.dmg', arch: {x64: 'x64', arm64: 'arm64'}}},
	zip: {darwin: {ext: '.zip', arch: {x64: 'x64', arm64: 'arm64'}}},
	appimage: {linux: {ext: '.AppImage', arch: {x64: 'x86_64', arm64: 'aarch64'}}},
	deb: {linux: {ext: '.deb', arch: {x64: 'amd64', arm64: 'arm64'}}},
	rpm: {linux: {ext: '.rpm', arch: {x64: 'x86_64', arm64: 'aarch64'}}},
	tar_gz: {linux: {ext: '.tar.gz', arch: {x64: 'x64', arm64: 'arm64'}}},
};

type VersionFile = {
	url: string;
	sha256: string | null;
};

type VersionInfo = {
	version: string;
	pub_date: string;
	files: Record<string, VersionFile>;
};

interface LatestFilenameLookupParams {
	channel: DesktopChannel;
	plat: DesktopPlatform;
	arch: DesktopArch;
	format: DesktopFormat;
}

interface ManifestFilenameResolutionParams extends LatestFilenameLookupParams {
	filename: string;
}

export class DownloadService {
	constructor(private readonly storageService: IStorageService) {}

	async resolveLatestDesktopRedirect(params: {
		channel: DesktopChannel;
		plat: DesktopPlatform;
		arch: DesktopArch;
		format: DesktopFormat;
		host: string;
		forwardedProto: string;
		requestUrl: string;
	}): Promise<string | null> {
		const manifestKey = `desktop/${params.channel}/${params.plat}/${params.arch}/manifest.json`;

		try {
			const manifest = await this.readJsonObjectFromStorage<DesktopManifest>(manifestKey);
			if (!manifest || !manifest.files) {
				return null;
			}

			const entry = manifest.files[params.format];
			if (!entry) {
				return null;
			}

			const filename = this.extractFilename(entry);
			if (filename.trim().length === 0) {
				return null;
			}

			const resolvedFilename = await this.resolveManifestFilename({
				channel: params.channel,
				plat: params.plat,
				arch: params.arch,
				format: params.format,
				filename,
			});
			if (!resolvedFilename) {
				return null;
			}

			const encodedFilename = encodeURIComponent(resolvedFilename);
			const dest = new URL(params.requestUrl);
			dest.hostname = params.host || dest.hostname;
			const scheme = this.resolveScheme(params.forwardedProto);
			dest.protocol = scheme.endsWith(':') ? scheme : `${scheme}:`;
			if (scheme === 'https') {
				dest.port = '';
			}
			dest.pathname = `${DOWNLOAD_PREFIX}/desktop/${params.channel}/${params.plat}/${params.arch}/${encodedFilename}`;

			return dest.toString();
		} catch (error) {
			if (error instanceof S3ServiceException && (error.name === 'NoSuchKey' || error.name === 'NotFound')) {
				return null;
			}
			throw error;
		}
	}

	async getLatestDesktopVersion(params: {
		channel: DesktopChannel;
		plat: DesktopPlatform;
		arch: DesktopArch;
		host: string;
		forwardedProto: string;
		requestUrl: string;
	}): Promise<VersionInfo | null> {
		const manifestKey = `desktop/${params.channel}/${params.plat}/${params.arch}/manifest.json`;

		try {
			const manifest = await this.readJsonObjectFromStorage<DesktopManifest>(manifestKey);
			if (!manifest || !manifest.files) {
				return null;
			}

			const baseUrl = this.buildBaseUrl(params.host, params.forwardedProto, params.requestUrl);
			const files: Record<string, VersionFile> = {};

			const sha256Promises: Array<Promise<{format: string; url: string; sha256: string | null}>> = [];

			for (const [format, entry] of Object.entries(manifest.files)) {
				const filename = this.extractFilename(entry);
				if (filename.trim().length === 0) {
					continue;
				}

				const encodedFilename = encodeURIComponent(filename);
				const url = `${baseUrl}${DOWNLOAD_PREFIX}/desktop/${params.channel}/${params.plat}/${params.arch}/${encodedFilename}`;

				const embeddedSha256 = this.extractEmbeddedSha256(entry);
				if (embeddedSha256) {
					sha256Promises.push(Promise.resolve({format, url, sha256: embeddedSha256}));
				} else {
					sha256Promises.push(
						(async () => {
							try {
								const sha256Key = `desktop/${params.channel}/${params.plat}/${params.arch}/${filename}.sha256`;
								const streamResult = await this.storageService.streamObject({
									bucket: Config.s3.buckets.downloads,
									key: sha256Key,
								});
								if (streamResult) {
									const body = Readable.toWeb(streamResult.body);
									const text = await new Response(body as ReadableStream).text();
									return {format, url, sha256: text.trim().split(/\s+/u)[0]};
								}
							} catch {
								return {format, url, sha256: null};
							}
							return {format, url, sha256: null};
						})(),
					);
				}
			}

			const results = await Promise.all(sha256Promises);
			for (const result of results) {
				files[result.format] = {
					url: result.url,
					sha256: result.sha256,
				};
			}

			return {
				version: manifest.version,
				pub_date: manifest.pub_date,
				files,
			};
		} catch (error) {
			if (error instanceof S3ServiceException && (error.name === 'NoSuchKey' || error.name === 'NotFound')) {
				return null;
			}
			throw error;
		}
	}

	async listDesktopVersions(params: {
		channel: DesktopChannel;
		plat: DesktopPlatform;
		arch: DesktopArch;
		limit: number;
		before?: string | null;
		after?: string | null;
		host: string;
		forwardedProto: string;
		requestUrl: string;
	}): Promise<{versions: Array<VersionInfo>; hasMore: boolean}> {
		const prefix = `desktop/${params.channel}/${params.plat}/${params.arch}/`;

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
					files: Map<DesktopFormat, {filename: string; sha256Key: string | null}>;
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

			const sha256Promises: Array<Promise<{key: string; hash: string | null}>> = [];
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

			const baseUrl = this.buildBaseUrl(params.host, params.forwardedProto, params.requestUrl);
			const versions: Array<VersionInfo> = [];

			for (const version of paginatedVersions) {
				const entry = versionMap.get(version);
				if (!entry) {
					continue;
				}

				const files: Record<string, VersionFile> = {};
				for (const [format, fileInfo] of entry.files) {
					const encodedFilename = encodeURIComponent(fileInfo.filename);
					files[format] = {
						url: `${baseUrl}${DOWNLOAD_PREFIX}/desktop/${params.channel}/${params.plat}/${params.arch}/${encodedFilename}`,
						sha256: fileInfo.sha256Key ? (sha256Map.get(fileInfo.sha256Key) ?? null) : null,
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

	async resolveVersionedDesktopRedirect(params: {
		channel: DesktopChannel;
		plat: DesktopPlatform;
		arch: DesktopArch;
		version: string;
		format: DesktopFormat;
		host: string;
		forwardedProto: string;
		requestUrl: string;
	}): Promise<string | null> {
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

		const s3Prefix = `desktop/${params.channel}/${params.plat}/${params.arch}/`;

		for (const filename of filenames) {
			const key = `${s3Prefix}${filename}`;
			try {
				const metadata = await this.storageService.getObjectMetadata(Config.s3.buckets.downloads, key);

				if (metadata) {
					const encodedFilename = encodeURIComponent(filename);
					const baseUrl = this.buildBaseUrl(params.host, params.forwardedProto, params.requestUrl);
					const dest = `${baseUrl}${DOWNLOAD_PREFIX}/desktop/${params.channel}/${params.plat}/${params.arch}/${encodedFilename}`;
					return dest;
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

	async resolveDownloadRedirect(params: {path: string}): Promise<string | null> {
		const key = this.buildKeyFromPath(params.path);
		if (!key) {
			return null;
		}

		const keysToTry = [key];
		const normalizedKey = this.normalizePlatformArchKey(key);
		if (normalizedKey) {
			keysToTry.push(normalizedKey);
		}

		for (const candidateKey of keysToTry) {
			try {
				const metadata = await this.storageService.getObjectMetadata(Config.s3.buckets.downloads, candidateKey);
				if (metadata) {
					return this.storageService.getPresignedDownloadURL({
						bucket: Config.s3.buckets.downloads,
						key: candidateKey,
					});
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

	private resolveScheme(forwardedProto: string): string {
		return forwardedProto.length > 0 ? forwardedProto.split(',')[0].trim() : 'https';
	}

	private buildBaseUrl(host: string, forwardedProto: string, requestUrl: string): string {
		const request = new URL(requestUrl);
		const scheme = this.resolveScheme(forwardedProto);
		const protocol = scheme.endsWith(':') ? scheme : `${scheme}:`;
		const port = scheme === 'https' ? '' : request.port;
		const portSuffix = port.length > 0 ? `:${port}` : '';
		const resolvedHost = host.length > 0 ? host : request.hostname;
		return `${protocol}//${resolvedHost}${portSuffix}`;
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

	private async readJsonObjectFromStorage<T>(key: string): Promise<T | null> {
		const streamResult = await this.storageService.streamObject({
			bucket: Config.s3.buckets.downloads,
			key,
		});

		if (!streamResult) {
			return null;
		}

		const body = Readable.toWeb(streamResult.body);
		const text = await new Response(body as ReadableStream).text();
		return JSON.parse(text) as T;
	}

	private async resolveManifestFilename(params: ManifestFilenameResolutionParams): Promise<string | null> {
		const manifestFilename = params.filename.trim();
		if (manifestFilename.length === 0) {
			return null;
		}

		if (this.isFilenameCompatibleWithRequestedArch({...params, filename: manifestFilename})) {
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

	private async findLatestFilenameForRequestedArch(params: LatestFilenameLookupParams): Promise<string | null> {
		const prefix = `desktop/${params.channel}/${params.plat}/${params.arch}/`;
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
		const archSuffix = archMap[arch as 'x64' | 'arm64'];
		const filenames: Array<string> = [];

		if (format === 'setup') {
			filenames.push(`fluxer-${channel}-${version}-${archSuffix}-setup${ext}`);
			filenames.push(`Fluxer-${channel}-${version}-${archSuffix}-Setup${ext}`);
			filenames.push(`fluxer-${version}-${archSuffix}-setup${ext}`);
			filenames.push(`Fluxer-${version}-${archSuffix}-Setup${ext}`);
		} else {
			filenames.push(`fluxer-${channel}-${version}-${archSuffix}${ext}`);
			filenames.push(`fluxer-${version}-${archSuffix}${ext}`);
			filenames.push(`Fluxer-${channel}-${version}-${archSuffix}${ext}`);
			filenames.push(`Fluxer-${version}-${archSuffix}${ext}`);
		}

		return filenames;
	}

	private parseVersionFromFilename(
		filename: string,
		channel: DesktopChannel,
		plat: DesktopPlatform,
		arch: DesktopArch,
	): {version: string; format: DesktopFormat} | null {
		const formats = Object.keys(FORMAT_MAPPINGS) as Array<DesktopFormat>;

		for (const format of formats) {
			const mapping = FORMAT_MAPPINGS[format][plat];
			if (!mapping) {
				continue;
			}

			const {ext, arch: archMap} = mapping;
			const archSuffix = archMap[arch as 'x64' | 'arm64'];
			const escapedExt = this.escapeRegex(ext);

			const patterns = [
				new RegExp(
					`^[Ff]luxer-${this.escapeRegex(channel)}-(\\d+\\.\\d+\\.\\d+)-${this.escapeRegex(archSuffix)}(?:-[Ss]etup)?${escapedExt}$`,
					'u',
				),
				new RegExp(`^[Ff]luxer-(\\d+\\.\\d+\\.\\d+)-${this.escapeRegex(archSuffix)}(?:-[Ss]etup)?${escapedExt}$`, 'u'),
			];

			for (const pattern of patterns) {
				const match = filename.match(pattern);
				if (match) {
					return {version: match[1], format};
				}
			}
		}

		return null;
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
		const match = key.match(/^(desktop\/(stable|canary)\/(win32|darwin|linux))-(x64|arm64)(\/.*)$/u);
		if (!match) {
			return null;
		}

		const [, prefix, , , arch, suffix] = match;
		return `${prefix}/${arch}${suffix}`;
	}
}
