// SPDX-License-Identifier: AGPL-3.0-or-later

import type {DesktopArch, DesktopChannel, DesktopPlatform} from '@fluxer/schema/src/domains/download/DownloadSchemas';
import {isJsonRecord} from '../utils/JsonBoundaryUtils';

const DESKTOP_BUCKET_PREFIX = 'desktop';
const MIN_RELEASE_ROUTE_COUNT = 28;
const MAX_RELEASE_ROUTE_COUNT = 128;
const MIN_RELEASE_ASSET_COUNT = 24;

interface DesktopReleaseAsset {
	storage_key: string;
	release_asset: string;
	sha256: string;
	size: number;
}

interface DesktopReleaseDescriptor {
	schema_version: 1;
	channel: DesktopChannel;
	version: string;
	release_tag: string;
	source_sha: string;
	assets: Array<DesktopReleaseAsset>;
}

interface DesktopReleaseReadiness {
	schema_version: 1;
	channel: DesktopChannel;
	version: string;
	release_tag: string;
	source_sha: string;
	descriptor_sha256: string;
}

interface DesktopArtifactScope {
	channel: DesktopChannel;
	plat: DesktopPlatform;
	arch: DesktopArch;
}

export function parseDesktopArtifactScope(key: string): DesktopArtifactScope | null {
	const segments = key.split('/');
	if (segments.length !== 5 || segments[0] !== DESKTOP_BUCKET_PREFIX || segments[4].length === 0) {
		return null;
	}
	const [, channel, plat, arch] = segments;
	if (
		(channel !== 'stable' && channel !== 'canary') ||
		(plat !== 'win32' && plat !== 'darwin' && plat !== 'linux') ||
		(arch !== 'x64' && arch !== 'arm64')
	) {
		return null;
	}
	return {channel, plat, arch};
}

function parseDesktopReleaseAsset(value: unknown): DesktopReleaseAsset | null {
	if (
		!isJsonRecord(value) ||
		typeof value.storage_key !== 'string' ||
		typeof value.release_asset !== 'string' ||
		typeof value.sha256 !== 'string' ||
		typeof value.size !== 'number'
	) {
		return null;
	}
	if (
		!/^desktop\/(?:stable|canary)\/(?:win32|darwin|linux)\/(?:x64|arm64)\/[A-Za-z0-9._-]+$/u.test(value.storage_key) ||
		!/^[A-Za-z0-9._-]+$/u.test(value.release_asset) ||
		!/^[a-f0-9]{64}$/u.test(value.sha256) ||
		!Number.isSafeInteger(value.size) ||
		value.size <= 0
	) {
		return null;
	}
	return {
		storage_key: value.storage_key,
		release_asset: value.release_asset,
		sha256: value.sha256,
		size: value.size,
	};
}

export function parseDesktopReleaseDescriptor(value: unknown): DesktopReleaseDescriptor | null {
	if (
		!isJsonRecord(value) ||
		value.schema_version !== 1 ||
		(value.channel !== 'stable' && value.channel !== 'canary') ||
		typeof value.version !== 'string' ||
		!/^\d+\.\d+\.\d+$/u.test(value.version) ||
		typeof value.release_tag !== 'string' ||
		typeof value.source_sha !== 'string' ||
		!/^[a-f0-9]{40}$/u.test(value.source_sha) ||
		!Array.isArray(value.assets) ||
		value.assets.length < MIN_RELEASE_ROUTE_COUNT ||
		value.assets.length > MAX_RELEASE_ROUTE_COUNT
	) {
		return null;
	}
	const expectedTag = `fluxer-desktop-${value.channel}@${value.version}`;
	const expectedStoragePrefix = `desktop/${value.channel}/`;
	const expectedReleasePrefix = `${value.channel === 'canary' ? 'Fluxer-Canary' : 'Fluxer'}-${value.version}-`;
	const descriptorName = `${expectedReleasePrefix}release-manifest.json`;
	if (value.release_tag !== expectedTag) {
		return null;
	}
	const storageKeys = new Set<string>();
	const routeCounts = new Map<string, number>();
	const releaseAssets = new Map<string, {sha256: string; size: number}>();
	const releaseAssetNames = new Map<string, string>([[descriptorName.toLowerCase(), descriptorName]]);
	const assets: Array<DesktopReleaseAsset> = [];
	for (const rawAsset of value.assets) {
		const asset = parseDesktopReleaseAsset(rawAsset);
		if (
			!asset ||
			!asset.storage_key.startsWith(expectedStoragePrefix) ||
			!asset.release_asset.startsWith(expectedReleasePrefix) ||
			storageKeys.has(asset.storage_key)
		) {
			return null;
		}
		storageKeys.add(asset.storage_key);
		const [, , platform, arch, filename] = asset.storage_key.split('/');
		const platformToken = platform === 'win32' ? 'win' : platform === 'darwin' ? 'mac' : 'linux';
		const releaseFilename =
			platform === 'darwin' && filename.toLowerCase() === 'releases.json' ? 'releases.json' : filename;
		const expectedReleaseAsset = filename.startsWith(expectedReleasePrefix)
			? filename
			: `${expectedReleasePrefix}${platformToken}-${arch}-${releaseFilename}`;
		if (
			asset.release_asset !== expectedReleaseAsset ||
			asset.release_asset.toLowerCase() === descriptorName.toLowerCase()
		) {
			return null;
		}
		const caseFoldedReleaseAsset = asset.release_asset.toLowerCase();
		const existingReleaseAssetName = releaseAssetNames.get(caseFoldedReleaseAsset);
		if (existingReleaseAssetName && existingReleaseAssetName !== asset.release_asset) {
			return null;
		}
		releaseAssetNames.set(caseFoldedReleaseAsset, asset.release_asset);
		const scope = `${platform}/${arch}`;
		routeCounts.set(scope, (routeCounts.get(scope) ?? 0) + 1);
		const existing = releaseAssets.get(asset.release_asset);
		if (existing && (existing.sha256 !== asset.sha256 || existing.size !== asset.size)) {
			return null;
		}
		releaseAssets.set(asset.release_asset, {sha256: asset.sha256, size: asset.size});
		assets.push(asset);
	}
	if (releaseAssets.size < MIN_RELEASE_ASSET_COUNT || releaseAssets.size > MAX_RELEASE_ROUTE_COUNT) {
		return null;
	}
	const expectedRouteCounts = new Map([
		['darwin/arm64', 4],
		['darwin/x64', 4],
		['linux/arm64', 4],
		['linux/x64', 4],
		['win32/arm64', 6],
		['win32/x64', 6],
	]);
	if (
		routeCounts.size !== expectedRouteCounts.size ||
		Array.from(expectedRouteCounts).some(([scope, count]) => (routeCounts.get(scope) ?? 0) < count)
	) {
		return null;
	}
	return {
		schema_version: 1,
		channel: value.channel,
		version: value.version,
		release_tag: value.release_tag,
		source_sha: value.source_sha,
		assets,
	};
}

export function parseDesktopReleaseReadiness(value: unknown): DesktopReleaseReadiness | null {
	if (
		!isJsonRecord(value) ||
		value.schema_version !== 1 ||
		(value.channel !== 'stable' && value.channel !== 'canary') ||
		typeof value.version !== 'string' ||
		!/^\d+\.\d+\.\d+$/u.test(value.version) ||
		typeof value.release_tag !== 'string' ||
		typeof value.source_sha !== 'string' ||
		!/^[a-f0-9]{40}$/u.test(value.source_sha) ||
		typeof value.descriptor_sha256 !== 'string' ||
		!/^[a-f0-9]{64}$/u.test(value.descriptor_sha256)
	) {
		return null;
	}
	return {
		schema_version: 1,
		channel: value.channel,
		version: value.version,
		release_tag: value.release_tag,
		source_sha: value.source_sha,
		descriptor_sha256: value.descriptor_sha256,
	};
}
