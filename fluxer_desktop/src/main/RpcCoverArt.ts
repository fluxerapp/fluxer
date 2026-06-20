// SPDX-License-Identifier: AGPL-3.0-or-later

import {cacheRpcCoverArt} from '@electron/main/RpcCoverArtProtocol';

const ALLOWED_IMAGE_HOSTS = [
	'cdn.discordapp.com',
	'media.discordapp.net',
	'i.scdn.co',
	'i.ytimg.com',
	'static-cdn.jtvnw.net',
];

const protocolUrlCache = new Map<string, string>();

function isAllowedImageHost(url: string): boolean {
	try {
		const {hostname} = new URL(url);
		return ALLOWED_IMAGE_HOSTS.some((host) => hostname === host || hostname.endsWith(`.${host}`));
	} catch {
		return false;
	}
}

function isLocalImageReference(url: string): boolean {
	return (
		url.startsWith('fluxer-rpc-art://') ||
		url.startsWith('data:') ||
		url.startsWith('blob:') ||
		!url.startsWith('http://') && !url.startsWith('https://')
	);
}

export async function resolveRpcCoverArtUrl(url: string | undefined): Promise<string | undefined> {
	if (!url || isLocalImageReference(url)) return url;
	if (isAllowedImageHost(url)) return url;

	const cached = protocolUrlCache.get(url);
	if (cached) return cached;

	try {
		const response = await fetch(url, {redirect: 'follow'});
		if (!response.ok) return url;
		const contentType = response.headers.get('content-type') ?? 'image/jpeg';
		if (!contentType.startsWith('image/')) return url;
		const buffer = Buffer.from(await response.arrayBuffer());
		if (buffer.length === 0 || buffer.length > 4 * 1024 * 1024) return url;
		const protocolUrl = cacheRpcCoverArt(url, contentType, buffer);
		protocolUrlCache.set(url, protocolUrl);
		return protocolUrl;
	} catch {
		return url;
	}
}

export async function resolveRpcActivityAssetsForDisplay(
	assets: {large_image?: string; large_text?: string; small_image?: string; small_text?: string} | undefined,
): Promise<typeof assets> {
	if (!assets) return assets;
	const [largeImage, smallImage] = await Promise.all([
		resolveRpcCoverArtUrl(assets.large_image),
		resolveRpcCoverArtUrl(assets.small_image),
	]);
	if (largeImage === assets.large_image && smallImage === assets.small_image) return assets;
	return {...assets, large_image: largeImage, small_image: smallImage};
}

export function sanitizeRpcActivityAssetsForGateway(
	assets: {large_image?: string; large_text?: string; small_image?: string; small_text?: string} | undefined,
): typeof assets {
	if (!assets) return assets;
	const sanitize = (image: string | undefined): string | undefined => {
		if (!image) return undefined;
		if (image.startsWith('data:') || image.startsWith('fluxer-rpc-art://')) return undefined;
		if (image.length > 256) return undefined;
		return image;
	};
	const largeImage = sanitize(assets.large_image);
	const smallImage = sanitize(assets.small_image);
	if (largeImage === assets.large_image && smallImage === assets.small_image) {
		return assets;
	}
	return {
		...assets,
		large_image: largeImage,
		small_image: smallImage,
	};
}
