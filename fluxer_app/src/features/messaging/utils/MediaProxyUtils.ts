// SPDX-License-Identifier: AGPL-3.0-or-later

import {MEDIA_PROXY_IMAGE_SIZES, type MediaProxyImageSize} from '@fluxer/constants/src/MediaProxyImageSizes';

export const MEDIA_PROXY_IMAGE_SIZE_LADDER = MEDIA_PROXY_IMAGE_SIZES;

export const LARGEST_MEDIA_PROXY_IMAGE_SIZE = MEDIA_PROXY_IMAGE_SIZE_LADDER[MEDIA_PROXY_IMAGE_SIZE_LADDER.length - 1];
const MEDIA_PROXY_DOWNSCALE_TOLERANCE = 1.1;
const MEDIA_PROXY_DOWNSCALE_MIN_DPR = 2;

export function snapMediaProxyImageSize(cssPixels: number, allowDownscale = false): MediaProxyImageSize {
	const devicePixelRatio = mediaDevicePixelRatio();
	const target = cssPixels * devicePixelRatio;
	if (allowDownscale && devicePixelRatio >= MEDIA_PROXY_DOWNSCALE_MIN_DPR) {
		let below: MediaProxyImageSize | undefined;
		for (const rung of MEDIA_PROXY_IMAGE_SIZE_LADDER) {
			if (rung > target) break;
			below = rung;
		}
		if (below !== undefined && target / below <= MEDIA_PROXY_DOWNSCALE_TOLERANCE) return below;
	}
	return MEDIA_PROXY_IMAGE_SIZE_LADDER.find((rung) => target <= rung) ?? LARGEST_MEDIA_PROXY_IMAGE_SIZE;
}

export interface MediaProxyOptions {
	width?: number;
	height?: number;
	format?: string;
	quality?: 'high' | 'low' | 'lossless';
	animated?: boolean;
}

export function resolvePreferredImageFormat(_sourceContentType?: string): 'webp' {
	return 'webp';
}

export function mediaDevicePixelRatio(): number {
	if (typeof window === 'undefined') return 1;
	const ratio = window.devicePixelRatio;
	return Number.isFinite(ratio) && ratio > 0 ? ratio : 1;
}

type FitInsideMediaProxyOptions = MediaProxyOptions & {
	width?: number;
	height?: number;
};

function isSvgProxyUrl(url: URL): boolean {
	const path = url.pathname.toLowerCase();
	return path.endsWith('.svg');
}

function resolveProxyDimension(value: number | undefined): number | undefined {
	if (value === undefined || !Number.isFinite(value)) return undefined;
	const pixels = Math.ceil(value);
	return pixels > 0 ? pixels : undefined;
}

function appendMediaProxyParams(url: URL, options: MediaProxyOptions): void {
	if (url.protocol === 'data:' || url.protocol === 'blob:' || isSvgProxyUrl(url)) {
		return;
	}
	const {format, quality, animated} = options;
	const width = resolveProxyDimension(options.width);
	const height = resolveProxyDimension(options.height);
	if (format) {
		url.searchParams.set('format', format);
	}
	if (width !== undefined) {
		url.searchParams.set('width', width.toString());
	}
	if (height !== undefined) {
		url.searchParams.set('height', height.toString());
	}
	if (quality) {
		url.searchParams.set('quality', quality);
	}
	if (animated === true) {
		url.searchParams.set('animated', 'true');
	} else {
		url.searchParams.delete('animated');
	}
}

function getFitInsideProxyOptions(options: FitInsideMediaProxyOptions): MediaProxyOptions {
	const {width, height, ...rest} = options;
	if (width !== undefined && height !== undefined) {
		return {...rest, width};
	}
	return options;
}

export function buildMediaProxyURL(originalUrl: string, options: MediaProxyOptions = {}): string {
	if (!originalUrl) return originalUrl;
	const url = new URL(originalUrl);
	appendMediaProxyParams(url, options);
	return url.toString();
}

export function buildFitInsideMediaProxyURL(originalUrl: string, options: FitInsideMediaProxyOptions = {}): string {
	if (!originalUrl) return originalUrl;
	const url = new URL(originalUrl);
	appendMediaProxyParams(url, getFitInsideProxyOptions(options));
	return url.toString();
}

function readProxyDimensionParam(url: URL, key: 'width' | 'height'): number | undefined {
	const raw = url.searchParams.get(key);
	if (raw === null) return undefined;
	return resolveProxyDimension(Number(raw));
}

function carriedProxyDimensions(proxyURL: string): {width?: number; height?: number} | undefined {
	let parsed: URL;
	try {
		parsed = new URL(proxyURL);
	} catch {
		return undefined;
	}
	const width = readProxyDimensionParam(parsed, 'width');
	const height = readProxyDimensionParam(parsed, 'height');
	if (width === undefined && height === undefined) return undefined;
	return {width, height};
}

function variantDimensions(proxyURL: string, width?: number, height?: number): {width?: number; height?: number} {
	return carriedProxyDimensions(proxyURL) ?? {width, height};
}

export function stripMediaProxyParams(proxyURL: string): string {
	const url = new URL(proxyURL);
	url.searchParams.delete('width');
	url.searchParams.delete('height');
	url.searchParams.delete('format');
	url.searchParams.delete('quality');
	url.searchParams.delete('animated');
	return url.toString();
}

export function buildAnimatedImageProxyURL(proxyURL: string, width?: number, height?: number): string {
	if (!proxyURL) return proxyURL;
	const target = variantDimensions(proxyURL, width, height);
	const baseURL = stripMediaProxyParams(proxyURL);
	return buildMediaProxyURL(baseURL, {
		width: target.width,
		height: target.height,
		animated: true,
	});
}

export function buildFittedAnimatedImageProxyURL(proxyURL: string, width?: number, height?: number): string {
	if (!proxyURL) return proxyURL;
	const target = variantDimensions(proxyURL, width, height);
	const baseURL = stripMediaProxyParams(proxyURL);
	return buildFitInsideMediaProxyURL(baseURL, {
		width: target.width,
		height: target.height,
		animated: true,
	});
}

export function buildStaticGifPreviewURL(proxyURL: string, width?: number, height?: number): string {
	if (!proxyURL) return proxyURL;
	const target = variantDimensions(proxyURL, width, height);
	return buildMediaProxyURL(stripMediaProxyParams(proxyURL), {
		format: 'webp',
		width: target.width,
		height: target.height,
		animated: false,
	});
}

export function buildFittedStaticGifPreviewURL(proxyURL: string, width?: number, height?: number): string {
	if (!proxyURL) return proxyURL;
	const target = variantDimensions(proxyURL, width, height);
	return buildFitInsideMediaProxyURL(stripMediaProxyParams(proxyURL), {
		format: 'webp',
		width: target.width,
		height: target.height,
		animated: false,
	});
}
