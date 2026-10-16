// SPDX-License-Identifier: AGPL-3.0-or-later

export type AssetKind =
	| 'avatar'
	| 'guild_icon'
	| 'banner'
	| 'splash'
	| 'embed_splash'
	| 'emoji'
	| 'sticker'
	| 'attachment';
type AssetExtension = 'png' | 'jpeg' | 'webp' | 'gif' | 'apng' | 'avif' | 'heic' | 'heif' | 'jxl' | 'svg';
type CanonicalExtension = 'png' | 'jpeg' | 'webp' | 'gif' | 'apng';
type AnimationPolicy = 'never' | 'always' | 'premium';
type AssetExtensionLabelStyle = 'display' | 'extension';

interface AssetDimensionPolicy {
	min: number;
	max: number;
}

interface AssetFormatPolicyEntry {
	upload: ReadonlyArray<AssetExtension>;
	mimes: Readonly<Record<AssetExtension, string>>;
	storeAs: ReadonlyArray<CanonicalExtension>;
	animated: AnimationPolicy;
	maxBytes: number;
	dims?: AssetDimensionPolicy;
}

interface AssetUploadExtensionOptions {
	animatedAllowed?: boolean;
}

interface AssetUploadExtensionFormatOptions extends AssetUploadExtensionOptions {
	labelStyle?: AssetExtensionLabelStyle;
}

const MIME_BY_EXTENSION: Readonly<Record<AssetExtension, string>> = {
	png: 'image/png',
	jpeg: 'image/jpeg',
	webp: 'image/webp',
	gif: 'image/gif',
	apng: 'image/apng',
	avif: 'image/avif',
	heic: 'image/heic',
	heif: 'image/heif',
	jxl: 'image/jxl',
	svg: 'image/svg+xml',
};
const EXTENSION_DISPLAY: Readonly<Record<AssetExtension, string>> = {
	png: 'PNG',
	jpeg: 'JPEG',
	webp: 'WebP',
	gif: 'GIF',
	apng: 'APNG',
	avif: 'AVIF',
	heic: 'HEIC',
	heif: 'HEIF',
	jxl: 'JXL',
	svg: 'SVG',
};
const RASTER_FULL = Object.freeze<Array<AssetExtension>>([
	'png',
	'jpeg',
	'webp',
	'gif',
	'apng',
	'avif',
	'heic',
	'heif',
	'jxl',
	'svg',
]);
const RASTER_STATIC_ONLY = Object.freeze<Array<AssetExtension>>([
	'png',
	'jpeg',
	'webp',
	'avif',
	'heic',
	'heif',
	'jxl',
	'svg',
]);
const STICKER_UPLOAD = Object.freeze<Array<AssetExtension>>(['png', 'jpeg', 'apng', 'gif', 'webp', 'avif', 'svg']);
const STORE_ANIMATED = Object.freeze<Array<CanonicalExtension>>(['webp', 'apng', 'gif']);
const STORE_STATIC = Object.freeze<Array<CanonicalExtension>>(['webp', 'png', 'jpeg']);
const STORE_STICKER = Object.freeze<Array<CanonicalExtension>>(['webp', 'apng', 'gif', 'png']);
const EXTENSIONS_FILTERED_WHEN_ANIMATION_IS_UNAVAILABLE: ReadonlySet<AssetExtension> = new Set(['gif', 'apng']);
const EXTENSIONS_WITH_STATIC_ONLY_ANIMATION_POLICY: ReadonlySet<AssetExtension> = new Set(['avif']);
const KNOWN_ANIMATED_EXTENSIONS = Object.freeze<Array<AssetExtension>>(['gif', 'apng', 'webp']);

function pickMimes(exts: ReadonlyArray<AssetExtension>): Readonly<Record<AssetExtension, string>> {
	const out: Partial<Record<AssetExtension, string>> = {};
	for (const ext of exts) {
		out[ext] = MIME_BY_EXTENSION[ext];
	}
	return Object.freeze(out as Record<AssetExtension, string>);
}

export const ASSET_FORMAT_POLICY: Readonly<Record<AssetKind, AssetFormatPolicyEntry>> = Object.freeze({
	avatar: {
		upload: RASTER_FULL,
		mimes: pickMimes(RASTER_FULL),
		storeAs: STORE_ANIMATED,
		animated: 'premium',
		maxBytes: 10 * 1024 * 1024,
		dims: {min: 128, max: 1024},
	},
	guild_icon: {
		upload: RASTER_FULL,
		mimes: pickMimes(RASTER_FULL),
		storeAs: STORE_ANIMATED,
		animated: 'premium',
		maxBytes: 10 * 1024 * 1024,
		dims: {min: 128, max: 1024},
	},
	banner: {
		upload: RASTER_FULL,
		mimes: pickMimes(RASTER_FULL),
		storeAs: STORE_ANIMATED,
		animated: 'premium',
		maxBytes: 10 * 1024 * 1024,
		dims: {min: 480, max: 2400},
	},
	splash: {
		upload: RASTER_STATIC_ONLY,
		mimes: pickMimes(RASTER_STATIC_ONLY),
		storeAs: STORE_STATIC,
		animated: 'never',
		maxBytes: 10 * 1024 * 1024,
		dims: {min: 480, max: 2400},
	},
	embed_splash: {
		upload: RASTER_STATIC_ONLY,
		mimes: pickMimes(RASTER_STATIC_ONLY),
		storeAs: STORE_STATIC,
		animated: 'never',
		maxBytes: 10 * 1024 * 1024,
		dims: {min: 480, max: 2400},
	},
	emoji: {
		upload: RASTER_FULL,
		mimes: pickMimes(RASTER_FULL),
		storeAs: STORE_ANIMATED,
		animated: 'always',
		maxBytes: 512 * 1024,
		dims: {min: 32, max: 512},
	},
	sticker: {
		upload: STICKER_UPLOAD,
		mimes: pickMimes(STICKER_UPLOAD),
		storeAs: STORE_STICKER,
		animated: 'always',
		maxBytes: 512 * 1024,
		dims: {min: 128, max: 512},
	},
	attachment: {
		upload: RASTER_FULL,
		mimes: pickMimes(RASTER_FULL),
		storeAs: STORE_ANIMATED,
		animated: 'always',
		maxBytes: 500 * 1024 * 1024,
	},
});

export function getPolicy(kind: AssetKind): AssetFormatPolicyEntry {
	return ASSET_FORMAT_POLICY[kind];
}

export function getUploadExtensions(
	kind: AssetKind,
	options: AssetUploadExtensionOptions = {},
): ReadonlyArray<AssetExtension> {
	const policy = ASSET_FORMAT_POLICY[kind];
	if (options.animatedAllowed !== false) return policy.upload;
	return policy.upload.filter((ext) => !EXTENSIONS_FILTERED_WHEN_ANIMATION_IS_UNAVAILABLE.has(ext));
}

export function getAcceptString(kind: AssetKind, options: AssetUploadExtensionOptions = {}): string {
	const policy = ASSET_FORMAT_POLICY[kind];
	const upload = getUploadExtensions(kind, options);
	const exts = upload.map((ext) => `.${ext}`);
	const mimes = upload.map((ext) => policy.mimes[ext]);
	return [...exts, ...mimes].join(',');
}

export function getExtensionWhitelist(kind: AssetKind): ReadonlyArray<AssetExtension> {
	return ASSET_FORMAT_POLICY[kind].upload;
}

function isExtensionStaticOnlyForAnimatedAsset(kind: AssetKind, ext: AssetExtension): boolean {
	return ASSET_FORMAT_POLICY[kind].animated !== 'never' && EXTENSIONS_WITH_STATIC_ONLY_ANIMATION_POLICY.has(ext);
}

function getExtensionDisplayName(
	ext: AssetExtension,
	options: Pick<AssetUploadExtensionFormatOptions, 'labelStyle'> = {},
): string {
	return options.labelStyle === 'extension' ? ext : EXTENSION_DISPLAY[ext];
}

export function formatAssetUploadExtensions(kind: AssetKind, options: AssetUploadExtensionFormatOptions = {}): string {
	return getUploadExtensions(kind, options)
		.map((ext) => getExtensionDisplayName(ext, options))
		.join(', ');
}

export function formatKnownAnimatedAssetExtensions(kind: AssetKind): string {
	const policy = ASSET_FORMAT_POLICY[kind];
	return KNOWN_ANIMATED_EXTENSIONS.filter(
		(ext) => policy.upload.includes(ext) && !isExtensionStaticOnlyForAnimatedAsset(kind, ext),
	)
		.map((ext) => EXTENSION_DISPLAY[ext])
		.join(', ');
}

export function isExtensionAllowed(kind: AssetKind, ext: string): boolean {
	const withoutDot = ext.toLowerCase().replace(/^\./, '');
	const normalized = (withoutDot === 'jpg' ? 'jpeg' : withoutDot) as AssetExtension;
	return ASSET_FORMAT_POLICY[kind].upload.includes(normalized);
}
