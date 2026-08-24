// SPDX-License-Identifier: AGPL-3.0-or-later

const FORMAT_CONTENT_TYPES: Record<string, string> = {
	gif: 'image/gif',
	loopedmp4: 'video/mp4',
	mediumgif: 'image/gif',
	mediummp4: 'video/mp4',
	mediumwebm: 'video/webm',
	mediumwebp: 'image/webp',
	mp4: 'video/mp4',
	nanogif: 'image/gif',
	nanomp4: 'video/mp4',
	nanowebm: 'video/webm',
	nanowebp: 'image/webp',
	tinygif: 'image/gif',
	tinymp4: 'video/mp4',
	tinywebm: 'video/webm',
	tinywebp: 'image/webp',
	webm: 'video/webm',
	webp: 'image/webp',
};

export const PREVIEW_FORMAT_PRIORITY = [
	'tinywebm',
	'tinymp4',
	'mediumwebm',
	'mediummp4',
	'webm',
	'mp4',
	'loopedmp4',
	'nanowebm',
	'nanomp4',
	'tinywebp',
	'tinygif',
	'mediumwebp',
	'webp',
	'mediumgif',
	'gif',
	'nanowebp',
	'nanogif',
] as const;

export function inferFormatContentType(formatKey: string): string {
	return FORMAT_CONTENT_TYPES[formatKey] ?? '';
}
