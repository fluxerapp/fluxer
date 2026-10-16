// SPDX-License-Identifier: AGPL-3.0-or-later

const VIDEO_FILE_EXTENSION_REGEX = /\.(mp4|webm|mov|m4v)(?:$|\?)/iu;
const IMAGE_FILE_EXTENSION_REGEX = /\.(gif|webp|png|jpe?g|avif)(?:$|\?)/iu;

function testSourcePath(value: string, pattern: RegExp): boolean {
	try {
		const url = new URL(value);
		return pattern.test(url.pathname);
	} catch {
		return pattern.test(value);
	}
}

function isVideoSourceUrl(value: string): boolean {
	return testSourcePath(value, VIDEO_FILE_EXTENSION_REGEX);
}

function statesItsMediaKind(value: string): boolean {
	return testSourcePath(value, VIDEO_FILE_EXTENSION_REGEX) || testSourcePath(value, IMAGE_FILE_EXTENSION_REGEX);
}

export function resolvesToVideo(contentType: string, proxySrc: string, mediaSourceUrl: string | null): boolean {
	if (contentType.startsWith('video/')) return true;
	if (contentType.startsWith('image/')) return false;
	if (statesItsMediaKind(proxySrc)) return isVideoSourceUrl(proxySrc);
	return mediaSourceUrl !== null && isVideoSourceUrl(mediaSourceUrl);
}
