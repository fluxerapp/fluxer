// SPDX-License-Identifier: AGPL-3.0-or-later

export const LOCAL_APP_STATIC_CDN_ENDPOINT_PLACEHOLDER = '{{STATIC_CDN_ENDPOINT}}';

const REWRITTEN_FILE_NAMES: ReadonlySet<string> = new Set(['manifest.json', 'browserconfig.xml']);
const ABSOLUTE_WEB_ASSET_URL = /https?:\/\/[^"'<>\s]+\/web\/[^"'<>\s)]+/gu;
const WEB_ASSET_PATH_PREFIX = '/web/';

interface UnsupportedLocalAppStaticMetadataFileErrorContext {
	readonly fileName: string;
}

class UnsupportedLocalAppStaticMetadataFileError extends Error {
	constructor({fileName}: UnsupportedLocalAppStaticMetadataFileErrorContext) {
		super(`Unsupported local app static metadata file: ${fileName}`);
		this.name = 'UnsupportedLocalAppStaticMetadataFileError';
	}
}

export function shouldRewriteLocalAppStaticMetadata(fileName: string): boolean {
	return REWRITTEN_FILE_NAMES.has(fileName);
}

export function rewriteLocalAppStaticMetadata(fileName: string, content: string): string {
	if (!shouldRewriteLocalAppStaticMetadata(fileName)) {
		throw new UnsupportedLocalAppStaticMetadataFileError({fileName});
	}
	return content
		.replaceAll(LOCAL_APP_STATIC_CDN_ENDPOINT_PLACEHOLDER, '')
		.replace(ABSOLUTE_WEB_ASSET_URL, rewriteAbsoluteWebAssetURL);
}

function rewriteAbsoluteWebAssetURL(source: string): string {
	let parsed: URL;
	try {
		parsed = new URL(source);
	} catch {
		return source;
	}
	if (!parsed.pathname.startsWith(WEB_ASSET_PATH_PREFIX)) {
		return source;
	}
	return `${parsed.pathname}${parsed.search}${parsed.hash}`;
}
