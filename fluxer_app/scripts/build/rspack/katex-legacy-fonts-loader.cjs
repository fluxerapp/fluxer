// SPDX-License-Identifier: AGPL-3.0-or-later

const LEGACY_FONT_SOURCE = /,\s*url\([^)]*\)\s*format\("(?:woff|truetype)"\)/g;

class MissingKatexWoff2SourceError extends Error {
	constructor(resource) {
		super(`${resource} declares no format("woff2") source, refusing to drop its legacy font sources`);
		this.name = 'MissingKatexWoff2SourceError';
	}
}

module.exports = function katexLegacyFontsLoader(source) {
	this.cacheable(true);
	if (!source.includes('format("woff2")')) {
		throw new MissingKatexWoff2SourceError(this.resourcePath);
	}
	return source.replace(LEGACY_FONT_SOURCE, '');
};
