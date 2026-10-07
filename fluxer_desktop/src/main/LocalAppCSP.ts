// SPDX-License-Identifier: AGPL-3.0-or-later

const DESKTOP_CEILING_CONNECT_SOURCES: ReadonlyArray<string> = Object.freeze(['https:', 'wss:', 'blob:', 'data:']);
const DESKTOP_CEILING_IMAGE_SOURCES: ReadonlyArray<string> = Object.freeze(['https:', 'blob:', 'data:']);
const DESKTOP_CEILING_MEDIA_SOURCES: ReadonlyArray<string> = Object.freeze(['https:', 'blob:', 'data:']);

const FRAME_SOURCES: ReadonlyArray<string> = Object.freeze([
	'https://www.youtube.com/embed/',
	'https://www.youtube.com/s/player/',
]);

const IMAGE_SOURCES: ReadonlyArray<string> = Object.freeze([
	'https://*.fluxer.app',
	'https://i.ytimg.com',
	'https://*.youtube.com',
	'https://*.fluxer.media',
	'https://fluxer.media',
]);

const MEDIA_SOURCES: ReadonlyArray<string> = Object.freeze([
	'https://*.fluxer.app',
	'https://*.youtube.com',
	'https://*.fluxer.media',
	'https://fluxer.media',
]);

const SCRIPT_SOURCES: ReadonlyArray<string> = Object.freeze(['https://*.fluxer.app']);

const STYLE_SOURCES: ReadonlyArray<string> = Object.freeze([
	'https://*.fluxer.app',
	'https://fonts.googleapis.com',
	'https://api.fonts.coollabs.io',
]);

const FONT_SOURCES: ReadonlyArray<string> = Object.freeze([
	'https://*.fluxer.app',
	'https://fonts.gstatic.com',
	'https://api.fonts.coollabs.io',
]);

const CONNECT_SOURCES: ReadonlyArray<string> = Object.freeze([
	'https://*.fluxer.app',
	'wss://*.fluxer.app',
	'https://*.fluxer.media',
	'wss://*.fluxer.media',
	'https://fluxer-uploads.ewr1.vultrobjects.com',
	'https://fluxerstatus.com',
	'https://fluxer.media',
]);

const LOOPBACK_HTTP_SOURCES: ReadonlyArray<string> = Object.freeze([
	'http://localhost:*',
	'http://*.localhost:*',
	'http://127.0.0.1:*',
]);

const WORKER_SOURCES: ReadonlyArray<string> = Object.freeze(['https://*.fluxer.app', 'blob:']);

const MANIFEST_SOURCES: ReadonlyArray<string> = Object.freeze(['https://*.fluxer.app']);

const SELF_SOURCE = "'self'";
const NONE_SOURCE = "'none'";
const DIRECTIVE_SEPARATOR = '; ';

interface LocalAppCSPRequest {
	readonly nonce: string;
	readonly cleartextInstanceOrigins?: ReadonlyArray<string>;
}

export function buildLocalAppCSP({nonce, cleartextInstanceOrigins = []}: LocalAppCSPRequest): string {
	const cleartextSources = [...LOOPBACK_HTTP_SOURCES];
	extendFrom(cleartextSources, cleartextInstanceOrigins.filter(isCleartextOriginSource));

	const directives: Array<string> = [];

	directives.push(`default-src ${SELF_SOURCE}`);

	const script = [SELF_SOURCE, `'nonce-${nonce}'`, "'wasm-unsafe-eval'", 'blob:'];
	extendFrom(script, SCRIPT_SOURCES);
	directives.push(`script-src ${script.join(' ')}`);

	const style = [SELF_SOURCE, "'unsafe-inline'"];
	extendFrom(style, STYLE_SOURCES);
	directives.push(`style-src ${style.join(' ')}`);

	const image = [SELF_SOURCE, 'blob:', 'data:'];
	extendFrom(image, IMAGE_SOURCES);
	extendFrom(image, DESKTOP_CEILING_IMAGE_SOURCES);
	extendFrom(image, cleartextSources);
	directives.push(`img-src ${image.join(' ')}`);

	const media = [SELF_SOURCE, 'blob:'];
	extendFrom(media, MEDIA_SOURCES);
	extendFrom(media, DESKTOP_CEILING_MEDIA_SOURCES);
	extendFrom(media, cleartextSources);
	directives.push(`media-src ${media.join(' ')}`);

	const font = [SELF_SOURCE, 'data:'];
	extendFrom(font, FONT_SOURCES);
	directives.push(`font-src ${font.join(' ')}`);

	const connect = [SELF_SOURCE, 'blob:', 'data:'];
	extendFrom(connect, CONNECT_SOURCES);
	extendFrom(connect, DESKTOP_CEILING_CONNECT_SOURCES);
	extendFrom(connect, cleartextSources);
	directives.push(`connect-src ${connect.join(' ')}`);

	const frame = [SELF_SOURCE];
	extendFrom(frame, FRAME_SOURCES);
	directives.push(`frame-src ${frame.join(' ')}`);

	const worker = [SELF_SOURCE, 'blob:'];
	extendFrom(worker, WORKER_SOURCES);
	directives.push(`worker-src ${worker.join(' ')}`);

	const manifest = [SELF_SOURCE];
	extendFrom(manifest, MANIFEST_SOURCES);
	directives.push(`manifest-src ${manifest.join(' ')}`);

	directives.push(`object-src ${NONE_SOURCE}`);
	directives.push(`base-uri ${SELF_SOURCE}`);
	directives.push(`frame-ancestors ${NONE_SOURCE}`);

	return directives.join(DIRECTIVE_SEPARATOR);
}

function isCleartextOriginSource(origin: string): boolean {
	let url: URL;
	try {
		url = new URL(origin);
	} catch {
		return false;
	}
	return url.protocol === 'http:' && url.origin === origin && !url.hostname.startsWith('[');
}

function extendFrom(target: Array<string>, defaults: ReadonlyArray<string>): void {
	for (const source of defaults) {
		if (!target.includes(source)) {
			target.push(source);
		}
	}
}
