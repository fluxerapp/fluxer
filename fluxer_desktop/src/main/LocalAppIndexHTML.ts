// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomBytes} from 'node:crypto';
import {LOCAL_APP_STATIC_CDN_ENDPOINT_PLACEHOLDER} from '@electron/main/LocalAppStaticMetadata';
import {
	BOOT_STATE_EXPECTED_ATTRIBUTE,
	BOOT_STATE_EXPECTED_VALUE,
	BOOT_STATE_FORMAT_META_NAME,
} from '@fluxer/desktop_ipc/src/BootStateContract';
import {readInstanceProductName} from '@fluxer/desktop_ipc/src/InstanceProductNameContract';

const CSP_NONCE_PLACEHOLDER = '{{CSP_NONCE_PLACEHOLDER}}';
const MEDIA_PRECONNECT_TAG = '<link rel="preconnect" href="{{MEDIA_ENDPOINT}}">';
const CSP_NONCE_BYTES = 16;
const HTML_OPEN_TAG = /<html(?=[\s>])[^>]*>/iu;
const HTML_CLASS_ATTRIBUTE = /\sclass\s*=/iu;
const THEME_CLASS_PREFIX = 'theme-';
const SAFE_THEME_TOKEN = /^[a-z0-9][a-z0-9_-]{0,63}$/iu;
const BOOT_SESSION_ATTRIBUTE = 'data-boot-session';
const TITLE_ELEMENT = /<title(?=[\s>])[^>]*>[^<]*<\/title>/iu;
const META_ELEMENT = /<meta\s[^>]*>/giu;
const META_NAME_ATTRIBUTE = /\sname="([^"]*)"/iu;
const META_CONTENT_ATTRIBUTE = /\scontent="([0-9.]+)"/iu;
const SERVED_PRODUCT_NAME_META = /<meta\s+name="apple-mobile-web-app-title"\s+content="[^"]*"/iu;
const SIGNED_IN_PRELOADS_ELEMENT = /<script type="application\/json" id="fluxer-boot-app-preloads">([^<]*)<\/script>/u;
const HEAD_CLOSE_TAG = '</head>';
const PRELOAD_HREF = /^\/[A-Za-z0-9._/-]+$/u;
const PRELOAD_INTEGRITY = /^sha(?:256|384|512)-[A-Za-z0-9+/=]+$/u;
const PRELOAD_DESTINATIONS: ReadonlySet<string> = new Set(['script', 'style']);
const HTML_ESCAPES: Readonly<Record<string, string>> = Object.freeze({
	'&': '&amp;',
	'<': '&lt;',
	'>': '&gt;',
	'"': '&quot;',
	"'": '&#39;',
});

interface LocalAppIndexHTMLInput {
	readonly html: string;
	readonly nonce: string;
	readonly prebootTheme: string | null;
	readonly productName: string | null;
	readonly bootStateExpectedFor: (declaredFormat: string | null) => boolean;
	readonly bootSession: boolean | null;
}

export function randomLocalAppCSPNonce(): string {
	return randomBytes(CSP_NONCE_BYTES).toString('hex');
}

export function readDeclaredBootStateFormat(html: string): string | null {
	for (const [element] of html.matchAll(META_ELEMENT)) {
		if (META_NAME_ATTRIBUTE.exec(element)?.[1] === BOOT_STATE_FORMAT_META_NAME) {
			return META_CONTENT_ATTRIBUTE.exec(element)?.[1] ?? null;
		}
	}
	return null;
}

export function rewriteLocalAppIndexHTML(input: LocalAppIndexHTMLInput): string {
	const substituted = input.html
		.replaceAll(CSP_NONCE_PLACEHOLDER, input.nonce)
		.replaceAll(LOCAL_APP_STATIC_CDN_ENDPOINT_PLACEHOLDER, '')
		.replaceAll(MEDIA_PRECONNECT_TAG, '');
	const bootStateExpected = input.bootStateExpectedFor(readDeclaredBootStateFormat(substituted));
	const stamped = stampHTMLOpenTag(substituted, input.prebootTheme, bootStateExpected, input.bootSession);
	const preloaded = bootStateExpected ? stampSignedInPreloads(stamped) : stamped;
	return stampProductName(preloaded, input.productName);
}

function stampSignedInPreloads(html: string): string {
	const element = SIGNED_IN_PRELOADS_ELEMENT.exec(html);
	const headClose = html.indexOf(HEAD_CLOSE_TAG);
	if (element == null || headClose < 0) {
		return html;
	}
	let entries: unknown;
	try {
		entries = JSON.parse(element[1] ?? '');
	} catch {
		return html;
	}
	if (!Array.isArray(entries)) {
		return html;
	}
	const links = entries.map(signedInPreloadLink).filter((link) => link !== '');
	return `${html.slice(0, headClose)}${links.join('')}${html.slice(headClose)}`;
}

function signedInPreloadLink(entry: unknown): string {
	if (typeof entry !== 'object' || entry === null) return '';
	const {href, as, integrity} = entry as Record<string, unknown>;
	if (typeof href !== 'string' || !PRELOAD_HREF.test(href)) return '';
	if (typeof as !== 'string' || !PRELOAD_DESTINATIONS.has(as)) return '';
	if (integrity !== undefined && (typeof integrity !== 'string' || !PRELOAD_INTEGRITY.test(integrity))) return '';
	const integrityAttributes = integrity === undefined ? '' : ` crossorigin="anonymous" integrity="${integrity}"`;
	return `<link rel="preload" as="${as}" fetchpriority="low" href="${href}"${integrityAttributes}>`;
}

function escapeHTML(value: string): string {
	return value.replace(/[&<>"']/gu, (character) => HTML_ESCAPES[character] ?? character);
}

function stampProductName(html: string, productName: string | null): string {
	const name = readInstanceProductName(productName);
	if (name === null) {
		return html;
	}
	const escaped = escapeHTML(name);
	return html
		.replace(TITLE_ELEMENT, (element) => `${element.slice(0, element.indexOf('>') + 1)}${escaped}</title>`)
		.replace(SERVED_PRODUCT_NAME_META, `<meta name="apple-mobile-web-app-title" content="${escaped}"`);
}

function stampHTMLOpenTag(
	html: string,
	prebootTheme: string | null,
	bootStateExpected: boolean,
	bootSession: boolean | null,
): string {
	const openTag = HTML_OPEN_TAG.exec(html);
	if (openTag == null) {
		return html;
	}
	const tag = openTag[0];
	const attributes = [
		themeClassAttribute(tag, prebootTheme),
		bootStateAttribute(tag, bootStateExpected),
		bootSessionAttribute(tag, bootSession),
	]
		.filter((attribute) => attribute !== '')
		.join('');
	if (attributes === '') {
		return html;
	}
	const stamped = `${tag.slice(0, -1)}${attributes}>`;
	return `${html.slice(0, openTag.index)}${stamped}${html.slice(openTag.index + tag.length)}`;
}

function themeClassAttribute(tag: string, theme: string | null): string {
	if (theme == null || !SAFE_THEME_TOKEN.test(theme) || HTML_CLASS_ATTRIBUTE.test(tag)) {
		return '';
	}
	return ` class="${THEME_CLASS_PREFIX}${theme}"`;
}

function bootSessionAttribute(tag: string, signedIn: boolean | null): string {
	if (signedIn === null || tag.includes(`${BOOT_SESSION_ATTRIBUTE}=`)) {
		return '';
	}
	return ` ${BOOT_SESSION_ATTRIBUTE}="${signedIn ? '1' : '0'}"`;
}

function bootStateAttribute(tag: string, expected: boolean): string {
	if (!expected || tag.includes(`${BOOT_STATE_EXPECTED_ATTRIBUTE}=`)) {
		return '';
	}
	return ` ${BOOT_STATE_EXPECTED_ATTRIBUTE}="${BOOT_STATE_EXPECTED_VALUE}"`;
}
