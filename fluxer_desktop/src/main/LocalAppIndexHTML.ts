// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomBytes} from 'node:crypto';
import {LOCAL_APP_STATIC_CDN_ENDPOINT_PLACEHOLDER} from '@electron/main/LocalAppStaticMetadata';

const CSP_NONCE_PLACEHOLDER = '{{CSP_NONCE_PLACEHOLDER}}';
const MEDIA_PRECONNECT_TAG = '<link rel="preconnect" href="{{MEDIA_ENDPOINT}}">';
const CSP_NONCE_BYTES = 16;
const HTML_OPEN_TAG = /<html(?=[\s>])[^>]*>/iu;
const HTML_CLASS_ATTRIBUTE = /\sclass\s*=/iu;
const THEME_CLASS_PREFIX = 'theme-';
const SAFE_THEME_TOKEN = /^[a-z0-9][a-z0-9_-]{0,63}$/iu;

interface LocalAppIndexHTMLInput {
	readonly html: string;
	readonly nonce: string;
	readonly prebootTheme: string | null;
}

export function randomLocalAppCSPNonce(): string {
	return randomBytes(CSP_NONCE_BYTES).toString('hex');
}

export function rewriteLocalAppIndexHTML(input: LocalAppIndexHTMLInput): string {
	const substituted = input.html
		.replaceAll(CSP_NONCE_PLACEHOLDER, input.nonce)
		.replaceAll(LOCAL_APP_STATIC_CDN_ENDPOINT_PLACEHOLDER, '')
		.replaceAll(MEDIA_PRECONNECT_TAG, '');
	return stampPrebootTheme(substituted, input.prebootTheme);
}

function stampPrebootTheme(html: string, theme: string | null): string {
	if (theme == null || !SAFE_THEME_TOKEN.test(theme)) {
		return html;
	}
	const openTag = HTML_OPEN_TAG.exec(html);
	if (openTag == null) {
		return html;
	}
	const tag = openTag[0];
	if (HTML_CLASS_ATTRIBUTE.test(tag)) {
		return html;
	}
	const stamped = `${tag.slice(0, -1)} class="${THEME_CLASS_PREFIX}${theme}">`;
	return `${html.slice(0, openTag.index)}${stamped}${html.slice(openTag.index + tag.length)}`;
}
