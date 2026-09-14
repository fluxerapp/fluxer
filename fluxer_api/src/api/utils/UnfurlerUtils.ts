// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {Config} from '@app/api/Config';
import {Logger} from '@app/api/Logger';
import * as InviteUtils from '@app/api/utils/InviteUtils';
import {URL_REGEX} from '@fluxer/constants/src/Core';
import * as idna from 'idna-uts46-hx';

const MARKETING_PATH_PREFIXES = ['/channels/', '/theme/'];

function normalizeHostname(hostname: string | undefined) {
	return hostname?.trim().toLowerCase() || '';
}

let _marketingHostname: string | null = null;

function getMarketingHostname() {
	if (!_marketingHostname) {
		_marketingHostname = normalizeHostname(Config.hosts.marketing);
	}
	return _marketingHostname;
}

const isMarketingPath = (hostname: string, pathname: string) =>
	hostname === getMarketingHostname() && MARKETING_PATH_PREFIXES.some((prefix) => pathname.startsWith(prefix));

function getWebAppHostname() {
	try {
		return new URL(Config.endpoints.webApp).hostname;
	} catch {
		return '';
	}
}

let _excludedHostnames: Set<string> | null = null;

function getExcludedHostnames(): Set<string> {
	if (!_excludedHostnames) {
		_excludedHostnames = new Set<string>();
		const addHostname = (hostname: string | undefined) => {
			const normalized = normalizeHostname(hostname);
			if (normalized) {
				_excludedHostnames!.add(normalized);
			}
		};
		addHostname(Config.hosts.invite);
		addHostname(Config.hosts.gift);
		Config.hosts.unfurlIgnored.forEach(addHostname);
		addHostname(getWebAppHostname());
	}
	return _excludedHostnames;
}

function idnaEncodeURL(url: string) {
	try {
		const parsedUrl = new URL(url);
		const encodedDomain = idna.toAscii(parsedUrl.hostname).toLowerCase();
		parsedUrl.hostname = encodedDomain;
		parsedUrl.username = '';
		parsedUrl.password = '';
		return parsedUrl.toString();
	} catch (error) {
		Logger.error({error}, 'Failed to encode URL');
		return '';
	}
}

function isValidURL(url: string) {
	try {
		const parsedUrl = new URL(url);
		return parsedUrl.protocol === 'http:' || parsedUrl.protocol === 'https:';
	} catch {
		return false;
	}
}

function isFluxerAppExcludedURL(url: string) {
	try {
		const parsedUrl = new URL(url);
		const hostname = normalizeHostname(parsedUrl.hostname);
		const isMarketingPathMatch = isMarketingPath(hostname, parsedUrl.pathname);
		return isMarketingPathMatch || getExcludedHostnames().has(hostname);
	} catch {
		return false;
	}
}

export function extractURLs(inputText: string) {
	let text = inputText;
	text = text.replace(/`[^`]*`/g, '');
	text = text.replace(/```.*?```/gs, '');
	text = text.replace(/\|\|([\s\S]*?)\|\|/g, ' $1 ');
	text = text.replace(/\|\|/g, ' ');
	text = text.replace(/\[([^\]]+)\]\((https?:\/\/[^\s)]+)\)/g, '$2');
	text = text.replace(/<https?:\/\/[^\s]+>/g, '');
	const urls = text.match(URL_REGEX) || [];
	const seen = new Set<string>();
	const result: Array<string> = [];
	for (const url of urls) {
		if (!isValidURL(url)) continue;
		if (InviteUtils.findInvite(url) != null) continue;
		if (isFluxerAppExcludedURL(url)) continue;
		const encoded = idnaEncodeURL(url);
		if (!encoded) continue;
		if (!seen.has(encoded)) {
			seen.add(encoded);
			result.push(encoded);
			if (result.length >= 5) break;
		}
	}
	return result;
}

export function hashUnfurlContent(content: string | null | undefined): string {
	return createHash('sha256')
		.update(content ?? '')
		.digest('hex');
}
