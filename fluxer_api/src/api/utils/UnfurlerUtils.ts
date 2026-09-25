// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {stripOwnAttachmentSignature} from '@app/api/attachment/AttachmentUrls';
import {Config} from '@app/api/Config';
import {Logger} from '@app/api/Logger';
import * as InviteUtils from '@app/api/utils/InviteUtils';
import {URL_REGEX} from '@fluxer/constants/src/Core';
import * as idna from 'idna-uts46-hx';

const CLIENT_ROUTE_PATH_PREFIXES = ['/channels/', '/theme/', '/invite/', '/gift/', '/oauth2/', '/users/'];

interface ExcludedLinkBase {
	hostname: string;
	pathPrefix: string;
}

function normalizeHostname(hostname: string | undefined) {
	return hostname?.trim().toLowerCase() || '';
}

function getWebAppHostnames(): Array<string> {
	return Config.endpoints.webAppOrigins.flatMap((origin) => {
		try {
			return [new URL(origin).hostname];
		} catch {
			return [];
		}
	});
}

function endpointLinkBase(endpoint: string): ExcludedLinkBase | null {
	try {
		const url = new URL(endpoint);
		return {hostname: normalizeHostname(url.hostname), pathPrefix: `${url.pathname.replace(/\/+$/, '')}/`};
	} catch {
		return null;
	}
}

let _excludedLinkBases: Array<ExcludedLinkBase> | null = null;

function getExcludedLinkBases(): Array<ExcludedLinkBase> {
	if (!_excludedLinkBases) {
		const bases: Array<ExcludedLinkBase | null> = [
			...Config.hosts.unfurlIgnored.map((hostname) => ({hostname: normalizeHostname(hostname), pathPrefix: '/'})),
			endpointLinkBase(Config.endpoints.invite),
			endpointLinkBase(Config.endpoints.gift),
		];
		for (const hostname of [...getWebAppHostnames(), Config.hosts.marketing]) {
			for (const pathPrefix of CLIENT_ROUTE_PATH_PREFIXES) {
				bases.push({hostname: normalizeHostname(hostname), pathPrefix});
			}
		}
		_excludedLinkBases = bases.filter((base): base is ExcludedLinkBase => base !== null && base.hostname !== '');
	}
	return _excludedLinkBases;
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
		return getExcludedLinkBases().some(
			(base) => base.hostname === hostname && parsedUrl.pathname.startsWith(base.pathPrefix),
		);
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
		const canonical = stripOwnAttachmentSignature(encoded);
		if (!seen.has(canonical)) {
			seen.add(canonical);
			result.push(canonical);
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
