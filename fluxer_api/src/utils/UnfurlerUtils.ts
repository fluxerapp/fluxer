/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import * as idna from 'idna-uts46-hx';
import {Config} from '~/Config';
import {URL_REGEX} from '~/Constants';
import {Logger} from '~/Logger';
import * as InviteUtils from '~/utils/InviteUtils';

const MARKETING_PATH_PREFIXES = ['/channels/', '/theme/'];

const WEB_APP_HOSTNAME = (() => {
	try {
		return new URL(Config.endpoints.webApp).hostname;
	} catch {
		return '';
	}
})();

const EXCLUDED_HOSTNAMES = new Set<string>();

const normalizeHostname = (hostname: string | undefined) => hostname?.trim().toLowerCase() || '';
const MARKETING_HOSTNAME = normalizeHostname(Config.hosts.marketing);
const isMarketingPath = (hostname: string, pathname: string) =>
	hostname === MARKETING_HOSTNAME && MARKETING_PATH_PREFIXES.some((prefix) => pathname.startsWith(prefix));

const addHostname = (hostname: string | undefined) => {
	const normalized = normalizeHostname(hostname);
	if (!normalized) return;
	EXCLUDED_HOSTNAMES.add(normalized);
};

addHostname(Config.hosts.invite);
addHostname(Config.hosts.gift);
Config.hosts.unfurlIgnored.forEach(addHostname);
addHostname(WEB_APP_HOSTNAME);

export const idnaEncodeURL = (url: string) => {
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
};

export const isValidURL = (url: string) => {
	try {
		const parsedUrl = new URL(url);
		return parsedUrl.protocol === 'http:' || parsedUrl.protocol === 'https:';
	} catch {
		return false;
	}
};

export const isFluxerAppExcludedURL = (url: string) => {
	try {
		const parsedUrl = new URL(url);
		const hostname = normalizeHostname(parsedUrl.hostname);
		const isMarketingPathMatch = isMarketingPath(hostname, parsedUrl.pathname);

		return isMarketingPathMatch || EXCLUDED_HOSTNAMES.has(hostname);
	} catch {
		return false;
	}
};

export const extractURLs = (inputText: string) => {
	let text = inputText;
	text = text.replace(/`[^`]*`/g, '');
	text = text.replace(/```.*?```/gs, '');
	text = text.replace(/\|\|([\s\S]*?)\|\|/g, ' $1 ');
	text = text.replace(/\|\|/g, ' ');
	text = text.replace(/\[([^\]]+)\]\((https?:\/\/[^\s)]+)\)/g, '$2');
	text = text.replace(/<https?:\/\/[^\s]+>/g, '');
	const urls = text.match(URL_REGEX) || [];
	const validURLs = urls.filter(isValidURL);
	const filteredURLs = validURLs
		.filter((url) => InviteUtils.findInvite(url) == null)
		.filter((url) => !isFluxerAppExcludedURL(url));
	const encodedURLs = filteredURLs.map(idnaEncodeURL).filter(Boolean);
	const uniqueURLs = Array.from(new Set(encodedURLs));
	return uniqueURLs.slice(0, 5);
};
