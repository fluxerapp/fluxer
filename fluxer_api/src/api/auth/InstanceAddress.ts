// SPDX-License-Identifier: AGPL-3.0-or-later

import type {APIConfig} from '@app/api/config/APIConfig';
import {parseLoginHandle} from '@app/api/user/UniqueUsernames';
import {USERNAME_MODE_DISCRIMINATOR} from '@app/api/user/UserTag';
import {normaliseUsernameCandidate} from '@app/api/utils/UsernameSuggestionUtils';

const ADDRESS_HOST_REGEX = /^[^\s/?#@\\]+$/u;

function normaliseHost(host: string): string | null {
	const trimmed = host.trim();
	if (!ADDRESS_HOST_REGEX.test(trimmed)) return null;
	const hostname = URL.parse(`http://${trimmed}`)?.hostname.replace(/\.$/u, '');
	return hostname ? hostname : null;
}

function hostOfUrl(url: string): string | null {
	return URL.parse(url)?.host ?? null;
}

function getInstanceHosts(config: APIConfig): Set<string> {
	const hosts = new Set<string>();
	for (const host of [
		hostOfUrl(config.endpoints.webApp),
		...config.endpoints.webAppOrigins.map(hostOfUrl),
		config.instance.baseDomain,
	]) {
		const normalised = host === null ? null : normaliseHost(host);
		if (normalised !== null) hosts.add(normalised);
	}
	return hosts;
}

export function getPrimaryInstanceHost(config: APIConfig): string {
	return URL.parse(config.endpoints.webApp)?.hostname ?? config.instance.baseDomain;
}

export function getLocalPartAtInstance(config: APIConfig, address: string): string | null {
	const at = address.indexOf('@');
	if (at <= 0 || address.lastIndexOf('@') !== at) return null;
	const host = normaliseHost(address.slice(at + 1));
	return host !== null && getInstanceHosts(config).has(host) ? address.slice(0, at) : null;
}

export function usernameFromInstanceLocalPart(localPart: string): string | null {
	const handle = parseLoginHandle(localPart);
	if (handle) {
		return handle.discriminator === null || handle.discriminator === USERNAME_MODE_DISCRIMINATOR
			? handle.username
			: null;
	}
	return normaliseUsernameCandidate(localPart) || null;
}
