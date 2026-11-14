// SPDX-License-Identifier: AGPL-3.0-or-later

import {OFFICIAL_WEB_APP_HOSTS} from '@fluxer/instance_bootstrap/src/OfficialWebAppHosts';

const OFFICIAL_HOSTS = new Set<string>(OFFICIAL_WEB_APP_HOSTS);

export function isWebUpdateHost(host: string, webAppEndpoint: string): boolean {
	if (OFFICIAL_HOSTS.has(host)) {
		return true;
	}
	try {
		return new URL(webAppEndpoint).host === host;
	} catch {
		return false;
	}
}
