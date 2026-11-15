// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {OAuthAuthorizeModal} from '@app/features/auth/components/modals/OAuthAuthorizeModal';
import Authentication from '@app/features/auth/state/Authentication';
import {isDesktopLocalAppDocument} from '@app/features/platform/DesktopLocalAppRuntime';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';

const AUTHORIZE_PATH_SUFFIX = '/oauth2/authorize';

function addEndpointHost(hosts: Set<string>, endpoint: string | null | undefined): void {
	if (!endpoint) return;
	try {
		hosts.add(new URL(endpoint).host.toLowerCase());
	} catch {}
}

function getAllowedOAuthAuthorizeHosts(): ReadonlySet<string> {
	const hosts = new Set<string>();
	if (typeof location !== 'undefined' && !isDesktopLocalAppDocument()) {
		hosts.add(location.host.toLowerCase());
	}
	addEndpointHost(hosts, RuntimeConfig.webAppBaseUrl);
	addEndpointHost(hosts, RuntimeConfig.apiEndpoint);
	addEndpointHost(hosts, RuntimeConfig.apiPublicEndpoint);
	addEndpointHost(hosts, RuntimeConfig.marketingEndpoint);
	return hosts;
}

function parseUrl(rawUrl: string): URL | null {
	try {
		const fallbackBase = typeof location !== 'undefined' ? location.href : RuntimeConfig.webAppBaseUrl;
		return new URL(rawUrl, fallbackBase);
	} catch {
		return null;
	}
}

function isOAuthAuthorizePath(pathname: string): boolean {
	const normalizedPathname = pathname.replace(/\/+$/, '');
	return normalizedPathname === AUTHORIZE_PATH_SUFFIX || normalizedPathname.endsWith(AUTHORIZE_PATH_SUFFIX);
}

export function parseOAuthAuthorizeModalUrl(rawUrl: string): URL | null {
	const parsed = parseUrl(rawUrl);
	if (!parsed) return null;
	if (parsed.protocol !== 'https:' && parsed.protocol !== 'http:') return null;
	if (!getAllowedOAuthAuthorizeHosts().has(parsed.host.toLowerCase())) return null;
	if (!isOAuthAuthorizePath(parsed.pathname)) return null;
	return parsed;
}

export function openOAuthAuthorizeModalFromUrl(rawUrl: string): boolean {
	if (!Authentication.isAuthenticated) return false;
	const parsed = parseOAuthAuthorizeModalUrl(rawUrl);
	if (!parsed) return false;
	ModalCommands.pushWithKey(
		modal(() => (
			<OAuthAuthorizeModal
				search={parsed.search}
				data-flx="auth.oauth-authorize-modal-commands.oauth-authorize-modal"
			/>
		)),
		`oauth-authorize:${parsed.search}`,
	);
	return true;
}
