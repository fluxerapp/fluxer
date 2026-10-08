// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChildLogger} from '@electron/common/Logger';
import {DesktopLocalAppAPIProxyRequestHandler} from '@electron/main/LocalAppAPIProxyRequestHandler';
import {DesktopLocalAppFileRequestHandler, type LocalAppIndexContext} from '@electron/main/LocalAppFileRequestHandler';
import type {DesktopLocalAppFiles} from '@electron/main/LocalAppFileResolver';
import {isServedLocalAppExtension} from '@electron/main/LocalAppMime';
import type {DesktopLocalAppAuthorization} from '@electron/main/LocalAppProtocolAuthorization';
import type {DesktopLocalAppProxyClient} from '@electron/main/LocalAppProxyClient';
import {localAppProxyNotFoundResponse} from '@electron/main/LocalAppProxyHTTPPolicy';
import {DesktopLocalAppRemoteResourceRequestHandler} from '@electron/main/LocalAppRemoteResourceRequestHandler';
import {type LocalAppRuntimePlan, parseLocalAppRuntimeRoute} from '@electron/main/LocalAppRuntimePlans';
import {isLocalAppURL} from '@electron/main/LocalAppURL';
import {
	LOCAL_APP_API_PATH_PREFIX,
	LOCAL_APP_REMOTE_PROXY_PATH_PREFIX,
} from '@fluxer/desktop_ipc/src/LocalAppRouteContract';

const log = createChildLogger('LocalAppRequest');

const NAVIGATION_PREFERENCE_HEADER = 'Upgrade-Insecure-Requests';
const SEC_FETCH_DEST_HEADER = 'Sec-Fetch-Dest';

const BLOCKED_PROXY_DOCUMENT_DESTINATIONS: ReadonlySet<string> = new Set([
	'document',
	'embed',
	'frame',
	'iframe',
	'object',
]);

interface LocalAppRequestRuntimePlans {
	planForRoute(runtimeKey: string): Promise<LocalAppRuntimePlan | null>;
}

interface DesktopLocalAppRequestHandlerDependencies {
	readonly authorization: DesktopLocalAppAuthorization;
	readonly files: DesktopLocalAppFiles;
	readonly indexContext: () => LocalAppIndexContext;
	readonly apiProxyClient: DesktopLocalAppProxyClient;
	readonly resourceProxyClient: DesktopLocalAppProxyClient;
	readonly runtimePlans: LocalAppRequestRuntimePlans;
	readonly shutdownSignal: AbortSignal;
}

function isBlockedProxyDocumentDestination(destination: string): boolean {
	return BLOCKED_PROXY_DOCUMENT_DESTINATIONS.has(destination);
}

function isProxyDocumentRequest(request: Request): boolean {
	if (isBlockedProxyDocumentDestination(request.destination)) {
		return true;
	}
	if (isBlockedProxyDocumentDestination(request.headers.get(SEC_FETCH_DEST_HEADER) ?? '')) {
		return true;
	}
	return request.headers.get(NAVIGATION_PREFERENCE_HEADER) != null;
}

export class DesktopLocalAppRequestHandler {
	private readonly authorization: DesktopLocalAppAuthorization;
	private readonly shutdownSignal: AbortSignal;
	private readonly apiProxyRequestHandler: DesktopLocalAppAPIProxyRequestHandler;
	private readonly remoteResourceRequestHandler: DesktopLocalAppRemoteResourceRequestHandler;
	private readonly fileRequestHandler: DesktopLocalAppFileRequestHandler;

	public constructor(dependencies: DesktopLocalAppRequestHandlerDependencies) {
		this.authorization = dependencies.authorization;
		this.shutdownSignal = dependencies.shutdownSignal;
		this.apiProxyRequestHandler = new DesktopLocalAppAPIProxyRequestHandler({
			proxyClient: dependencies.apiProxyClient,
			runtimePlans: dependencies.runtimePlans,
		});
		this.remoteResourceRequestHandler = new DesktopLocalAppRemoteResourceRequestHandler({
			proxyClient: dependencies.resourceProxyClient,
			runtimePlans: dependencies.runtimePlans,
		});
		this.fileRequestHandler = new DesktopLocalAppFileRequestHandler({
			files: dependencies.files,
			indexContext: dependencies.indexContext,
			logger: log,
		});
	}

	public async handle(request: Request): Promise<Response> {
		if (!this.authorization.hasValidRequestAuthorization(request) && !isUnauthenticatedAssetRead(request)) {
			return localAppProxyNotFoundResponse('Not found');
		}
		const signal = AbortSignal.any([request.signal, this.shutdownSignal]);
		signal.throwIfAborted();
		if (matchesLocalAppRoute(request.url, LOCAL_APP_API_PATH_PREFIX)) {
			if (isProxyDocumentRequest(request)) {
				return localAppProxyNotFoundResponse('Not found');
			}
			return await this.apiProxyRequestHandler.handle(request, signal);
		}
		if (matchesLocalAppRoute(request.url, LOCAL_APP_REMOTE_PROXY_PATH_PREFIX)) {
			if (isProxyDocumentRequest(request)) {
				return localAppProxyNotFoundResponse('Not found');
			}
			return await this.remoteResourceRequestHandler.handle(request, signal);
		}
		return await this.fileRequestHandler.handle(request, signal);
	}
}

const ASSET_PATH_PREFIX = '/assets/';

function isUnauthenticatedAssetRead(request: Request): boolean {
	if (request.method !== 'GET' && request.method !== 'HEAD') {
		return false;
	}
	if (!isLocalAppURL(request.url)) {
		return false;
	}
	let pathname: string;
	try {
		pathname = new URL(request.url).pathname;
	} catch {
		return false;
	}
	if (!pathname.startsWith(ASSET_PATH_PREFIX)) {
		return false;
	}
	return isServedLocalAppExtension(pathname);
}

function matchesLocalAppRoute(value: string, basePath: string): boolean {
	if (!isLocalAppURL(value)) {
		return false;
	}
	return parseLocalAppRuntimeRoute(value, basePath) != null;
}
