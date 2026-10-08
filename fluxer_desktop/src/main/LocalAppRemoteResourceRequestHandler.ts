// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChildLogger} from '@electron/common/Logger';
import {type DesktopLocalAppProxyClient, emitLocalAppUploadFailure} from '@electron/main/LocalAppProxyClient';
import {
	buildProxyResponseHeaders,
	buildRemoteResourceRequestHeaders,
	isAllowedRemoteProxyTarget,
	isSupportedRemoteResourceMethod,
	LOCAL_APP_PROXY_FAILURE_HEADERS,
	LOCAL_APP_PROXY_TEXT_RESPONSE_HEADERS,
	LOCAL_APP_REMOTE_PROXY_METHOD_NOT_ALLOWED_HEADERS,
	LocalAppProxyCacheDefault,
	localAppProxyNotFoundResponse,
	readLocalAppUploadId,
	readRequestContentLength,
	remoteProxyTargetURL,
} from '@electron/main/LocalAppProxyHTTPPolicy';
import {type LocalAppRuntimePlan, parseLocalAppRuntimeRoute} from '@electron/main/LocalAppRuntimePlans';
import {isLocalAppURL} from '@electron/main/LocalAppURL';
import {HttpStatus} from '@fluxer/constants/src/HttpConstants';
import {LOCAL_APP_REMOTE_PROXY_PATH_PREFIX} from '@fluxer/desktop_ipc/src/LocalAppRouteContract';

const log = createChildLogger('LocalAppRemoteResourceProxy');

interface LocalAppRemoteResourceRuntimePlans {
	planForRoute(runtimeKey: string): Promise<LocalAppRuntimePlan | null>;
}

interface DesktopLocalAppRemoteResourceRequestHandlerDependencies {
	readonly proxyClient: DesktopLocalAppProxyClient;
	readonly runtimePlans: LocalAppRemoteResourceRuntimePlans;
}

interface ForwardRemoteResourceRequest {
	readonly request: Request;
	readonly targetURL: string;
	readonly plan: LocalAppRuntimePlan;
	readonly signal: AbortSignal;
}

export class DesktopLocalAppRemoteResourceRequestHandler {
	private readonly proxyClient: DesktopLocalAppProxyClient;
	private readonly runtimePlans: LocalAppRemoteResourceRuntimePlans;

	public constructor(dependencies: DesktopLocalAppRemoteResourceRequestHandlerDependencies) {
		this.proxyClient = dependencies.proxyClient;
		this.runtimePlans = dependencies.runtimePlans;
	}

	public async handle(request: Request, signal: AbortSignal): Promise<Response> {
		if (!isLocalAppURL(request.url)) {
			return localAppProxyNotFoundResponse('Not found');
		}
		if (!isSupportedRemoteResourceMethod(request.method)) {
			return new Response('Method not allowed', {
				status: HttpStatus.METHOD_NOT_ALLOWED,
				headers: LOCAL_APP_REMOTE_PROXY_METHOD_NOT_ALLOWED_HEADERS,
			});
		}
		const route = parseLocalAppRuntimeRoute(request.url, LOCAL_APP_REMOTE_PROXY_PATH_PREFIX);
		if (route == null) {
			return localAppProxyNotFoundResponse('Unknown local app resource runtime');
		}
		const targetURL = remoteProxyTargetURL(request.url);
		if (targetURL == null) {
			return new Response('Missing remote resource URL', {
				status: HttpStatus.BAD_REQUEST,
				headers: LOCAL_APP_PROXY_TEXT_RESPONSE_HEADERS,
			});
		}
		const plan = await this.runtimePlans.planForRoute(route.runtimeKey);
		if (plan == null) {
			return localAppProxyNotFoundResponse('Unknown local app resource runtime');
		}
		return await this.forward({request, targetURL, plan, signal});
	}

	private async forward({request, targetURL, plan, signal}: ForwardRemoteResourceRequest): Promise<Response> {
		if (!isAllowedRemoteProxyTarget({method: request.method, targetURL, plan})) {
			log.warn('Blocked a local app remote resource proxy request', {instanceKey: plan.instanceKey});
			return new Response('Remote resource is not allowed for the requested instance', {
				status: HttpStatus.FORBIDDEN,
				headers: LOCAL_APP_PROXY_TEXT_RESPONSE_HEADERS,
			});
		}
		const uploadId = readLocalAppUploadId(request.headers);
		let response: Response | null = null;
		try {
			response = await this.proxyClient.fetch({
				targetURL,
				method: request.method,
				headers: buildRemoteResourceRequestHeaders(request.headers),
				body: request.body,
				signal,
				acceptEncoding: null,
				uploadId,
				uploadTotalBytes: readRequestContentLength(request.headers),
			});
			const body = await this.proxyClient.responseBodyForMethod({
				method: request.method,
				response,
				description: 'Local app remote resource proxy response',
			});
			const headers = buildProxyResponseHeaders({
				headers: response.headers,
				cacheDefault: LocalAppProxyCacheDefault.UPSTREAM,
			});
			const upstreamContentLength = response.headers.get('content-length');
			if (upstreamContentLength != null && response.headers.get('content-encoding') == null) {
				headers.set('Content-Length', upstreamContentLength);
			}
			return new Response(body, {
				status: response.status,
				statusText: response.statusText,
				headers,
			});
		} catch (error) {
			emitLocalAppUploadFailure(uploadId);
			return await this.failureResponse(response, error);
		}
	}

	private async failureResponse(response: Response | null, error: unknown): Promise<Response> {
		const settlement = await this.proxyClient.settleFailure({
			response,
			failure: error,
			description: 'Local app remote resource proxy request',
		});
		log.warn('Local app remote resource proxy request failed', {
			bodyCancellationFailed: settlement.bodyCancellationFailed,
			error: settlement.error,
		});
		return new Response('Network error during resource request', {
			status: HttpStatus.BAD_GATEWAY,
			headers: LOCAL_APP_PROXY_FAILURE_HEADERS,
		});
	}
}
