// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChildLogger} from '@electron/common/Logger';
import {
	type DesktopLocalAppProxyClient,
	emitLocalAppUploadFailure,
	LOCAL_APP_PROXY_ACCEPT_ENCODING,
} from '@electron/main/LocalAppProxyClient';
import {
	buildAPIRequestHeaders,
	buildAPITargetURL,
	buildProxyResponseHeaders,
	LOCAL_APP_PROXY_FAILURE_HEADERS,
	LOCAL_APP_PROXY_HEAD_METHOD,
	LocalAppProxyCacheDefault,
	localAppProxyNotFoundResponse,
	readLocalAppUploadId,
	readRequestContentLength,
} from '@electron/main/LocalAppProxyHTTPPolicy';
import {
	httpOriginSource,
	type LocalAppRuntimePlan,
	parseLocalAppRuntimeRoute,
} from '@electron/main/LocalAppRuntimePlans';
import {isLocalAppURL} from '@electron/main/LocalAppURL';
import {t} from '@electron/main/MainI18n';
import {HttpStatus} from '@fluxer/constants/src/HttpConstants';
import {LOCAL_APP_API_PATH_PREFIX} from '@fluxer/desktop_ipc/src/LocalAppRouteContract';

const log = createChildLogger('LocalAppAPIProxy');

interface LocalAppAPIProxyRuntimePlans {
	planForRoute(runtimeKey: string): Promise<LocalAppRuntimePlan | null>;
}

interface DesktopLocalAppAPIProxyRequestHandlerDependencies {
	readonly proxyClient: DesktopLocalAppProxyClient;
	readonly runtimePlans: LocalAppAPIProxyRuntimePlans;
}

interface ForwardAPIProxyRequest {
	readonly request: Request;
	readonly plan: LocalAppRuntimePlan;
	readonly localPathPrefix: string;
	readonly signal: AbortSignal;
}

export class DesktopLocalAppAPIProxyRequestHandler {
	private readonly proxyClient: DesktopLocalAppProxyClient;
	private readonly runtimePlans: LocalAppAPIProxyRuntimePlans;

	public constructor(dependencies: DesktopLocalAppAPIProxyRequestHandlerDependencies) {
		this.proxyClient = dependencies.proxyClient;
		this.runtimePlans = dependencies.runtimePlans;
	}

	public async handle(request: Request, signal: AbortSignal): Promise<Response> {
		if (!isLocalAppURL(request.url)) {
			return localAppProxyNotFoundResponse('Not found');
		}
		const route = parseLocalAppRuntimeRoute(request.url, LOCAL_APP_API_PATH_PREFIX);
		if (route == null) {
			return localAppProxyNotFoundResponse('Unknown local app API runtime');
		}
		const plan = await this.runtimePlans.planForRoute(route.runtimeKey);
		if (plan == null) {
			return localAppProxyNotFoundResponse('Unknown local app API runtime');
		}
		if (httpOriginSource(plan.endpoints.apiEndpoint) == null) {
			log.warn('Active instance API endpoint is not proxyable', {instanceKey: plan.instanceKey});
			return unavailableResponse();
		}
		return await this.forward({request, plan, localPathPrefix: route.localPathPrefix, signal});
	}

	private async forward({request, plan, localPathPrefix, signal}: ForwardAPIProxyRequest): Promise<Response> {
		const uploadId = readLocalAppUploadId(request.headers);
		const targetURL = buildAPITargetURL({
			apiEndpoint: plan.endpoints.apiEndpoint,
			requestURL: request.url,
			localPathPrefix,
		});
		let response: Response | null = null;
		try {
			response = await this.proxyClient.fetch({
				targetURL,
				method: request.method,
				headers: buildAPIRequestHeaders({
					headers: request.headers,
					method: request.method,
					origin: httpOriginSource(plan.endpoints.webAppEndpoint),
				}),
				body: request.body,
				signal,
				acceptEncoding: LOCAL_APP_PROXY_ACCEPT_ENCODING,
				uploadId,
				uploadTotalBytes: readRequestContentLength(request.headers),
			});
			return await this.buildResponse(request, response);
		} catch (error) {
			emitLocalAppUploadFailure(uploadId);
			return await this.failureResponse(response, error);
		}
	}

	private async buildResponse(request: Request, response: Response): Promise<Response> {
		const headers = buildProxyResponseHeaders({
			headers: response.headers,
			cacheDefault: LocalAppProxyCacheDefault.NO_STORE,
		});
		const body =
			request.method === LOCAL_APP_PROXY_HEAD_METHOD
				? await this.proxyClient.responseBodyForMethod({
						method: request.method,
						response,
						description: 'Local app API proxy response',
					})
				: response.body;
		return new Response(body, {status: response.status, statusText: response.statusText, headers});
	}

	private async failureResponse(response: Response | null, error: unknown): Promise<Response> {
		const settlement = await this.proxyClient.settleFailure({
			response,
			failure: error,
			description: 'Local app API proxy request',
		});
		log.warn('Local app API proxy request failed', {
			bodyCancellationFailed: settlement.bodyCancellationFailed,
			error: settlement.error,
		});
		return new Response('Network error during API request', {
			status: HttpStatus.BAD_GATEWAY,
			headers: LOCAL_APP_PROXY_FAILURE_HEADERS,
		});
	}
}

function unavailableResponse(): Response {
	return new Response(t('desktop.localApp.apiUnavailable'), {
		status: HttpStatus.SERVICE_UNAVAILABLE,
		headers: LOCAL_APP_PROXY_FAILURE_HEADERS,
	});
}
