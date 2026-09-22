// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAPIApp} from '@app/api/App';
import {Config} from '@app/api/Config';
import {resetApiServicesForTesting} from '@app/api/CreateApiContext';
import {
	resetCassandraQueryExecutorForTesting,
	setCassandraQueryExecutorForTesting,
} from '@app/api/database/CassandraQueryExecution';
import {NullSearchProvider} from '@app/api/infrastructure/NullSearchProvider';
import {resetAbuseTrackingForTests} from '@app/api/middleware/AbusiveIpAutoBanner';
import {ipBanCache} from '@app/api/middleware/IpBanMiddleware';
import {
	setInjectedAccountPolicyEvaluator,
	setInjectedIpInfoService,
	setInjectedRegistrationRiskEvaluator,
} from '@app/api/middleware/ServiceMiddleware';
import {
	setInjectedBlueskyOAuthService,
	setInjectedGatewayService,
	setInjectedKVProvider,
	setInjectedMediaService,
	setInjectedSearchProviderService,
	setInjectedWorkerService,
} from '@app/api/middleware/ServiceRegistry';
import {
	getInstanceConfigRepository,
	setInjectedStorageService,
	setInjectedUnfurlerService,
} from '@app/api/middleware/ServiceSingletons';
import {torExitListCache} from '@app/api/middleware/TorExitListCache';
import type {ISearchProvider} from '@app/api/search/ISearchProvider';
import {drainSearchTasks} from '@app/api/search/SearchTaskTracker';
import {createCurrentBehaviorTestAccountPolicyEvaluator} from '@app/api/test/AccountPolicyTestEvaluator';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {MockBlueskyOAuthService} from '@app/api/test/mocks/MockBlueskyOAuthService';
import {MockKVProvider} from '@app/api/test/mocks/MockKVProvider';
import {MockStorageService} from '@app/api/test/mocks/MockStorageService';
import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import {NoopUnfurlerService} from '@app/api/test/mocks/NoopUnfurlerService';
import {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import {NoopWorkerService} from '@app/api/test/NoopWorkerService';
import {resetServiceStateForTesting} from '@app/api/test/ResetServiceState';
import {InMemorySearchProvider} from '@app/api/test/search/InMemorySearchProvider';
import {TestMediaService} from '@app/api/test/TestMediaService';
import type {HonoApp} from '@app/api/types/HonoEnv';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';

export interface ApiTestHarness {
	app: HonoApp;
	kvProvider: IKVProvider;
	storageService: MockStorageService;
	mockBlueskyOAuthService: MockBlueskyOAuthService;
	reset: () => Promise<void>;
	resetData: () => Promise<void>;
	shutdown: () => Promise<void>;
	requestJson: (params: {
		path: string;
		method?: string;
		body?: unknown;
		headers?: Record<string, string>;
	}) => Promise<Response>;
}

interface CreateApiTestHarnessOptions {
	search?: 'disabled' | 'enabled';
}

export async function createApiTestHarness(options: CreateApiTestHarnessOptions = {}): Promise<ApiTestHarness> {
	resetApiServicesForTesting();
	await clearBannedIpsState();
	setCassandraQueryExecutorForTesting(new InMemoryCassandraQueryExecutor());
	const kvProvider = new MockKVProvider();
	setInjectedKVProvider(kvProvider);
	setInjectedGatewayService(new NoopGatewayService());
	setInjectedWorkerService(new NoopWorkerService());
	const storageService = new MockStorageService();
	setInjectedStorageService(storageService);
	const mediaService = new TestMediaService(storageService);
	setInjectedMediaService(mediaService);
	const harnessLogger = new NoopLogger();
	let searchProvider: ISearchProvider | null = null;
	if (options.search === 'enabled') {
		searchProvider = new InMemorySearchProvider();
		await searchProvider.initialize();
	} else {
		searchProvider = new NullSearchProvider();
		await searchProvider.initialize();
	}
	setInjectedSearchProviderService(searchProvider);
	setInjectedUnfurlerService(new NoopUnfurlerService());
	const mockBlueskyOAuthService = new MockBlueskyOAuthService();
	setInjectedBlueskyOAuthService(mockBlueskyOAuthService);
	setInjectedAccountPolicyEvaluator(createCurrentBehaviorTestAccountPolicyEvaluator());
	await resetServiceStateForTesting();
	const {
		app,
		initialize: initializeApp,
		shutdown: shutdownApp,
	} = await createAPIApp({
		config: Config,
		logger: harnessLogger,
	});
	try {
		await initializeApp();
	} catch (error) {
		console.error('Failed to initialize API app for tests:', error);
		throw error;
	}
	async function reset(): Promise<void> {
		resetApiServicesForTesting();
		resetCassandraQueryExecutorForTesting();
		getInstanceConfigRepository().clearCacheForTesting();
		kvProvider.reset();
		mockBlueskyOAuthService.reset();
		setInjectedIpInfoService(undefined);
		setInjectedAccountPolicyEvaluator(createCurrentBehaviorTestAccountPolicyEvaluator());
		setInjectedRegistrationRiskEvaluator(undefined);
		setInjectedUnfurlerService(undefined);
		resetAbuseTrackingForTests();
		torExitListCache.clearForTesting();
	}
	async function resetData(): Promise<void> {
		resetApiServicesForTesting();
		resetCassandraQueryExecutorForTesting();
		getInstanceConfigRepository().clearCacheForTesting();
		kvProvider.reset();
		setInjectedUnfurlerService(undefined);
	}
	async function shutdown(): Promise<void> {
		try {
			await drainSearchTasks();
		} catch (_error) {}
		try {
			await shutdownApp();
		} catch (_error) {}
		resetCassandraQueryExecutorForTesting();
		getInstanceConfigRepository().clearCacheForTesting();
		if (searchProvider) {
			try {
				await searchProvider.shutdown();
			} catch (_error) {}
		}
		setInjectedWorkerService(new NoopWorkerService());
		setInjectedGatewayService(new NoopGatewayService());
		setInjectedKVProvider(new MockKVProvider());
		setInjectedIpInfoService(undefined);
		setInjectedAccountPolicyEvaluator(createCurrentBehaviorTestAccountPolicyEvaluator());
		setInjectedRegistrationRiskEvaluator(undefined);
		setInjectedUnfurlerService(undefined);
		resetAbuseTrackingForTests();
		const fallbackStorageService = new MockStorageService();
		setInjectedStorageService(fallbackStorageService);
		setInjectedMediaService(new TestMediaService(fallbackStorageService));
		setInjectedSearchProviderService(new NullSearchProvider());
		setInjectedBlueskyOAuthService(new MockBlueskyOAuthService());
		await resetServiceStateForTesting();
		resetApiServicesForTesting();
	}
	async function requestJson(params: {
		path: string;
		method?: string;
		body?: unknown;
		headers?: Record<string, string>;
	}): Promise<Response> {
		const {path, body, method = 'GET', headers} = params;
		const mergedHeaders = new Headers(headers);
		if (!mergedHeaders.has('content-type')) {
			mergedHeaders.set('content-type', 'application/json');
		}
		if (!mergedHeaders.has('x-forwarded-for')) {
			mergedHeaders.set('x-forwarded-for', '127.0.0.1');
		}
		const contentType = mergedHeaders.get('content-type');
		let requestBody: string | undefined;
		if (body !== undefined) {
			if (typeof body === 'string') {
				requestBody = body;
			} else if (contentType === 'application/json') {
				requestBody = JSON.stringify(body);
			} else {
				requestBody = JSON.stringify(body);
			}
		}
		return app.request(path, {
			method,
			headers: mergedHeaders,
			body: requestBody,
		});
	}
	return {
		app,
		kvProvider,
		storageService,
		mockBlueskyOAuthService,
		reset,
		resetData,
		shutdown,
		requestJson,
	};
}

async function clearBannedIpsState(): Promise<void> {
	ipBanCache.resetCaches();
	resetAbuseTrackingForTests();
}
