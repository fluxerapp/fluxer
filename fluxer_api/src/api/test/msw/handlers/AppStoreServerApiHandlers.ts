// SPDX-License-Identifier: AGPL-3.0-or-later

import {createPublicKey, randomUUID} from 'node:crypto';
import {readFile} from 'node:fs/promises';
import {Config} from '@app/api/Config';
import {jwtVerify} from 'jose';
import {HttpResponse, http, type RequestHandler} from 'msw';

type AppStoreFakeEnvironment = 'production' | 'sandbox';

const APP_STORE_FAKE_HOSTS: Record<AppStoreFakeEnvironment, string> = {
	production: 'https://api.storekit.apple.com',
	sandbox: 'https://api.storekit-sandbox.apple.com',
};
const APP_STORE_FAKE_ENVIRONMENT_NAMES: Record<AppStoreFakeEnvironment, string> = {
	production: 'Production',
	sandbox: 'Sandbox',
};
const DEFAULT_SUBSCRIPTION_GROUP = '21000000';
const UUID_PATTERN = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i;

export type AppStoreFakePayload = Record<string, unknown>;

export interface AppStoreFakeTransaction extends AppStoreFakePayload {
	transactionId: string;
	originalTransactionId: string;
	productId: string;
}

export interface AppStoreFakeSubscription {
	originalTransactionId: string;
	status: number;
	latestTransactionId: string;
	subscriptionGroupIdentifier?: string;
	customerId?: string;
	renewalInfo?: AppStoreFakePayload | null;
}

export interface AppStoreFakeError {
	status: number;
	errorCode?: number;
	errorMessage?: string;
	retryAfter?: string;
}

export interface AppStoreFakeErrorMatch {
	environment?: AppStoreFakeEnvironment;
	method?: string;
	path?: string;
}

export interface AppStoreFakeRequest {
	environment: AppStoreFakeEnvironment;
	method: string;
	path: string;
	body: unknown;
	bundleId: string | null;
}

export interface AppStoreServerApiFakeOptions {
	sign: (payload: AppStoreFakePayload) => Promise<string>;
	bundleId?: string;
	appAppleId?: number;
}

export interface AppStoreServerApiFake {
	handlers: Array<RequestHandler>;
	requests: Array<AppStoreFakeRequest>;
	addTransaction(environment: AppStoreFakeEnvironment, transaction: AppStoreFakeTransaction): void;
	updateTransaction(environment: AppStoreFakeEnvironment, transactionId: string, patch: AppStoreFakePayload): void;
	getTransaction(environment: AppStoreFakeEnvironment, transactionId: string): AppStoreFakeTransaction | null;
	putSubscription(environment: AppStoreFakeEnvironment, subscription: AppStoreFakeSubscription): void;
	getSubscription(environment: AppStoreFakeEnvironment, originalTransactionId: string): AppStoreFakeSubscription | null;
	failNext(match: AppStoreFakeErrorMatch, error: AppStoreFakeError): void;
	addNotificationHistory(environment: AppStoreFakeEnvironment, entry: AppStoreFakeNotificationHistoryEntry): void;
}

interface AppStoreFakeNotificationHistoryEntry {
	signedPayload: string;
	notificationType: string;
	sentAt: number;
	failed: boolean;
}

const NOTIFICATION_HISTORY_PAGE_SIZE = 20;

interface QueuedError {
	match: AppStoreFakeErrorMatch;
	error: AppStoreFakeError;
}

function errorResponse(error: AppStoreFakeError): Response {
	const headers: Record<string, string> = {};
	if (error.retryAfter !== undefined) {
		headers['Retry-After'] = error.retryAfter;
	}
	if (error.errorCode === undefined) {
		return new HttpResponse(null, {status: error.status, headers});
	}
	return HttpResponse.json(
		{errorCode: error.errorCode, errorMessage: error.errorMessage ?? 'App Store fake error'},
		{status: error.status, headers},
	);
}

async function configuredPrivateKey(): Promise<string | null> {
	if (Config.appStore.privateKey) {
		return Config.appStore.privateKey.replaceAll('\\n', '\n');
	}
	if (Config.appStore.privateKeyPath) {
		return await readFile(Config.appStore.privateKeyPath, 'utf8');
	}
	return null;
}

async function authorizedBundleId(request: Request): Promise<string | null> {
	const header = request.headers.get('authorization');
	const privateKey = await configuredPrivateKey();
	if (!header?.startsWith('Bearer ') || !privateKey) {
		return null;
	}
	try {
		const publicKey = createPublicKey(privateKey);
		const {payload, protectedHeader} = await jwtVerify(header.slice('Bearer '.length), publicKey, {
			algorithms: ['ES256'],
			issuer: Config.appStore.issuerId,
			audience: 'appstoreconnect-v1',
			typ: 'JWT',
		});
		if (protectedHeader.kid !== Config.appStore.keyId || typeof payload.bid !== 'string') {
			return null;
		}
		if (typeof payload.iat !== 'number' || typeof payload.exp !== 'number' || payload.exp - payload.iat > 3600) {
			return null;
		}
		return payload.bid;
	} catch {
		return null;
	}
}

export function createAppStoreServerApiFake(options: AppStoreServerApiFakeOptions): AppStoreServerApiFake {
	const bundleId = options.bundleId ?? 'com.fluxer';
	const appAppleId = options.appAppleId ?? 1234567890;
	const transactions: Record<AppStoreFakeEnvironment, Map<string, AppStoreFakeTransaction>> = {
		production: new Map(),
		sandbox: new Map(),
	};
	const subscriptions: Record<AppStoreFakeEnvironment, Map<string, AppStoreFakeSubscription>> = {
		production: new Map(),
		sandbox: new Map(),
	};
	const queuedErrors: Array<QueuedError> = [];
	const requests: Array<AppStoreFakeRequest> = [];
	const notificationHistory: Record<AppStoreFakeEnvironment, Array<AppStoreFakeNotificationHistoryEntry>> = {
		production: [],
		sandbox: [],
	};

	function takeQueuedError(environment: AppStoreFakeEnvironment, method: string, path: string): Response | null {
		const index = queuedErrors.findIndex(
			({match}) =>
				(match.environment === undefined || match.environment === environment) &&
				(match.method === undefined || match.method === method) &&
				(match.path === undefined || path.includes(match.path)),
		);
		if (index === -1) {
			return null;
		}
		const [queued] = queuedErrors.splice(index, 1);
		return errorResponse(queued.error);
	}

	async function signTransaction(
		environment: AppStoreFakeEnvironment,
		transaction: AppStoreFakeTransaction,
	): Promise<string> {
		return await options.sign({
			bundleId,
			environment: APP_STORE_FAKE_ENVIRONMENT_NAMES[environment],
			type: 'Auto-Renewable Subscription',
			inAppOwnershipType: 'PURCHASED',
			purchaseDate: Date.now(),
			...transaction,
			signedDate: typeof transaction.signedDate === 'number' ? transaction.signedDate : Date.now(),
		});
	}

	async function signRenewalInfo(
		environment: AppStoreFakeEnvironment,
		transaction: AppStoreFakeTransaction,
		renewalInfo: AppStoreFakePayload,
	): Promise<string> {
		return await options.sign({
			originalTransactionId: transaction.originalTransactionId,
			productId: transaction.productId,
			autoRenewProductId: transaction.productId,
			autoRenewStatus: 1,
			environment: APP_STORE_FAKE_ENVIRONMENT_NAMES[environment],
			...renewalInfo,
			signedDate: typeof renewalInfo.signedDate === 'number' ? renewalInfo.signedDate : Date.now(),
		});
	}

	function notFound(errorCode = 4040010): Response {
		return errorResponse({status: 404, errorCode, errorMessage: 'Transaction id not found.'});
	}

	function route(
		environment: AppStoreFakeEnvironment,
		method: 'get' | 'put' | 'post',
		path: string,
		handle: (params: Record<string, string>, body: unknown) => Promise<Response>,
	): RequestHandler {
		return http[method](`${APP_STORE_FAKE_HOSTS[environment]}${path}`, async ({request, params}) => {
			const url = new URL(request.url);
			const raw = request.method === 'GET' ? '' : await request.text();
			const body: unknown = raw.length > 0 ? JSON.parse(raw) : null;
			const authorized = await authorizedBundleId(request);
			requests.push({environment, method: request.method, path: url.pathname, body, bundleId: authorized});
			if (!authorized) {
				return new HttpResponse(null, {status: 401});
			}
			const queued = takeQueuedError(environment, request.method, url.pathname);
			if (queued) {
				return queued;
			}
			const stringParams: Record<string, string> = {};
			const paginationToken = url.searchParams.get('paginationToken');
			if (paginationToken !== null) {
				stringParams['paginationToken'] = paginationToken;
			}
			for (const [key, value] of Object.entries(params)) {
				if (typeof value === 'string') {
					stringParams[key] = value;
				}
			}
			return await handle(stringParams, body);
		});
	}

	function findTransaction(
		environment: AppStoreFakeEnvironment,
		transactionId: string,
	): AppStoreFakeTransaction | undefined {
		return transactions[environment].get(transactionId);
	}

	function environmentHandlers(environment: AppStoreFakeEnvironment): Array<RequestHandler> {
		return [
			route(environment, 'get', '/inApps/v1/subscriptions/:transactionId', async ({transactionId}) => {
				const transaction = findTransaction(environment, transactionId);
				if (!transaction) {
					return notFound();
				}
				const owner = subscriptions[environment].get(transaction.originalTransactionId);
				const customerId = owner?.customerId ?? transaction.originalTransactionId;
				const groups = new Map<string, Array<AppStoreFakePayload>>();
				for (const subscription of subscriptions[environment].values()) {
					if ((subscription.customerId ?? subscription.originalTransactionId) !== customerId) {
						continue;
					}
					const latest = findTransaction(environment, subscription.latestTransactionId);
					if (!latest) {
						continue;
					}
					const groupId = subscription.subscriptionGroupIdentifier ?? DEFAULT_SUBSCRIPTION_GROUP;
					const items = groups.get(groupId) ?? [];
					items.push({
						originalTransactionId: subscription.originalTransactionId,
						status: subscription.status,
						signedTransactionInfo: await signTransaction(environment, latest),
						...(subscription.renewalInfo === null
							? {}
							: {signedRenewalInfo: await signRenewalInfo(environment, latest, subscription.renewalInfo ?? {})}),
					});
					groups.set(groupId, items);
				}
				return HttpResponse.json({
					environment: APP_STORE_FAKE_ENVIRONMENT_NAMES[environment],
					bundleId,
					...(environment === 'production' ? {appAppleId} : {}),
					data: [...groups.entries()].map(([subscriptionGroupIdentifier, lastTransactions]) => ({
						subscriptionGroupIdentifier,
						lastTransactions,
					})),
				});
			}),
			route(environment, 'get', '/inApps/v1/transactions/:transactionId', async ({transactionId}) => {
				const transaction = findTransaction(environment, transactionId);
				if (!transaction) {
					return notFound();
				}
				return HttpResponse.json({signedTransactionInfo: await signTransaction(environment, transaction)});
			}),
			route(
				environment,
				'put',
				'/inApps/v1/transactions/:originalTransactionId/appAccountToken',
				async ({originalTransactionId}, body) => {
					const token =
						body !== null && typeof body === 'object' && 'appAccountToken' in body ? body.appAccountToken : null;
					if (typeof token !== 'string' || !UUID_PATTERN.test(token)) {
						return errorResponse({status: 400, errorCode: 4000183});
					}
					const transaction = findTransaction(environment, originalTransactionId);
					if (!transaction) {
						return notFound(4040005);
					}
					if (transaction.originalTransactionId !== originalTransactionId) {
						return errorResponse({status: 400, errorCode: 4000187});
					}
					if (transaction.inAppOwnershipType === 'FAMILY_SHARED') {
						return errorResponse({status: 400, errorCode: 4000185});
					}
					const subscription = subscriptions[environment].get(originalTransactionId);
					const latest = subscription ? findTransaction(environment, subscription.latestTransactionId) : transaction;
					if (latest) {
						latest.appAccountToken = token;
					}
					if (subscription && subscription.renewalInfo !== null) {
						subscription.renewalInfo = {...subscription.renewalInfo, appAccountToken: token};
					}
					return new HttpResponse(null, {status: 200});
				},
			),
			route(environment, 'post', '/inApps/v1/notifications/history', async ({paginationToken}, body) => {
				const filter = (body ?? {}) as {
					startDate?: number;
					endDate?: number;
					notificationType?: string;
					onlyFailures?: boolean;
				};
				if (typeof filter.startDate !== 'number' || typeof filter.endDate !== 'number') {
					return errorResponse({status: 400, errorCode: 4000028});
				}
				const matching = notificationHistory[environment].filter(
					(entry) =>
						entry.sentAt >= filter.startDate! &&
						entry.sentAt <= filter.endDate! &&
						(filter.notificationType === undefined || entry.notificationType === filter.notificationType) &&
						(!filter.onlyFailures || entry.failed),
				);
				const offset = paginationToken ? Number(paginationToken) : 0;
				const page = matching.slice(offset, offset + NOTIFICATION_HISTORY_PAGE_SIZE);
				const nextOffset = offset + NOTIFICATION_HISTORY_PAGE_SIZE;
				const hasMore = nextOffset < matching.length;
				return HttpResponse.json({
					notificationHistory: page.map((entry) => ({signedPayload: entry.signedPayload})),
					hasMore,
					...(hasMore ? {paginationToken: String(nextOffset)} : {}),
				});
			}),
			route(environment, 'post', '/inApps/v1/notifications/test', async () =>
				HttpResponse.json({testNotificationToken: `${randomUUID()}_${Date.now()}`}),
			),
		];
	}

	return {
		handlers: [...environmentHandlers('production'), ...environmentHandlers('sandbox')],
		requests,
		addTransaction(environment, transaction) {
			transactions[environment].set(transaction.transactionId, {...transaction});
		},
		updateTransaction(environment, transactionId, patch) {
			const existing = transactions[environment].get(transactionId);
			if (!existing) {
				throw new Error(`Unknown fake App Store transaction ${transactionId}`);
			}
			Object.assign(existing, patch);
		},
		getTransaction(environment, transactionId) {
			return transactions[environment].get(transactionId) ?? null;
		},
		putSubscription(environment, subscription) {
			subscriptions[environment].set(subscription.originalTransactionId, {...subscription});
		},
		getSubscription(environment, originalTransactionId) {
			return subscriptions[environment].get(originalTransactionId) ?? null;
		},
		failNext(match, error) {
			queuedErrors.push({match, error});
		},
		addNotificationHistory(environment, entry) {
			notificationHistory[environment].push({...entry});
		},
	};
}
