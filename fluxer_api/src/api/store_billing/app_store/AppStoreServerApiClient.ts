// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {readFile} from 'node:fs/promises';
import {Config} from '@app/api/Config';
import {
	type AppStoreRenewalInfoPayload,
	type AppStoreTransactionPayload,
	verifyRenewalInfo,
	verifyTransaction,
} from '@app/api/store_billing/app_store/AppStoreJwsVerifier';
import type {StoreEnvironment} from '@app/api/store_billing/StoreBillingTypes';
import {ms, seconds} from 'itty-time';
import {type CryptoKey, importPKCS8, SignJWT} from 'jose';
import {z} from 'zod';

export const APP_STORE_PRODUCTION_BASE_URL = 'https://api.storekit.apple.com';
export const APP_STORE_SANDBOX_BASE_URL = 'https://api.storekit-sandbox.apple.com';
export const APP_STORE_API_AUDIENCE = 'appstoreconnect-v1';
export const APP_STORE_TRANSACTION_ID_NOT_FOUND_ERROR = 4040010;
const APP_STORE_RETRYABLE_ERROR_CODES: ReadonlySet<number> = new Set([4040002, 4040004, 4040006, 5000001, 4290000]);
const APP_STORE_TOKEN_TTL_SECONDS = seconds('1 hour');
const APP_STORE_TOKEN_REUSE_SECONDS = seconds('50 minutes');
const APP_STORE_REQUEST_TIMEOUT_MS = ms('10 seconds');

const AppStoreErrorBodySchema = z.object({
	errorCode: z.number(),
	errorMessage: z.string().nullish(),
});

const TransactionInfoResponseSchema = z.object({
	signedTransactionInfo: z.string().min(1),
});

const StatusResponseSchema = z.object({
	environment: z.string().nullish(),
	bundleId: z.string().nullish(),
	appAppleId: z.number().nullish(),
	data: z.array(
		z.object({
			subscriptionGroupIdentifier: z.string(),
			lastTransactions: z.array(
				z.object({
					originalTransactionId: z.string().min(1),
					status: z.number(),
					signedTransactionInfo: z.string().min(1),
					signedRenewalInfo: z.string().nullish(),
				}),
			),
		}),
	),
});

const NotificationHistoryResponseSchema = z.object({
	notificationHistory: z.array(z.object({signedPayload: z.string().min(1)})).nullish(),
	hasMore: z.boolean().nullish(),
	paginationToken: z.string().nullish(),
});

const TestNotificationResponseSchema = z.object({
	testNotificationToken: z.string().min(1),
});

export class AppStoreServerApiError extends Error {
	constructor(
		message: string,
		readonly status: number | null,
		readonly errorCode: number | null,
		readonly retryable: boolean,
		readonly retryAfter: Date | null = null,
	) {
		super(message);
		this.name = 'AppStoreServerApiError';
	}
}

export function isAppStoreTransactionNotFound(error: unknown): boolean {
	return error instanceof AppStoreServerApiError && error.errorCode === APP_STORE_TRANSACTION_ID_NOT_FOUND_ERROR;
}

export interface AppStoreLastTransaction {
	originalTransactionId: string;
	status: number;
	transaction: AppStoreTransactionPayload;
	renewalInfo: AppStoreRenewalInfoPayload | null;
}

export interface AppStoreSubscriptionGroup {
	subscriptionGroupIdentifier: string;
	lastTransactions: Array<AppStoreLastTransaction>;
}

export interface AppStoreSubscriptionStatuses {
	environment: StoreEnvironment;
	bundleId: string;
	appAppleId: number | null;
	groups: Array<AppStoreSubscriptionGroup>;
}

export interface AppStoreTransactionInfo {
	environment: StoreEnvironment;
	transaction: AppStoreTransactionPayload;
}

interface AppStoreTransactionRequest {
	environment: StoreEnvironment | null;
	bundleId: string;
	transactionId: string;
}

interface AppStoreNotificationHistoryRequest {
	environment: StoreEnvironment;
	bundleId: string;
	startDate: Date;
	endDate: Date;
	notificationType?: string;
	onlyFailures?: boolean;
	paginationToken?: string | null;
}

interface AppStoreNotificationHistoryPage {
	signedPayloads: Array<string>;
	paginationToken: string | null;
}

interface AppStoreSetAppAccountTokenRequest {
	environment: StoreEnvironment;
	bundleId: string;
	originalTransactionId: string;
	appAccountToken: string;
}

interface AppStoreRequest {
	environment: StoreEnvironment;
	bundleId: string;
	method: 'GET' | 'PUT' | 'POST';
	path: string;
	body?: unknown;
}

interface CachedAuthToken {
	fingerprint: string;
	token: string;
	reuseUntilSeconds: number;
}

interface AppStoreServerApiClientOptions {
	now?: () => number;
	timeoutMs?: number;
}

function normalizePem(value: string): string {
	return value.replaceAll('\\n', '\n');
}

function hasText(value: string | undefined): value is string {
	return value !== undefined && value.trim().length > 0;
}

function parseRetryAfter(value: string | null): Date | null {
	if (!value || !/^\d+$/.test(value.trim())) {
		return null;
	}
	const epochMs = Number.parseInt(value.trim(), 10);
	return Number.isSafeInteger(epochMs) ? new Date(epochMs) : null;
}

function parseJsonOrNull(raw: string): unknown {
	if (raw.length === 0) {
		return null;
	}
	try {
		return JSON.parse(raw);
	} catch {
		return null;
	}
}

function invalidResponse(path: string, status: number): AppStoreServerApiError {
	return new AppStoreServerApiError(
		`App Store Server API returned an unexpected body for ${path}`,
		status,
		null,
		false,
	);
}

export class AppStoreServerApiClient {
	private readonly authTokens = new Map<string, CachedAuthToken>();
	private readonly signingKeys = new Map<string, Promise<CryptoKey>>();
	private readonly now: () => number;
	private readonly timeoutMs: number;

	constructor(options: AppStoreServerApiClientOptions = {}) {
		this.now = options.now ?? Date.now;
		this.timeoutMs = options.timeoutMs ?? APP_STORE_REQUEST_TIMEOUT_MS;
	}

	async getAllSubscriptionStatuses(request: AppStoreTransactionRequest): Promise<AppStoreSubscriptionStatuses> {
		return await this.withEnvironmentFallback(request.environment, async (environment) => {
			const path = `/inApps/v1/subscriptions/${encodeURIComponent(request.transactionId)}`;
			const body = await this.send({environment, bundleId: request.bundleId, method: 'GET', path});
			const parsed = StatusResponseSchema.safeParse(body);
			if (!parsed.success) {
				throw invalidResponse(path, 200);
			}
			if (parsed.data.bundleId && parsed.data.bundleId !== request.bundleId) {
				throw invalidResponse(path, 200);
			}
			const groups: Array<AppStoreSubscriptionGroup> = [];
			for (const group of parsed.data.data) {
				const lastTransactions: Array<AppStoreLastTransaction> = [];
				for (const item of group.lastTransactions) {
					const transaction = await verifyTransaction(item.signedTransactionInfo, {expectedEnvironment: environment});
					const renewalInfo = item.signedRenewalInfo
						? await verifyRenewalInfo(item.signedRenewalInfo, {expectedEnvironment: environment})
						: null;
					lastTransactions.push({
						originalTransactionId: item.originalTransactionId,
						status: item.status,
						transaction,
						renewalInfo,
					});
				}
				groups.push({subscriptionGroupIdentifier: group.subscriptionGroupIdentifier, lastTransactions});
			}
			return {environment, bundleId: request.bundleId, appAppleId: parsed.data.appAppleId ?? null, groups};
		});
	}

	async getTransactionInfo(request: AppStoreTransactionRequest): Promise<AppStoreTransactionInfo> {
		return await this.withEnvironmentFallback(request.environment, async (environment) => {
			const path = `/inApps/v1/transactions/${encodeURIComponent(request.transactionId)}`;
			const body = await this.send({environment, bundleId: request.bundleId, method: 'GET', path});
			const parsed = TransactionInfoResponseSchema.safeParse(body);
			if (!parsed.success) {
				throw invalidResponse(path, 200);
			}
			const transaction = await verifyTransaction(parsed.data.signedTransactionInfo, {
				expectedEnvironment: environment,
			});
			if (transaction.bundleId !== request.bundleId) {
				throw invalidResponse(path, 200);
			}
			return {environment, transaction};
		});
	}

	async setAppAccountToken(request: AppStoreSetAppAccountTokenRequest): Promise<void> {
		await this.send({
			environment: request.environment,
			bundleId: request.bundleId,
			method: 'PUT',
			path: `/inApps/v1/transactions/${encodeURIComponent(request.originalTransactionId)}/appAccountToken`,
			body: {appAccountToken: request.appAccountToken.toLowerCase()},
		});
	}

	async getNotificationHistory(request: AppStoreNotificationHistoryRequest): Promise<AppStoreNotificationHistoryPage> {
		const query = request.paginationToken ? `?paginationToken=${encodeURIComponent(request.paginationToken)}` : '';
		const path = `/inApps/v1/notifications/history${query}`;
		const body = await this.send({
			environment: request.environment,
			bundleId: request.bundleId,
			method: 'POST',
			path,
			body: {
				startDate: request.startDate.getTime(),
				endDate: request.endDate.getTime(),
				...(request.notificationType ? {notificationType: request.notificationType} : {}),
				...(request.onlyFailures ? {onlyFailures: true} : {}),
			},
		});
		const parsed = NotificationHistoryResponseSchema.safeParse(body);
		if (!parsed.success) {
			throw invalidResponse(path, 200);
		}
		return {
			signedPayloads: (parsed.data.notificationHistory ?? []).map((item) => item.signedPayload),
			paginationToken: parsed.data.hasMore ? (parsed.data.paginationToken ?? null) : null,
		};
	}

	async requestTestNotification(request: {environment: StoreEnvironment; bundleId: string}): Promise<string> {
		const path = '/inApps/v1/notifications/test';
		const body = await this.send({environment: request.environment, bundleId: request.bundleId, method: 'POST', path});
		const parsed = TestNotificationResponseSchema.safeParse(body);
		if (!parsed.success) {
			throw invalidResponse(path, 200);
		}
		return parsed.data.testNotificationToken;
	}

	private async withEnvironmentFallback<T>(
		environment: StoreEnvironment | null,
		run: (environment: StoreEnvironment) => Promise<T>,
	): Promise<T> {
		if (environment) {
			return await run(environment);
		}
		try {
			return await run('production');
		} catch (error) {
			if (!isAppStoreTransactionNotFound(error)) {
				throw error;
			}
			return await run('sandbox');
		}
	}

	private async send(request: AppStoreRequest): Promise<unknown> {
		const baseUrl = request.environment === 'production' ? APP_STORE_PRODUCTION_BASE_URL : APP_STORE_SANDBOX_BASE_URL;
		const token = await this.getAuthToken(request.bundleId);
		const headers: Record<string, string> = {Authorization: `Bearer ${token}`, Accept: 'application/json'};
		if (request.body !== undefined) {
			headers['Content-Type'] = 'application/json';
		}
		let response: Response;
		let raw: string;
		try {
			response = await fetch(`${baseUrl}${request.path}`, {
				method: request.method,
				headers,
				body: request.body === undefined ? undefined : JSON.stringify(request.body),
				redirect: 'manual',
				signal: AbortSignal.timeout(this.timeoutMs),
			});
			raw = await response.text();
		} catch (error) {
			throw new AppStoreServerApiError(
				`App Store Server API request failed: ${error instanceof Error ? error.message : String(error)}`,
				null,
				null,
				true,
			);
		}
		const body = parseJsonOrNull(raw);
		if (response.ok) {
			return body;
		}
		const parsedError = AppStoreErrorBodySchema.safeParse(body);
		const errorCode = parsedError.success ? parsedError.data.errorCode : null;
		const retryable =
			(errorCode !== null && APP_STORE_RETRYABLE_ERROR_CODES.has(errorCode)) ||
			response.status === 429 ||
			(errorCode === null && response.status >= 500);
		if (response.status === 401) {
			this.authTokens.delete(request.bundleId);
		}
		throw new AppStoreServerApiError(
			`App Store Server API ${request.method} ${request.path} failed with ${response.status}${errorCode === null ? '' : ` (${errorCode})`}`,
			response.status,
			errorCode,
			retryable,
			parseRetryAfter(response.headers.get('retry-after')),
		);
	}

	private async getAuthToken(bundleId: string): Promise<string> {
		const cfg = Config.appStore;
		if (!hasText(cfg.issuerId) || !hasText(cfg.keyId)) {
			throw new AppStoreServerApiError('App Store Server API credentials are not configured', null, null, false);
		}
		const privateKey = await this.resolvePrivateKey();
		const fingerprint = createHash('sha256')
			.update(JSON.stringify([bundleId, cfg.issuerId, cfg.keyId, privateKey]))
			.digest('hex');
		const nowSeconds = Math.floor(this.now() / 1000);
		const cached = this.authTokens.get(bundleId);
		if (cached && cached.fingerprint === fingerprint && cached.reuseUntilSeconds > nowSeconds) {
			return cached.token;
		}
		const key = await this.signingKey(privateKey);
		const token = await new SignJWT({bid: bundleId})
			.setProtectedHeader({alg: 'ES256', kid: cfg.keyId, typ: 'JWT'})
			.setIssuer(cfg.issuerId)
			.setIssuedAt(nowSeconds)
			.setExpirationTime(nowSeconds + APP_STORE_TOKEN_TTL_SECONDS)
			.setAudience(APP_STORE_API_AUDIENCE)
			.sign(key);
		this.authTokens.set(bundleId, {fingerprint, token, reuseUntilSeconds: nowSeconds + APP_STORE_TOKEN_REUSE_SECONDS});
		return token;
	}

	private async resolvePrivateKey(): Promise<string> {
		const cfg = Config.appStore;
		if (hasText(cfg.privateKey)) {
			return normalizePem(cfg.privateKey);
		}
		if (hasText(cfg.privateKeyPath)) {
			return normalizePem(await readFile(cfg.privateKeyPath, 'utf8'));
		}
		throw new AppStoreServerApiError('App Store Server API credentials are not configured', null, null, false);
	}

	private async signingKey(privateKey: string): Promise<CryptoKey> {
		const cached = this.signingKeys.get(privateKey);
		if (cached) {
			return await cached;
		}
		const pending = importPKCS8(privateKey, 'ES256');
		this.signingKeys.set(privateKey, pending);
		try {
			return await pending;
		} catch (error) {
			this.signingKeys.delete(privateKey);
			throw new AppStoreServerApiError(
				`App Store Server API signing key is invalid: ${error instanceof Error ? error.message : String(error)}`,
				null,
				null,
				false,
			);
		}
	}
}
