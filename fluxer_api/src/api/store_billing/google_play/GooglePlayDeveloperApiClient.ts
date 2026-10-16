// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GooglePlayAccessTokenProvider} from '@app/api/store_billing/google_play/GooglePlayAccessTokenProvider';
import {
	classifyGooglePlayStatus,
	GooglePlayApiError,
	parseRetryAfterMs,
} from '@app/api/store_billing/google_play/GooglePlayApiError';
import {isGooglePlayPackageConfigured} from '@app/api/store_billing/StoreBillingConfig';
import {ms} from 'itty-time';
import {z} from 'zod';

export const GOOGLE_PLAY_API_BASE_URL = 'https://androidpublisher.googleapis.com/androidpublisher/v3/applications';
const REQUEST_TIMEOUT_MS = ms('15 seconds');
const VOIDED_PURCHASE_TYPE_SUBSCRIPTIONS_AND_PRODUCTS = '1';

const ExternalAccountIdentifiersSchema = z.object({
	externalAccountId: z.string().optional(),
	obfuscatedExternalAccountId: z.string().optional(),
	obfuscatedExternalProfileId: z.string().optional(),
	obfuscatedAccountId: z.string().optional(),
	obfuscatedProfileId: z.string().optional(),
});

const SubscriptionLineItemSchema = z.object({
	productId: z.string(),
	expiryTime: z.string().optional(),
	latestSuccessfulOrderId: z.string().optional(),
	autoRenewingPlan: z
		.object({
			autoRenewEnabled: z.boolean().optional(),
		})
		.optional(),
	prepaidPlan: z
		.object({
			allowExtendAfterTime: z.string().optional(),
		})
		.optional(),
	offerDetails: z
		.object({
			basePlanId: z.string().optional(),
			offerId: z.string().optional(),
			offerTags: z.array(z.string()).optional(),
		})
		.optional(),
	deferredItemReplacement: z
		.object({
			productId: z.string().optional(),
		})
		.optional(),
});

const SubscriptionPurchaseV2Schema = z.object({
	kind: z.string().optional(),
	regionCode: z.string().optional(),
	lineItems: z.array(SubscriptionLineItemSchema).optional(),
	startTime: z.string().optional(),
	subscriptionState: z.string().optional(),
	latestOrderId: z.string().optional(),
	linkedPurchaseToken: z.string().optional(),
	pausedStateContext: z
		.object({
			autoResumeTime: z.string().optional(),
		})
		.optional(),
	canceledStateContext: z
		.object({
			userInitiatedCancellation: z.object({cancelTime: z.string().optional()}).optional(),
			systemInitiatedCancellation: z.object({}).optional(),
			developerInitiatedCancellation: z.object({}).optional(),
			replacementCancellation: z.object({}).optional(),
		})
		.optional(),
	testPurchase: z.object({}).optional(),
	acknowledgementState: z.string().optional(),
	externalAccountIdentifiers: ExternalAccountIdentifiersSchema.optional(),
	outOfAppPurchaseContext: z
		.object({
			expiredExternalAccountIdentifiers: ExternalAccountIdentifiersSchema.optional(),
			expiredPurchaseToken: z.string().optional(),
		})
		.optional(),
	etag: z.string().optional(),
});

export type SubscriptionPurchaseV2 = z.infer<typeof SubscriptionPurchaseV2Schema>;
export type SubscriptionPurchaseLineItem = z.infer<typeof SubscriptionLineItemSchema>;

const ProductLineItemSchema = z.object({
	productId: z.string(),
	productOfferDetails: z
		.object({
			offerId: z.string().optional(),
			purchaseOptionId: z.string().optional(),
			quantity: z.number().int().optional(),
			refundableQuantity: z.number().int().optional(),
			consumptionState: z.string().optional(),
		})
		.optional(),
});

const ProductPurchaseV2Schema = z.object({
	kind: z.string().optional(),
	productLineItem: z.array(ProductLineItemSchema).optional(),
	purchaseStateContext: z
		.object({
			purchaseState: z.string().optional(),
		})
		.optional(),
	testPurchaseContext: z
		.object({
			fopType: z.string().optional(),
		})
		.optional(),
	orderId: z.string().optional(),
	obfuscatedExternalAccountId: z.string().optional(),
	obfuscatedExternalProfileId: z.string().optional(),
	regionCode: z.string().optional(),
	purchaseCompletionTime: z.string().optional(),
	acknowledgementState: z.string().optional(),
});

export type ProductPurchaseV2 = z.infer<typeof ProductPurchaseV2Schema>;

const VoidedPurchaseSchema = z.object({
	kind: z.string().optional(),
	purchaseToken: z.string(),
	purchaseTimeMillis: z.string().optional(),
	voidedTimeMillis: z.string().optional(),
	orderId: z.string().optional(),
	voidedSource: z.number().int().optional(),
	voidedReason: z.number().int().optional(),
	voidedQuantity: z.number().int().optional(),
});

export type GooglePlayVoidedPurchase = z.infer<typeof VoidedPurchaseSchema>;

const VoidedPurchasesResponseSchema = z.object({
	voidedPurchases: z.array(VoidedPurchaseSchema).optional(),
	tokenPagination: z
		.object({
			nextPageToken: z.string().optional(),
		})
		.optional(),
});

const GoogleErrorBodySchema = z.object({
	error: z.object({
		code: z.number().optional(),
		message: z.string().optional(),
		status: z.string().optional(),
		errors: z.array(z.object({reason: z.string().optional()})).optional(),
	}),
});

export type GooglePlayCancellationType = 'USER_REQUESTED_STOP_RENEWALS' | 'DEVELOPER_REQUESTED_STOP_PAYMENTS';

export interface GooglePlayAcknowledgeOptions {
	obfuscatedAccountId?: string;
}

export interface ListGooglePlayVoidedPurchasesParams {
	startTime: Date;
	endTime?: Date;
	pageToken?: string;
	maxResults?: number;
}

export interface GooglePlayVoidedPurchasesPage {
	voidedPurchases: Array<GooglePlayVoidedPurchase>;
	nextPageToken: string | null;
}

interface GooglePlayDeveloperApiClientOptions {
	accessTokenProvider: GooglePlayAccessTokenProvider;
	baseUrl?: string;
	timeoutMs?: number;
	now?: () => number;
}

interface RequestParams {
	method: 'GET' | 'POST';
	packageName: string;
	path: string;
	query?: Record<string, string>;
	body?: unknown;
}

function segment(value: string): string {
	return encodeURIComponent(value);
}

export class GooglePlayDeveloperApiClient {
	private readonly accessTokenProvider: GooglePlayAccessTokenProvider;
	private readonly baseUrl: string;
	private readonly timeoutMs: number;
	private readonly now: () => number;

	constructor(options: GooglePlayDeveloperApiClientOptions) {
		this.accessTokenProvider = options.accessTokenProvider;
		this.baseUrl = (options.baseUrl ?? GOOGLE_PLAY_API_BASE_URL).replace(/\/+$/u, '');
		this.timeoutMs = options.timeoutMs ?? REQUEST_TIMEOUT_MS;
		this.now = options.now ?? Date.now;
	}

	async getSubscription(packageName: string, purchaseToken: string): Promise<SubscriptionPurchaseV2> {
		const body = await this.request({
			method: 'GET',
			packageName,
			path: `/purchases/subscriptionsv2/tokens/${segment(purchaseToken)}`,
		});
		return this.parse(SubscriptionPurchaseV2Schema, body, 'subscriptionsv2.get');
	}

	async cancelSubscription(
		packageName: string,
		purchaseToken: string,
		cancellationType: GooglePlayCancellationType,
	): Promise<void> {
		await this.request({
			method: 'POST',
			packageName,
			path: `/purchases/subscriptionsv2/tokens/${segment(purchaseToken)}:cancel`,
			body: {cancellationContext: {cancellationType}},
		});
	}

	async revokeSubscription(packageName: string, purchaseToken: string): Promise<void> {
		await this.request({
			method: 'POST',
			packageName,
			path: `/purchases/subscriptionsv2/tokens/${segment(purchaseToken)}:revoke`,
			body: {revocationContext: {fullRefund: {}}},
		});
	}

	async acknowledgeSubscription(
		packageName: string,
		productId: string,
		purchaseToken: string,
		options: GooglePlayAcknowledgeOptions = {},
	): Promise<void> {
		await this.request({
			method: 'POST',
			packageName,
			path: `/purchases/subscriptions/${segment(productId)}/tokens/${segment(purchaseToken)}:acknowledge`,
			body: options.obfuscatedAccountId ? {externalAccountIds: {obfuscatedAccountId: options.obfuscatedAccountId}} : {},
		});
	}

	async getProduct(packageName: string, purchaseToken: string): Promise<ProductPurchaseV2> {
		const body = await this.request({
			method: 'GET',
			packageName,
			path: `/purchases/productsv2/tokens/${segment(purchaseToken)}`,
		});
		return this.parse(ProductPurchaseV2Schema, body, 'productsv2.get');
	}

	async consumeProduct(packageName: string, productId: string, purchaseToken: string): Promise<void> {
		await this.request({
			method: 'POST',
			packageName,
			path: `/purchases/products/${segment(productId)}/tokens/${segment(purchaseToken)}:consume`,
		});
	}

	async refundOrder(packageName: string, orderId: string): Promise<void> {
		await this.request({
			method: 'POST',
			packageName,
			path: `/orders/${segment(orderId)}:refund`,
			query: {revoke: 'true'},
		});
	}

	async listVoidedPurchases(
		packageName: string,
		params: ListGooglePlayVoidedPurchasesParams,
	): Promise<GooglePlayVoidedPurchasesPage> {
		const query: Record<string, string> = {
			type: VOIDED_PURCHASE_TYPE_SUBSCRIPTIONS_AND_PRODUCTS,
			startTime: String(params.startTime.getTime()),
		};
		if (params.endTime) {
			query['endTime'] = String(params.endTime.getTime());
		}
		if (params.pageToken) {
			query['token'] = params.pageToken;
		}
		if (params.maxResults !== undefined) {
			query['maxResults'] = String(params.maxResults);
		}
		const body = await this.request({method: 'GET', packageName, path: '/purchases/voidedpurchases', query});
		const parsed = this.parse(VoidedPurchasesResponseSchema, body, 'voidedpurchases.list');
		return {
			voidedPurchases: parsed.voidedPurchases ?? [],
			nextPageToken: parsed.tokenPagination?.nextPageToken ?? null,
		};
	}

	private parse<T>(schema: z.ZodType<T>, body: unknown, operation: string): T {
		const parsed = schema.safeParse(body);
		if (!parsed.success) {
			throw new GooglePlayApiError(
				`Google Play ${operation} response is malformed`,
				'retryable',
				200,
				'malformed_response',
			);
		}
		return parsed.data;
	}

	private async request(params: RequestParams): Promise<unknown> {
		if (!isGooglePlayPackageConfigured(params.packageName)) {
			throw new GooglePlayApiError('Google Play package is not configured', 'rejected', null, 'package_not_configured');
		}
		const accessToken = await this.accessTokenProvider.getAccessToken();
		const response = await this.send(params, accessToken);
		if (response.status === 401) {
			await response.text().catch(() => '');
			this.accessTokenProvider.invalidate(accessToken);
			const retryToken = await this.accessTokenProvider.getAccessToken();
			return await this.readResponse(params, await this.send(params, retryToken));
		}
		return await this.readResponse(params, response);
	}

	private async send(params: RequestParams, accessToken: string): Promise<Response> {
		const url = new URL(`${this.baseUrl}/${segment(params.packageName)}${params.path}`);
		for (const [key, value] of Object.entries(params.query ?? {})) {
			url.searchParams.set(key, value);
		}
		try {
			return await fetch(url, {
				method: params.method,
				headers: {
					Authorization: `Bearer ${accessToken}`,
					Accept: 'application/json',
					...(params.body === undefined ? {} : {'Content-Type': 'application/json'}),
				},
				body: params.body === undefined ? undefined : JSON.stringify(params.body),
				signal: AbortSignal.timeout(this.timeoutMs),
			});
		} catch (error) {
			throw new GooglePlayApiError(
				`Google Play ${params.method} request failed: ${error instanceof Error ? error.message : String(error)}`,
				'retryable',
				null,
				'network_error',
			);
		}
	}

	private async readResponse(params: RequestParams, response: Response): Promise<unknown> {
		const text = await response.text().catch(() => '');
		let body: unknown = null;
		if (text.length > 0) {
			try {
				body = JSON.parse(text);
			} catch {
				body = null;
			}
		}
		if (response.ok) {
			return body ?? {};
		}
		const parsedError = GoogleErrorBodySchema.safeParse(body);
		const reason = parsedError.success
			? (parsedError.data.error.errors?.find((entry) => entry.reason)?.reason ?? parsedError.data.error.status ?? null)
			: null;
		throw new GooglePlayApiError(
			`Google Play ${params.method} request failed with ${response.status}`,
			classifyGooglePlayStatus(response.status),
			response.status,
			reason,
			parseRetryAfterMs(response.headers.get('retry-after'), this.now()),
		);
	}
}
