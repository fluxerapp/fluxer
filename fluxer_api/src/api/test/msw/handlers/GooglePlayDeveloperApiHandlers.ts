// SPDX-License-Identifier: AGPL-3.0-or-later

import {createPublicKey, generateKeyPairSync, type KeyObject} from 'node:crypto';
import {Config} from '@app/api/Config';
import {
	GOOGLE_DEFAULT_TOKEN_URI,
	GOOGLE_PLAY_ANDROIDPUBLISHER_SCOPE,
} from '@app/api/store_billing/google_play/GooglePlayAccessTokenProvider';
import {
	GOOGLE_PLAY_API_BASE_URL,
	type GooglePlayVoidedPurchase,
	type ProductPurchaseV2,
	type SubscriptionPurchaseV2,
} from '@app/api/store_billing/google_play/GooglePlayDeveloperApiClient';
import {GOOGLE_OIDC_JWKS_URL} from '@app/api/store_billing/google_play/GooglePlayPushVerifier';
import {type JWTPayload, jwtVerify, SignJWT} from 'jose';
import {HttpResponse, http, type RequestHandler} from 'msw';

export type GooglePlayFakeOperation =
	| 'token'
	| 'jwks'
	| 'subscriptions.get'
	| 'subscriptions.cancel'
	| 'subscriptions.revoke'
	| 'subscriptions.acknowledge'
	| 'products.get'
	| 'products.consume'
	| 'orders.refund'
	| 'voidedpurchases.list';

export interface GooglePlayFakeFailure {
	status: number;
	reason?: string;
	retryAfter?: string;
}

export interface GooglePlayFakeSpies {
	tokenAssertions: Array<{header: Record<string, unknown>; claims: JWTPayload}>;
	apiRequests: Array<{operation: GooglePlayFakeOperation; method: string; packageName: string; authorization: string}>;
	acknowledged: Array<{packageName: string; productId: string; purchaseToken: string; body: unknown}>;
	consumed: Array<{packageName: string; productId: string; purchaseToken: string}>;
	canceled: Array<{packageName: string; purchaseToken: string; body: unknown}>;
	revoked: Array<{packageName: string; purchaseToken: string; body: unknown}>;
	refunded: Array<{packageName: string; orderId: string; revoke: boolean}>;
	voidedQueries: Array<Record<string, string>>;
	jwksFetches: number;
}

interface StoredPurchase<T> {
	packageName: string;
	purchase: T;
}

interface StoredVoidedPurchase {
	packageName: string;
	voided: GooglePlayVoidedPurchase;
}

interface SignPushTokenOptions {
	claims?: Record<string, unknown>;
	omitClaims?: Array<string>;
	signer?: 'trusted' | 'untrusted';
	kid?: string;
}

interface FakeGooglePlayOptions {
	jwksUrl?: string;
	tokenUri?: string;
}

const API_PATH_PATTERN = new RegExp(`^${escapeRegExp(new URL(GOOGLE_PLAY_API_BASE_URL).pathname)}/([^/]+)(/.*)$`, 'u');
const TRUSTED_KID = 'fake-google-oidc-key';

function escapeRegExp(value: string): string {
	return value.replace(/[.*+?^${}()|[\]\\]/gu, '\\$&');
}

function emptySpies(): GooglePlayFakeSpies {
	return {
		tokenAssertions: [],
		apiRequests: [],
		acknowledged: [],
		consumed: [],
		canceled: [],
		revoked: [],
		refunded: [],
		voidedQueries: [],
		jwksFetches: 0,
	};
}

function googleError(status: number, reason: string, headers: Record<string, string> = {}): HttpResponse<string> {
	return new HttpResponse(
		JSON.stringify({error: {code: status, message: reason, status: reason, errors: [{reason, domain: 'global'}]}}),
		{status, headers: {'Content-Type': 'application/json', ...headers}},
	);
}

function json(body: unknown, status = 200): HttpResponse<string> {
	return new HttpResponse(JSON.stringify(body), {status, headers: {'Content-Type': 'application/json'}});
}

async function readJson(request: Request): Promise<unknown> {
	const text = await request.text();
	if (text.length === 0) {
		return null;
	}
	try {
		return JSON.parse(text);
	} catch {
		return null;
	}
}

function futureIso(ms: number): string {
	return new Date(Date.now() + ms).toISOString();
}

export function buildFakeSubscriptionPurchase(overrides: Partial<SubscriptionPurchaseV2> = {}): SubscriptionPurchaseV2 {
	return {
		kind: 'androidpublisher#subscriptionPurchaseV2',
		regionCode: 'US',
		startTime: new Date(Date.now() - 60_000).toISOString(),
		subscriptionState: 'SUBSCRIPTION_STATE_ACTIVE',
		acknowledgementState: 'ACKNOWLEDGEMENT_STATE_PENDING',
		lineItems: [
			{
				productId: 'plutonium',
				expiryTime: futureIso(30 * 24 * 60 * 60 * 1000),
				latestSuccessfulOrderId: 'GPA.1111-2222-3333-44444',
				autoRenewingPlan: {autoRenewEnabled: true},
				offerDetails: {basePlanId: 'monthly'},
			},
		],
		...overrides,
	};
}

export function buildFakeProductPurchase(overrides: Partial<ProductPurchaseV2> = {}): ProductPurchaseV2 {
	return {
		kind: 'androidpublisher#productPurchaseV2',
		regionCode: 'US',
		orderId: 'GPA.5555-6666-7777-88888',
		purchaseCompletionTime: new Date(Date.now() - 60_000).toISOString(),
		purchaseStateContext: {purchaseState: 'PURCHASED'},
		acknowledgementState: 'ACKNOWLEDGEMENT_STATE_PENDING',
		productLineItem: [
			{
				productId: 'gift_1_month',
				productOfferDetails: {quantity: 1, consumptionState: 'CONSUMPTION_STATE_YET_TO_BE_CONSUMED'},
			},
		],
		...overrides,
	};
}

export function encodeGooglePlayDeveloperNotification(notification: Record<string, unknown>): string {
	return Buffer.from(JSON.stringify(notification), 'utf8').toString('base64');
}

export function buildGooglePlayPushEnvelope(
	notification: Record<string, unknown>,
	messageId = '136969346945',
): Record<string, unknown> {
	return {
		message: {
			attributes: {},
			data: encodeGooglePlayDeveloperNotification(notification),
			messageId,
			message_id: messageId,
			publishTime: new Date().toISOString(),
		},
		subscription: 'projects/fluxer-test/subscriptions/play-rtdn',
	};
}

export class FakeGooglePlayDeveloperApi {
	readonly spies: GooglePlayFakeSpies = emptySpies();
	readonly jwksUrl: string;
	readonly tokenUri: string;
	private readonly subscriptions = new Map<string, StoredPurchase<SubscriptionPurchaseV2>>();
	private readonly products = new Map<string, StoredPurchase<ProductPurchaseV2>>();
	private readonly goneTokens = new Set<string>();
	private readonly voided: Array<StoredVoidedPurchase> = [];
	private readonly failures = new Map<GooglePlayFakeOperation, Array<GooglePlayFakeFailure>>();
	private readonly accessTokens = new Map<string, number>();
	private accessTokenCounter = 0;
	private accessTokenExpiresIn: number | string = 3600;
	private serviceAccountKey: KeyObject | null = null;
	private readonly trustedKeys = generateKeyPairSync('rsa', {modulusLength: 2048});
	private readonly untrustedKeys = generateKeyPairSync('rsa', {modulusLength: 2048});

	constructor(options: FakeGooglePlayOptions = {}) {
		this.jwksUrl = options.jwksUrl ?? GOOGLE_OIDC_JWKS_URL;
		this.tokenUri = options.tokenUri ?? GOOGLE_DEFAULT_TOKEN_URI;
	}

	get handlers(): Array<RequestHandler> {
		const apiOrigin = new URL(GOOGLE_PLAY_API_BASE_URL).origin;
		return [
			http.post(this.tokenUri, ({request}) => this.handleToken(request)),
			http.get(this.jwksUrl, () => this.handleJwks()),
			http.all(`${apiOrigin}/*`, ({request}) => this.handleApi(request)),
		];
	}

	reset(): void {
		this.subscriptions.clear();
		this.products.clear();
		this.goneTokens.clear();
		this.voided.length = 0;
		this.failures.clear();
		this.accessTokens.clear();
		this.accessTokenCounter = 0;
		this.accessTokenExpiresIn = 3600;
		this.serviceAccountKey = null;
		Object.assign(this.spies, emptySpies());
	}

	setSubscription(packageName: string, purchaseToken: string, purchase: SubscriptionPurchaseV2): void {
		this.subscriptions.set(purchaseToken, {packageName, purchase: structuredClone(purchase)});
	}

	getSubscription(purchaseToken: string): SubscriptionPurchaseV2 | null {
		return this.subscriptions.get(purchaseToken)?.purchase ?? null;
	}

	setProduct(packageName: string, purchaseToken: string, purchase: ProductPurchaseV2): void {
		this.products.set(purchaseToken, {packageName, purchase: structuredClone(purchase)});
	}

	getProduct(purchaseToken: string): ProductPurchaseV2 | null {
		return this.products.get(purchaseToken)?.purchase ?? null;
	}

	markTokenGone(purchaseToken: string): void {
		this.goneTokens.add(purchaseToken);
	}

	addVoidedPurchase(packageName: string, voided: GooglePlayVoidedPurchase): void {
		this.voided.push({packageName, voided});
	}

	failNext(operation: GooglePlayFakeOperation, failure: GooglePlayFakeFailure): void {
		const queue = this.failures.get(operation) ?? [];
		queue.push(failure);
		this.failures.set(operation, queue);
	}

	setAccessTokenExpiresIn(value: number | string): void {
		this.accessTokenExpiresIn = value;
	}

	setServiceAccountPrivateKey(pem: string): void {
		this.serviceAccountKey = createPublicKey(pem);
	}

	revokeAccessTokens(): void {
		this.accessTokens.clear();
	}

	async signPushToken(options: SignPushTokenOptions = {}): Promise<string> {
		const nowSeconds = Math.floor(Date.now() / 1000);
		const claims: Record<string, unknown> = {
			iss: 'https://accounts.google.com',
			aud: Config.googlePlay.pushAudience,
			azp: '112233445566778899000',
			sub: '112233445566778899000',
			email: Config.googlePlay.pushServiceAccountEmail,
			email_verified: true,
			iat: nowSeconds,
			exp: nowSeconds + 3600,
			...options.claims,
		};
		for (const key of options.omitClaims ?? []) {
			delete claims[key];
		}
		const keys = options.signer === 'untrusted' ? this.untrustedKeys : this.trustedKeys;
		return await new SignJWT(claims)
			.setProtectedHeader({alg: 'RS256', typ: 'JWT', kid: options.kid ?? TRUSTED_KID})
			.sign(keys.privateKey);
	}

	private takeFailure(operation: GooglePlayFakeOperation): HttpResponse<string> | null {
		const failure = this.failures.get(operation)?.shift();
		if (!failure) {
			return null;
		}
		return googleError(
			failure.status,
			failure.reason ?? `fake_${failure.status}`,
			failure.retryAfter ? {'Retry-After': failure.retryAfter} : {},
		);
	}

	private handleJwks(): HttpResponse<string> {
		this.spies.jwksFetches += 1;
		const failure = this.takeFailure('jwks');
		if (failure) {
			return failure;
		}
		const jwk = this.trustedKeys.publicKey.export({format: 'jwk'});
		return json({keys: [{...jwk, kid: TRUSTED_KID, alg: 'RS256', use: 'sig'}]});
	}

	private resolveServiceAccountKey(): KeyObject | null {
		if (this.serviceAccountKey) {
			return this.serviceAccountKey;
		}
		const pem = Config.googlePlay.privateKey;
		return pem ? createPublicKey(pem.replaceAll('\\n', '\n')) : null;
	}

	private async handleToken(request: Request): Promise<HttpResponse<string>> {
		const failure = this.takeFailure('token');
		if (failure) {
			return failure;
		}
		const form = new URLSearchParams(await request.text());
		if (form.get('grant_type') !== 'urn:ietf:params:oauth:grant-type:jwt-bearer') {
			return json({error: 'unsupported_grant_type'}, 400);
		}
		const assertion = form.get('assertion');
		const key = this.resolveServiceAccountKey();
		if (!assertion || !key) {
			return json({error: 'invalid_request'}, 400);
		}
		try {
			const {payload, protectedHeader} = await jwtVerify(assertion, key, {
				algorithms: ['RS256'],
				audience: this.tokenUri,
			});
			if (payload['scope'] !== GOOGLE_PLAY_ANDROIDPUBLISHER_SCOPE || typeof payload.iss !== 'string') {
				return json({error: 'invalid_scope'}, 400);
			}
			if (payload.iat === undefined || payload.exp === undefined || payload.exp - payload.iat > 3600) {
				return json({error: 'invalid_grant', error_description: 'Invalid JWT'}, 400);
			}
			this.spies.tokenAssertions.push({header: {...protectedHeader}, claims: payload});
		} catch {
			return json({error: 'invalid_grant', error_description: 'Invalid JWT Signature.'}, 400);
		}
		this.accessTokenCounter += 1;
		const token = `ya29.fake-${this.accessTokenCounter}`;
		this.accessTokens.set(token, Date.now() + 3600 * 1000);
		return json({access_token: token, expires_in: this.accessTokenExpiresIn, token_type: 'Bearer'});
	}

	private async handleApi(request: Request): Promise<HttpResponse<string>> {
		const url = new URL(request.url);
		const match = API_PATH_PATTERN.exec(url.pathname);
		if (!match) {
			return googleError(404, 'notFound');
		}
		const packageName = decodeURIComponent(match[1]);
		const rest = match[2];
		const route = this.route(request.method, rest);
		if (!route) {
			return googleError(404, 'notFound');
		}
		const authorization = request.headers.get('authorization') ?? '';
		this.spies.apiRequests.push({operation: route.operation, method: request.method, packageName, authorization});
		const bearer = authorization.startsWith('Bearer ') ? authorization.slice('Bearer '.length) : '';
		const expiry = this.accessTokens.get(bearer);
		if (expiry === undefined || expiry <= Date.now()) {
			return googleError(401, 'authError');
		}
		const failure = this.takeFailure(route.operation);
		if (failure) {
			return failure;
		}
		switch (route.operation) {
			case 'subscriptions.get':
				return this.handleSubscriptionGet(packageName, route.token);
			case 'subscriptions.cancel':
				return this.handleSubscriptionCancel(packageName, route.token, await readJson(request));
			case 'subscriptions.revoke':
				return this.handleSubscriptionRevoke(packageName, route.token, await readJson(request));
			case 'subscriptions.acknowledge':
				return this.handleSubscriptionAcknowledge(packageName, route.productId, route.token, await readJson(request));
			case 'products.get':
				return this.handleProductGet(packageName, route.token);
			case 'products.consume':
				return this.handleProductConsume(packageName, route.productId, route.token);
			case 'orders.refund':
				return this.handleRefund(packageName, route.token, url.searchParams.get('revoke') === 'true');
			case 'voidedpurchases.list':
				return this.handleVoidedList(packageName, url.searchParams);
			default:
				return googleError(404, 'notFound');
		}
	}

	private route(
		method: string,
		rest: string,
	): {operation: GooglePlayFakeOperation; token: string; productId: string} | null {
		const patterns: Array<{method: string; pattern: RegExp; operation: GooglePlayFakeOperation}> = [
			{method: 'GET', pattern: /^\/purchases\/subscriptionsv2\/tokens\/([^/:]+)$/u, operation: 'subscriptions.get'},
			{
				method: 'POST',
				pattern: /^\/purchases\/subscriptionsv2\/tokens\/([^/:]+):cancel$/u,
				operation: 'subscriptions.cancel',
			},
			{
				method: 'POST',
				pattern: /^\/purchases\/subscriptionsv2\/tokens\/([^/:]+):revoke$/u,
				operation: 'subscriptions.revoke',
			},
			{
				method: 'POST',
				pattern: /^\/purchases\/subscriptions\/([^/]+)\/tokens\/([^/:]+):acknowledge$/u,
				operation: 'subscriptions.acknowledge',
			},
			{method: 'GET', pattern: /^\/purchases\/productsv2\/tokens\/([^/:]+)$/u, operation: 'products.get'},
			{
				method: 'POST',
				pattern: /^\/purchases\/products\/([^/]+)\/tokens\/([^/:]+):consume$/u,
				operation: 'products.consume',
			},
			{method: 'POST', pattern: /^\/orders\/([^/:]+):refund$/u, operation: 'orders.refund'},
			{method: 'GET', pattern: /^\/purchases\/voidedpurchases$/u, operation: 'voidedpurchases.list'},
		];
		for (const entry of patterns) {
			if (entry.method !== method) {
				continue;
			}
			const match = entry.pattern.exec(rest);
			if (!match) {
				continue;
			}
			if (match.length === 3) {
				return {
					operation: entry.operation,
					productId: decodeURIComponent(match[1]),
					token: decodeURIComponent(match[2]),
				};
			}
			return {operation: entry.operation, productId: '', token: match[1] ? decodeURIComponent(match[1]) : ''};
		}
		return null;
	}

	private findSubscription(
		packageName: string,
		token: string,
	): {record: StoredPurchase<SubscriptionPurchaseV2>} | HttpResponse<string> {
		if (this.goneTokens.has(token)) {
			return googleError(410, 'purchaseTokenNoLongerValid');
		}
		const record = this.subscriptions.get(token);
		if (!record || record.packageName !== packageName) {
			return googleError(404, 'purchaseTokenNotFound');
		}
		return {record};
	}

	private findProduct(
		packageName: string,
		token: string,
	): {record: StoredPurchase<ProductPurchaseV2>} | HttpResponse<string> {
		if (this.goneTokens.has(token)) {
			return googleError(410, 'purchaseTokenNoLongerValid');
		}
		const record = this.products.get(token);
		if (!record || record.packageName !== packageName) {
			return googleError(404, 'purchaseTokenNotFound');
		}
		return {record};
	}

	private handleSubscriptionGet(packageName: string, token: string): HttpResponse<string> {
		const found = this.findSubscription(packageName, token);
		if (found instanceof HttpResponse) {
			return found;
		}
		return json(found.record.purchase);
	}

	private handleSubscriptionCancel(packageName: string, token: string, body: unknown): HttpResponse<string> {
		const found = this.findSubscription(packageName, token);
		if (found instanceof HttpResponse) {
			return found;
		}
		this.spies.canceled.push({packageName, purchaseToken: token, body});
		const purchase = found.record.purchase;
		const developerStop =
			JSON.stringify(body ?? {}).includes('DEVELOPER_REQUESTED_STOP_PAYMENTS') ||
			purchase.subscriptionState !== 'SUBSCRIPTION_STATE_ACTIVE';
		purchase.subscriptionState = 'SUBSCRIPTION_STATE_CANCELED';
		purchase.canceledStateContext = developerStop
			? {developerInitiatedCancellation: {}}
			: {userInitiatedCancellation: {cancelTime: new Date().toISOString()}};
		for (const lineItem of purchase.lineItems ?? []) {
			if (lineItem.autoRenewingPlan) {
				lineItem.autoRenewingPlan.autoRenewEnabled = false;
			}
		}
		return json({});
	}

	private handleSubscriptionRevoke(packageName: string, token: string, body: unknown): HttpResponse<string> {
		const found = this.findSubscription(packageName, token);
		if (found instanceof HttpResponse) {
			return found;
		}
		this.spies.revoked.push({packageName, purchaseToken: token, body});
		this.expireSubscription(found.record.purchase);
		return json({});
	}

	private expireSubscription(purchase: SubscriptionPurchaseV2): void {
		purchase.subscriptionState = 'SUBSCRIPTION_STATE_EXPIRED';
		purchase.canceledStateContext = {developerInitiatedCancellation: {}};
		for (const lineItem of purchase.lineItems ?? []) {
			lineItem.expiryTime = new Date().toISOString();
			if (lineItem.autoRenewingPlan) {
				lineItem.autoRenewingPlan.autoRenewEnabled = false;
			}
		}
	}

	private handleSubscriptionAcknowledge(
		packageName: string,
		productId: string,
		token: string,
		body: unknown,
	): HttpResponse<string> {
		const found = this.findSubscription(packageName, token);
		if (found instanceof HttpResponse) {
			return found;
		}
		const purchase = found.record.purchase;
		if (!(purchase.lineItems ?? []).some((lineItem) => lineItem.productId === productId)) {
			return googleError(400, 'productNotOwnedByUser');
		}
		if (purchase.acknowledgementState === 'ACKNOWLEDGEMENT_STATE_ACKNOWLEDGED') {
			return googleError(400, 'invalidPurchaseState');
		}
		if (purchase.subscriptionState === 'SUBSCRIPTION_STATE_PENDING') {
			return googleError(400, 'invalidPurchaseState');
		}
		purchase.acknowledgementState = 'ACKNOWLEDGEMENT_STATE_ACKNOWLEDGED';
		delete purchase.outOfAppPurchaseContext;
		this.spies.acknowledged.push({packageName, productId, purchaseToken: token, body});
		return json({});
	}

	private handleProductGet(packageName: string, token: string): HttpResponse<string> {
		const found = this.findProduct(packageName, token);
		if (found instanceof HttpResponse) {
			return found;
		}
		return json(found.record.purchase);
	}

	private handleProductConsume(packageName: string, productId: string, token: string): HttpResponse<string> {
		const found = this.findProduct(packageName, token);
		if (found instanceof HttpResponse) {
			return found;
		}
		const purchase = found.record.purchase;
		const lineItem = (purchase.productLineItem ?? []).find((item) => item.productId === productId);
		if (!lineItem) {
			return googleError(400, 'productNotOwnedByUser');
		}
		if (purchase.purchaseStateContext?.purchaseState !== 'PURCHASED') {
			return googleError(400, 'invalidPurchaseState');
		}
		if (lineItem.productOfferDetails?.consumptionState === 'CONSUMPTION_STATE_CONSUMED') {
			return googleError(400, 'purchaseAlreadyConsumed');
		}
		lineItem.productOfferDetails = {...lineItem.productOfferDetails, consumptionState: 'CONSUMPTION_STATE_CONSUMED'};
		purchase.acknowledgementState = 'ACKNOWLEDGEMENT_STATE_ACKNOWLEDGED';
		this.spies.consumed.push({packageName, productId, purchaseToken: token});
		return json({});
	}

	private handleRefund(packageName: string, orderId: string, revoke: boolean): HttpResponse<string> {
		const subscription = [...this.subscriptions.entries()].find(
			([, record]) =>
				record.packageName === packageName &&
				(record.purchase.lineItems ?? []).some((lineItem) => lineItem.latestSuccessfulOrderId === orderId),
		);
		const product = [...this.products.entries()].find(
			([, record]) => record.packageName === packageName && record.purchase.orderId === orderId,
		);
		const match = subscription ?? product;
		if (!match) {
			return googleError(404, 'orderNotFound');
		}
		this.spies.refunded.push({packageName, orderId, revoke});
		const [purchaseToken] = match;
		if (revoke) {
			if (subscription) {
				this.expireSubscription(subscription[1].purchase);
			}
			if (product) {
				product[1].purchase.purchaseStateContext = {purchaseState: 'CANCELLED'};
			}
		}
		if (!this.voided.some((entry) => entry.voided.orderId === orderId)) {
			const now = String(Date.now());
			this.voided.push({
				packageName,
				voided: {
					kind: 'androidpublisher#voidedPurchase',
					purchaseToken,
					orderId,
					purchaseTimeMillis: now,
					voidedTimeMillis: now,
					voidedSource: 1,
					voidedReason: 0,
				},
			});
		}
		return json({});
	}

	private handleVoidedList(packageName: string, params: URLSearchParams): HttpResponse<string> {
		this.spies.voidedQueries.push(Object.fromEntries(params.entries()));
		const startTime = Number(params.get('startTime') ?? '0');
		const endTime = params.has('endTime') ? Number(params.get('endTime')) : Number.POSITIVE_INFINITY;
		const includeSubscriptions = params.get('type') === '1';
		const matching = this.voided
			.filter((entry) => entry.packageName === packageName)
			.filter((entry) => {
				const voidedAt = Number(entry.voided.voidedTimeMillis ?? '0');
				return voidedAt >= startTime && voidedAt <= endTime;
			})
			.filter((entry) => includeSubscriptions || !this.subscriptions.has(entry.voided.purchaseToken))
			.map((entry) => entry.voided);
		const offset = Number(params.get('token') ?? '0');
		const pageSize = Number(params.get('maxResults') ?? '1000');
		const page = matching.slice(offset, offset + pageSize);
		const nextOffset = offset + pageSize;
		return json({
			voidedPurchases: page,
			...(nextOffset < matching.length ? {tokenPagination: {nextPageToken: String(nextOffset)}} : {}),
		});
	}
}

export function createGooglePlayDeveloperApiHandlers(options: FakeGooglePlayOptions = {}): FakeGooglePlayDeveloperApi {
	return new FakeGooglePlayDeveloperApi(options);
}
