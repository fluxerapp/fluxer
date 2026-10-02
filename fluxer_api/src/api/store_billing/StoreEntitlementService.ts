// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {setTimeout as sleep} from 'node:timers/promises';
import type {UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {StorePurchaseRow} from '@app/api/database/types/StoreBillingTypes';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import {Logger} from '@app/api/Logger';
import {
	AppStoreJwsVerificationError,
	toStoreEnvironment,
	verifyTransaction,
} from '@app/api/store_billing/app_store/AppStoreJwsVerifier';
import {AppStorePurchaseSync, AppStorePurchaseSyncError} from '@app/api/store_billing/app_store/AppStorePurchaseSync';
import {
	type AppStoreServerApiClient,
	AppStoreServerApiError,
	isAppStoreTransactionNotFound,
} from '@app/api/store_billing/app_store/AppStoreServerApiClient';
import {GooglePlaySettlement} from '@app/api/store_billing/GooglePlaySettlement';
import {isGooglePlayApiError} from '@app/api/store_billing/google_play/GooglePlayApiError';
import type {GooglePlayDeveloperApiClient} from '@app/api/store_billing/google_play/GooglePlayDeveloperApiClient';
import {
	buildGooglePlayProductSnapshot,
	buildGooglePlaySubscriptionSnapshot,
} from '@app/api/store_billing/google_play/GooglePlayPurchaseSync';
import {
	getAppStoreProductSlot,
	getGooglePlayOneTimeProductSlot,
	isAppStoreConfigured,
	isGooglePlayConfigured,
	isGooglePlayPackageConfigured,
	isGooglePlaySubscriptionProduct,
	isSandboxEntitlementAllowed,
} from '@app/api/store_billing/StoreBillingConfig';
import {resolveStoreAccessEnd, selectActiveStoreSubscription} from '@app/api/store_billing/StoreBillingMappers';
import type {StoreBillingRepository} from '@app/api/store_billing/StoreBillingRepository';
import {
	buildGooglePlayStoreKey,
	GOOGLE_PLAY_VOIDED_REASON,
	isTerminalStorePurchaseState,
	isVoidedGooglePlaySubscriptionStillRenewing,
	LIFETIME_REFUND_REASON,
	PAID_SUBSCRIPTION_STATES,
	redactStoreKey,
	type StoreEnvironment,
	type StoreProvider,
	type StorePurchaseSnapshot,
	type StoreSlot,
	UNSUPPORTED_QUANTITY_REASON,
} from '@app/api/store_billing/StoreBillingTypes';
import {StoreEntitlementWriter} from '@app/api/store_billing/StoreEntitlementWriter';
import {StoreGiftFulfilment} from '@app/api/store_billing/StoreGiftFulfilment';
import {
	REFRESH_WATCH_DELAY_MS,
	STORE_PURCHASE_REFRESH_QUEUE_KEY,
	scheduleStorePurchaseRefresh,
} from '@app/api/store_billing/StorePurchaseRefresh';
import type {StripeGiftReversalHandler} from '@app/api/stripe/services/StripeGiftReversalHandler';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import {UserFlags, UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {PremiumPurchaseBlockedError} from '@fluxer/errors/src/domains/payment/PremiumPurchaseBlockedError';
import {StoreBillingUnavailableError} from '@fluxer/errors/src/domains/payment/StoreBillingUnavailableError';
import {StorePurchaseInvalidError} from '@fluxer/errors/src/domains/payment/StorePurchaseInvalidError';
import {StorePurchaseOwnedByOtherAccountError} from '@fluxer/errors/src/domains/payment/StorePurchaseOwnedByOtherAccountError';
import {StorePurchaseSandboxNotEntitledError} from '@fluxer/errors/src/domains/payment/StorePurchaseSandboxNotEntitledError';
import {UnknownStorePurchaseError} from '@fluxer/errors/src/domains/payment/UnknownStorePurchaseError';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {seconds} from 'itty-time';

const PURCHASE_LOCK_TTL_SECONDS = seconds('30 seconds');
const PURCHASE_LOCK_ATTEMPTS = 100;
const PURCHASE_LOCK_RETRY_MS = 100;
const PURCHASE_WRITE_ATTEMPTS = 4;
const FAMILY_SHARED_OWNERSHIP = 'FAMILY_SHARED';

interface StoreEntitlementServiceDeps {
	repository: StoreBillingRepository;
	userRepository: IUserRepository;
	userCacheService: UserCacheService;
	gatewayService: IGatewayService;
	kvClient: IKVProvider;
	snowflakeService: ISnowflakeService;
	appStoreClient: AppStoreServerApiClient;
	googlePlayClient: GooglePlayDeveloperApiClient;
	giftReversalHandler: StripeGiftReversalHandler;
}

interface ActiveStoreEntitlement {
	provider: StoreProvider;
	storePurchaseId: bigint;
	entitledUntil: Date;
	expiresAt: Date;
	graceEndsAt: Date | null;
	autoRenew: boolean;
	slot: StoreSlot;
}

interface StorePurchaseClaimResult {
	purchase: StorePurchaseRow;
	giftCode: string | null;
}

interface ClaimGooglePlayPurchaseParams {
	purchaseToken: string;
	productId: string;
	packageName?: string;
}

interface SyncAppStorePurchaseParams {
	environment: StoreEnvironment | null;
	bundleId: string;
	productId: string;
	transactionId: string;
	originalTransactionId: string;
	hintAccountToken: string | null;
}

interface SyncGooglePlaySubscriptionParams {
	packageName: string;
	purchaseToken: string;
}

interface SyncGooglePlayProductParams {
	packageName: string;
	purchaseToken: string;
	productId: string | null;
}

interface ApplyGooglePlayVoidedParams {
	packageName: string;
	purchaseToken: string;
	orderId: string | null;
	productType: 'subscription' | 'one_time' | null;
	voidedAt: Date | null;
}

interface SyncContext {
	claimerId: UserID | null;
	hintAccountToken: string | null;
}

interface PersistResult {
	row: StorePurchaseRow;
	previous: StorePurchaseRow | null;
}

const NOTIFICATION_CONTEXT: SyncContext = {claimerId: null, hintAccountToken: null};

class StorePurchaseRejectedByStoreError extends StorePurchaseInvalidError {}

function normalizeAccountToken(token: string | null | undefined): string | null {
	return token ? token.toLowerCase() : null;
}

function toStoreBillingError(error: unknown): unknown {
	if (error instanceof AppStoreJwsVerificationError || error instanceof AppStorePurchaseSyncError) {
		return new StorePurchaseInvalidError();
	}
	if (error instanceof AppStoreServerApiError) {
		if (isAppStoreTransactionNotFound(error)) {
			return new StorePurchaseRejectedByStoreError();
		}
		const status = error.status ?? 0;
		if (!error.retryable && status >= 400 && status < 500 && status !== 401 && status !== 403) {
			return new StorePurchaseInvalidError();
		}
		return new StoreBillingUnavailableError();
	}
	if (isGooglePlayApiError(error)) {
		return error.kind === 'invalid_token'
			? new StorePurchaseRejectedByStoreError()
			: new StoreBillingUnavailableError();
	}
	return error;
}

export class StoreEntitlementService {
	private readonly appStoreSync: AppStorePurchaseSync;
	private readonly entitlementWriter: StoreEntitlementWriter;
	private readonly googlePlaySettlement: GooglePlaySettlement;
	private readonly giftFulfilment: StoreGiftFulfilment;

	constructor(private readonly deps: StoreEntitlementServiceDeps) {
		this.appStoreSync = new AppStorePurchaseSync(deps.appStoreClient);
		this.entitlementWriter = new StoreEntitlementWriter(deps);
		this.googlePlaySettlement = new GooglePlaySettlement(deps.repository, deps.googlePlayClient);
		this.giftFulfilment = new StoreGiftFulfilment({
			repository: deps.repository,
			userRepository: deps.userRepository,
			giftReversalHandler: deps.giftReversalHandler,
			googlePlaySettlement: this.googlePlaySettlement,
			entitlementWriter: this.entitlementWriter,
		});
	}

	async getOrCreateAccountToken(userId: UserID): Promise<string> {
		return this.deps.repository.getOrCreateAccountToken(userId);
	}

	async getActiveStoreEntitlement(userId: UserID): Promise<ActiveStoreEntitlement | null> {
		if (Config.instance.selfHosted) {
			return null;
		}
		const rows = await this.deps.repository.listPurchasesForUser(userId);
		const row = selectActiveStoreSubscription(rows, new Date());
		const entitledUntil = row ? resolveStoreAccessEnd(row) : null;
		if (!row || !entitledUntil || !row.expires_at) {
			return null;
		}
		return {
			provider: row.provider,
			storePurchaseId: row.id,
			entitledUntil,
			expiresAt: row.expires_at,
			graceEndsAt: row.state === 'grace' ? (row.grace_ends_at ?? null) : null,
			autoRenew: row.auto_renew === true,
			slot: row.slot,
		};
	}

	async hasStorePurchases(userId: UserID): Promise<boolean> {
		if (Config.instance.selfHosted) {
			return false;
		}
		const rows = await this.deps.repository.listPurchasesForUser(userId);
		return rows.length > 0;
	}

	async applyStoreEntitlementToUser(userId: UserID): Promise<void> {
		await this.entitlementWriter.applyEntitlement(userId, []);
	}

	async reapplyAfterStripeChange(userId: UserID): Promise<boolean> {
		try {
			if (!(await this.hasStorePurchases(userId))) {
				return false;
			}
			await this.applyStoreEntitlementToUser(userId);
			return true;
		} catch (error) {
			Logger.warn({error, userId: userId.toString()}, 'Failed to reapply the store entitlement after a Stripe change');
			return false;
		}
	}

	async claimAppStoreTransaction(userId: UserID, signedTransaction: string): Promise<StorePurchaseClaimResult> {
		if (!isAppStoreConfigured()) {
			throw new StoreBillingUnavailableError();
		}
		let transaction: Awaited<ReturnType<typeof verifyTransaction>>;
		try {
			transaction = await verifyTransaction(signedTransaction);
		} catch (error) {
			throw toStoreBillingError(error);
		}
		if (!getAppStoreProductSlot(transaction.productId)) {
			throw new StorePurchaseInvalidError();
		}
		const row = await this.syncFromAppStore(
			{
				environment: toStoreEnvironment(transaction.environment),
				bundleId: transaction.bundleId,
				productId: transaction.productId,
				transactionId: transaction.transactionId,
				originalTransactionId: transaction.originalTransactionId,
				hintAccountToken: transaction.appAccountToken ?? null,
			},
			{claimerId: userId, hintAccountToken: normalizeAccountToken(transaction.appAccountToken)},
		);
		return this.finishClaim(userId, row);
	}

	async claimGooglePlayPurchase(
		userId: UserID,
		params: ClaimGooglePlayPurchaseParams,
	): Promise<StorePurchaseClaimResult> {
		if (!isGooglePlayConfigured()) {
			throw new StoreBillingUnavailableError();
		}
		const packageName = params.packageName ?? Config.googlePlay.packages[0];
		if (!packageName || !isGooglePlayPackageConfigured(packageName)) {
			throw new StorePurchaseInvalidError();
		}
		const context: SyncContext = {claimerId: userId, hintAccountToken: null};
		let row: StorePurchaseRow | null;
		if (isGooglePlaySubscriptionProduct(params.productId)) {
			row = await this.syncFromGooglePlaySubscription(packageName, params.purchaseToken, context, params.productId);
		} else if (getGooglePlayOneTimeProductSlot(params.productId)) {
			row = await this.syncFromGooglePlayProduct(packageName, params.purchaseToken, params.productId, context);
		} else {
			throw new StorePurchaseInvalidError();
		}
		if (!row) {
			throw new StorePurchaseInvalidError();
		}
		return this.finishClaim(userId, row);
	}

	async syncAppStorePurchase(params: SyncAppStorePurchaseParams): Promise<StorePurchaseRow | null> {
		if (!isAppStoreConfigured()) {
			return null;
		}
		return this.syncFromAppStore(params, {
			claimerId: null,
			hintAccountToken: normalizeAccountToken(params.hintAccountToken),
		});
	}

	async syncGooglePlaySubscription(params: SyncGooglePlaySubscriptionParams): Promise<StorePurchaseRow | null> {
		if (!isGooglePlayConfigured() || !isGooglePlayPackageConfigured(params.packageName)) {
			return null;
		}
		return this.syncFromGooglePlaySubscription(params.packageName, params.purchaseToken, NOTIFICATION_CONTEXT, null);
	}

	async syncGooglePlayProduct(params: SyncGooglePlayProductParams): Promise<StorePurchaseRow | null> {
		if (!isGooglePlayConfigured() || !isGooglePlayPackageConfigured(params.packageName)) {
			return null;
		}
		return this.syncFromGooglePlayProduct(
			params.packageName,
			params.purchaseToken,
			params.productId,
			NOTIFICATION_CONTEXT,
		);
	}

	async refreshStorePurchase(storeKey: string): Promise<StorePurchaseRow | null> {
		if (Config.instance.selfHosted) {
			return null;
		}
		const row = await this.deps.repository.findPurchase(storeKey);
		if (!row) {
			await this.deps.kvClient.zrem(STORE_PURCHASE_REFRESH_QUEUE_KEY, storeKey);
			return null;
		}
		const configured =
			row.provider === 'app_store'
				? isAppStoreConfigured()
				: isGooglePlayConfigured() && isGooglePlayPackageConfigured(row.app_id);
		if (!configured) {
			await this.deps.kvClient.zrem(STORE_PURCHASE_REFRESH_QUEUE_KEY, storeKey);
			return row;
		}
		let result: StorePurchaseRow | null;
		try {
			result = await this.syncStoredPurchase(row);
		} catch (error) {
			if (error instanceof StorePurchaseRejectedByStoreError) {
				return this.markPurchaseUnverifiable(storeKey);
			}
			if (!(error instanceof StorePurchaseInvalidError)) {
				throw error;
			}
			result = null;
		}
		if (result === null) {
			Logger.error(
				{storeKey: redactStoreKey(storeKey)},
				'A stored purchase could not be refreshed with the current store config',
			);
			await this.deps.kvClient.zadd(STORE_PURCHASE_REFRESH_QUEUE_KEY, Date.now() + REFRESH_WATCH_DELAY_MS, storeKey);
			return row;
		}
		return result;
	}

	async applyGooglePlayVoided(params: ApplyGooglePlayVoidedParams): Promise<StorePurchaseRow | null> {
		if (!isGooglePlayConfigured() || !isGooglePlayPackageConfigured(params.packageName)) {
			return null;
		}
		const storeKey = buildGooglePlayStoreKey(params.purchaseToken);
		const known = await this.deps.repository.findPurchase(storeKey);
		const kind =
			known?.kind ??
			(params.productType === 'subscription' ? 'subscription' : params.productType === 'one_time' ? 'gift' : null);
		try {
			if (kind === 'subscription') {
				await this.syncGooglePlaySubscription({packageName: params.packageName, purchaseToken: params.purchaseToken});
			} else if (kind === 'gift') {
				await this.syncGooglePlayProduct({
					packageName: params.packageName,
					purchaseToken: params.purchaseToken,
					productId: known?.product_id ?? null,
				});
			}
		} catch (error) {
			if (!(error instanceof StorePurchaseInvalidError)) {
				throw error;
			}
		}
		if (!(await this.deps.repository.findPurchase(storeKey))) {
			Logger.info({storeKey: redactStoreKey(storeKey)}, 'Ignored a voided Google Play purchase that is not stored');
			return null;
		}
		return this.withPurchaseLock(storeKey, () =>
			this.markGooglePlayVoided(storeKey, params.orderId, params.voidedAt ?? new Date()),
		);
	}

	async listStorePurchases(userId: UserID): Promise<Array<StorePurchaseRow>> {
		if (Config.instance.selfHosted) {
			return [];
		}
		const rows = await this.deps.repository.listPurchasesForUser(userId);
		return rows.sort((left, right) => right.created_at.getTime() - left.created_at.getTime());
	}

	async releaseStorePurchase(userId: UserID, purchaseId: bigint): Promise<void> {
		const rows = await this.listStorePurchases(userId);
		const row = rows.find((entry) => entry.id === purchaseId);
		if (!row) {
			throw new UnknownStorePurchaseError();
		}
		if (row.kind !== 'subscription') {
			throw new StorePurchaseInvalidError();
		}
		const released = await this.withPurchaseLock(row.store_key, async () => {
			for (let attempt = 0; attempt < PURCHASE_WRITE_ATTEMPTS; attempt++) {
				const current = await this.deps.repository.findPurchase(row.store_key);
				if (!current || current.user_id !== userId) {
					throw new UnknownStorePurchaseError();
				}
				const unbound = await this.deps.repository.releasePurchase(current);
				if (unbound) {
					return unbound;
				}
			}
			throw new StoreBillingUnavailableError();
		});
		Logger.info(
			{userId: userId.toString(), storeKey: redactStoreKey(row.store_key)},
			'Store purchase released by its owner',
		);
		await this.entitlementWriter.applyEntitlement(userId, [released]);
	}

	async unbindAllForUser(userId: UserID): Promise<void> {
		if (Config.instance.selfHosted) {
			return;
		}
		const rows = await this.deps.repository.listPurchasesForUser(userId);
		for (const row of rows) {
			await this.withPurchaseLock(row.store_key, async () => {
				for (let attempt = 0; attempt < PURCHASE_WRITE_ATTEMPTS; attempt++) {
					const current = await this.deps.repository.findPurchase(row.store_key);
					if (!current || current.user_id !== userId) {
						return;
					}
					if (await this.deps.repository.unbindPurchase(current)) {
						return;
					}
				}
				throw new StoreBillingUnavailableError();
			});
		}
	}

	async stopBillingForDeletedUser(userId: UserID): Promise<void> {
		try {
			const rows = await this.listGooglePlaySubscriptionsToStop(userId);
			for (const row of rows) {
				try {
					await this.deps.googlePlayClient.cancelSubscription(
						row.app_id,
						row.store_reference,
						'DEVELOPER_REQUESTED_STOP_PAYMENTS',
					);
				} catch (error) {
					Logger.warn(
						{error, userId: userId.toString(), storeKey: redactStoreKey(row.store_key)},
						'Failed to stop Google Play billing for a deleted user',
					);
				}
			}
		} catch (error) {
			Logger.warn({error, userId: userId.toString()}, 'Failed to stop store billing for a deleted user');
		}
	}

	async revokeForBannedUser(userId: UserID): Promise<void> {
		try {
			const rows = await this.listGooglePlaySubscriptionsToStop(userId);
			for (const row of rows) {
				try {
					await this.deps.googlePlayClient.revokeSubscription(row.app_id, row.store_reference);
				} catch (error) {
					Logger.warn(
						{error, userId: userId.toString(), storeKey: redactStoreKey(row.store_key)},
						'Failed to revoke a Google Play subscription for a banned user',
					);
				}
			}
		} catch (error) {
			Logger.warn({error, userId: userId.toString()}, 'Failed to revoke store subscriptions for a banned user');
		}
	}

	async scheduleRefresh(row: StorePurchaseRow): Promise<void> {
		await scheduleStorePurchaseRefresh(this.deps.kvClient, row);
	}

	private async syncStoredPurchase(row: StorePurchaseRow): Promise<StorePurchaseRow | null> {
		if (row.provider === 'app_store') {
			return this.syncAppStorePurchase({
				environment: row.environment,
				bundleId: row.app_id,
				productId: row.product_id,
				transactionId: row.latest_transaction_id ?? row.store_reference,
				originalTransactionId: row.store_reference,
				hintAccountToken: null,
			});
		}
		if (row.kind === 'subscription') {
			return this.syncGooglePlaySubscription({packageName: row.app_id, purchaseToken: row.store_reference});
		}
		return this.syncGooglePlayProduct({
			packageName: row.app_id,
			purchaseToken: row.store_reference,
			productId: row.product_id,
		});
	}

	private async listGooglePlaySubscriptionsToStop(userId: UserID): Promise<Array<StorePurchaseRow>> {
		if (Config.instance.selfHosted || !isGooglePlayConfigured()) {
			return [];
		}
		const rows = await this.deps.repository.listPurchasesForUser(userId);
		return rows.filter(
			(row) =>
				row.provider === 'google_play' &&
				row.kind === 'subscription' &&
				(!isTerminalStorePurchaseState(row.state) || isVoidedGooglePlaySubscriptionStillRenewing(row)) &&
				isGooglePlayPackageConfigured(row.app_id),
		);
	}

	private async finishClaim(userId: UserID, row: StorePurchaseRow): Promise<StorePurchaseClaimResult> {
		if (row.environment === 'sandbox' && !isSandboxEntitlementAllowed(userId)) {
			throw new StorePurchaseSandboxNotEntitledError();
		}
		if (row.revocation_reason === UNSUPPORTED_QUANTITY_REASON) {
			throw new StorePurchaseInvalidError();
		}
		if (row.kind === 'subscription') {
			const user = await this.deps.userRepository.findUnique(userId);
			if (user?.premiumType === UserPremiumTypes.LIFETIME) {
				throw new PremiumPurchaseBlockedError('lifetime', {provider: row.provider});
			}
		}
		return {purchase: row, giftCode: row.gift_code ?? null};
	}

	private async syncFromAppStore(params: SyncAppStorePurchaseParams, context: SyncContext): Promise<StorePurchaseRow> {
		let snapshot: StorePurchaseSnapshot;
		try {
			snapshot = await this.appStoreSync.loadPurchase({
				environment: params.environment,
				bundleId: params.bundleId,
				productId: params.productId,
				transactionId: params.transactionId,
				originalTransactionId: params.originalTransactionId,
			});
		} catch (error) {
			throw toStoreBillingError(error);
		}
		return this.withPurchaseLock(snapshot.storeKey, () => this.applySnapshot(snapshot, context));
	}

	private async syncFromGooglePlaySubscription(
		packageName: string,
		purchaseToken: string,
		context: SyncContext,
		expectedProductId: string | null,
	): Promise<StorePurchaseRow | null> {
		const storeKey = buildGooglePlayStoreKey(purchaseToken);
		let snapshot: StorePurchaseSnapshot | null;
		try {
			const purchase = await this.deps.googlePlayClient.getSubscription(packageName, purchaseToken);
			snapshot = buildGooglePlaySubscriptionSnapshot({packageName, purchaseToken, purchase, now: new Date()});
		} catch (error) {
			if (context.claimerId === null && isGooglePlayApiError(error, 'invalid_token')) {
				return this.markPurchaseUnverifiable(storeKey);
			}
			throw toStoreBillingError(error);
		}
		if (!snapshot || (expectedProductId !== null && snapshot.productId !== expectedProductId)) {
			if (context.claimerId !== null) {
				throw new StorePurchaseInvalidError();
			}
			Logger.warn({packageName}, 'Ignored a Google Play subscription with no configured product');
			return null;
		}
		const resolved = snapshot;
		return this.withPurchaseLock(storeKey, () => this.applySnapshot(resolved, context));
	}

	private async syncFromGooglePlayProduct(
		packageName: string,
		purchaseToken: string,
		productId: string | null,
		context: SyncContext,
	): Promise<StorePurchaseRow | null> {
		const storeKey = buildGooglePlayStoreKey(purchaseToken);
		let snapshot: StorePurchaseSnapshot | null;
		try {
			const purchase = await this.deps.googlePlayClient.getProduct(packageName, purchaseToken);
			snapshot = buildGooglePlayProductSnapshot({
				packageName,
				purchaseToken,
				expectedProductId: productId ?? undefined,
				purchase,
				now: new Date(),
			});
		} catch (error) {
			if (context.claimerId === null && isGooglePlayApiError(error, 'invalid_token')) {
				return this.markPurchaseUnverifiable(storeKey);
			}
			throw toStoreBillingError(error);
		}
		if (!snapshot) {
			if (context.claimerId !== null) {
				throw new StorePurchaseInvalidError();
			}
			Logger.warn({packageName, productId}, 'Ignored a Google Play purchase with no configured product');
			return null;
		}
		const resolved = snapshot;
		return this.withPurchaseLock(storeKey, () => this.applySnapshot(resolved, context));
	}

	private async markPurchaseUnverifiable(storeKey: string): Promise<StorePurchaseRow | null> {
		return this.withPurchaseLock(storeKey, async () => {
			for (let attempt = 0; attempt < PURCHASE_WRITE_ATTEMPTS; attempt++) {
				const current = await this.deps.repository.findPurchase(storeKey);
				if (!current) {
					await this.deps.kvClient.zrem(STORE_PURCHASE_REFRESH_QUEUE_KEY, storeKey);
					return null;
				}
				if (current.kind !== 'subscription') {
					await this.deps.kvClient.zrem(STORE_PURCHASE_REFRESH_QUEUE_KEY, storeKey);
					return current;
				}
				const updated = await this.deps.repository.updatePurchase(current, {
					state: 'expired',
					entitled: false,
					synced_at: new Date(),
				});
				if (updated) {
					Logger.warn({storeKey: redactStoreKey(storeKey)}, 'The store can no longer verify a stored purchase');
					await this.scheduleRefresh(updated);
					if (updated.user_id !== null) {
						await this.entitlementWriter.applyEntitlement(updated.user_id, [current]);
					}
					return updated;
				}
			}
			throw new StoreBillingUnavailableError();
		});
	}

	private async markGooglePlayVoided(
		storeKey: string,
		orderId: string | null,
		voidedAt: Date,
	): Promise<StorePurchaseRow | null> {
		for (let attempt = 0; attempt < PURCHASE_WRITE_ATTEMPTS; attempt++) {
			const current = await this.deps.repository.findPurchase(storeKey);
			if (!current) {
				return null;
			}
			const isGift = current.kind === 'gift';
			if (!isGift && (!orderId || orderId !== current.latest_transaction_id)) {
				return current;
			}
			if (current.revocation_reason === GOOGLE_PLAY_VOIDED_REASON || (isGift && current.state === 'refunded')) {
				return isGift ? this.giftFulfilment.settleGift(current) : current;
			}
			const updated = await this.deps.repository.updatePurchase(current, {
				state: isGift ? 'refunded' : 'revoked',
				entitled: false,
				revoked_at: voidedAt,
				revocation_reason: GOOGLE_PLAY_VOIDED_REASON,
			});
			if (!updated) {
				continue;
			}
			Logger.info({storeKey: redactStoreKey(storeKey)}, 'Revoked a Google Play purchase that Google voided');
			await this.scheduleRefresh(updated);
			if (isGift) {
				return this.giftFulfilment.settleGift(updated);
			}
			if (updated.user_id !== null) {
				await this.entitlementWriter.applyEntitlement(updated.user_id, [current]);
			}
			return updated;
		}
		throw new StoreBillingUnavailableError();
	}

	private async withPurchaseLock<T>(storeKey: string, run: () => Promise<T>): Promise<T> {
		const lockKey = `store:purchase:lock:${storeKey}`;
		const token = randomUUID();
		for (let attempt = 0; attempt < PURCHASE_LOCK_ATTEMPTS; attempt++) {
			if (await this.deps.kvClient.acquireLock(lockKey, token, PURCHASE_LOCK_TTL_SECONDS)) {
				try {
					return await run();
				} finally {
					try {
						await this.deps.kvClient.releaseLock(lockKey, token);
					} catch (error) {
						Logger.warn({error, storeKey: redactStoreKey(storeKey)}, 'Failed to release a store purchase lock');
					}
				}
			}
			await sleep(PURCHASE_LOCK_RETRY_MS);
		}
		Logger.warn({storeKey: redactStoreKey(storeKey)}, 'Timed out waiting for a store purchase lock');
		throw new StoreBillingUnavailableError();
	}

	private async applySnapshot(snapshot: StorePurchaseSnapshot, context: SyncContext): Promise<StorePurchaseRow> {
		const persisted = await this.persistSnapshot(snapshot, context);
		let row = persisted.row;
		const previous = persisted.previous;
		const affectedUsers = new Map<UserID, Array<StorePurchaseRow>>();
		if (row.user_id !== null) {
			affectedUsers.set(row.user_id, previous && previous.user_id === row.user_id ? [previous] : []);
		}
		if (previous?.user_id != null && previous.user_id !== row.user_id) {
			affectedUsers.set(previous.user_id, [previous]);
		}
		if (
			snapshot.linkedStoreKey &&
			snapshot.provider === 'google_play' &&
			snapshot.state !== 'pending' &&
			snapshot.storeState !== 'SUBSCRIPTION_STATE_PENDING_PURCHASE_CANCELED'
		) {
			const superseded = await this.supersedeLinkedPurchase(snapshot.linkedStoreKey, row);
			if (superseded?.user_id != null && !affectedUsers.has(superseded.user_id)) {
				affectedUsers.set(superseded.user_id, []);
			}
		}
		if (context.claimerId !== null && row.user_id === context.claimerId && row.provider === 'app_store') {
			row = await this.ensureAppStoreAccountToken(row, snapshot);
		}
		const owner = row.user_id !== null ? await this.deps.userRepository.findUnique(row.user_id) : null;
		if (row.kind === 'gift') {
			row = await this.giftFulfilment.settleGift(row);
		} else if (row.provider === 'google_play') {
			row = await this.googlePlaySettlement.settleSubscription(row, owner);
		}
		await this.scheduleRefresh(row);
		for (const [userId, departedRows] of affectedUsers) {
			await this.entitlementWriter.applyEntitlement(userId, departedRows);
		}
		return row;
	}

	private async persistSnapshot(snapshot: StorePurchaseSnapshot, context: SyncContext): Promise<PersistResult> {
		for (let attempt = 0; attempt < PURCHASE_WRITE_ATTEMPTS; attempt++) {
			const existing = await this.deps.repository.findPurchase(snapshot.storeKey);
			const ownerId = await this.resolveOwner(existing, snapshot, context);
			const now = new Date();
			const fields = this.buildRowFields(snapshot, existing, ownerId, context, now);
			const boundAt = ownerId === null ? null : ownerId === existing?.user_id ? existing.bound_at : now;
			const releasedAt = ownerId === null ? (existing?.released_at ?? null) : null;
			if (!existing) {
				const row: StorePurchaseRow = {
					store_key: snapshot.storeKey,
					id: await this.deps.snowflakeService.generate(),
					...fields,
					user_id: ownerId,
					bound_at: boundAt,
					released_at: releasedAt,
					gift_code: null,
					created_at: now,
					updated_at: now,
					version: 1,
				};
				if (await this.deps.repository.insertPurchase(row)) {
					return {row, previous: null};
				}
				continue;
			}
			const updated = await this.deps.repository.updatePurchase(existing, {
				...fields,
				user_id: ownerId,
				bound_at: boundAt,
				released_at: releasedAt,
			});
			if (updated) {
				return {row: updated, previous: existing};
			}
		}
		throw new StoreBillingUnavailableError();
	}

	private buildRowFields(
		snapshot: StorePurchaseSnapshot,
		existing: StorePurchaseRow | null,
		ownerId: UserID | null,
		context: SyncContext,
		now: Date,
	): Omit<
		StorePurchaseRow,
		'store_key' | 'id' | 'user_id' | 'bound_at' | 'released_at' | 'gift_code' | 'created_at' | 'updated_at' | 'version'
	> {
		const stale = existing?.last_event_at != null && snapshot.lastEventAt.getTime() < existing.last_event_at.getTime();
		const accountToken = snapshot.accountToken ?? context.hintAccountToken ?? existing?.account_token ?? null;
		if (stale && existing) {
			const {
				store_key: _storeKey,
				id: _id,
				user_id: _userId,
				bound_at: _boundAt,
				released_at: _releasedAt,
				gift_code: _giftCode,
				created_at: _createdAt,
				updated_at: _updatedAt,
				version: _version,
				...kept
			} = existing;
			const entitled =
				ownerId === existing.user_id
					? existing.entitled && ownerId !== null
					: this.isStoredRowEntitled(existing, ownerId);
			return {...kept, entitled, synced_at: now};
		}
		let state = snapshot.state;
		let entitled = this.isEntitled(snapshot, ownerId);
		const voided =
			existing?.revocation_reason === GOOGLE_PLAY_VOIDED_REASON &&
			(snapshot.kind === 'gift' || snapshot.latestTransactionId === existing.latest_transaction_id);
		const unsupportedQuantity = snapshot.kind === 'gift' && snapshot.quantity > 1;
		if (voided) {
			state = snapshot.kind === 'gift' ? 'refunded' : 'revoked';
			entitled = false;
		}
		if (existing?.superseded_by_store_key) {
			state = 'superseded';
			entitled = false;
		}
		if (snapshot.kind === 'gift' && existing?.gift_code && state === 'purchased') {
			state = 'fulfilled';
		}
		if (existing?.revocation_reason === LIFETIME_REFUND_REASON || unsupportedQuantity) {
			entitled = false;
		}
		const keepRevocation =
			voided ||
			existing?.revocation_reason === LIFETIME_REFUND_REASON ||
			(unsupportedQuantity && existing?.revocation_reason === UNSUPPORTED_QUANTITY_REASON);
		const keptRevocationReason = keepRevocation ? (existing?.revocation_reason ?? null) : null;
		return {
			provider: snapshot.provider,
			kind: snapshot.kind,
			slot: snapshot.slot,
			environment: snapshot.environment,
			app_id: snapshot.appId,
			product_id: snapshot.productId,
			base_plan_id: snapshot.basePlanId,
			store_reference: snapshot.storeReference,
			latest_transaction_id: snapshot.latestTransactionId,
			app_transaction_id: snapshot.appTransactionId ?? existing?.app_transaction_id ?? null,
			account_token: accountToken,
			ownership_type: snapshot.ownershipType,
			state,
			store_state: snapshot.storeState,
			entitled,
			expires_at: snapshot.expiresAt,
			grace_ends_at: snapshot.graceEndsAt,
			auto_renew: snapshot.autoRenew,
			auto_renew_product_id: snapshot.autoRenewProductId,
			started_at: snapshot.startedAt,
			purchased_at: snapshot.purchasedAt,
			revoked_at: snapshot.revokedAt ?? (keepRevocation ? (existing?.revoked_at ?? null) : null),
			revocation_reason:
				snapshot.revocationReason ?? keptRevocationReason ?? (unsupportedQuantity ? UNSUPPORTED_QUANTITY_REASON : null),
			linked_store_key: snapshot.linkedStoreKey ?? existing?.linked_store_key ?? null,
			superseded_by_store_key: existing?.superseded_by_store_key ?? null,
			acknowledged: snapshot.acknowledged || (existing?.acknowledged ?? false),
			region: snapshot.region,
			last_event_at: snapshot.lastEventAt,
			synced_at: now,
		};
	}

	private isEntitled(snapshot: StorePurchaseSnapshot, ownerId: UserID | null): boolean {
		if (!snapshot.providerEntitled || ownerId === null) {
			return false;
		}
		return snapshot.environment === 'production' || isSandboxEntitlementAllowed(ownerId);
	}

	private isStoredRowEntitled(row: StorePurchaseRow, ownerId: UserID | null): boolean {
		if (ownerId === null || row.superseded_by_store_key || row.revocation_reason != null) {
			return false;
		}
		const paid =
			row.kind === 'gift'
				? row.state === 'purchased' || row.state === 'fulfilled'
				: PAID_SUBSCRIPTION_STATES.has(row.state);
		return paid && (row.environment === 'production' || isSandboxEntitlementAllowed(ownerId));
	}

	private async resolveOwner(
		existing: StorePurchaseRow | null,
		snapshot: StorePurchaseSnapshot,
		context: SyncContext,
	): Promise<UserID | null> {
		const claimerId = context.claimerId;
		if (existing?.user_id != null) {
			const signedToken =
				snapshot.provider === 'app_store' ? (snapshot.accountToken ?? context.hintAccountToken) : null;
			if (signedToken && signedToken !== existing.account_token && !existing.entitled) {
				const tokenOwner = await this.findLiveTokenOwner(signedToken);
				if (tokenOwner !== null && tokenOwner !== existing.user_id) {
					return this.acceptOwnerCandidate(tokenOwner, claimerId);
				}
			}
			if (claimerId === null || existing.user_id === claimerId) {
				return existing.user_id;
			}
			if (
				existing.environment === 'sandbox' &&
				!isSandboxEntitlementAllowed(existing.user_id) &&
				isSandboxEntitlementAllowed(claimerId)
			) {
				return claimerId;
			}
			if (await this.isLiveUser(existing.user_id)) {
				throw new StorePurchaseOwnedByOtherAccountError();
			}
			return claimerId;
		}
		if (snapshot.ownershipType === FAMILY_SHARED_OWNERSHIP && claimerId === null) {
			return null;
		}
		const token = snapshot.accountToken ?? context.hintAccountToken;
		if (token && !(existing && existing.account_token === token)) {
			const tokenOwner = await this.findLiveTokenOwner(token);
			if (tokenOwner !== null) {
				return this.acceptOwnerCandidate(tokenOwner, claimerId);
			}
		}
		if (snapshot.provider === 'google_play' && !existing?.released_at) {
			for (const linkedKey of [snapshot.linkedStoreKey, snapshot.expiredStoreKey]) {
				if (!linkedKey) {
					continue;
				}
				const linked = await this.deps.repository.findPurchase(linkedKey);
				if (linked?.user_id != null && (await this.isLiveUser(linked.user_id))) {
					return this.acceptOwnerCandidate(linked.user_id, claimerId);
				}
			}
			if (snapshot.expiredAccountToken) {
				const expiredOwner = await this.findLiveTokenOwner(snapshot.expiredAccountToken);
				if (expiredOwner !== null) {
					return this.acceptOwnerCandidate(expiredOwner, claimerId);
				}
			}
		}
		return claimerId;
	}

	private acceptOwnerCandidate(candidate: UserID, claimerId: UserID | null): UserID {
		if (claimerId !== null && candidate !== claimerId) {
			throw new StorePurchaseOwnedByOtherAccountError();
		}
		return candidate;
	}

	private async findLiveTokenOwner(token: string): Promise<UserID | null> {
		const userId = await this.deps.repository.findUserIdByAccountToken(token);
		if (userId === null || !(await this.isLiveUser(userId))) {
			return null;
		}
		return userId;
	}

	private async isLiveUser(userId: UserID): Promise<boolean> {
		const user = await this.deps.userRepository.findUnique(userId);
		return user !== null && (user.flags & UserFlags.DELETED) === 0n;
	}

	private async supersedeLinkedPurchase(linkedKey: string, row: StorePurchaseRow): Promise<StorePurchaseRow | null> {
		if (linkedKey === row.store_key) {
			return null;
		}
		for (let attempt = 0; attempt < PURCHASE_WRITE_ATTEMPTS; attempt++) {
			const linked = await this.deps.repository.findPurchase(linkedKey);
			if (!linked || linked.superseded_by_store_key === row.store_key) {
				return null;
			}
			const updated = await this.deps.repository.updatePurchase(linked, {
				state: 'superseded',
				entitled: false,
				superseded_by_store_key: row.store_key,
			});
			if (updated) {
				await this.scheduleRefresh(updated);
				Logger.info(
					{storeKey: redactStoreKey(row.store_key), supersededStoreKey: redactStoreKey(linkedKey)},
					'Google Play purchase superseded by a linked purchase',
				);
				return updated;
			}
		}
		return null;
	}

	private async ensureAppStoreAccountToken(
		row: StorePurchaseRow,
		snapshot: StorePurchaseSnapshot,
	): Promise<StorePurchaseRow> {
		if (row.user_id === null || snapshot.ownershipType === FAMILY_SHARED_OWNERSHIP) {
			return row;
		}
		const token = await this.deps.repository.getOrCreateAccountToken(row.user_id);
		if (snapshot.accountToken === token) {
			return row;
		}
		try {
			await this.deps.appStoreClient.setAppAccountToken({
				environment: row.environment,
				bundleId: row.app_id,
				originalTransactionId: row.store_reference,
				appAccountToken: token,
			});
		} catch (error) {
			Logger.warn(
				{error, storeKey: redactStoreKey(row.store_key)},
				'Failed to set the App Store account token on a claimed purchase',
			);
			return row;
		}
		const updated = await this.deps.repository.updatePurchase(row, {account_token: token});
		return updated ?? row;
	}
}
