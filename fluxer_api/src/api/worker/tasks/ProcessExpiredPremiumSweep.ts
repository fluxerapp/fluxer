// SPDX-License-Identifier: AGPL-3.0-or-later

import type {StorePurchaseRow} from '@app/api/database/types/StoreBillingTypes';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import {sanitizeExpiredPremiumMemberPerks} from '@app/api/guild/services/member/PremiumMemberSanitization';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {PremiumStateReconciliationQueueService} from '@app/api/infrastructure/PremiumStateReconciliationQueueService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import {Logger} from '@app/api/Logger';
import {createRequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {User} from '@app/api/models/User';
import {
	isAppStoreConfigured,
	isGooglePlayConfigured,
	isGooglePlayPackageConfigured,
} from '@app/api/store_billing/StoreBillingConfig';
import {resolveStoreAccessEnd} from '@app/api/store_billing/StoreBillingMappers';
import {isTerminalStorePurchaseState} from '@app/api/store_billing/StoreBillingTypes';
import type {StoreEntitlementService} from '@app/api/store_billing/StoreEntitlementService';
import {isPremiumTieringActive} from '@app/api/stripe/BillingConfigCache';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {checkIsPremium, createPremiumClearPatch, shouldStripExpiredPremium} from '@app/api/user/UserHelpers';
import {dispatchUserUpdate} from '@app/api/user/UserMappers';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskName} from '@app/api/worker/WorkerLaneConfig';
import {PremiumFlags, UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {ms} from 'itty-time';

const BATCH_SIZE = 100;
const STORE_REFRESH_STALE_MS = ms('30 minutes');
const STORE_REFRESH_DEFER_LIMIT_MS = ms('1 day');

interface SweepResult {
	processed: number;
	stripped: number;
	sanitized: number;
	reconcileEnqueued: number;
	storeRefreshEnqueued: number;
	skipped: number;
	failed: number;
}

interface SweepDeps {
	userRepository: UserRepository;
	guildRepository: GuildRepository;
	userCacheService: UserCacheService;
	gatewayService: IGatewayService;
	premiumStateReconciliationQueueService: PremiumStateReconciliationQueueService;
	storeEntitlementService: StoreEntitlementService;
	workerService: IWorkerService<WorkerTaskName>;
}

function isStoreProviderConfigured(row: StorePurchaseRow): boolean {
	if (row.provider === 'app_store') {
		return isAppStoreConfigured();
	}
	return isGooglePlayConfigured() && isGooglePlayPackageConfigured(row.app_id);
}

function shouldDeferToStoreRefresh(row: StorePurchaseRow, now: number): boolean {
	if (row.kind !== 'subscription' || isTerminalStorePurchaseState(row.state) || !isStoreProviderConfigured(row)) {
		return false;
	}
	const syncedMs = row.synced_at?.getTime() ?? 0;
	const accessEndMs = resolveStoreAccessEnd(row)?.getTime();
	if (accessEndMs === undefined) {
		return false;
	}
	return (
		syncedMs < now - STORE_REFRESH_STALE_MS &&
		syncedMs < accessEndMs &&
		accessEndMs > now - STORE_REFRESH_DEFER_LIMIT_MS
	);
}

async function deferToStoreRefresh(user: User, deps: SweepDeps): Promise<number> {
	const now = Date.now();
	const rows = (await deps.storeEntitlementService.listStorePurchases(user.id)).filter((row) =>
		shouldDeferToStoreRefresh(row, now),
	);
	for (const row of rows) {
		await deps.workerService.addJob(
			'refreshStorePurchase',
			{storeKey: row.store_key},
			{jobKey: `refreshStorePurchase:${row.store_key}`},
		);
	}
	return rows.length;
}

async function processUser(user: User, deps: SweepDeps, result: SweepResult): Promise<void> {
	if (user.isBot) {
		result.skipped += 1;
		return;
	}
	if ((user.premiumType ?? 0) <= 0) {
		result.skipped += 1;
		return;
	}
	if (user.premiumType === UserPremiumTypes.LIFETIME) {
		result.skipped += 1;
		return;
	}
	if ((user.premiumFlags & PremiumFlags.ENABLED_OVERRIDE) !== 0) {
		result.skipped += 1;
		return;
	}
	if (!shouldStripExpiredPremium(user)) {
		if (user.premiumType === UserPremiumTypes.SUBSCRIPTION && (user.stripeSubscriptionId || user.stripeCustomerId)) {
			try {
				await deps.premiumStateReconciliationQueueService.enqueueUser(user.id);
				result.reconcileEnqueued += 1;
			} catch (error) {
				Logger.warn(
					{userId: user.id.toString(), error},
					'Failed to enqueue active subscription user for reconciliation during sweep',
				);
			}
		}
		result.skipped += 1;
		return;
	}
	if (checkIsPremium(user)) {
		result.skipped += 1;
		return;
	}
	const storeRefreshes = await deferToStoreRefresh(user, deps);
	if (storeRefreshes > 0) {
		result.storeRefreshEnqueued += storeRefreshes;
		result.skipped += 1;
		return;
	}
	try {
		let currentUser = await deps.userRepository.patchUpsert(user.id, createPremiumClearPatch(), user.toRow());
		deps.userCacheService.setUserPartialResponseFromUserInBackground(currentUser);
		await dispatchUserUpdate(deps.gatewayService, currentUser);
		result.stripped += 1;
		const hasBeenSanitized = !!(user.premiumFlags & PremiumFlags.PERKS_SANITIZED);
		if (!hasBeenSanitized) {
			try {
				const didSanitize = await sanitizeExpiredPremiumMemberPerks({
					userId: user.id,
					guildIds: await deps.userRepository.getUserGuildIds(user.id),
					guildRepository: deps.guildRepository,
					gatewayService: deps.gatewayService,
					userCacheService: deps.userCacheService,
					requestCache: createRequestCache(),
				});
				if (didSanitize) {
					result.sanitized += 1;
				}
			} catch (error) {
				Logger.error(
					{userId: user.id.toString(), error},
					'Failed to sanitize guild member perks during expired premium sweep',
				);
			}
			const flaggedUser = await deps.userRepository.patchUpsert(
				currentUser.id,
				{premium_flags: currentUser.premiumFlags | PremiumFlags.PERKS_SANITIZED},
				currentUser.toRow(),
			);
			currentUser = flaggedUser;
			deps.userCacheService.setUserPartialResponseFromUserInBackground(currentUser);
			await dispatchUserUpdate(deps.gatewayService, currentUser);
		}
	} catch (error) {
		Logger.error({userId: user.id.toString(), error}, 'Failed to strip expired premium during sweep');
		result.failed += 1;
	}
}

async function processExpiredPremiumSweepCore(deps: SweepDeps): Promise<SweepResult> {
	const result: SweepResult = {
		processed: 0,
		stripped: 0,
		sanitized: 0,
		reconcileEnqueued: 0,
		storeRefreshEnqueued: 0,
		skipped: 0,
		failed: 0,
	};
	if (!isPremiumTieringActive()) {
		Logger.debug('Skipping expired premium sweep because premium tiering is not active');
		return result;
	}
	Logger.debug('Starting expired premium sweep');
	let pageState: string | null = null;
	do {
		const page = await deps.userRepository.scanAllUsersPage(BATCH_SIZE, pageState);
		pageState = page.pageState;
		const users = page.users;
		if (users.length === 0) continue;
		for (const user of users) {
			try {
				await processUser(user, deps, result);
			} catch (error) {
				Logger.error({error, userId: user.id}, 'Unhandled error processing user in expired premium sweep');
				result.failed += 1;
			}
		}
		result.processed += users.length;
		if (result.processed % 1000 === 0) {
			Logger.debug(
				{processed: result.processed, stripped: result.stripped, sanitized: result.sanitized},
				'Expired premium sweep progress',
			);
		}
	} while (pageState);
	Logger.info(
		{
			processed: result.processed,
			stripped: result.stripped,
			sanitized: result.sanitized,
			reconcileEnqueued: result.reconcileEnqueued,
			storeRefreshEnqueued: result.storeRefreshEnqueued,
			skipped: result.skipped,
			failed: result.failed,
		},
		'Completed expired premium sweep',
	);
	return result;
}

const processExpiredPremiumSweep: WorkerTaskHandler = async (_payload, helpers) => {
	helpers.logger.debug('Processing expired premium sweep task');
	const {
		userRepository,
		guildRepository,
		userCacheService,
		gatewayService,
		premiumStateReconciliationQueueService,
		storeEntitlementService,
		workerService,
	} = getWorkerDependencies();
	await processExpiredPremiumSweepCore({
		userRepository,
		guildRepository,
		userCacheService,
		gatewayService,
		premiumStateReconciliationQueueService,
		storeEntitlementService,
		workerService,
	});
};

export default processExpiredPremiumSweep;
