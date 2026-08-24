// SPDX-License-Identifier: AGPL-3.0-or-later

import {PremiumFlags, UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import type Stripe from 'stripe';
import type {UserID} from '../../BrandedTypes';
import {Config} from '../../Config';
import type {UserRow} from '../../database/types/UserTypes';
import {Logger} from '../../Logger';
import {getBillingRepository} from '../../middleware/ServiceRegistry';
import type {User} from '../../models/User';
import {canProvisionPremiumFromSubscriptionStatus} from '../../stripe/StripeSubscriptionAccessPolicy';
import {
	getPrimarySubscriptionItem,
	getSubscriptionPremiumPeriodEnd,
	getSubscriptionStartDate,
} from '../../stripe/StripeSubscriptionPeriod';
import {createPremiumClearPatch, getEffectivePremiumUntil} from '../../user/UserHelpers';
import {mapUserToPrivateResponse} from '../../user/UserMappers';
import {getWorkerDependencies} from '../WorkerContext';

interface ReconcileResult {
	status: 'patched' | 'no_change' | 'skipped' | 'no_active_subscription' | 'stripped_no_subscription' | 'missing_user';
	patchedFields: Array<string>;
}

const MAX_USERS_PER_RUN = 250;
const RETRY_DELAY_MS = 5 * 60 * 1000;

function getStripeSubscriptionCustomerId(subscription: Stripe.Subscription): string | null {
	if (!subscription.customer) {
		return null;
	}
	return typeof subscription.customer === 'string' ? subscription.customer : subscription.customer.id;
}

function getStripeSubscriptionBillingCycle(subscription: Stripe.Subscription): string | null {
	const item = getPrimarySubscriptionItem(subscription);
	const interval = item?.price?.recurring?.interval;
	if (interval === 'month') {
		return 'monthly';
	}
	if (interval === 'year') {
		return 'yearly';
	}
	return null;
}

function buildStripePremiumRepairPatch(user: User, subscription: Stripe.Subscription): Partial<UserRow> {
	const premiumUntil = getSubscriptionPremiumPeriodEnd(subscription);
	const subscriptionCustomerId = getStripeSubscriptionCustomerId(subscription);
	const premiumWillCancel = Boolean(subscription.cancel_at_period_end || subscription.cancel_at != null);
	const premiumBillingCycle = getStripeSubscriptionBillingCycle(subscription);
	const patch: Partial<UserRow> = {};
	if (user.premiumType !== UserPremiumTypes.SUBSCRIPTION) {
		patch.premium_type = UserPremiumTypes.SUBSCRIPTION;
	}
	const subscriptionStartDate = getSubscriptionStartDate(subscription);
	if (!user.premiumSince || user.premiumSince.getTime() > subscriptionStartDate.getTime()) {
		patch.premium_since = subscriptionStartDate;
	}
	if (premiumUntil && user.premiumUntil?.getTime() !== premiumUntil.getTime()) {
		patch.premium_until = premiumUntil;
	}
	if (user.premiumWillCancel !== premiumWillCancel) {
		patch.premium_will_cancel = premiumWillCancel;
	}
	if (premiumBillingCycle && user.premiumBillingCycle !== premiumBillingCycle) {
		patch.premium_billing_cycle = premiumBillingCycle;
	}
	if (user.stripeSubscriptionId !== subscription.id) {
		patch.stripe_subscription_id = subscription.id;
	}
	if (subscriptionCustomerId && user.stripeCustomerId !== subscriptionCustomerId) {
		patch.stripe_customer_id = subscriptionCustomerId;
	}
	return patch;
}

async function patchUserAndDispatch(
	user: User,
	patch: Partial<UserRow>,
): Promise<{
	updatedUser: User;
	patchedFields: Array<string>;
}> {
	const {gatewayService, userCacheService, userRepository} = getWorkerDependencies();
	const patchedFields = Object.keys(patch);
	const updatedUser = await userRepository.patchUpsert(user.id, patch, user.toRow());
	userCacheService.setUserPartialResponseFromUserInBackground(updatedUser);
	await gatewayService.dispatchPresence({
		userId: updatedUser.id,
		event: 'USER_UPDATE',
		data: mapUserToPrivateResponse(updatedUser),
	});
	return {updatedUser, patchedFields};
}

async function mirrorSubscriptionSnapshot(user: User, subscription: Stripe.Subscription): Promise<void> {
	try {
		await getBillingRepository().subscriptions.upsertFromStripe(subscription, {
			knownUserId: user.id,
			snapshotCapturedAt: new Date(),
		});
	} catch (error) {
		Logger.error(
			{error, userId: user.id.toString(), subscriptionId: subscription.id},
			'Failed to refresh billing subscription mirror during premium reconciliation queue processing',
		);
	}
}

function chooseEffectiveSubscription(user: User, subscriptions: Array<Stripe.Subscription>): Stripe.Subscription {
	if (user.stripeSubscriptionId) {
		const directSubscription = subscriptions.find((entry) => entry.id === user.stripeSubscriptionId);
		if (directSubscription) {
			return directSubscription;
		}
	}
	const sorted = [...subscriptions].sort((left, right) => {
		const leftEnd = getSubscriptionPremiumPeriodEnd(left)?.getTime() ?? 0;
		const rightEnd = getSubscriptionPremiumPeriodEnd(right)?.getTime() ?? 0;
		if (leftEnd !== rightEnd) {
			return rightEnd - leftEnd;
		}
		return right.created - left.created;
	});
	return sorted[0]!;
}

function trackMostRecentTerminalSubscription(
	current: Stripe.Subscription | null,
	candidate: Stripe.Subscription,
): Stripe.Subscription | null {
	if (!candidate.ended_at) {
		return current;
	}
	if (!current || (current.ended_at ?? 0) < candidate.ended_at) {
		return candidate;
	}
	return current;
}

async function getEffectiveActiveStripeSubscription(
	stripe: Stripe,
	user: User,
): Promise<{
	subscription: Stripe.Subscription | null;
	activeCount: number;
	mostRecentTerminalSubscription: Stripe.Subscription | null;
}> {
	const subscriptionsById = new Map<string, Stripe.Subscription>();
	let mostRecentTerminalSubscription: Stripe.Subscription | null = null;
	if (user.stripeSubscriptionId) {
		try {
			const subscription = await stripe.subscriptions.retrieve(user.stripeSubscriptionId);
			if (canProvisionPremiumFromSubscriptionStatus(subscription.status)) {
				subscriptionsById.set(subscription.id, subscription);
			} else {
				mostRecentTerminalSubscription = trackMostRecentTerminalSubscription(
					mostRecentTerminalSubscription,
					subscription,
				);
			}
		} catch (error) {
			Logger.warn(
				{
					userId: user.id.toString(),
					stripeSubscriptionId: user.stripeSubscriptionId,
					error,
				},
				'Failed to retrieve Stripe subscription during premium reconciliation queue processing',
			);
		}
	}
	if (user.stripeCustomerId) {
		try {
			const subscriptions = await stripe.subscriptions.list({
				customer: user.stripeCustomerId,
				status: 'all',
				limit: 100,
			});
			for (const subscription of subscriptions.data) {
				if (!canProvisionPremiumFromSubscriptionStatus(subscription.status)) {
					mostRecentTerminalSubscription = trackMostRecentTerminalSubscription(
						mostRecentTerminalSubscription,
						subscription,
					);
					continue;
				}
				subscriptionsById.set(subscription.id, subscription);
			}
		} catch (error) {
			Logger.warn(
				{
					userId: user.id.toString(),
					stripeCustomerId: user.stripeCustomerId,
					error,
				},
				'Failed to list Stripe subscriptions during premium reconciliation queue processing',
			);
		}
	}
	const activeSubscriptions = [...subscriptionsById.values()];
	if (activeSubscriptions.length === 0) {
		return {
			subscription: null,
			activeCount: 0,
			mostRecentTerminalSubscription,
		};
	}
	return {
		subscription: chooseEffectiveSubscription(user, activeSubscriptions),
		activeCount: activeSubscriptions.length,
		mostRecentTerminalSubscription,
	};
}

async function reconcileUserPremiumStateFromStripe(params: {userId: UserID; stripe: Stripe}): Promise<ReconcileResult> {
	const {userId, stripe} = params;
	const {userRepository} = getWorkerDependencies();
	const user = await userRepository.findUnique(userId);
	if (!user) {
		return {status: 'missing_user', patchedFields: []};
	}
	if (user.isBot || user.premiumType === UserPremiumTypes.LIFETIME) {
		return {status: 'skipped', patchedFields: []};
	}
	if (!user.stripeSubscriptionId && !user.stripeCustomerId) {
		return {status: 'skipped', patchedFields: []};
	}
	const {subscription, activeCount, mostRecentTerminalSubscription} = await getEffectiveActiveStripeSubscription(
		stripe,
		user,
	);
	if (!subscription) {
		const hasStalePremium = user.premiumType === UserPremiumTypes.SUBSCRIPTION;
		const hasNonStripePremium = Config.instance.selfHosted || (user.premiumFlags & PremiumFlags.ENABLED_OVERRIDE) !== 0;
		if (hasStalePremium && !hasNonStripePremium) {
			const patch: Partial<UserRow> = {};
			let effectivePremiumUntil = getEffectivePremiumUntil(user);
			if (mostRecentTerminalSubscription?.ended_at && user.premiumUntil) {
				const subscriptionEndedAt = new Date(mostRecentTerminalSubscription.ended_at * 1000);
				if (subscriptionEndedAt.getTime() < user.premiumUntil.getTime()) {
					patch.premium_until = subscriptionEndedAt;
					patch.premium_grace_ends_at = subscriptionEndedAt;
					effectivePremiumUntil = getEffectivePremiumUntil({
						premiumUntil: subscriptionEndedAt,
						premiumGiftExtensionEndsAt: user.premiumGiftExtensionEndsAt,
					});
				}
			}
			const hasFutureLocalEntitlement = effectivePremiumUntil != null && Date.now() <= effectivePremiumUntil.getTime();
			if (hasFutureLocalEntitlement) {
				if (user.premiumWillCancel !== true) {
					patch.premium_will_cancel = true;
				}
				if (Object.keys(patch).length === 0) {
					return {status: 'no_active_subscription', patchedFields: []};
				}
				const {patchedFields} = await patchUserAndDispatch(user, patch);
				return {status: 'patched', patchedFields};
			}
			const clearPatch = createPremiumClearPatch();
			const {patchedFields} = await patchUserAndDispatch(user, clearPatch);
			return {status: 'stripped_no_subscription', patchedFields};
		}
		return {status: 'no_active_subscription', patchedFields: []};
	}
	if (activeCount > 1) {
		Logger.warn(
			{
				userId: user.id.toString(),
				stripeCustomerId: user.stripeCustomerId,
				activeCount,
			},
			'Multiple active Stripe subscriptions found during premium reconciliation queue processing',
		);
	}
	await mirrorSubscriptionSnapshot(user, subscription);
	const patch = buildStripePremiumRepairPatch(user, subscription);
	const patchedFields = Object.keys(patch);
	if (patchedFields.length === 0) {
		return {status: 'no_change', patchedFields: []};
	}
	await patchUserAndDispatch(user, patch);
	return {status: 'patched', patchedFields};
}

const processPremiumStateReconciliationQueue: WorkerTaskHandler = async (_payload, helpers) => {
	const {premiumStateReconciliationQueueService, stripe} = getWorkerDependencies();
	if (!stripe) {
		helpers.logger.debug('Stripe is disabled, skipping premium reconciliation queue processing');
		return;
	}
	const readyUserIds = await premiumStateReconciliationQueueService.getReadyUserIds(Date.now(), MAX_USERS_PER_RUN);
	if (readyUserIds.length === 0) {
		helpers.logger.debug('Premium reconciliation queue is empty');
		return;
	}
	let patchedCount = 0;
	let noChangeCount = 0;
	let skippedCount = 0;
	let noActiveSubscriptionCount = 0;
	let strippedNoSubscriptionCount = 0;
	let failedCount = 0;
	let requeuedCount = 0;
	for (const userId of readyUserIds) {
		try {
			await premiumStateReconciliationQueueService.removeUser(userId);
		} catch (error) {
			Logger.warn(
				{error, userId: userId.toString()},
				'Failed to remove user from premium reconciliation queue before processing',
			);
		}
		try {
			const result = await reconcileUserPremiumStateFromStripe({
				userId,
				stripe,
			});
			if (result.status === 'patched') {
				patchedCount += 1;
				Logger.info(
					{
						userId: userId.toString(),
						patchedFields: result.patchedFields,
					},
					'Reconciled premium state from Stripe via worker queue',
				);
			} else if (result.status === 'stripped_no_subscription') {
				strippedNoSubscriptionCount += 1;
				Logger.info(
					{
						userId: userId.toString(),
						patchedFields: result.patchedFields,
					},
					'Stripped expired premium with no active Stripe subscription via worker queue',
				);
			} else if (result.status === 'no_change') {
				noChangeCount += 1;
			} else if (result.status === 'no_active_subscription') {
				noActiveSubscriptionCount += 1;
			} else {
				skippedCount += 1;
			}
		} catch (error) {
			failedCount += 1;
			Logger.error(
				{error, userId: userId.toString()},
				'Failed to process premium reconciliation queue entry, requeuing with delay',
			);
			try {
				await premiumStateReconciliationQueueService.enqueueUser(userId, new Date(Date.now() + RETRY_DELAY_MS));
				requeuedCount += 1;
			} catch (queueError) {
				Logger.error(
					{error: queueError, userId: userId.toString()},
					'Failed to requeue premium reconciliation queue entry after error',
				);
			}
		}
	}
	helpers.logger.info(
		{
			processed: readyUserIds.length,
			patchedCount,
			noChangeCount,
			skippedCount,
			noActiveSubscriptionCount,
			strippedNoSubscriptionCount,
			failedCount,
			requeuedCount,
		},
		'Finished processing premium reconciliation queue',
	);
};

export default processPremiumStateReconciliationQueue;
