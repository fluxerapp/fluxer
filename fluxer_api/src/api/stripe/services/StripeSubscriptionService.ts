// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {NoActiveSubscriptionError} from '@fluxer/errors/src/domains/payment/NoActiveSubscriptionError';
import {StripeError} from '@fluxer/errors/src/domains/payment/StripeError';
import {StripeInvalidProductConfigurationError} from '@fluxer/errors/src/domains/payment/StripeInvalidProductConfigurationError';
import {StripeNoActiveSubscriptionError} from '@fluxer/errors/src/domains/payment/StripeNoActiveSubscriptionError';
import {StripeNoSubscriptionError} from '@fluxer/errors/src/domains/payment/StripeNoSubscriptionError';
import {StripePaymentNotAvailableError} from '@fluxer/errors/src/domains/payment/StripePaymentNotAvailableError';
import {StripeSubscriptionAlreadyCancelingError} from '@fluxer/errors/src/domains/payment/StripeSubscriptionAlreadyCancelingError';
import {StripeSubscriptionNotCancelingError} from '@fluxer/errors/src/domains/payment/StripeSubscriptionNotCancelingError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {CurrentSubscriptionPriceResponse} from '@fluxer/schema/src/domains/premium/PremiumSchemas';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import {seconds} from 'itty-time';
import type Stripe from 'stripe';
import type {UserID} from '../../BrandedTypes';
import type {GiftCodeDurationType} from '../../database/types/PaymentTypes';
import type {UserRow} from '../../database/types/UserTypes';
import type {IGatewayService} from '../../infrastructure/IGatewayService';
import {Logger} from '../../Logger';
import {getBillingRepository} from '../../middleware/ServiceRegistry';
import {addGiftCodeDuration} from '../../models/GiftCode';
import type {User} from '../../models/User';
import type {IUserRepository} from '../../user/IUserRepository';
import {mapUserToPrivateResponse} from '../../user/UserMappers';
import type {Currency} from '../../utils/CurrencyUtils';
import type {RecurringBillingCycle} from '../ProductRegistry';
import {
	getPrimarySubscriptionItem,
	getSubscriptionEntitlementPeriodEndUnix,
	getSubscriptionItemPeriodEndUnix,
	getSubscriptionPremiumPeriodEnd,
} from '../StripeSubscriptionPeriod';
import {extractId} from '../StripeUtils';

type BillingCycleChangeEffectiveAt = 'now' | 'period_end';

export class StripeSubscriptionService {
	constructor(
		private stripe: Stripe | null,
		private userRepository: IUserRepository,
		private productRegistry: {
			getRecurringSubscriptionPriceId: (billingCycle: RecurringBillingCycle, currency: string) => string | null;
		},
		private cacheService: ICacheService,
		private gatewayService: IGatewayService,
	) {}

	async cancelSubscriptionAtPeriodEnd(userId: UserID): Promise<void> {
		if (!this.stripe) {
			throw new StripePaymentNotAvailableError();
		}
		const user = await this.userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		if (!user.stripeSubscriptionId) {
			throw new StripeNoActiveSubscriptionError();
		}
		if (user.premiumWillCancel) {
			throw new StripeSubscriptionAlreadyCancelingError();
		}
		try {
			const subscription = await this.stripe.subscriptions.retrieve(user.stripeSubscriptionId, {
				expand: ['items.data.price', 'schedule'],
			});
			const schedule = await this.loadSubscriptionSchedule(subscription.schedule);
			if (schedule) {
				await this.cancelScheduledSubscriptionAtPeriodEnd(user, subscription, schedule);
				return;
			}
			const updatedSubscription = await this.stripe.subscriptions.update(user.stripeSubscriptionId, {
				cancel_at_period_end: true,
			});
			try {
				await getBillingRepository().subscriptions.upsertFromStripe(updatedSubscription, {
					knownUserId: user.id,
					snapshotCapturedAt: new Date(),
				});
			} catch (mirrorErr) {
				Logger.error(
					{mirrorErr, subId: updatedSubscription.id},
					'Mirror upsert failed after Stripe write; reconciler will heal',
				);
			}
			const updatedUser = await this.userRepository.patchUpsert(
				userId,
				{
					premium_will_cancel: true,
				},
				user.toRow(),
			);
			await this.dispatchUser(updatedUser);
			Logger.debug({userId, subscriptionId: user.stripeSubscriptionId}, 'Subscription set to cancel at period end');
		} catch (error: unknown) {
			Logger.error(
				{error, userId, subscriptionId: user.stripeSubscriptionId},
				'Failed to cancel subscription at period end',
			);
			const message = error instanceof Error ? error.message : 'Failed to cancel subscription';
			throw new StripeError(message);
		}
	}

	async cancelSubscriptionImmediately(userId: UserID, reason?: string): Promise<void> {
		if (!this.stripe) {
			throw new StripePaymentNotAvailableError();
		}
		const user = await this.userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		if (!user.stripeSubscriptionId) {
			throw new StripeNoActiveSubscriptionError();
		}
		try {
			const canceledSubscription = await this.stripe.subscriptions.cancel(
				user.stripeSubscriptionId,
				{
					invoice_now: false,
					prorate: false,
					...(reason
						? {
								cancellation_details: {
									comment: reason,
								},
							}
						: {}),
				},
				{idempotencyKey: `admin-cancel-now:${user.id}:${user.stripeSubscriptionId}`},
			);
			try {
				await getBillingRepository().subscriptions.upsertFromStripe(canceledSubscription, {
					knownUserId: user.id,
					snapshotCapturedAt: new Date(),
				});
			} catch (mirrorErr) {
				Logger.error(
					{mirrorErr, subId: canceledSubscription.id},
					'Mirror upsert failed after Stripe write; reconciler will heal',
				);
			}
			const patch: Partial<UserRow> = {
				premium_will_cancel: false,
				premium_billing_cycle: null,
				stripe_subscription_id: null,
				premium_grace_ends_at: null,
			};
			if (user.premiumType !== UserPremiumTypes.LIFETIME) {
				Object.assign(patch, {
					premium_type: UserPremiumTypes.NONE,
					premium_since: null,
					premium_until: null,
				});
			}
			const updatedUser = await this.userRepository.patchUpsert(userId, patch, user.toRow());
			await this.dispatchUser(updatedUser);
			Logger.debug({userId, subscriptionId: user.stripeSubscriptionId}, 'Subscription canceled immediately');
		} catch (error: unknown) {
			Logger.error(
				{error, userId, subscriptionId: user.stripeSubscriptionId},
				'Failed to cancel subscription immediately',
			);
			const message = error instanceof Error ? error.message : 'Failed to cancel subscription immediately';
			throw new StripeError(message);
		}
	}

	async reactivateSubscription(userId: UserID): Promise<void> {
		if (!this.stripe) {
			throw new StripePaymentNotAvailableError();
		}
		const user = await this.userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		if (!user.stripeSubscriptionId) {
			throw new StripeNoSubscriptionError();
		}
		if (!user.premiumWillCancel) {
			throw new StripeSubscriptionNotCancelingError();
		}
		try {
			let subscription = await this.stripe.subscriptions.retrieve(user.stripeSubscriptionId, {
				expand: ['schedule'],
			});
			const schedule = await this.loadSubscriptionSchedule(subscription.schedule);
			if (schedule) {
				await this.stripe.subscriptionSchedules.release(schedule.id, {
					preserve_cancel_date: false,
				});
				subscription = await this.stripe.subscriptions.retrieve(user.stripeSubscriptionId, {
					expand: ['items.data.price'],
				});
			}
			const clearCancellationParams = this.getClearCancellationUpdateParams(subscription);
			const reactivatedSubscription = clearCancellationParams
				? await this.stripe.subscriptions.update(user.stripeSubscriptionId, clearCancellationParams)
				: subscription;
			try {
				await getBillingRepository().subscriptions.upsertFromStripe(reactivatedSubscription, {
					knownUserId: user.id,
					snapshotCapturedAt: new Date(),
				});
			} catch (mirrorErr) {
				Logger.error(
					{mirrorErr, subId: reactivatedSubscription.id},
					'Mirror upsert failed after Stripe write; reconciler will heal',
				);
			}
			const updatedUser = await this.userRepository.patchUpsert(
				userId,
				{
					premium_will_cancel: false,
				},
				user.toRow(),
			);
			await this.dispatchUser(updatedUser);
			Logger.debug({userId, subscriptionId: user.stripeSubscriptionId}, 'Subscription reactivated');
		} catch (error: unknown) {
			Logger.error({error, userId, subscriptionId: user.stripeSubscriptionId}, 'Failed to reactivate subscription');
			const message = error instanceof Error ? error.message : 'Failed to reactivate subscription';
			throw new StripeError(message);
		}
	}

	async cancelPendingSubscriptionChange(userId: UserID): Promise<void> {
		if (!this.stripe) {
			throw new StripePaymentNotAvailableError();
		}
		const user = await this.userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		if (!user.stripeSubscriptionId) {
			throw new StripeNoActiveSubscriptionError();
		}
		try {
			const subscription = await this.stripe.subscriptions.retrieve(user.stripeSubscriptionId, {
				expand: ['items.data.price', 'schedule'],
			});
			const schedule = await this.loadSubscriptionSchedule(subscription.schedule);
			if (!schedule || !this.subscriptionScheduleHasPendingBillingCycleChange(schedule, subscription)) {
				Logger.debug(
					{userId, subscriptionId: user.stripeSubscriptionId, scheduleId: schedule?.id ?? null},
					'No pending subscription billing-cycle change to cancel',
				);
				return;
			}
			await this.stripe.subscriptionSchedules.release(schedule.id, {
				preserve_cancel_date: false,
			});
			const releasedSubscription = await this.stripe.subscriptions.retrieve(subscription.id, {
				expand: ['items.data.price'],
			});
			try {
				await getBillingRepository().subscriptions.upsertFromStripe(releasedSubscription, {
					knownUserId: user.id,
					snapshotCapturedAt: new Date(),
				});
			} catch (mirrorErr) {
				Logger.error(
					{mirrorErr, subId: releasedSubscription.id},
					'Mirror upsert failed after releasing pending billing-cycle schedule; reconciler will heal',
				);
			}
			const patch: Record<string, unknown> = {
				premium_will_cancel: false,
			};
			const computedPremiumUntil = getSubscriptionPremiumPeriodEnd(releasedSubscription);
			if (computedPremiumUntil) {
				patch['premium_until'] = computedPremiumUntil;
			}
			const updatedCustomerId = extractId(releasedSubscription.customer);
			if (updatedCustomerId && updatedCustomerId !== user.stripeCustomerId) {
				patch['stripe_customer_id'] = updatedCustomerId;
			}
			const updatedUser = await this.userRepository.patchUpsert(userId, patch, user.toRow());
			await this.dispatchUser(updatedUser);
			Logger.debug(
				{userId, subscriptionId: user.stripeSubscriptionId, scheduleId: schedule.id},
				'Pending subscription billing-cycle change canceled',
			);
		} catch (error: unknown) {
			Logger.error(
				{error, userId, subscriptionId: user.stripeSubscriptionId},
				'Failed to cancel pending subscription billing-cycle change',
			);
			const message =
				error instanceof Error ? error.message : 'Failed to cancel pending subscription billing-cycle change';
			throw new StripeError(message);
		}
	}

	async changeBillingCycle(
		userId: UserID,
		billingCycle: RecurringBillingCycle,
		effectiveAt: BillingCycleChangeEffectiveAt = 'now',
	): Promise<void> {
		if (!this.stripe) {
			throw new StripePaymentNotAvailableError();
		}
		const user = await this.userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		if (!user.stripeSubscriptionId) {
			throw new StripeNoActiveSubscriptionError();
		}
		try {
			const subscription = await this.stripe.subscriptions.retrieve(user.stripeSubscriptionId, {
				expand: ['schedule'],
			});
			const item = getPrimarySubscriptionItem(subscription);
			if (!item?.id || !item.price?.recurring || !item.price.currency) {
				throw new StripeError('Subscription is missing a recurring primary item');
			}
			const currentBillingCycle = this.getBillingCycleFromInterval(item.price.recurring.interval);
			if (!currentBillingCycle) {
				throw new StripeError('Unsupported recurring interval for subscription');
			}
			if (currentBillingCycle === billingCycle) {
				Logger.debug(
					{userId, subscriptionId: user.stripeSubscriptionId, billingCycle},
					'Skipping billing cycle change because the target cycle is already active',
				);
				return;
			}
			const targetPriceId = this.productRegistry.getRecurringSubscriptionPriceId(billingCycle, item.price.currency);
			if (!targetPriceId) {
				throw new StripeInvalidProductConfigurationError();
			}
			const scheduledChange = await this.loadSubscriptionSchedule(subscription.schedule);
			if (scheduledChange && this.subscriptionScheduleHasFutureTargetPrice(scheduledChange, targetPriceId)) {
				const patch: Record<string, unknown> = {
					premium_will_cancel: false,
				};
				const computedPremiumUntil = getSubscriptionPremiumPeriodEnd(subscription);
				if (computedPremiumUntil) {
					patch['premium_until'] = computedPremiumUntil;
				}
				const updatedCustomerId = extractId(subscription.customer);
				if (updatedCustomerId && updatedCustomerId !== user.stripeCustomerId) {
					patch['stripe_customer_id'] = updatedCustomerId;
				}
				const updatedUser = await this.userRepository.patchUpsert(userId, patch, user.toRow());
				await this.dispatchUser(updatedUser);
				Logger.debug(
					{
						userId,
						subscriptionId: user.stripeSubscriptionId,
						scheduleId: scheduledChange.id,
						toBillingCycle: billingCycle,
						targetPriceId,
					},
					'Skipping immediate billing cycle change because the target cycle is already scheduled',
				);
				return;
			}
			if (effectiveAt === 'period_end') {
				await this.scheduleBillingCycleChangeAtPeriodEnd({
					user,
					subscription,
					item,
					currentBillingCycle,
					targetBillingCycle: billingCycle,
					targetPriceId,
				});
				return;
			}
			const clearCancellationParams = this.getClearCancellationUpdateParams(subscription) ?? {};
			const updatedSubscription = await this.stripe.subscriptions.update(user.stripeSubscriptionId, {
				...clearCancellationParams,
				items: [
					{
						id: item.id,
						price: targetPriceId,
						quantity: item.quantity ?? 1,
					},
				],
				payment_behavior: 'error_if_incomplete',
				proration_behavior: 'always_invoice',
			});
			try {
				await getBillingRepository().subscriptions.upsertFromStripe(updatedSubscription, {
					knownUserId: user.id,
					snapshotCapturedAt: new Date(),
				});
			} catch (mirrorErr) {
				Logger.error(
					{mirrorErr, subId: updatedSubscription.id},
					'Mirror upsert failed after Stripe write; reconciler will heal',
				);
			}
			const patch: Record<string, unknown> = {
				premium_billing_cycle: billingCycle,
				premium_will_cancel: false,
			};
			const computedPremiumUntil = getSubscriptionPremiumPeriodEnd(updatedSubscription);
			if (computedPremiumUntil) {
				patch['premium_until'] = computedPremiumUntil;
			}
			const updatedCustomerId = extractId(updatedSubscription.customer);
			if (updatedCustomerId && updatedCustomerId !== user.stripeCustomerId) {
				patch['stripe_customer_id'] = updatedCustomerId;
			}
			const updatedUser = await this.userRepository.patchUpsert(userId, patch, user.toRow());
			await this.dispatchUser(updatedUser);
			Logger.debug(
				{
					userId,
					subscriptionId: user.stripeSubscriptionId,
					fromBillingCycle: currentBillingCycle,
					toBillingCycle: billingCycle,
					targetPriceId,
					premiumUntil: computedPremiumUntil,
				},
				'Subscription billing cycle changed',
			);
		} catch (error: unknown) {
			Logger.error(
				{error, userId, subscriptionId: user.stripeSubscriptionId, billingCycle, effectiveAt},
				'Failed to change subscription billing cycle',
			);
			if (error instanceof StripeError || error instanceof StripeInvalidProductConfigurationError) {
				throw error;
			}
			const message = error instanceof Error ? error.message : 'Failed to change subscription billing cycle';
			throw new StripeError(message);
		}
	}

	private async scheduleBillingCycleChangeAtPeriodEnd({
		user,
		subscription,
		item,
		currentBillingCycle,
		targetBillingCycle,
		targetPriceId,
	}: {
		user: User;
		subscription: Stripe.Subscription;
		item: Stripe.SubscriptionItem;
		currentBillingCycle: RecurringBillingCycle;
		targetBillingCycle: RecurringBillingCycle;
		targetPriceId: string;
	}): Promise<void> {
		if (!this.stripe) {
			throw new StripePaymentNotAvailableError();
		}
		const periodEnd = getSubscriptionEntitlementPeriodEndUnix(subscription, item);
		if (!periodEnd || periodEnd <= Math.floor(Date.now() / 1000)) {
			throw new StripeError('Subscription is missing a future period end for scheduled billing cycle change');
		}
		let currentSubscription = subscription;
		if (subscription.cancel_at || subscription.cancel_at_period_end) {
			currentSubscription = await this.stripe.subscriptions.update(
				subscription.id,
				this.getClearCancellationUpdateParams(subscription) ?? {proration_behavior: 'none'},
			);
			try {
				await getBillingRepository().subscriptions.upsertFromStripe(currentSubscription, {
					knownUserId: user.id,
					snapshotCapturedAt: new Date(),
				});
			} catch (mirrorErr) {
				Logger.error(
					{mirrorErr, subId: currentSubscription.id},
					'Mirror upsert failed after clearing cancellation for scheduled billing cycle change; reconciler will heal',
				);
			}
		}
		const scheduleId = extractId(currentSubscription.schedule);
		const schedule = scheduleId
			? await this.stripe.subscriptionSchedules.retrieve(scheduleId)
			: await this.stripe.subscriptionSchedules.create({
					from_subscription: currentSubscription.id,
				});
		const currentPhase = this.buildCurrentSchedulePhase(schedule, currentSubscription, item, periodEnd);
		const firstInvoiceCredit = await this.buildPeriodEndCycleSwapCredit({
			subscription: currentSubscription,
			item,
			currentBillingCycle,
			targetBillingCycle,
			targetPriceId,
		});
		const firstInvoiceCreditAmountMinor =
			firstInvoiceCredit?.price_data?.unit_amount != null ? -firstInvoiceCredit.price_data.unit_amount : null;
		const targetPhase: Stripe.SubscriptionScheduleUpdateParams.Phase = {
			start_date: periodEnd,
			billing_cycle_anchor: 'phase_start',
			items: [
				{
					price: targetPriceId,
					quantity: item.quantity ?? 1,
				},
			],
			proration_behavior: 'none',
		};
		if (firstInvoiceCredit) {
			targetPhase.add_invoice_items = [firstInvoiceCredit];
			targetPhase.metadata = {
				first_invoice_credit_amount_minor: String(firstInvoiceCreditAmountMinor),
				first_invoice_credit_source: 'current_period_payment',
			};
		}
		await this.stripe.subscriptionSchedules.update(schedule.id, {
			end_behavior: 'release',
			proration_behavior: 'none',
			metadata: {
				user_id: user.id.toString(),
				pending_billing_cycle: targetBillingCycle,
			},
			phases: [currentPhase, targetPhase],
		});
		const patch: Record<string, unknown> = {
			premium_will_cancel: false,
		};
		const computedPremiumUntil = getSubscriptionPremiumPeriodEnd(currentSubscription);
		if (computedPremiumUntil) {
			patch['premium_until'] = computedPremiumUntil;
		}
		const updatedCustomerId = extractId(currentSubscription.customer);
		if (updatedCustomerId && updatedCustomerId !== user.stripeCustomerId) {
			patch['stripe_customer_id'] = updatedCustomerId;
		}
		const updatedUser = await this.userRepository.patchUpsert(user.id, patch, user.toRow());
		await this.dispatchUser(updatedUser);
		Logger.debug(
			{
				userId: user.id,
				subscriptionId: currentSubscription.id,
				scheduleId: schedule.id,
				fromBillingCycle: currentBillingCycle,
				toBillingCycle: targetBillingCycle,
				targetPriceId,
				firstInvoiceCreditAmountMinor,
				periodEnd,
			},
			'Subscription billing cycle change scheduled for period end',
		);
	}

	private async cancelScheduledSubscriptionAtPeriodEnd(
		user: User,
		subscription: Stripe.Subscription,
		schedule: Stripe.SubscriptionSchedule,
	): Promise<void> {
		if (!this.stripe) {
			throw new StripePaymentNotAvailableError();
		}
		const item = getPrimarySubscriptionItem(subscription);
		const periodEnd = getSubscriptionEntitlementPeriodEndUnix(subscription, item);
		if (!item || !periodEnd || periodEnd <= Math.floor(Date.now() / 1000)) {
			throw new StripeError('Subscription is missing a future period end for scheduled cancellation');
		}
		const currentPhase = this.buildCurrentSchedulePhase(schedule, subscription, item, periodEnd);
		await this.stripe.subscriptionSchedules.update(schedule.id, {
			end_behavior: 'cancel',
			proration_behavior: 'none',
			metadata: {
				...schedule.metadata,
				user_id: user.id.toString(),
				pending_billing_cycle: '',
				cancellation_source: 'self_serve',
			},
			phases: [currentPhase],
		});
		const updatedSubscription = await this.stripe.subscriptions.retrieve(subscription.id, {
			expand: ['items.data.price'],
		});
		try {
			await getBillingRepository().subscriptions.upsertFromStripe(updatedSubscription, {
				knownUserId: user.id,
				snapshotCapturedAt: new Date(),
			});
		} catch (mirrorErr) {
			Logger.error(
				{mirrorErr, subId: updatedSubscription.id},
				'Mirror upsert failed after schedule-managed cancellation; reconciler will heal',
			);
		}
		const patch: Record<string, unknown> = {
			premium_will_cancel: true,
		};
		const computedPremiumUntil = getSubscriptionPremiumPeriodEnd(updatedSubscription);
		if (computedPremiumUntil) {
			patch['premium_until'] = computedPremiumUntil;
		}
		const updatedCustomerId = extractId(updatedSubscription.customer);
		if (updatedCustomerId && updatedCustomerId !== user.stripeCustomerId) {
			patch['stripe_customer_id'] = updatedCustomerId;
		}
		const updatedUser = await this.userRepository.patchUpsert(user.id, patch, user.toRow());
		await this.dispatchUser(updatedUser);
		Logger.debug(
			{userId: user.id, subscriptionId: subscription.id, scheduleId: schedule.id, periodEnd},
			'Subscription schedule set to cancel at period end',
		);
	}

	private async buildPeriodEndCycleSwapCredit({
		subscription,
		item,
		currentBillingCycle,
		targetBillingCycle,
		targetPriceId,
	}: {
		subscription: Stripe.Subscription;
		item: Stripe.SubscriptionItem;
		currentBillingCycle: RecurringBillingCycle;
		targetBillingCycle: RecurringBillingCycle;
		targetPriceId: string;
	}): Promise<Stripe.SubscriptionScheduleUpdateParams.Phase.AddInvoiceItem | null> {
		if (!this.stripe || currentBillingCycle !== 'monthly' || targetBillingCycle !== 'yearly') {
			return null;
		}
		if (subscription.trial_end != null && subscription.trial_end > Math.floor(Date.now() / 1000)) {
			return null;
		}
		const currentAmountMinor = item.price.unit_amount;
		if (currentAmountMinor == null || currentAmountMinor <= 0) {
			return null;
		}
		const targetPrice = await this.stripe.prices.retrieve(targetPriceId);
		const targetAmountMinor = targetPrice.unit_amount;
		const targetProductId = extractId(targetPrice.product);
		if (!targetProductId || targetAmountMinor == null || targetAmountMinor <= 0) {
			return null;
		}
		const quantity = item.quantity ?? 1;
		const creditAmountMinor = Math.min(currentAmountMinor * quantity, targetAmountMinor * quantity);
		if (creditAmountMinor <= 0) {
			return null;
		}
		const creditInvoiceItem: Stripe.SubscriptionScheduleUpdateParams.Phase.AddInvoiceItem & {discountable: boolean} = {
			discountable: false,
			price_data: {
				currency: targetPrice.currency,
				product: targetProductId,
				tax_behavior: 'unspecified',
				unit_amount: -creditAmountMinor,
			},
			quantity: 1,
			metadata: {
				reason: 'period_end_cycle_swap_credit',
				credited_price_id: item.price.id,
				credited_amount_minor: String(creditAmountMinor),
			},
		};
		return creditInvoiceItem;
	}

	private getClearCancellationUpdateParams(subscription: Stripe.Subscription): Stripe.SubscriptionUpdateParams | null {
		if (subscription.cancel_at_period_end) {
			return {
				cancel_at_period_end: false,
				proration_behavior: 'none',
			};
		}
		if (subscription.cancel_at != null) {
			return {
				cancel_at: null,
				proration_behavior: 'none',
			};
		}
		return null;
	}

	private async loadSubscriptionSchedule(
		scheduleRef: Stripe.Subscription['schedule'],
	): Promise<Stripe.SubscriptionSchedule | null> {
		if (!scheduleRef) {
			return null;
		}
		if (typeof scheduleRef === 'object') {
			return scheduleRef;
		}
		if (!this.stripe) {
			return null;
		}
		return this.stripe.subscriptionSchedules.retrieve(scheduleRef);
	}

	private subscriptionScheduleHasFutureTargetPrice(
		schedule: Stripe.SubscriptionSchedule,
		targetPriceId: string,
	): boolean {
		const now = Math.floor(Date.now() / 1000);
		return schedule.phases.some(
			(phase) =>
				typeof phase.start_date === 'number' &&
				phase.start_date > now &&
				phase.items.some((phaseItem) => extractId(phaseItem.price) === targetPriceId),
		);
	}

	private subscriptionScheduleHasPendingBillingCycleChange(
		schedule: Stripe.SubscriptionSchedule,
		subscription: Stripe.Subscription,
	): boolean {
		const now = Math.floor(Date.now() / 1000);
		const currentItem = getPrimarySubscriptionItem(subscription);
		const currentPriceId = extractId(currentItem?.price);
		const currentBillingCycle = this.getBillingCycleFromInterval(currentItem?.price?.recurring?.interval ?? '');
		const metadataTargetBillingCycle =
			schedule.metadata?.pending_billing_cycle === 'monthly' || schedule.metadata?.pending_billing_cycle === 'yearly'
				? schedule.metadata.pending_billing_cycle
				: null;
		if (metadataTargetBillingCycle && metadataTargetBillingCycle !== currentBillingCycle) {
			return true;
		}
		return schedule.phases.some(
			(phase) =>
				typeof phase.start_date === 'number' &&
				phase.start_date > now &&
				phase.items.some((phaseItem) => {
					const phasePriceId = extractId(phaseItem.price);
					return phasePriceId != null && phasePriceId !== currentPriceId;
				}),
		);
	}

	private buildCurrentSchedulePhase(
		schedule: Stripe.SubscriptionSchedule,
		subscription: Stripe.Subscription,
		item: Stripe.SubscriptionItem,
		periodEnd: number,
	): Stripe.SubscriptionScheduleUpdateParams.Phase {
		const now = Math.floor(Date.now() / 1000);
		const phase =
			schedule.phases.find((candidate) => candidate.start_date <= now && candidate.end_date >= now) ??
			schedule.phases[0] ??
			null;
		const phaseItems =
			phase?.items
				.map((phaseItem) => {
					const price = extractId(phaseItem.price);
					if (!price) {
						return null;
					}
					return {
						price,
						quantity: phaseItem.quantity ?? 1,
					};
				})
				.filter((phaseItem): phaseItem is {price: string; quantity: number} => phaseItem !== null) ?? [];
		const currentPhase: Stripe.SubscriptionScheduleUpdateParams.Phase = {
			start_date: phase?.start_date ?? subscription.start_date ?? subscription.created,
			end_date: periodEnd,
			items:
				phaseItems.length > 0
					? phaseItems
					: [
							{
								price: item.price.id,
								quantity: item.quantity ?? 1,
							},
						],
			proration_behavior: 'none',
		};
		const trialEnd = subscription.trial_end;
		if (trialEnd != null && trialEnd > now) {
			if (trialEnd >= periodEnd) {
				currentPhase.trial = true;
			} else {
				currentPhase.trial_end = trialEnd;
			}
		}
		return currentPhase;
	}

	async getCurrentSubscriptionPrice(userId: UserID): Promise<CurrentSubscriptionPriceResponse> {
		if (!this.stripe) {
			return null;
		}
		const user = await this.userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		if (!user.stripeSubscriptionId) {
			return null;
		}
		const cacheKey = `stripe:subscription:current_price:${user.stripeSubscriptionId}`;
		try {
			return await this.cacheService.getOrSet<CurrentSubscriptionPriceResponse>(
				cacheKey,
				async () => this.loadCurrentSubscriptionPrice(user.stripeSubscriptionId!),
				StripeSubscriptionService.CURRENT_PRICE_CACHE_TTL_SECONDS,
			);
		} catch (error) {
			Logger.warn(
				{error, userId, subscriptionId: user.stripeSubscriptionId},
				'Failed to load current subscription price from Stripe',
			);
			return null;
		}
	}

	private async loadCurrentSubscriptionPrice(subscriptionId: string): Promise<CurrentSubscriptionPriceResponse> {
		if (!this.stripe) {
			return null;
		}
		const subscription = await this.stripe.subscriptions.retrieve(subscriptionId, {
			expand: ['items.data.price'],
		});
		const item = getPrimarySubscriptionItem(subscription);
		const price = item?.price;
		if (!price || price.unit_amount == null || !price.recurring || !price.currency) {
			return null;
		}
		const billingCycle = this.getBillingCycleFromInterval(price.recurring.interval);
		if (!billingCycle) {
			return null;
		}
		const currency = price.currency.toUpperCase() as Currency;
		const listPriceId = this.productRegistry.getRecurringSubscriptionPriceId(billingCycle, currency);
		const listAmountMinor = listPriceId ? await this.getListPriceAmountMinor(listPriceId) : null;
		const isGrandfathered = listPriceId != null && listPriceId !== price.id;
		return {
			price_id: price.id,
			amount_minor: price.unit_amount,
			currency,
			billing_cycle: billingCycle,
			is_grandfathered: isGrandfathered,
			list_price_id: listPriceId,
			list_amount_minor: listAmountMinor,
		};
	}

	private async getListPriceAmountMinor(priceId: string): Promise<number | null> {
		if (!this.stripe) {
			return null;
		}
		try {
			return await this.cacheService.getOrSet<number | null>(
				`stripe:list_price_amount:${priceId}`,
				async () => {
					const price = await this.stripe!.prices.retrieve(priceId);
					return price.unit_amount ?? null;
				},
				StripeSubscriptionService.LIST_PRICE_CACHE_TTL_SECONDS,
			);
		} catch (error) {
			Logger.warn({error, priceId}, 'Failed to retrieve Stripe list price amount');
			return null;
		}
	}

	async extendSubscriptionWithGiftTrialDuration(
		user: User,
		durationType: GiftCodeDurationType,
		durationQuantity: number,
		idempotencyKey: string,
	): Promise<void> {
		if (!this.stripe || !user.stripeSubscriptionId) {
			Logger.debug(
				{
					userId: user.id,
					durationType,
					durationQuantity,
					idempotencyKey,
					hasStripeClient: Boolean(this.stripe),
					hasStripeSubscriptionId: Boolean(user.stripeSubscriptionId),
				},
				'Cannot extend subscription with gift trial duration: no active Stripe subscription context',
			);
			throw new NoActiveSubscriptionError();
		}
		const appliedKey = `gift_trial_applied:${user.id}:${idempotencyKey}`;
		const inflightKey = `gift_trial_inflight:${user.id}:${idempotencyKey}`;
		Logger.debug(
			{
				userId: user.id,
				subscriptionId: user.stripeSubscriptionId,
				durationType,
				durationQuantity,
				idempotencyKey,
				appliedKey,
				inflightKey,
			},
			'Starting Stripe gift trial extension',
		);
		if (await this.cacheService.get<boolean>(appliedKey)) {
			Logger.debug({userId: user.id, idempotencyKey}, 'Gift trial extension already applied (idempotent hit)');
			return;
		}
		if (await this.cacheService.get<boolean>(inflightKey)) {
			Logger.debug({userId: user.id, idempotencyKey}, 'Gift trial extension in-flight; skipping duplicate');
			return;
		}
		await this.cacheService.set(inflightKey, true, seconds('1 minute'));
		Logger.debug({userId: user.id, idempotencyKey, inflightKey}, 'Gift trial inflight sentinel set');
		const userLockKey = `gift_trial_user_lock:${user.id}`;
		const userLockToken = await this.acquireUserTrialLock(userLockKey, user.id);
		Logger.debug(
			{
				userId: user.id,
				idempotencyKey,
				userLockKey,
				lockAcquired: Boolean(userLockToken),
			},
			'Gift trial user lock acquisition completed',
		);
		try {
			if (await this.cacheService.get<boolean>(appliedKey)) {
				Logger.debug({userId: user.id, idempotencyKey}, 'Gift trial extension already applied after lock acquisition');
				return;
			}
			const subscription = await this.stripe.subscriptions.retrieve(user.stripeSubscriptionId);
			Logger.debug(
				{
					userId: user.id,
					subscriptionId: user.stripeSubscriptionId,
					retrievedTrialEnd: subscription.trial_end,
				},
				'Retrieved Stripe subscription before gift trial extension',
			);
			const currentTrialEnd = subscription.trial_end;
			const item = getPrimarySubscriptionItem(subscription);
			const currentPeriodEnd = getSubscriptionItemPeriodEndUnix(item);
			const baseUnix = currentTrialEnd ?? currentPeriodEnd;
			Logger.debug(
				{
					userId: user.id,
					subscriptionId: user.stripeSubscriptionId,
					currentTrialEnd,
					currentPeriodEnd,
					baseUnix,
					durationType,
					durationQuantity,
				},
				'Computed Stripe gift trial extension base timestamp',
			);
			if (!baseUnix) {
				throw new StripeError('Subscription has no trial_end or current_period_end');
			}
			const baseDate = new Date(baseUnix * 1000);
			const newTrialEnd = addGiftCodeDuration(baseDate, durationType, durationQuantity);
			if (newTrialEnd === null) {
				throw new StripeError('Gift duration must be greater than zero when extending subscription trial');
			}
			const newTrialEndUnix = Math.floor(newTrialEnd.getTime() / 1000);
			Logger.debug(
				{
					userId: user.id,
					subscriptionId: user.stripeSubscriptionId,
					baseDate,
					newTrialEnd,
					newTrialEndUnix,
				},
				'Computed new Stripe trial end for gift extension',
			);
			const stripeIdempotencyKey = `gift_trial_extend:${user.id}:${idempotencyKey}`;
			const trialExtendedSubscription = await this.stripe.subscriptions.update(
				user.stripeSubscriptionId,
				{
					trial_end: newTrialEndUnix,
					proration_behavior: 'none',
				},
				{idempotencyKey: stripeIdempotencyKey},
			);
			try {
				await getBillingRepository().subscriptions.upsertFromStripe(trialExtendedSubscription, {
					knownUserId: user.id,
					snapshotCapturedAt: new Date(),
				});
			} catch (mirrorErr) {
				Logger.error(
					{mirrorErr, subId: trialExtendedSubscription.id},
					'Mirror upsert failed after Stripe write; reconciler will heal',
				);
			}
			Logger.debug(
				{
					userId: user.id,
					subscriptionId: user.stripeSubscriptionId,
					stripeIdempotencyKey,
					newTrialEndUnix,
				},
				'Updated Stripe subscription trial end from gift extension',
			);
			await this.cacheService.set(appliedKey, true, seconds('365 days'));
			Logger.debug({userId: user.id, idempotencyKey, appliedKey}, 'Gift trial applied sentinel set');
			Logger.debug(
				{
					userId: user.id,
					subscriptionId: user.stripeSubscriptionId,
					baseDate,
					newTrialEnd,
					durationType,
					durationQuantity,
					idempotencyKey,
				},
				'Extended subscription with gift trial period',
			);
		} catch (error: unknown) {
			Logger.error(
				{error, userId: user.id, subscriptionId: user.stripeSubscriptionId, idempotencyKey},
				'Failed to extend subscription with gift trial',
			);
			const message = error instanceof Error ? error.message : 'Failed to extend subscription with gift';
			throw new StripeError(message);
		} finally {
			if (userLockToken) {
				await this.releaseUserTrialLock(userLockKey, userLockToken, user.id);
			}
			await this.cacheService.delete(inflightKey);
			Logger.debug(
				{
					userId: user.id,
					idempotencyKey,
					inflightKey,
					lockReleased: Boolean(userLockToken),
				},
				'Finished Stripe gift trial extension cleanup',
			);
		}
	}

	private static readonly CURRENT_PRICE_CACHE_TTL_SECONDS = seconds('5 minutes');
	private static readonly LIST_PRICE_CACHE_TTL_SECONDS = seconds('1 hour');
	private static readonly USER_TRIAL_LOCK_TTL_SECONDS = seconds('30 seconds');
	private static readonly USER_TRIAL_LOCK_MAX_WAIT_MS = 15000;
	private static readonly USER_TRIAL_LOCK_RETRY_DELAY_MS = 100;

	private async acquireUserTrialLock(lockKey: string, userId: UserID): Promise<string | null> {
		const startTime = Date.now();
		while (Date.now() - startTime < StripeSubscriptionService.USER_TRIAL_LOCK_MAX_WAIT_MS) {
			const token = await this.cacheService.acquireLock(lockKey, StripeSubscriptionService.USER_TRIAL_LOCK_TTL_SECONDS);
			if (token) {
				return token;
			}
			await new Promise<void>((resolve) => {
				const timeout = setTimeout(resolve, StripeSubscriptionService.USER_TRIAL_LOCK_RETRY_DELAY_MS);
				timeout.unref?.();
			});
		}
		Logger.warn({userId, lockKey}, 'Timed out waiting for user trial extension lock');
		return null;
	}

	private async releaseUserTrialLock(lockKey: string, token: string, userId: UserID): Promise<void> {
		try {
			const released = await this.cacheService.releaseLock(lockKey, token);
			if (!released) {
				Logger.warn({userId, lockKey}, 'User trial extension lock token no longer matched on release');
			}
		} catch (error) {
			Logger.error({error, userId, lockKey}, 'Failed to release user trial extension lock');
		}
	}

	private async dispatchUser(user: User): Promise<void> {
		await this.gatewayService.dispatchPresence({
			userId: user.id,
			event: 'USER_UPDATE',
			data: mapUserToPrivateResponse(user),
		});
	}

	private getBillingCycleFromInterval(interval: string): RecurringBillingCycle | null {
		if (interval === 'month') {
			return 'monthly';
		}
		if (interval === 'year') {
			return 'yearly';
		}
		return null;
	}
}
