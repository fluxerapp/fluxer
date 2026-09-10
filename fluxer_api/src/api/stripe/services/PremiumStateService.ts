// SPDX-License-Identifier: AGPL-3.0-or-later

import {PremiumFlags, UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {
	CurrentSubscriptionPriceResponse,
	ListPriceSwitchState,
	PendingSubscriptionChangeKind,
	PendingSubscriptionChangeResponse,
	PremiumBillingInvoiceResponse,
	PremiumBillingPaymentMethodResponse,
	PremiumBillingSubscriptionResponse,
	PremiumPricingState,
	PremiumStateResponse,
	PriceIdsResponse,
	PricingMode,
	SelfServeRefundEligibilityResponse,
	SelfServeRefundIneligibilityReason,
} from '@fluxer/schema/src/domains/premium/PremiumSchemas';
import type Stripe from 'stripe';
import type {UserID} from '../../BrandedTypes';
import type {BillingRepository} from '../../billing/repositories/BillingRepository';
import {Config} from '../../Config';
import type {
	BillingInvoiceRow,
	BillingPaymentMethodRow,
	BillingRefundRow,
	BillingSubscriptionRow,
} from '../../database/types/BillingTypes';
import type {IGatewayService} from '../../infrastructure/IGatewayService';
import {Logger} from '../../Logger';
import type {User} from '../../models/User';
import type {IUserRepository} from '../../user/IUserRepository';
import {checkHasActivePaidPremium} from '../../user/UserHelpers';
import {mapUserToPrivateResponse} from '../../user/UserMappers';
import {
	type Currency,
	getBaseCurrencyPreferences,
	getBaseGiftCurrencyPreferences,
	getCurrencyPreferences,
	getGiftCurrencyPreferences,
} from '../../utils/CurrencyUtils';
import type {RecurringBillingCycle} from '../ProductRegistry';
import {ProductRegistry} from '../ProductRegistry';
import {getPrimarySubscriptionItem} from '../StripeSubscriptionPeriod';
import {SELF_SERVE_REFUND_COOLDOWN_DAYS, SELF_SERVE_REFUND_WINDOW_DAYS} from './StripeRefundService';

const INVOICE_LIMIT = 12;
const MILLISECONDS_PER_DAY = 24 * 60 * 60 * 1000;
const ACTIVE_SUBSCRIPTION_STATUSES = new Set(['active', 'trialing']);
const RELEVANT_SUBSCRIPTION_STATUSES = new Set(['past_due', 'unpaid', 'incomplete']);
const REFUND_BLOCKING_STATUSES = new Set(['pending', 'succeeded', 'requires_action']);

interface ResolvedPriceIds {
	monthly: string | null;
	yearly: string | null;
	gift_1_month: string | null;
	gift_1_year: string | null;
	currency: Currency;
	gift_currency: Currency;
}

interface InvoiceResult {
	rows: Array<BillingInvoiceRow>;
	allRows: Array<BillingInvoiceRow>;
	hasMore: boolean;
}

interface StripePriceDetails {
	priceId: string | null;
	amountMinor: number | null;
	currency: Currency | null;
	billingCycle: RecurringBillingCycle | null;
}

function toIso(value: Date | null | undefined): string | null {
	return value?.toISOString() ?? null;
}

function toNumber(value: bigint | number | null | undefined): number {
	if (value == null) return 0;
	return Number(value);
}

function nullableNumber(value: bigint | number | null | undefined): number | null {
	if (value == null) return null;
	return Number(value);
}

function positiveIntegerFromMetadata(value: string | null | undefined): number | null {
	if (value == null) return null;
	const parsed = Number(value);
	if (!Number.isSafeInteger(parsed) || parsed <= 0) return null;
	return parsed;
}

function normalizeBillingCycle(value: string | null | undefined): RecurringBillingCycle | null {
	if (value === 'monthly' || value === 'yearly') return value;
	return null;
}

function billingCycleFromInterval(value: string | null | undefined): RecurringBillingCycle | null {
	if (value === 'month') return 'monthly';
	if (value === 'year') return 'yearly';
	return null;
}

function normalizeCurrency(value: string | null | undefined): Currency | null {
	const currency = value?.toUpperCase();
	if (
		currency === 'USD' ||
		currency === 'EUR' ||
		currency === 'BRL' ||
		currency === 'INR' ||
		currency === 'PLN' ||
		currency === 'TRY'
	) {
		return currency;
	}
	return null;
}

function compareNullableDatesDesc(left: Date | null | undefined, right: Date | null | undefined): number {
	return (right?.getTime() ?? 0) - (left?.getTime() ?? 0);
}

function subscriptionRank(row: BillingSubscriptionRow): number {
	if (row.status && ACTIVE_SUBSCRIPTION_STATUSES.has(row.status)) return 0;
	if (row.status && RELEVANT_SUBSCRIPTION_STATUSES.has(row.status)) return 1;
	return 2;
}

function sortSubscriptionsByRelevance(rows: Array<BillingSubscriptionRow>): Array<BillingSubscriptionRow> {
	return rows.sort((left, right) => {
		const rankDiff = subscriptionRank(left) - subscriptionRank(right);
		if (rankDiff !== 0) return rankDiff;
		return compareNullableDatesDesc(left.current_period_end, right.current_period_end);
	});
}

function normalizeCountryCode(countryCode: string | null | undefined): string | null {
	const normalized = countryCode?.trim().toUpperCase();
	return normalized && normalized.length === 2 ? normalized : null;
}

function stripeIdOf(
	value:
		| string
		| {
				id?: string | null;
		  }
		| null
		| undefined,
): string | null {
	if (!value) return null;
	if (typeof value === 'string') return value;
	return typeof value.id === 'string' ? value.id : null;
}

function refundEligibility({
	reason,
	invoice,
	cooldownExpiresAt,
}: {
	reason: SelfServeRefundIneligibilityReason | null;
	invoice?: BillingInvoiceRow | null;
	cooldownExpiresAt?: Date | null;
}): SelfServeRefundEligibilityResponse {
	const paidAt = invoice?.paid_at ?? invoice?.stripe_created_at ?? null;
	const windowExpiresAt = paidAt
		? new Date(paidAt.getTime() + SELF_SERVE_REFUND_WINDOW_DAYS * MILLISECONDS_PER_DAY)
		: null;
	return {
		eligible: reason === null && invoice != null,
		reason,
		invoice_id: invoice?.provider_id ?? null,
		invoice_amount_paid_cents: invoice ? nullableNumber(invoice.amount_paid) : null,
		currency: invoice?.currency ?? null,
		paid_at: toIso(paidAt),
		refund_window_expires_at: toIso(windowExpiresAt),
		cooldown_expires_at: toIso(cooldownExpiresAt),
		cancels_subscription: invoice?.subscription_id != null,
	};
}

function mapInvoice(row: BillingInvoiceRow): PremiumBillingInvoiceResponse {
	return {
		id: row.provider_id,
		number: row.number ?? null,
		amount_due: toNumber(row.amount_due),
		amount_paid: toNumber(row.amount_paid),
		currency: row.currency ?? 'usd',
		status: row.status ?? null,
		created_at: toIso(row.stripe_created_at),
		paid_at: toIso(row.paid_at),
		billing_reason: row.billing_reason ?? null,
		subscription_id: row.subscription_id ?? null,
		hosted_invoice_url: row.hosted_invoice_url ?? null,
		invoice_pdf: row.invoice_pdf ?? null,
	};
}

function mapPaymentMethod(row: BillingPaymentMethodRow): PremiumBillingPaymentMethodResponse {
	return {
		id: row.provider_id,
		type: row.type ?? null,
		card_brand: row.card_brand ?? null,
		card_last4: row.card_last4 ?? null,
		card_exp_month: row.card_exp_month ?? null,
		card_exp_year: row.card_exp_year ?? null,
		is_default: row.is_default ?? false,
	};
}

export class PremiumStateService {
	private readonly productRegistry = new ProductRegistry();

	constructor(
		private readonly userRepository: IUserRepository,
		private readonly gatewayService: IGatewayService,
		private readonly billingRepository: BillingRepository,
		private readonly stripe: Stripe | null = null,
	) {}

	async getState(userId: UserID, countryCode?: string): Promise<PremiumStateResponse> {
		const user = await this.userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		return this.buildState(user, countryCode);
	}

	async setPerksDisabled(userId: UserID, disabled: boolean): Promise<PremiumStateResponse> {
		const user = await this.userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		const nextFlags = disabled
			? user.premiumFlags | PremiumFlags.PERKS_DISABLED
			: user.premiumFlags & ~PremiumFlags.PERKS_DISABLED;
		const updatedUser =
			nextFlags === user.premiumFlags
				? user
				: await this.userRepository.patchUpsert(user.id, {premium_flags: nextFlags}, user.toRow());
		if (updatedUser !== user) {
			await this.gatewayService.dispatchPresence({
				userId: updatedUser.id,
				event: 'USER_UPDATE',
				data: mapUserToPrivateResponse(updatedUser),
			});
		}
		return this.buildState(updatedUser);
	}

	private async buildState(user: User, countryCode?: string): Promise<PremiumStateResponse> {
		const [customerIds, subscription] = await Promise.all([
			this.resolveCustomerIds(user),
			this.resolvePrimarySubscription(user),
		]);
		await this.repairPaymentMethodsFromStripe(user, customerIds, subscription);
		const [invoices, paymentMethods, subscriptionPrice, pendingSubscriptionChange, pricing] = await Promise.all([
			this.resolveInvoices(customerIds),
			this.resolvePaymentMethods(customerIds),
			this.resolveSubscriptionPrice(subscription, user),
			this.resolvePendingSubscriptionChange(user),
			this.resolvePricing(countryCode),
		]);
		const refundEligibilityState = await this.resolveRefundEligibility(user, invoices.allRows);
		const listPriceSwitch = this.resolveListPriceSwitch(subscription, subscriptionPrice, pendingSubscriptionChange);
		const pendingBillingCycleChange =
			pendingSubscriptionChange?.change_kind === 'billing_cycle' ? pendingSubscriptionChange : null;
		const activePaidPremium = checkHasActivePaidPremium(user);
		const isEffectivePremium = user.isPremium();
		const actualPremiumEndAt = user.effectivePremiumUntil;
		return {
			actual: {
				premium_type: user.premiumType ?? UserPremiumTypes.NONE,
				premium_since: toIso(user.premiumSince),
				premium_until: toIso(actualPremiumEndAt),
				premium_will_cancel: user.premiumWillCancel,
				premium_billing_cycle: normalizeBillingCycle(user.premiumBillingCycle),
				premium_lifetime_sequence: user.premiumLifetimeSequence,
				premium_grace_ends_at: toIso(user.premiumGraceEndsAt),
				has_active_paid_premium: activePaidPremium,
				is_visionary: user.premiumType === UserPremiumTypes.LIFETIME,
				has_ever_purchased: user.hasEverPurchased,
			},
			effective: {
				is_premium: isEffectivePremium,
				premium_type: isEffectivePremium ? (user.premiumType ?? UserPremiumTypes.NONE) : UserPremiumTypes.NONE,
				premium_since: isEffectivePremium ? toIso(user.premiumSince) : null,
				premium_until: toIso(actualPremiumEndAt),
				premium_will_cancel: user.premiumWillCancel,
				premium_billing_cycle: normalizeBillingCycle(user.premiumBillingCycle),
				premium_lifetime_sequence: user.premiumLifetimeSequence,
				premium_grace_ends_at: toIso(user.premiumGraceEndsAt),
				premium_enabled_override: (user.premiumFlags & PremiumFlags.ENABLED_OVERRIDE) !== 0,
				premium_purchase_disabled: (user.premiumFlags & PremiumFlags.PURCHASE_DISABLED) !== 0,
				premium_perks_disabled: (user.premiumFlags & PremiumFlags.PERKS_DISABLED) !== 0,
				self_hosted: Config.instance.selfHosted,
				bot: user.isBot,
			},
			billing: {
				stripe_customer_id: customerIds[0] ?? user.stripeCustomerId ?? null,
				current_subscription_price: subscriptionPrice,
				pending_subscription_change: pendingBillingCycleChange,
				list_price_switch: listPriceSwitch,
				subscription: subscription ? await this.mapSubscription(subscription) : null,
				invoices: invoices.rows.map(mapInvoice),
				invoices_has_more: invoices.hasMore,
				payment_methods: paymentMethods.map(mapPaymentMethod),
				refund_eligibility: refundEligibilityState,
			},
			pricing,
		};
	}

	private async resolveCustomerIds(user: User): Promise<Array<string>> {
		const ids = new Set<string>();
		if (user.stripeCustomerId) {
			ids.add(user.stripeCustomerId);
		}
		const rows = await this.billingRepository.customers.findByUserId(user.id);
		for (const row of rows) {
			if (!row.deleted) ids.add(row.provider_id);
		}
		return [...ids];
	}

	private async resolvePrimarySubscription(user: User): Promise<BillingSubscriptionRow | null> {
		const byId = new Map<string, BillingSubscriptionRow>();
		const add = (row: BillingSubscriptionRow | null) => {
			if (row) byId.set(row.provider_id, row);
		};
		if (user.stripeSubscriptionId) {
			add(await this.billingRepository.subscriptions.findById(user.stripeSubscriptionId));
		}
		for (const row of await this.billingRepository.subscriptions.listByUser(user.id)) {
			add(row);
		}
		const customerIds = await this.resolveCustomerIds(user);
		const customerSubscriptions = await Promise.all(
			customerIds.map((customerId) => this.billingRepository.subscriptions.listByCustomer(customerId)),
		);
		for (const rows of customerSubscriptions) {
			for (const row of rows) {
				add(row);
			}
		}
		return sortSubscriptionsByRelevance([...byId.values()])[0] ?? null;
	}

	private async resolveInvoices(customerIds: Array<string>): Promise<InvoiceResult> {
		const byId = new Map<string, BillingInvoiceRow>();
		let hasMore = false;
		const customerInvoices = await Promise.all(
			customerIds.map((customerId) => this.billingRepository.invoices.listByCustomer(customerId, {pageSize: 100})),
		);
		for (const result of customerInvoices) {
			for (const row of result.rows) {
				byId.set(row.provider_id, row);
			}
			if (result.pageState) {
				hasMore = true;
			}
		}
		const rows = [...byId.values()].sort((left, right) =>
			compareNullableDatesDesc(left.stripe_created_at, right.stripe_created_at),
		);
		return {rows: rows.slice(0, INVOICE_LIMIT), allRows: rows, hasMore: hasMore || rows.length > INVOICE_LIMIT};
	}

	private async resolvePaymentMethods(customerIds: Array<string>): Promise<Array<BillingPaymentMethodRow>> {
		const byId = new Map<string, BillingPaymentMethodRow>();
		const customerPaymentMethods = await Promise.all(
			customerIds.map((customerId) => this.billingRepository.paymentMethods.listByCustomer(customerId)),
		);
		for (const rows of customerPaymentMethods) {
			for (const row of rows) {
				byId.set(row.provider_id, row);
			}
		}
		return [...byId.values()].sort((left, right) => {
			if ((left.is_default ?? false) !== (right.is_default ?? false)) return left.is_default ? -1 : 1;
			return compareNullableDatesDesc(left.stripe_created_at, right.stripe_created_at);
		});
	}

	private async repairPaymentMethodsFromStripe(
		user: User,
		customerIds: Array<string>,
		subscription: BillingSubscriptionRow | null,
	): Promise<void> {
		if (!this.stripe || customerIds.length === 0) return;
		await Promise.all(
			customerIds.map(async (customerId) => {
				try {
					await this.repairCustomerPaymentMethodsFromStripe(user, customerId, subscription);
				} catch (error) {
					Logger.warn(
						{error, userId: user.id.toString(), customerId},
						'Failed to lazily repair Stripe billing payment methods',
					);
				}
			}),
		);
	}

	private async repairCustomerPaymentMethodsFromStripe(
		user: User,
		customerId: string,
		mirroredSubscription: BillingSubscriptionRow | null,
	): Promise<void> {
		if (!this.stripe) return;
		let customer = await this.stripe.customers.retrieve(customerId, {
			expand: ['invoice_settings.default_payment_method'],
		});
		await this.billingRepository.customers.upsertFromStripe(customer, {knownUserId: user.id});
		if ('deleted' in customer && customer.deleted) {
			return;
		}
		let subscription = await this.resolveStripeSubscriptionForPaymentMethodRepair(
			user,
			customerId,
			mirroredSubscription,
		);
		const customerDefaultPaymentMethodId = stripeIdOf(customer.invoice_settings?.default_payment_method);
		const subscriptionDefaultPaymentMethodId = stripeIdOf(subscription?.default_payment_method);
		let defaultPaymentMethodId = customerDefaultPaymentMethodId ?? subscriptionDefaultPaymentMethodId;
		if (
			subscription &&
			customerDefaultPaymentMethodId &&
			customerDefaultPaymentMethodId !== subscriptionDefaultPaymentMethodId
		) {
			subscription = await this.stripe.subscriptions.update(subscription.id, {
				default_payment_method: customerDefaultPaymentMethodId,
				expand: ['default_payment_method'],
			});
			defaultPaymentMethodId = customerDefaultPaymentMethodId;
		} else if (!customerDefaultPaymentMethodId && subscriptionDefaultPaymentMethodId) {
			customer = await this.stripe.customers.update(customerId, {
				invoice_settings: {default_payment_method: subscriptionDefaultPaymentMethodId},
				expand: ['invoice_settings.default_payment_method'],
			});
			defaultPaymentMethodId = subscriptionDefaultPaymentMethodId;
		}
		await this.billingRepository.customers.upsertFromStripe(customer, {knownUserId: user.id});
		if (subscription) {
			await this.billingRepository.subscriptions.upsertFromStripe(subscription, {
				knownUserId: user.id,
				snapshotCapturedAt: new Date(),
			});
		}
		const listedPaymentMethodIds = new Set<string>();
		const paymentMethods = await this.stripe.customers.listPaymentMethods(customerId, {limit: 100});
		for (const paymentMethod of paymentMethods.data) {
			listedPaymentMethodIds.add(paymentMethod.id);
			await this.billingRepository.paymentMethods.upsertFromStripe(paymentMethod, {
				isDefault: paymentMethod.id === defaultPaymentMethodId,
			});
		}
		if (defaultPaymentMethodId && !listedPaymentMethodIds.has(defaultPaymentMethodId)) {
			const paymentMethod = await this.stripe.customers.retrievePaymentMethod(customerId, defaultPaymentMethodId);
			await this.billingRepository.paymentMethods.upsertFromStripe(paymentMethod, {isDefault: true});
		}
	}

	private async resolveStripeSubscriptionForPaymentMethodRepair(
		user: User,
		customerId: string,
		mirroredSubscription: BillingSubscriptionRow | null,
	): Promise<Stripe.Subscription | null> {
		if (!this.stripe) return null;
		const subscriptions = await this.stripe.subscriptions.list({
			customer: customerId,
			status: 'all',
			limit: 100,
			expand: ['data.default_payment_method'],
		});
		const activeSubscriptions = subscriptions.data.filter((subscription) =>
			ACTIVE_SUBSCRIPTION_STATUSES.has(subscription.status),
		);
		return (
			activeSubscriptions.find((subscription) => subscription.id === user.stripeSubscriptionId) ??
			activeSubscriptions.find((subscription) => subscription.id === mirroredSubscription?.provider_id) ??
			activeSubscriptions.sort((left, right) => {
				const leftEnd = getPrimarySubscriptionItem(left)?.current_period_end ?? left.created;
				const rightEnd = getPrimarySubscriptionItem(right)?.current_period_end ?? right.created;
				return rightEnd - leftEnd;
			})[0] ??
			null
		);
	}

	private async resolveSubscriptionPrice(
		subscription: BillingSubscriptionRow | null,
		user: User,
	): Promise<CurrentSubscriptionPriceResponse> {
		if (!subscription) return null;
		const price = subscription.primary_price_id
			? await this.billingRepository.prices.findById(subscription.primary_price_id)
			: null;
		const billingCycle =
			billingCycleFromInterval(price?.interval) ?? normalizeBillingCycle(user.premiumBillingCycle) ?? null;
		if (!billingCycle) return null;
		const currency = (price?.currency ?? subscription.currency ?? 'usd').toUpperCase();
		const listPriceId = this.productRegistry.getRecurringSubscriptionPriceId(billingCycle, currency);
		const listPrice = listPriceId ? await this.billingRepository.prices.findById(listPriceId) : null;
		const itemAmount = subscription.items?.[0]?.[3] ?? null;
		const amountMinor = nullableNumber(price?.unit_amount ?? itemAmount);
		if (amountMinor == null) return null;
		return {
			price_id: price?.provider_id ?? subscription.primary_price_id ?? '',
			amount_minor: amountMinor,
			currency: currency as Currency,
			billing_cycle: billingCycle,
			is_grandfathered: listPriceId != null && listPriceId !== (price?.provider_id ?? subscription.primary_price_id),
			list_price_id: listPriceId,
			list_amount_minor: nullableNumber(listPrice?.unit_amount),
		};
	}

	private async mapSubscription(row: BillingSubscriptionRow): Promise<PremiumBillingSubscriptionResponse> {
		const price = row.primary_price_id ? await this.billingRepository.prices.findById(row.primary_price_id) : null;
		return {
			id: row.provider_id,
			status: row.status ?? null,
			current_period_start: toIso(row.current_period_start),
			current_period_end: toIso(row.current_period_end),
			cancel_at_period_end: row.cancel_at_period_end ?? false,
			cancel_at: toIso(row.cancel_at),
			canceled_at: toIso(row.canceled_at),
			plan_interval: price?.interval ?? null,
			plan_amount_minor: nullableNumber(price?.unit_amount ?? row.items?.[0]?.[3] ?? null),
			plan_currency: price?.currency ?? row.currency ?? null,
			default_payment_method_id: row.default_payment_method ?? null,
		};
	}

	private async resolvePendingSubscriptionChange(user: User): Promise<PendingSubscriptionChangeResponse> {
		if (!this.stripe || !user.stripeSubscriptionId) return null;
		try {
			const subscription = await this.stripe.subscriptions.retrieve(user.stripeSubscriptionId, {
				expand: ['items.data.price', 'schedule'],
			});
			const schedule = await this.loadSubscriptionSchedule(subscription.schedule);
			if (!schedule || (schedule.status !== 'active' && schedule.status !== 'not_started')) {
				return null;
			}
			return this.mapPendingSubscriptionChange(subscription, schedule, normalizeBillingCycle(user.premiumBillingCycle));
		} catch (error) {
			Logger.warn(
				{error, userId: user.id, subscriptionId: user.stripeSubscriptionId},
				'Failed to resolve pending Stripe subscription schedule change',
			);
			return null;
		}
	}

	private async loadSubscriptionSchedule(
		scheduleRef: Stripe.Subscription['schedule'],
	): Promise<Stripe.SubscriptionSchedule | null> {
		if (!scheduleRef) return null;
		if (typeof scheduleRef === 'object') return scheduleRef;
		if (!this.stripe) return null;
		return this.stripe.subscriptionSchedules.retrieve(scheduleRef);
	}

	private async mapPendingSubscriptionChange(
		subscription: Stripe.Subscription,
		schedule: Stripe.SubscriptionSchedule,
		fallbackCurrentBillingCycle: RecurringBillingCycle | null,
	): Promise<PendingSubscriptionChangeResponse> {
		const now = Math.floor(Date.now() / 1000);
		const futurePhase = schedule.phases
			.filter((phase) => typeof phase.start_date === 'number' && phase.start_date > now)
			.sort((left, right) => left.start_date - right.start_date)[0];
		if (!futurePhase) return null;
		const currentItem = getPrimarySubscriptionItem(subscription);
		const currentPriceDetails = await this.resolveStripePriceDetails(currentItem?.price ?? null);
		const currentBillingCycle = currentPriceDetails.billingCycle ?? fallbackCurrentBillingCycle;
		const targetItem = futurePhase.items[0] ?? null;
		const targetPriceDetails = await this.resolveStripePriceDetails(targetItem?.price ?? null);
		const metadataTargetBillingCycle = normalizeBillingCycle(schedule.metadata?.pending_billing_cycle);
		const targetBillingCycle = targetPriceDetails.billingCycle ?? metadataTargetBillingCycle ?? currentBillingCycle;
		if (!targetBillingCycle) {
			return null;
		}
		const changeKind: PendingSubscriptionChangeKind =
			targetBillingCycle === currentBillingCycle ? 'price' : 'billing_cycle';
		if (
			changeKind === 'price' &&
			(targetPriceDetails.priceId == null || targetPriceDetails.priceId === currentPriceDetails.priceId)
		) {
			return null;
		}
		const quantity = targetItem?.quantity ?? currentItem?.quantity ?? 1;
		const recurringAmountMinor =
			targetPriceDetails.amountMinor == null ? null : targetPriceDetails.amountMinor * quantity;
		const addInvoiceItemsTotal = await this.sumPhaseAddInvoiceItems(futurePhase);
		const metadataCreditAmountMinor = positiveIntegerFromMetadata(
			futurePhase.metadata?.first_invoice_credit_amount_minor,
		);
		const firstInvoiceAdjustmentTotal =
			addInvoiceItemsTotal !== 0
				? addInvoiceItemsTotal
				: metadataCreditAmountMinor == null
					? 0
					: -metadataCreditAmountMinor;
		const initialAmountMinor =
			recurringAmountMinor == null ? null : Math.max(0, recurringAmountMinor + firstInvoiceAdjustmentTotal);
		const creditAmountMinor = firstInvoiceAdjustmentTotal < 0 ? -firstInvoiceAdjustmentTotal : null;
		return {
			schedule_id: schedule.id,
			change_kind: changeKind,
			current_billing_cycle: currentBillingCycle,
			target_billing_cycle: targetBillingCycle,
			effective_at: new Date(futurePhase.start_date * 1000).toISOString(),
			current_price_id: currentPriceDetails.priceId,
			target_price_id: targetPriceDetails.priceId,
			target_amount_minor: targetPriceDetails.amountMinor,
			currency: targetPriceDetails.currency,
			initial_amount_minor: initialAmountMinor,
			recurring_amount_minor: recurringAmountMinor,
			credit_amount_minor: creditAmountMinor,
		};
	}

	private resolveListPriceSwitch(
		subscription: BillingSubscriptionRow | null,
		subscriptionPrice: CurrentSubscriptionPriceResponse,
		pendingChange: PendingSubscriptionChangeResponse,
	): ListPriceSwitchState {
		const base = {
			pending: false,
			current_price_id: subscriptionPrice?.price_id ?? null,
			current_amount_minor: subscriptionPrice?.amount_minor ?? null,
			list_price_id: subscriptionPrice?.list_price_id ?? null,
			list_amount_minor: subscriptionPrice?.list_amount_minor ?? null,
			currency: subscriptionPrice?.currency ?? null,
			billing_cycle: subscriptionPrice?.billing_cycle ?? null,
			effective_at: toIso(subscription?.current_period_end),
		};
		if (Config.instance.selfHosted || !Config.stripe.enabled || !Config.stripe.secretKey) {
			return {...base, available: false, reason: 'feature_unavailable'};
		}
		if (!subscription) {
			return {...base, available: false, reason: 'no_active_subscription'};
		}
		if (!subscriptionPrice) {
			return {...base, available: false, reason: 'unsupported_subscription'};
		}
		if (!subscription.status || !ACTIVE_SUBSCRIPTION_STATUSES.has(subscription.status)) {
			return {...base, available: false, reason: 'subscription_not_chargeable'};
		}
		if (subscription.cancel_at != null || subscription.cancel_at_period_end === true) {
			return {...base, available: false, reason: 'subscription_cancelling'};
		}
		if (pendingChange) {
			const targetsListPrice =
				subscriptionPrice.list_price_id != null && pendingChange.target_price_id === subscriptionPrice.list_price_id;
			return {
				...base,
				available: false,
				reason: targetsListPrice ? null : 'conflicting_pending_change',
				pending: targetsListPrice,
				effective_at: pendingChange.effective_at,
			};
		}
		if (subscriptionPrice.list_price_id == null || subscriptionPrice.list_amount_minor == null) {
			return {...base, available: false, reason: 'no_list_price'};
		}
		if (subscriptionPrice.list_price_id === subscriptionPrice.price_id) {
			return {...base, available: false, reason: 'already_on_list_price'};
		}
		if (subscriptionPrice.list_amount_minor >= subscriptionPrice.amount_minor) {
			return {...base, available: false, reason: 'not_a_price_decrease'};
		}
		if (base.effective_at == null) {
			return {...base, available: false, reason: 'missing_period_end'};
		}
		return {...base, available: true, reason: null};
	}

	private async resolveStripePriceDetails(
		priceRef: Stripe.Price | Stripe.DeletedPrice | string | null | undefined,
	): Promise<StripePriceDetails> {
		const priceId = stripeIdOf(priceRef);
		let amountMinor: number | null =
			typeof priceRef === 'object' && priceRef && 'unit_amount' in priceRef ? (priceRef.unit_amount ?? null) : null;
		let currency: Currency | null =
			typeof priceRef === 'object' && priceRef && 'currency' in priceRef ? normalizeCurrency(priceRef.currency) : null;
		let billingCycle: RecurringBillingCycle | null =
			typeof priceRef === 'object' && priceRef && 'recurring' in priceRef
				? billingCycleFromInterval(priceRef.recurring?.interval)
				: null;
		if (priceId && (amountMinor == null || currency == null || billingCycle == null)) {
			const mirroredPrice = await this.billingRepository.prices.findById(priceId);
			amountMinor ??= nullableNumber(mirroredPrice?.unit_amount);
			currency ??= normalizeCurrency(mirroredPrice?.currency);
			billingCycle ??= billingCycleFromInterval(mirroredPrice?.interval);
		}
		if (this.stripe && priceId && (amountMinor == null || currency == null || billingCycle == null)) {
			const livePrice = await this.stripe.prices.retrieve(priceId);
			amountMinor ??= livePrice.unit_amount ?? null;
			currency ??= normalizeCurrency(livePrice.currency);
			billingCycle ??= billingCycleFromInterval(livePrice.recurring?.interval);
		}
		return {priceId, amountMinor, currency, billingCycle};
	}

	private async sumPhaseAddInvoiceItems(phase: Stripe.SubscriptionSchedule.Phase): Promise<number> {
		let total = 0;
		for (const item of phase.add_invoice_items ?? []) {
			const invoiceItem = item as {
				price?: Stripe.Price | Stripe.DeletedPrice | string | null;
				price_data?: {
					unit_amount?: number | null;
				};
				quantity?: number | null;
			};
			const unitAmount =
				typeof invoiceItem.price_data?.unit_amount === 'number' ? invoiceItem.price_data.unit_amount : null;
			const quantity = invoiceItem.quantity ?? 1;
			if (unitAmount != null) {
				total += unitAmount * quantity;
				continue;
			}
			try {
				const priceDetails = await this.resolveStripePriceDetails(invoiceItem.price);
				if (priceDetails.amountMinor != null) {
					total += priceDetails.amountMinor * quantity;
				}
			} catch (error) {
				Logger.warn(
					{error, priceId: stripeIdOf(invoiceItem.price)},
					'Failed to resolve pending subscription schedule invoice item price',
				);
			}
		}
		return total;
	}

	private async resolvePricing(countryCode: string | null | undefined): Promise<PremiumPricingState> {
		const normalizedCountryCode = normalizeCountryCode(countryCode);
		const [localized, base] = await Promise.all([
			this.resolvePriceIds(normalizedCountryCode, 'localized'),
			this.resolvePriceIds(normalizedCountryCode, 'base'),
		]);
		return {
			country_code: normalizedCountryCode,
			localized,
			base,
		};
	}

	private async resolvePriceIds(
		countryCode: string | null,
		pricingMode: PricingMode,
	): Promise<PriceIdsResponse | null> {
		const resolved = this.resolveConfiguredPriceIds(countryCode, pricingMode);
		if (!resolved) return null;
		const [monthlyPrice, yearlyPrice, gift1MonthPrice, gift1YearPrice] = await Promise.all([
			resolved.monthly ? this.billingRepository.prices.findById(resolved.monthly) : null,
			resolved.yearly ? this.billingRepository.prices.findById(resolved.yearly) : null,
			resolved.gift_1_month ? this.billingRepository.prices.findById(resolved.gift_1_month) : null,
			resolved.gift_1_year ? this.billingRepository.prices.findById(resolved.gift_1_year) : null,
		]);
		return {
			...resolved,
			monthly_amount_minor: nullableNumber(monthlyPrice?.unit_amount),
			yearly_amount_minor: nullableNumber(yearlyPrice?.unit_amount),
			gift_1_month_amount_minor: nullableNumber(gift1MonthPrice?.unit_amount),
			gift_1_year_amount_minor: nullableNumber(gift1YearPrice?.unit_amount),
		};
	}

	private resolveConfiguredPriceIds(countryCode: string | null, pricingMode: PricingMode): ResolvedPriceIds | null {
		const recurringCurrencyPreferences =
			pricingMode === 'base' ? getBaseCurrencyPreferences(countryCode) : getCurrencyPreferences(countryCode);
		const giftCurrencyPreferences =
			pricingMode === 'base' ? getBaseGiftCurrencyPreferences(countryCode) : getGiftCurrencyPreferences(countryCode);
		const recurringPrices = this.resolveRecurringPriceIds(recurringCurrencyPreferences);
		const giftPrices = this.resolveGiftPriceIds(giftCurrencyPreferences);
		if (!recurringPrices || !giftPrices) return null;
		return {
			monthly: recurringPrices.monthly,
			yearly: recurringPrices.yearly,
			gift_1_month: giftPrices.gift_1_month,
			gift_1_year: giftPrices.gift_1_year,
			currency: recurringPrices.currency,
			gift_currency: giftPrices.gift_currency,
		};
	}

	private resolveRecurringPriceIds(
		preferredCurrencies: Array<Currency>,
	): Pick<ResolvedPriceIds, 'monthly' | 'yearly' | 'currency'> | null {
		for (const currency of preferredCurrencies) {
			const monthly = this.productRegistry.getRecurringSubscriptionPriceId('monthly', currency);
			const yearly = this.productRegistry.getRecurringSubscriptionPriceId('yearly', currency);
			if (monthly && yearly) {
				return {monthly, yearly, currency};
			}
		}
		return null;
	}

	private resolveGiftPriceIds(
		preferredCurrencies: Array<Currency>,
	): Pick<ResolvedPriceIds, 'gift_1_month' | 'gift_1_year' | 'gift_currency'> | null {
		for (const currency of preferredCurrencies) {
			const gift1Month = this.productRegistry.getGiftPriceId('gift_1_month', currency);
			const gift1Year = this.productRegistry.getGiftPriceId('gift_1_year', currency);
			if (gift1Month && gift1Year) {
				return {gift_1_month: gift1Month, gift_1_year: gift1Year, gift_currency: currency};
			}
		}
		return null;
	}

	private cooldownExpiresAt(user: User): Date | null {
		if (!user.firstRefundAt) return null;
		const expiresAt = new Date(user.firstRefundAt.getTime() + SELF_SERVE_REFUND_COOLDOWN_DAYS * MILLISECONDS_PER_DAY);
		return expiresAt.getTime() > Date.now() ? expiresAt : null;
	}

	private async invoiceHasBlockingRefund(invoice: BillingInvoiceRow): Promise<boolean> {
		const refunds = await this.billingRepository.refunds.listByInvoice(invoice.provider_id);
		return refunds.some(
			(refund: BillingRefundRow) => refund.status == null || REFUND_BLOCKING_STATUSES.has(refund.status),
		);
	}

	private async resolveRefundEligibility(
		user: User,
		invoices: Array<BillingInvoiceRow>,
	): Promise<SelfServeRefundEligibilityResponse> {
		if (Config.instance.selfHosted || !Config.stripe.enabled || !Config.stripe.secretKey) {
			return refundEligibility({reason: 'feature_unavailable'});
		}
		const cooldownExpiresAt = this.cooldownExpiresAt(user);
		const paidInvoices = invoices.filter((invoice) => invoice.status === 'paid' && toNumber(invoice.amount_paid) > 0);
		for (const invoice of paidInvoices) {
			if (await this.invoiceHasBlockingRefund(invoice)) {
				continue;
			}
			const paidAt = invoice.paid_at ?? invoice.stripe_created_at;
			if (!paidAt) {
				continue;
			}
			const windowExpiresAt = paidAt.getTime() + SELF_SERVE_REFUND_WINDOW_DAYS * MILLISECONDS_PER_DAY;
			let reason: SelfServeRefundIneligibilityReason | null = null;
			if (windowExpiresAt <= Date.now()) {
				reason = 'outside_refund_window';
			} else if (cooldownExpiresAt) {
				reason = 'cooldown_active';
			}
			return refundEligibility({reason, invoice, cooldownExpiresAt});
		}
		return refundEligibility({reason: 'no_refundable_purchase', cooldownExpiresAt});
	}
}
