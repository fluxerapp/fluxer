// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import GeoIP from '@app/features/app/state/GeoIP';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import PremiumState from '@app/features/premium/state/PremiumState';
import Users from '@app/features/user/state/Users';
import type {
	CurrentSubscriptionPriceResponse,
	PremiumStateResponse,
	PriceIdsResponse,
	SelfServeRefundResponse,
	SwitchToListPriceResponse,
} from '@fluxer/schema/src/domains/premium/PremiumSchemas';

const logger = new Logger('Premium');
export type PriceIds = PriceIdsResponse;

interface UrlResponse {
	url: string;
}

interface PremiumPerksDisabledRequest {
	disabled: boolean;
}

function normalizedCountryCode(countryCode?: string): string | undefined {
	return countryCode?.toUpperCase();
}

async function resolvePremiumStateCountryCode(countryCode?: string): Promise<string | undefined> {
	const explicitCountry = normalizedCountryCode(countryCode);
	if (explicitCountry) {
		return explicitCountry;
	}
	await GeoIP.ready();
	return normalizedCountryCode(GeoIP.countryCode ?? undefined);
}

function checkoutEndpoint(isGift: boolean): string {
	return isGift ? Endpoints.STRIPE_CHECKOUT_GIFT : Endpoints.STRIPE_CHECKOUT_SUBSCRIPTION;
}

function checkoutSessionBody(
	priceId: string,
	countryCode: string | undefined,
	isGift: boolean,
	paymentMethod?: CheckoutPaymentMethod,
): Record<string, string> {
	return {
		price_id: priceId,
		...(countryCode ? {country_code: countryCode} : {}),
		...(countryCode ? {client_geoip_country_code: countryCode} : {}),
		...(paymentMethod && !isGift ? {payment_method: paymentMethod} : {}),
	};
}

async function post(endpoint: string, body?: Record<string, string>): Promise<void> {
	await http.post(endpoint, body ? {body} : undefined);
}

export type CurrentSubscriptionPrice = NonNullable<CurrentSubscriptionPriceResponse>;

function applyPremiumStateToCurrentUser(state: PremiumStateResponse): void {
	const user = Users.currentUser;
	if (!user) return;
	const traits = new Set(user.traits);
	if (state.effective.is_premium) {
		traits.add('premium');
	} else {
		traits.delete('premium');
	}
	Users.handleUserUpdate({
		...user.toJSON(),
		premium_type: state.effective.premium_type,
		premium_since: state.effective.premium_since,
		premium_until: state.effective.premium_until,
		premium_will_cancel: state.effective.premium_will_cancel,
		premium_billing_cycle: state.effective.premium_billing_cycle,
		premium_lifetime_sequence: state.effective.premium_lifetime_sequence,
		premium_grace_ends_at: state.effective.premium_grace_ends_at,
		premium_enabled_override: state.effective.premium_enabled_override,
		premium_purchase_disabled: state.effective.premium_purchase_disabled,
		premium_perks_disabled: state.effective.premium_perks_disabled,
		has_ever_purchased: state.actual.has_ever_purchased,
		traits: Array.from(traits).sort(),
	});
}

async function fetchPremiumState(countryCode?: string): Promise<PremiumStateResponse> {
	const country = await resolvePremiumStateCountryCode(countryCode);
	const response = await http.get<PremiumStateResponse>(Endpoints.PREMIUM_STATE, {
		...(country ? {query: {country_code: country}} : {}),
	});
	return response.body;
}

function isAbortError(error: unknown): boolean {
	return (error instanceof DOMException || error instanceof Error) && error.name === 'AbortError';
}

export async function refreshPremiumState(countryCode?: string): Promise<PremiumStateResponse> {
	const currentUserId = Users.currentUser?.id;
	if (currentUserId) {
		PremiumState.beginLoad(currentUserId);
	}
	try {
		const state = await fetchPremiumState(countryCode);
		if (Users.currentUser?.id !== currentUserId) {
			PremiumState.finishLoad();
			return state;
		}
		if (currentUserId) {
			PremiumState.setState(currentUserId, state);
		}
		applyPremiumStateToCurrentUser(state);
		return state;
	} catch (error) {
		PremiumState.finishLoad();
		if (!isAbortError(error)) {
			logger.error('Premium state fetch failed', error);
		}
		throw error;
	}
}

export async function setPremiumPerksDisabled(disabled: boolean): Promise<PremiumStateResponse> {
	const currentUserId = Users.currentUser?.id;
	const response = await http.patch<PremiumStateResponse>(Endpoints.PREMIUM_PERKS_DISABLED, {
		body: {disabled} satisfies PremiumPerksDisabledRequest,
	});
	if (Users.currentUser?.id !== currentUserId) {
		return response.body;
	}
	if (currentUserId) {
		PremiumState.setState(currentUserId, response.body);
	}
	applyPremiumStateToCurrentUser(response.body);
	return response.body;
}

export async function createCustomerPortalSession(): Promise<string> {
	try {
		const response = await http.post<UrlResponse>(Endpoints.PREMIUM_CUSTOMER_PORTAL);
		logger.info('Customer portal session created');
		return response.body.url;
	} catch (error) {
		logger.error('Customer portal session creation failed', error);
		throw error;
	}
}

export type CheckoutPaymentMethod = 'card' | 'pix' | 'upi';

export async function createCheckoutSession(
	priceId: string,
	countryCode?: string,
	isGift: boolean = false,
	paymentMethod?: CheckoutPaymentMethod,
): Promise<string> {
	try {
		const country = normalizedCountryCode(countryCode);
		const response = await http.post<UrlResponse>(checkoutEndpoint(isGift), {
			body: checkoutSessionBody(priceId, country, isGift, paymentMethod),
		});
		logger.info('Checkout session created', {priceId, countryCode, isGift, paymentMethod});
		return response.body.url;
	} catch (error) {
		logger.error('Checkout session creation failed', error);
		throw error;
	}
}

export async function cancelSubscriptionAtPeriodEnd(): Promise<void> {
	try {
		await post(Endpoints.PREMIUM_CANCEL_SUBSCRIPTION);
		logger.info('Subscription set to cancel at period end');
	} catch (error) {
		logger.error('Failed to cancel subscription at period end', error);
		throw error;
	}
}

export async function reactivateSubscription(): Promise<void> {
	try {
		await post(Endpoints.PREMIUM_REACTIVATE_SUBSCRIPTION);
		logger.info('Subscription reactivated');
	} catch (error) {
		logger.error('Failed to reactivate subscription', error);
		throw error;
	}
}

export async function endPremiumGracePeriod(): Promise<void> {
	try {
		await post(Endpoints.PREMIUM_GRACE_END);
		logger.info('Premium grace period ended');
	} catch (error) {
		logger.error('Failed to end premium grace period', error);
		throw error;
	}
}

export type SubscriptionBillingCycleChangeEffectiveAt = 'now' | 'period_end';

export async function changeSubscriptionBillingCycle(
	billingCycle: 'monthly' | 'yearly',
	effectiveAt: SubscriptionBillingCycleChangeEffectiveAt = 'now',
): Promise<void> {
	try {
		await post(Endpoints.PREMIUM_CHANGE_SUBSCRIPTION, {
			billing_cycle: billingCycle,
			effective_at: effectiveAt,
		});
		logger.info('Subscription billing cycle changed', {billingCycle, effectiveAt});
	} catch (error) {
		logger.error('Failed to change subscription billing cycle', error);
		throw error;
	}
}

export async function switchSubscriptionToListPrice(): Promise<SwitchToListPriceResponse> {
	try {
		const response = await http.post<SwitchToListPriceResponse>(Endpoints.PREMIUM_SWITCH_TO_LIST_PRICE);
		logger.info('Subscription list price switch requested', response.body);
		return response.body;
	} catch (error) {
		logger.error('Failed to switch subscription to the current list price', error);
		throw error;
	}
}

export async function cancelPendingSubscriptionChange(): Promise<void> {
	try {
		await post(Endpoints.PREMIUM_CANCEL_PENDING_SUBSCRIPTION_CHANGE);
		logger.info('Pending subscription billing-cycle change canceled');
	} catch (error) {
		logger.error('Failed to cancel pending subscription billing-cycle change', error);
		throw error;
	}
}

export async function refundLatestPurchase(): Promise<SelfServeRefundResponse> {
	try {
		const response = await http.post<SelfServeRefundResponse>(Endpoints.PREMIUM_REFUND_LATEST);
		logger.info('Self-serve refund issued', response.body);
		return response.body;
	} catch (error) {
		logger.error('Failed to issue self-serve refund', error);
		throw error;
	}
}

export async function rejoinVisionaryGuild(): Promise<void> {
	try {
		await http.post(Endpoints.PREMIUM_VISIONARY_REJOIN);
		logger.info('Visionary guild rejoin requested');
	} catch (error) {
		logger.error('Failed to rejoin Visionary guild', error);
		throw error;
	}
}
