// SPDX-License-Identifier: AGPL-3.0-or-later

import {createStringType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {UserPremiumTypesSchema} from '@fluxer/schema/src/primitives/UserSettingsValidators';
import {z} from 'zod';

export const PricingModeEnum = z.enum(['localized', 'base']);

export type PricingMode = z.infer<typeof PricingModeEnum>;

export const WebhookReceivedResponse = z.object({
	received: z.boolean().describe('Whether the webhook was successfully received'),
});

export type WebhookReceivedResponse = z.infer<typeof WebhookReceivedResponse>;

export const UrlResponse = z.object({
	url: z.string().describe('The URL to redirect to'),
});

export type UrlResponse = z.infer<typeof UrlResponse>;

export const PriceIdsResponse = z.object({
	monthly: z.string().nullish().describe('Stripe price ID for the monthly subscription'),
	yearly: z.string().nullish().describe('Stripe price ID for the yearly subscription'),
	gift_1_month: z.string().nullish().describe('Stripe price ID for the 1 month gift'),
	gift_1_year: z.string().nullish().describe('Stripe price ID for the 1 year gift'),
	monthly_amount_minor: z.number().int().nullish().describe('Monthly price amount in the currency minor unit'),
	yearly_amount_minor: z.number().int().nullish().describe('Yearly price amount in the currency minor unit'),
	gift_1_month_amount_minor: z
		.number()
		.int()
		.nullish()
		.describe('Gift 1 month price amount in the currency minor unit'),
	gift_1_year_amount_minor: z.number().int().nullish().describe('Gift 1 year price amount in the currency minor unit'),
	currency: z.enum(['USD', 'EUR', 'BRL', 'INR', 'PLN', 'TRY']).describe('Currency for the prices'),
	gift_currency: z.enum(['USD', 'EUR', 'BRL', 'INR', 'PLN', 'TRY']).describe('Currency for gift prices'),
});

export type PriceIdsResponse = z.infer<typeof PriceIdsResponse>;

export const PriceIdsQueryRequest = z.object({
	country_code: createStringType(2, 2).optional().describe('Two-letter country code for regional pricing'),
	pricing_mode: PricingModeEnum.optional().describe('Whether to resolve localized or standard USD/EUR pricing'),
});

export type PriceIdsQueryRequest = z.infer<typeof PriceIdsQueryRequest>;

export const LocalizedCardPreapprovalContinueRequest = z.object({
	token: createStringType(1, 256).describe('Continuation token for the localized card preapproval flow'),
});

export type LocalizedCardPreapprovalContinueRequest = z.infer<typeof LocalizedCardPreapprovalContinueRequest>;

const LocalizedCardPreapprovalRejectedReason = z.enum([
	'country_mismatch',
	'missing_customer',
	'missing_payment_method',
	'missing_setup_intent',
	'payment_method_not_card',
	'unknown',
]);
export const LocalizedCardPreapprovalContinueResponse = z.discriminatedUnion('status', [
	z.object({
		status: z.literal('pending').describe('The preapproval result is still being processed'),
	}),
	z.object({
		status: z.literal('ready').describe('The preapproval succeeded and the paid checkout URL is ready'),
		url: z.string().describe('The URL to redirect to'),
	}),
	z.object({
		status: z.literal('rejected').describe('The preapproval failed and the paid checkout should not continue'),
		reason: LocalizedCardPreapprovalRejectedReason.describe('The reason the preapproval was rejected'),
		actual_country: createStringType(2, 2).nullish().describe('The detected card issuing country when available'),
	}),
	z.object({
		status: z.literal('expired').describe('The preapproval token has expired or is unknown'),
	}),
]);

export type LocalizedCardPreapprovalContinueResponse = z.infer<typeof LocalizedCardPreapprovalContinueResponse>;

export const CurrentSubscriptionPriceResponse = z
	.object({
		price_id: z.string().describe('The Stripe price ID the user is currently billed against'),
		amount_minor: z.number().int().describe('The amount the user is actually charged, in the currency minor unit'),
		currency: z.enum(['USD', 'EUR', 'BRL', 'INR', 'PLN', 'TRY']).describe('Currency of the charged amount'),
		billing_cycle: z.enum(['monthly', 'yearly']).describe('The recurring billing cycle of the active subscription'),
		is_grandfathered: z
			.boolean()
			.describe('Whether the user is on a legacy price that no longer matches the current list price'),
		list_amount_minor: z
			.number()
			.int()
			.nullable()
			.describe('The current list price for the same cycle/currency, in the currency minor unit'),
		list_price_id: z.string().nullable().describe('The current list Stripe price ID for the same cycle/currency'),
	})
	.nullable();

export type CurrentSubscriptionPriceResponse = z.infer<typeof CurrentSubscriptionPriceResponse>;

export const PendingSubscriptionChangeResponse = z
	.object({
		schedule_id: z.string().describe('Stripe subscription schedule ID managing the pending change'),
		current_billing_cycle: z.enum(['monthly', 'yearly']).nullable().describe('Current recurring billing cycle'),
		target_billing_cycle: z.enum(['monthly', 'yearly']).describe('Recurring billing cycle that will start later'),
		effective_at: z.string().describe('ISO timestamp when the pending change takes effect'),
		current_price_id: z.string().nullable().describe('Current Stripe price ID, when known'),
		target_price_id: z.string().nullable().describe('Stripe price ID that will be used after the change'),
		currency: z.enum(['USD', 'EUR', 'BRL', 'INR', 'PLN', 'TRY']).nullable().describe('Currency for the pending change'),
		initial_amount_minor: z
			.number()
			.int()
			.nullable()
			.describe('Estimated first invoice amount after one-time credits, in the currency minor unit'),
		recurring_amount_minor: z
			.number()
			.int()
			.nullable()
			.describe('Estimated normal recurring amount after the first invoice, in the currency minor unit'),
		credit_amount_minor: z
			.number()
			.int()
			.nullable()
			.describe('One-time credit applied to the first invoice, in the currency minor unit'),
	})
	.nullable();

export type PendingSubscriptionChangeResponse = z.infer<typeof PendingSubscriptionChangeResponse>;

export const ChangeSubscriptionRequest = z.object({
	billing_cycle: z
		.enum(['monthly', 'yearly'])
		.describe('The recurring billing cycle to switch the active subscription to'),
	effective_at: z.enum(['now', 'period_end']).optional().describe('When the billing cycle change should take effect'),
});

export type ChangeSubscriptionRequest = z.infer<typeof ChangeSubscriptionRequest>;

const PremiumBillingCycle = z.enum(['monthly', 'yearly']).nullable();
const PremiumActualState = z.object({
	premium_type: UserPremiumTypesSchema.nullable().describe('Actual subscription type before local perk disabling'),
	premium_since: z.string().nullable().describe('ISO timestamp when actual premium access first started'),
	premium_until: z
		.string()
		.nullable()
		.describe('ISO timestamp when actual premium access ends, including stacked gift time'),
	premium_will_cancel: z.boolean().describe('Whether the subscription is set to cancel at period end'),
	premium_billing_cycle: PremiumBillingCycle.describe('The actual recurring billing cycle, when known'),
	premium_lifetime_sequence: z.number().int().nullable().describe('Visionary sequence number, when applicable'),
	premium_grace_ends_at: z.string().nullable().describe('ISO timestamp when grace access ends, when applicable'),
	has_active_paid_premium: z
		.boolean()
		.describe('Whether paid premium access is currently active before local disabling'),
	is_visionary: z.boolean().describe('Whether the actual premium entitlement is lifetime Visionary access'),
	has_ever_purchased: z.boolean().describe('Whether the user has ever completed a premium purchase'),
});

const PremiumEffectiveState = z.object({
	is_premium: z.boolean().describe('Whether premium perks are currently effective for product gating'),
	premium_type: UserPremiumTypesSchema.nullable().describe('Effective premium type used by product gates'),
	premium_since: z.string().nullable().describe('Effective premium start timestamp exposed to the client'),
	premium_until: z
		.string()
		.nullable()
		.describe('Effective premium end timestamp exposed to the client, including stacked gift time'),
	premium_will_cancel: z.boolean().describe('Effective cancellation status exposed to the client'),
	premium_billing_cycle: PremiumBillingCycle.describe('Effective recurring billing cycle, when known'),
	premium_lifetime_sequence: z
		.number()
		.int()
		.nullable()
		.describe('Effective Visionary sequence number, when applicable'),
	premium_grace_ends_at: z.string().nullable().describe('Effective grace timestamp exposed to the client'),
	premium_enabled_override: z.boolean().describe('Whether backend premium override is enabled'),
	premium_purchase_disabled: z.boolean().describe('Whether premium purchase is disabled for this account'),
	premium_perks_disabled: z.boolean().describe('Whether the user temporarily disabled premium perks'),
	self_hosted: z.boolean().describe('Whether the instance treats all users as premium because it is self-hosted'),
	bot: z.boolean().describe('Whether the account is a bot account with premium-equivalent service access'),
});

export const PremiumBillingSubscriptionResponse = z.object({
	id: z.string(),
	status: z.string().nullable(),
	current_period_start: z.string().nullable(),
	current_period_end: z.string().nullable(),
	cancel_at_period_end: z.boolean(),
	cancel_at: z.string().nullable(),
	canceled_at: z.string().nullable(),
	plan_interval: z.string().nullable(),
	plan_amount_minor: z.number().int().nullable(),
	plan_currency: z.string().nullable(),
	default_payment_method_id: z.string().nullable(),
});

export type PremiumBillingSubscriptionResponse = z.infer<typeof PremiumBillingSubscriptionResponse>;

export const PremiumBillingInvoiceResponse = z.object({
	id: z.string(),
	number: z.string().nullable(),
	amount_due: z.number().int(),
	amount_paid: z.number().int(),
	currency: z.string(),
	status: z.string().nullable(),
	created_at: z.string().nullable(),
	paid_at: z.string().nullable(),
	billing_reason: z.string().nullable(),
	subscription_id: z.string().nullable(),
	hosted_invoice_url: z.string().nullable(),
	invoice_pdf: z.string().nullable(),
});

export type PremiumBillingInvoiceResponse = z.infer<typeof PremiumBillingInvoiceResponse>;

export const PremiumBillingPaymentMethodResponse = z.object({
	id: z.string(),
	type: z.string().nullable(),
	card_brand: z.string().nullable(),
	card_last4: z.string().nullable(),
	card_exp_month: z.number().int().nullable(),
	card_exp_year: z.number().int().nullable(),
	is_default: z.boolean(),
});

export type PremiumBillingPaymentMethodResponse = z.infer<typeof PremiumBillingPaymentMethodResponse>;

export const SelfServeRefundIneligibilityReason = z.enum([
	'no_refundable_purchase',
	'outside_refund_window',
	'cooldown_active',
	'feature_unavailable',
]);

export type SelfServeRefundIneligibilityReason = z.infer<typeof SelfServeRefundIneligibilityReason>;

export const SelfServeRefundEligibilityResponse = z.object({
	eligible: z
		.boolean()
		.describe('Whether the authenticated user can self-serve refund their latest purchase right now'),
	reason: SelfServeRefundIneligibilityReason.nullable().describe(
		'Why the user is not eligible, when eligible is false',
	),
	invoice_id: z.string().nullable().describe('Latest paid invoice considered for refund eligibility'),
	invoice_amount_paid_cents: z
		.number()
		.int()
		.nullable()
		.describe('Amount paid on the latest invoice in the currency minor unit'),
	currency: z.string().nullable().describe('Currency of the latest paid invoice'),
	paid_at: z.string().nullable().describe('ISO timestamp the latest invoice was paid'),
	refund_window_expires_at: z.string().nullable().describe('ISO timestamp after which the 3-day refund window closes'),
	cooldown_expires_at: z.string().nullable().describe('ISO timestamp the 30-day cooldown ends, if currently active'),
	cancels_subscription: z.boolean().describe('Whether issuing the refund will also cancel the active subscription'),
});

export type SelfServeRefundEligibilityResponse = z.infer<typeof SelfServeRefundEligibilityResponse>;

const PremiumBillingState = z.object({
	stripe_customer_id: z.string().nullable(),
	current_subscription_price: CurrentSubscriptionPriceResponse.nullable(),
	pending_subscription_change: PendingSubscriptionChangeResponse.nullable(),
	subscription: PremiumBillingSubscriptionResponse.nullable(),
	invoices: z.array(PremiumBillingInvoiceResponse),
	invoices_has_more: z.boolean(),
	payment_methods: z.array(PremiumBillingPaymentMethodResponse),
	refund_eligibility: SelfServeRefundEligibilityResponse,
});

export const PremiumPricingState = z.object({
	country_code: createStringType(2, 2).nullable().describe('Country code used to resolve localized prices'),
	localized: PriceIdsResponse.nullable().describe('Localized checkout prices resolved from mirrored billing data'),
	base: PriceIdsResponse.nullable().describe('Standard USD/EUR checkout prices resolved from mirrored billing data'),
});

export type PremiumPricingState = z.infer<typeof PremiumPricingState>;

export const PremiumStateResponse = z.object({
	actual: PremiumActualState,
	effective: PremiumEffectiveState,
	billing: PremiumBillingState,
	pricing: PremiumPricingState,
});

export type PremiumStateResponse = z.infer<typeof PremiumStateResponse>;

export const PremiumStateQueryRequest = z.object({
	country_code: createStringType(2, 2).optional().describe('Two-letter country code for regional pricing'),
});

export type PremiumStateQueryRequest = z.infer<typeof PremiumStateQueryRequest>;

export const UpdatePremiumPerksDisabledRequest = z.object({
	disabled: z.boolean().describe('Whether premium perks should be temporarily disabled'),
});

export type UpdatePremiumPerksDisabledRequest = z.infer<typeof UpdatePremiumPerksDisabledRequest>;

export const SelfServeRefundResponse = z.object({
	invoice_id: z.string(),
	payment_intent_id: z.string().nullable(),
	charge_id: z.string().nullable(),
	refund_id: z.string().nullable(),
	refunded_amount_cents: z.number().int(),
	invoice_amount_paid_cents: z.number().int(),
	currency: z.string(),
	subscription_id: z
		.string()
		.nullable()
		.describe('Subscription that was cancelled along with the refund, when applicable'),
});

export type SelfServeRefundResponse = z.infer<typeof SelfServeRefundResponse>;
