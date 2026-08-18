// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {isEuEeaCountryCode} from '@fluxer/constants/src/EuropeanEconomicArea';
import {FeatureNotAvailableSelfHostedError} from '@fluxer/errors/src/domains/core/FeatureNotAvailableSelfHostedError';
import {NotFoundError} from '@fluxer/errors/src/domains/core/NotFoundError';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import {StripeError} from '@fluxer/errors/src/domains/payment/StripeError';
import {StripeNoActiveSubscriptionError} from '@fluxer/errors/src/domains/payment/StripeNoActiveSubscriptionError';
import {StripePaymentNotAvailableError} from '@fluxer/errors/src/domains/payment/StripePaymentNotAvailableError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import {
	AdminBillingCancelImmediatelyRequest,
	AdminBillingOverviewResponse,
	AdminBillingRefundLatestInvoiceCancelRequest,
	AdminBillingRefundLatestInvoiceCancelResponse,
	AdminBillingRefundRequest,
	AdminInvoiceListResponse,
	AdminPaymentListResponse,
	AdminPaymentMethodListResponse,
	AdminSubscriptionResponse,
} from '@fluxer/schema/src/domains/admin/AdminBillingSchemas';
import type Stripe from 'stripe';
import {createUserID, type UserID} from '../../BrandedTypes';
import type {BillingRepository} from '../../billing/repositories/BillingRepository';
import {Config} from '../../Config';
import type {
	BillingChargeRow,
	BillingCheckoutSessionRow,
	BillingInvoiceRow,
	BillingPaymentIntentRow,
	BillingPaymentMethodRow,
	BillingPaymentRow,
	BillingPriceRow,
	BillingRefundRow,
	BillingSubscriptionRow,
} from '../../database/types/BillingTypes';
import type {ISnowflakeService} from '../../infrastructure/ISnowflakeService';
import {Logger} from '../../Logger';
import {requireAdminACL} from '../../middleware/AdminMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import {getBillingRepository} from '../../middleware/ServiceRegistry';
import type {Payment} from '../../models/Payment';
import type {User} from '../../models/User';
import type {StripeService} from '../../stripe/StripeService';
import type {HonoApp, HonoEnv} from '../../types/HonoEnv';
import type {IUserRepository} from '../../user/IUserRepository';
import {PaymentRepository} from '../../user/repositories/PaymentRepository';
import {Validator} from '../../Validator';
import {AdminRepository} from '../AdminRepository';
import {AdminAuditService} from '../services/AdminAuditService';

function ensureBillingFeatureAvailable(): void {
	if (Config.instance.selfHosted) {
		throw new FeatureNotAvailableSelfHostedError();
	}
}

async function getRequiredUser(userRepository: IUserRepository, userId: UserID): Promise<User> {
	const user = await userRepository.findUnique(userId);
	if (!user) {
		throw new UnknownUserError();
	}
	return user;
}

function buildPaymentNotFoundError(): NotFoundError {
	return new NotFoundError({
		code: APIErrorCodes.NOT_FOUND,
	});
}

interface AdminRefundPaymentIntentLookup {
	findById(paymentIntentId: string): Promise<BillingPaymentIntentRow | null>;
}

interface AdminRefundBillingLookup {
	paymentIntents: AdminRefundPaymentIntentLookup;
}

interface MirrorBillingPaymentRecord {
	charge: BillingChargeRow | null;
	checkoutSession: BillingCheckoutSessionRow | null;
	invoice: BillingInvoiceRow;
	primaryPayment: BillingPaymentRow | null;
	localPayment: Payment | null;
	refunds: Array<BillingRefundRow>;
}

type AdminImmediateCancelRefundPolicy = 'full_refund' | 'prorated_refund' | 'cancel_only';
type AdminImmediateCancelRefundPolicyBasis = 'support_policy' | 'eu_eea_withdrawal_no_waiver';

interface AdminImmediateCancelRefundTarget {
	amountPaidCents: number;
	chargeId: string | null;
	currency: string;
	invoiceId: string;
	invoiceCreatedAt: Date;
	paymentCompletedAt: Date | null;
	paymentIntentId: string | null;
	paidAt: Date | null;
	purchaseGeoipCountryCode: string | null;
	purchaseClientCountryCode: string | null;
	euWithdrawalWaiverRequired: boolean;
	euWithdrawalWaiverAccepted: boolean;
	euWithdrawalWaiverAcceptedAt: Date | null;
	euWithdrawalWaiverTextVersion: string | null;
	stripeBillingCountryCode: string | null;
	stripeCustomerCountryCode: string | null;
	stripePaymentMethodCountryCode: string | null;
	stripeTermsOfServiceAccepted: boolean | null;
}

interface AdminImmediateCancelRefundDecision {
	amountCents: number | null;
	basis: AdminImmediateCancelRefundPolicyBasis;
	cycleElapsedDays: number;
	policy: AdminImmediateCancelRefundPolicy;
}

const MILLISECONDS_PER_DAY = 24 * 60 * 60 * 1000;
const EU_EEA_WITHDRAWAL_REFUND_WINDOW_DAYS = 14;

function getElapsedDays(from: Date, now: Date): number {
	return Math.max(0, Math.floor((now.getTime() - from.getTime()) / MILLISECONDS_PER_DAY));
}

function normalizeCountryCode(value: string | null | undefined): string | null {
	const normalized = value?.trim().toUpperCase();
	return normalized && /^[A-Z]{2}$/.test(normalized) ? normalized : null;
}

function parseStripeMetadataBoolean(value: string | null | undefined): boolean | null {
	if (value === 'true') return true;
	if (value === 'false') return false;
	return null;
}

function getCheckoutSessionAcceptedAt(session: BillingCheckoutSessionRow | null): Date | null {
	const acceptedAt = session?.metadata?.get('eu_withdrawal_waiver_accepted_at');
	if (!acceptedAt) return null;
	const date = new Date(acceptedAt);
	return Number.isNaN(date.getTime()) ? null : date;
}

function getCheckoutSessionMetadata(session: BillingCheckoutSessionRow | null): Map<string, string> {
	return session?.metadata ?? new Map();
}

function getMetadataString(map: Map<string, string>, key: string): string | null {
	return map.get(key) ?? null;
}

function isManageableSubscriptionRow(row: BillingSubscriptionRow | null | undefined): row is BillingSubscriptionRow {
	return Boolean(row && row.status !== 'canceled' && row.status !== 'incomplete_expired');
}

function getSubscriptionStatusPriority(status: BillingSubscriptionRow['status']): number {
	switch (status) {
		case 'active':
			return 0;
		case 'trialing':
			return 1;
		case 'past_due':
			return 2;
		case 'unpaid':
			return 3;
		case 'paused':
			return 4;
		case 'incomplete':
			return 5;
		case 'canceled':
			return 6;
		case 'incomplete_expired':
			return 7;
		default:
			return 8;
	}
}

function getSubscriptionSortTimestamp(sub: BillingSubscriptionRow): number {
	const ts = sub.current_period_end ?? sub.canceled_at ?? sub.cancel_at ?? sub.started_at ?? sub.stripe_created_at;
	return ts ? ts.getTime() : 0;
}

function sortSubscriptionsByRelevance(subs: Array<BillingSubscriptionRow>): Array<BillingSubscriptionRow> {
	return [...subs].sort((left, right) => {
		const statusDiff = getSubscriptionStatusPriority(left.status) - getSubscriptionStatusPriority(right.status);
		if (statusDiff !== 0) return statusDiff;
		return getSubscriptionSortTimestamp(right) - getSubscriptionSortTimestamp(left);
	});
}

function getRefundAmount(refunds: Array<BillingRefundRow>): number {
	return refunds
		.filter((refund) => refund.status !== 'failed' && refund.status !== 'canceled')
		.reduce((total, refund) => total + Number(refund.amount ?? 0n), 0);
}

function buildPaymentStatus(
	invoice: BillingInvoiceRow,
	payment: BillingPaymentRow | null,
	refunds: Array<BillingRefundRow>,
): string {
	const refundedAmount = getRefundAmount(refunds);
	const amountPaid = Number(invoice.amount_paid ?? 0n);
	if (amountPaid > 0 && refundedAmount >= amountPaid) {
		return 'refunded';
	}
	if (refundedAmount > 0) {
		return 'partially_refunded';
	}
	return payment?.status ?? invoice.status ?? 'unknown';
}

function decideImmediateCancelRefund(params: {
	amountPaidCents: number;
	now: Date;
	refundTarget: AdminImmediateCancelRefundTarget | null;
	subscription: BillingSubscriptionRow;
}): AdminImmediateCancelRefundDecision {
	const startMs = params.subscription.current_period_start?.getTime() ?? null;
	const endMs = params.subscription.current_period_end?.getTime() ?? null;
	const cycle = startMs && endMs && endMs > startMs ? {start: startMs, end: endMs} : null;
	const cycleElapsedDays = cycle ? getElapsedDays(new Date(cycle.start), params.now) : 0;
	const purchaseDate =
		params.refundTarget?.paymentCompletedAt ??
		params.refundTarget?.paidAt ??
		params.refundTarget?.invoiceCreatedAt ??
		null;
	if (
		params.refundTarget?.euWithdrawalWaiverRequired &&
		!params.refundTarget.euWithdrawalWaiverAccepted &&
		purchaseDate &&
		getElapsedDays(purchaseDate, params.now) <= EU_EEA_WITHDRAWAL_REFUND_WINDOW_DAYS
	) {
		return {
			amountCents: params.amountPaidCents,
			basis: 'eu_eea_withdrawal_no_waiver',
			cycleElapsedDays,
			policy: 'full_refund',
		};
	}
	if (!cycle) {
		return {
			amountCents: params.amountPaidCents,
			basis: 'support_policy',
			cycleElapsedDays,
			policy: 'full_refund',
		};
	}
	const nowMs = params.now.getTime();
	const elapsedDays = cycleElapsedDays;
	if (elapsedDays <= 4) {
		return {
			amountCents: params.amountPaidCents,
			basis: 'support_policy',
			cycleElapsedDays: elapsedDays,
			policy: 'full_refund',
		};
	}
	if (elapsedDays <= 18) {
		const totalMs = Math.max(1, cycle.end - cycle.start);
		const remainingMs = Math.max(0, cycle.end - nowMs);
		const proratedAmount = Math.max(
			1,
			Math.min(params.amountPaidCents, Math.ceil(params.amountPaidCents * (remainingMs / totalMs))),
		);
		return {
			amountCents: proratedAmount,
			basis: 'support_policy',
			cycleElapsedDays: elapsedDays,
			policy: 'prorated_refund',
		};
	}
	return {
		amountCents: null,
		basis: 'support_policy',
		cycleElapsedDays: elapsedDays,
		policy: 'cancel_only',
	};
}

async function assertOwnedPaymentIntentForAdminRefund(
	userRepository: Pick<IUserRepository, 'getPaymentByPaymentIntent'>,
	billingRepository: AdminRefundBillingLookup,
	targetUser: Pick<User, 'id' | 'stripeCustomerId'>,
	paymentIntentId: string,
): Promise<void> {
	const payment = await userRepository.getPaymentByPaymentIntent(paymentIntentId);
	if (payment) {
		if (payment.userId === targetUser.id) {
			return;
		}
		throw buildPaymentNotFoundError();
	}
	if (!targetUser.stripeCustomerId) {
		throw buildPaymentNotFoundError();
	}
	const mirroredIntent = await billingRepository.paymentIntents.findById(paymentIntentId);
	if (mirroredIntent && mirroredIntent.customer_id === targetUser.stripeCustomerId) {
		return;
	}
	throw buildPaymentNotFoundError();
}

class BillingAdminControllerService {
	private readonly paymentRepository = new PaymentRepository();

	constructor(
		private readonly userRepository: IUserRepository,
		private readonly stripeService: StripeService | null,
		private readonly auditService: AdminAuditService,
		private readonly billingRepository: BillingRepository,
	) {}

	private get stripe(): Stripe | null {
		return this.stripeService?.getStripe() ?? null;
	}

	private async resolveCustomerIds(user: User): Promise<Array<string>> {
		const ids = new Set<string>();
		if (user.stripeCustomerId) {
			ids.add(user.stripeCustomerId);
		}
		const mirroredCustomers = await this.billingRepository.customers.findByUserId(user.id);
		for (const c of mirroredCustomers) {
			if (!c.deleted) ids.add(c.provider_id);
		}
		return [...ids];
	}

	async getResolvedStripeCustomerId(user: User): Promise<string | null> {
		const subscription = await this.resolvePrimaryStripeSubscription(user);
		if (subscription?.customer_id) return subscription.customer_id;
		const ids = await this.resolveCustomerIds(user);
		return ids[0] ?? user.stripeCustomerId ?? null;
	}

	private async resolveStripeSubscriptions(user: User): Promise<Array<BillingSubscriptionRow>> {
		const seen = new Set<string>();
		const out: Array<BillingSubscriptionRow> = [];
		const add = (row: BillingSubscriptionRow | null) => {
			if (row && !seen.has(row.provider_id)) {
				seen.add(row.provider_id);
				out.push(row);
			}
		};
		if (user.stripeSubscriptionId) {
			add(await this.billingRepository.subscriptions.findById(user.stripeSubscriptionId));
		}
		const userSubs = await this.billingRepository.subscriptions.listByUser(user.id);
		for (const ref of userSubs) {
			if (seen.has(ref.provider_id)) continue;
			add(await this.billingRepository.subscriptions.findById(ref.provider_id));
		}
		const customerIds = await this.resolveCustomerIds(user);
		for (const customerId of customerIds) {
			const refs = await this.billingRepository.subscriptions.listByCustomer(customerId);
			for (const ref of refs) {
				if (seen.has(ref.provider_id)) continue;
				add(await this.billingRepository.subscriptions.findById(ref.provider_id));
			}
		}
		return sortSubscriptionsByRelevance(out);
	}

	private async resolvePrimaryStripeSubscription(user: User): Promise<BillingSubscriptionRow | null> {
		return (await this.resolveStripeSubscriptions(user))[0] ?? null;
	}

	private async resolveManageableStripeSubscription(user: User): Promise<BillingSubscriptionRow | null> {
		return (await this.resolveStripeSubscriptions(user)).find(isManageableSubscriptionRow) ?? null;
	}

	private async syncResolvedStripeState(
		user: User,
		params: {
			customerId?: string | null;
			subscriptionId?: string | null;
			premiumWillCancel?: boolean;
		},
	): Promise<User> {
		const patch: {
			premium_will_cancel?: boolean;
			stripe_customer_id?: string | null;
			stripe_subscription_id?: string | null;
		} = {};
		if (params.customerId !== undefined && params.customerId !== user.stripeCustomerId) {
			patch.stripe_customer_id = params.customerId;
		}
		if (params.subscriptionId !== undefined && params.subscriptionId !== user.stripeSubscriptionId) {
			patch.stripe_subscription_id = params.subscriptionId;
		}
		if (params.premiumWillCancel !== undefined && params.premiumWillCancel !== user.premiumWillCancel) {
			patch.premium_will_cancel = params.premiumWillCancel;
		}
		if (Object.keys(patch).length === 0) {
			return user;
		}
		return this.userRepository.patchUpsert(user.id, patch, user.toRow());
	}

	async getUserPayments(user: User): Promise<Array<Payment>> {
		const payments = await this.paymentRepository.findPaymentsByUserId(user.id);
		return payments.sort((left, right) => right.createdAt.getTime() - left.createdAt.getTime());
	}

	async getUserSubscription(user: User): Promise<BillingSubscriptionRow | null> {
		return this.resolvePrimaryStripeSubscription(user);
	}

	async getUserPaymentMethods(user: User): Promise<Array<BillingPaymentMethodRow>> {
		const customerIds = await this.resolveCustomerIds(user);
		if (customerIds.length === 0) return [];
		const byId = new Map<string, BillingPaymentMethodRow>();
		for (const customerId of customerIds) {
			const rows = await this.billingRepository.paymentMethods.listByCustomer(customerId);
			for (const row of rows) {
				byId.set(row.provider_id, row);
			}
		}
		return [...byId.values()].sort((left, right) => {
			const lt = left.stripe_created_at?.getTime() ?? 0;
			const rt = right.stripe_created_at?.getTime() ?? 0;
			return rt - lt;
		});
	}

	async getUserInvoices(
		user: User,
		limit: number = 25,
	): Promise<{
		invoices: Array<BillingInvoiceRow>;
		has_more: boolean;
	}> {
		const customerIds = await this.resolveCustomerIds(user);
		if (customerIds.length === 0) return {invoices: [], has_more: false};
		const aggregated = new Map<string, BillingInvoiceRow>();
		let hasMore = false;
		for (const customerId of customerIds) {
			const result = await this.billingRepository.invoices.listByCustomer(customerId, {pageSize: 100});
			for (const inv of result.rows) {
				aggregated.set(inv.provider_id, inv);
			}
			if (result.pageState) hasMore = true;
		}
		const merged = [...aggregated.values()].sort((left, right) => {
			const lt = left.stripe_created_at?.getTime() ?? 0;
			const rt = right.stripe_created_at?.getTime() ?? 0;
			return rt - lt;
		});
		return {
			invoices: merged.slice(0, limit),
			has_more: hasMore || merged.length > limit,
		};
	}

	private async getRefundsForInvoiceAndCharge(params: {
		invoiceId: string;
		chargeId: string | null;
		paymentIntentId: string | null;
	}): Promise<Array<BillingRefundRow>> {
		const byId = new Map<string, BillingRefundRow>();
		const add = (rows: Array<BillingRefundRow>) => {
			for (const row of rows) byId.set(row.provider_id, row);
		};
		add(await this.billingRepository.refunds.listByInvoice(params.invoiceId));
		if (params.chargeId) {
			add(await this.billingRepository.refunds.listByCharge(params.chargeId));
		}
		if (params.paymentIntentId) {
			add(await this.billingRepository.refunds.listByPaymentIntent(params.paymentIntentId));
		}
		return [...byId.values()];
	}

	private async getCheckoutSessionForInvoice(params: {
		paymentIntentId: string | null;
		subscriptionId: string | null;
		customerIds: Array<string>;
	}): Promise<BillingCheckoutSessionRow | null> {
		for (const customerId of params.customerIds) {
			const result = await this.billingRepository.checkoutSessions.listByCustomer(customerId, {pageSize: 100});
			for (const ref of result.rows) {
				const session = await this.billingRepository.checkoutSessions.findById(ref.provider_id);
				if (!session) continue;
				if (params.paymentIntentId && session.payment_intent_id === params.paymentIntentId) {
					return session;
				}
				if (params.subscriptionId && session.subscription_id === params.subscriptionId) {
					return session;
				}
			}
		}
		return null;
	}

	async getMirrorBillingPaymentRecords(user: User, limit: number = 25): Promise<Array<MirrorBillingPaymentRecord>> {
		const {invoices} = await this.getUserInvoices(user, limit);
		const customerIds = await this.resolveCustomerIds(user);
		const localPayments = await this.getUserPayments(user);
		const localPaymentsByInvoiceId = new Map<string, Payment>();
		for (const lp of localPayments) {
			if (lp.invoiceId && !localPaymentsByInvoiceId.has(lp.invoiceId)) {
				localPaymentsByInvoiceId.set(lp.invoiceId, lp);
			}
		}
		const records = await Promise.all(
			invoices.map(async (invoice): Promise<MirrorBillingPaymentRecord> => {
				const primaryPayment = await this.billingRepository.payments.findPrimaryForInvoice(invoice.provider_id);
				const chargeId = primaryPayment?.charge_id ?? null;
				const paymentIntentId = primaryPayment?.payment_intent_id ?? null;
				const charge = chargeId ? await this.billingRepository.charges.findById(chargeId) : null;
				const refunds = await this.getRefundsForInvoiceAndCharge({
					invoiceId: invoice.provider_id,
					chargeId,
					paymentIntentId,
				});
				const checkoutSession = await this.getCheckoutSessionForInvoice({
					paymentIntentId,
					subscriptionId: invoice.subscription_id,
					customerIds,
				});
				return {
					charge,
					checkoutSession,
					invoice,
					primaryPayment,
					localPayment: invoice.provider_id ? (localPaymentsByInvoiceId.get(invoice.provider_id) ?? null) : null,
					refunds,
				};
			}),
		);
		return records.filter((record) => {
			const amountPaid = Number(record.invoice.amount_paid ?? 0n);
			return amountPaid > 0 || record.refunds.length > 0 || record.primaryPayment;
		});
	}

	private buildRefundTargetFromMirror(
		invoice: BillingInvoiceRow,
		primaryPayment: BillingPaymentRow | null,
		charge: BillingChargeRow | null,
		checkoutSession: BillingCheckoutSessionRow | null,
		expectedCustomerId: string | null,
		localPayment: Payment | null,
	): AdminImmediateCancelRefundTarget | null {
		if (!invoice.provider_id || invoice.status !== 'paid') return null;
		const amountPaid = Number(invoice.amount_paid ?? 0n);
		if (amountPaid <= 0) return null;
		if (expectedCustomerId && invoice.customer_id && invoice.customer_id !== expectedCustomerId) return null;
		if (!primaryPayment?.payment_intent_id && !primaryPayment?.charge_id) return null;
		const sessionMetadata = getCheckoutSessionMetadata(checkoutSession);
		const stripePaymentMethodCountryCode = normalizeCountryCode(charge?.card_country ?? null);
		const stripeBillingCountryCode: string | null = null;
		const stripeCustomerCountryCode: string | null = null;
		const stripeTermsOfServiceAccepted: boolean | null = null;
		const metadataWaiverRequired = parseStripeMetadataBoolean(
			getMetadataString(sessionMetadata, 'eu_withdrawal_waiver_required'),
		);
		const metadataWaiverAccepted = parseStripeMetadataBoolean(
			getMetadataString(sessionMetadata, 'eu_withdrawal_waiver_accepted'),
		);
		const stripeCountryRequiresWaiver = [
			stripeBillingCountryCode,
			stripeCustomerCountryCode,
			stripePaymentMethodCountryCode,
		].some((countryCode) => isEuEeaCountryCode(countryCode));
		const euWithdrawalWaiverRequired =
			(localPayment?.euWithdrawalWaiverRequired ?? false) ||
			metadataWaiverRequired === true ||
			stripeCountryRequiresWaiver;
		const euWithdrawalWaiverAccepted =
			(localPayment?.euWithdrawalWaiverAccepted ?? false) || metadataWaiverAccepted === true;
		return {
			amountPaidCents: amountPaid,
			chargeId: primaryPayment.charge_id ?? null,
			currency: invoice.currency ?? 'usd',
			invoiceId: invoice.provider_id,
			invoiceCreatedAt: invoice.stripe_created_at ?? new Date(0),
			paymentCompletedAt: localPayment?.completedAt ?? null,
			paymentIntentId: primaryPayment.payment_intent_id ?? null,
			paidAt: primaryPayment.paid_at ?? invoice.paid_at ?? null,
			purchaseGeoipCountryCode:
				localPayment?.purchaseGeoipCountryCode ??
				normalizeCountryCode(getMetadataString(sessionMetadata, 'purchase_geoip_country_code')),
			purchaseClientCountryCode:
				localPayment?.purchaseClientCountryCode ??
				normalizeCountryCode(getMetadataString(sessionMetadata, 'purchase_client_country_code')),
			euWithdrawalWaiverRequired,
			euWithdrawalWaiverAccepted,
			euWithdrawalWaiverAcceptedAt:
				localPayment?.euWithdrawalWaiverAcceptedAt ?? getCheckoutSessionAcceptedAt(checkoutSession),
			euWithdrawalWaiverTextVersion:
				localPayment?.euWithdrawalWaiverTextVersion ??
				getMetadataString(sessionMetadata, 'eu_withdrawal_waiver_text_version'),
			stripeBillingCountryCode,
			stripeCustomerCountryCode,
			stripePaymentMethodCountryCode,
			stripeTermsOfServiceAccepted,
		};
	}

	async issueRefund(params: {
		adminUserId: UserID;
		targetUser: User;
		paymentIntentId: string;
		amountCents?: number;
		reason?: string;
	}): Promise<void> {
		if (!this.stripe) {
			throw new StripePaymentNotAvailableError();
		}
		const customerId = await this.getResolvedStripeCustomerId(params.targetUser);
		const syncedTargetUser = customerId
			? await this.syncResolvedStripeState(params.targetUser, {customerId})
			: params.targetUser;
		await assertOwnedPaymentIntentForAdminRefund(
			this.userRepository,
			this.billingRepository,
			syncedTargetUser,
			params.paymentIntentId,
		);
		try {
			const refund = await this.stripe.refunds.create({
				payment_intent: params.paymentIntentId,
				...(params.amountCents !== undefined ? {amount: params.amountCents} : {}),
				...(params.reason ? {reason: 'requested_by_customer' as const} : {}),
				metadata: {
					admin_user_id: params.adminUserId.toString(),
					target_user_id: syncedTargetUser.id.toString(),
					...(params.reason ? {admin_reason: params.reason} : {}),
				},
			});
			try {
				await this.billingRepository.refunds.upsertFromStripe(refund, {
					customerId: syncedTargetUser.stripeCustomerId ?? undefined,
					userId: syncedTargetUser.id,
				});
			} catch (mirrorErr) {
				Logger.error({mirrorErr, refundId: refund.id}, 'Mirror upsert failed after admin refund; reconciler will heal');
			}
		} catch (error) {
			throw new StripeError(error instanceof Error ? error.message : 'Failed to refund payment');
		}
		const metadata = new Map<string, string>([['payment_intent_id', params.paymentIntentId]]);
		if (params.amountCents !== undefined) {
			metadata.set('amount_cents', String(params.amountCents));
		}
		if (params.reason) {
			metadata.set('reason', params.reason);
		}
		await this.auditService.createAuditLog({
			adminUserId: params.adminUserId,
			targetType: 'user',
			targetId: BigInt(syncedTargetUser.id),
			action: 'billing_refund',
			auditLogReason: params.reason ?? null,
			metadata,
		});
	}

	async resolveLatestRefundableInvoiceForImmediateCancel(
		user: User,
		subscription: BillingSubscriptionRow,
	): Promise<AdminImmediateCancelRefundTarget | null> {
		const localPayments = await this.getUserPayments(user);
		const localPaymentsByInvoiceId = new Map<string, Payment>();
		for (const payment of localPayments) {
			if (payment.invoiceId && !localPaymentsByInvoiceId.has(payment.invoiceId)) {
				localPaymentsByInvoiceId.set(payment.invoiceId, payment);
			}
		}
		const expectedCustomerId = user.stripeCustomerId ?? subscription.customer_id ?? null;
		const customerIds = await this.resolveCustomerIds(user);
		const tryInvoice = async (invoice: BillingInvoiceRow): Promise<AdminImmediateCancelRefundTarget | null> => {
			const primaryPayment = await this.billingRepository.payments.findPrimaryForInvoice(invoice.provider_id);
			const chargeId = primaryPayment?.charge_id ?? null;
			const paymentIntentId = primaryPayment?.payment_intent_id ?? null;
			const charge = chargeId ? await this.billingRepository.charges.findById(chargeId) : null;
			const checkoutSession = await this.getCheckoutSessionForInvoice({
				paymentIntentId,
				subscriptionId: invoice.subscription_id,
				customerIds,
			});
			return this.buildRefundTargetFromMirror(
				invoice,
				primaryPayment,
				charge,
				checkoutSession,
				expectedCustomerId,
				localPaymentsByInvoiceId.get(invoice.provider_id) ?? null,
			);
		};
		if (subscription.latest_invoice_id) {
			const invoice = await this.billingRepository.invoices.findById(subscription.latest_invoice_id);
			if (invoice) {
				const target = await tryInvoice(invoice);
				if (target) return target;
			}
		}
		const subInvoices = await this.billingRepository.invoices.listBySubscription(subscription.provider_id, {
			pageSize: 50,
		});
		for (const invoice of subInvoices.rows) {
			const target = await tryInvoice(invoice);
			if (target) return target;
		}
		for (const lp of localPayments) {
			if (!lp.invoiceId) continue;
			const invoice = await this.billingRepository.invoices.findById(lp.invoiceId);
			if (!invoice) continue;
			const target = await tryInvoice(invoice);
			if (target) return target;
		}
		return null;
	}

	async applyRefundPolicyAndCancelImmediately(params: {
		adminUserId: UserID;
		targetUser: User;
		reason?: string;
	}): Promise<AdminBillingRefundLatestInvoiceCancelResponse> {
		if (!this.stripe || !this.stripeService) {
			throw new StripePaymentNotAvailableError();
		}
		const subscription = await this.resolveManageableStripeSubscription(params.targetUser);
		if (!subscription) {
			throw new StripeNoActiveSubscriptionError();
		}
		const syncedTargetUser = await this.syncResolvedStripeState(params.targetUser, {
			customerId: subscription.customer_id ?? undefined,
			subscriptionId: subscription.provider_id,
			premiumWillCancel: subscription.cancel_at_period_end ?? undefined,
		});
		const refundTarget = await this.resolveLatestRefundableInvoiceForImmediateCancel(syncedTargetUser, subscription);
		const refundDecision = decideImmediateCancelRefund({
			amountPaidCents: refundTarget?.amountPaidCents ?? 0,
			now: new Date(),
			refundTarget,
			subscription,
		});
		if (refundDecision.amountCents !== null && !refundTarget) {
			throw new StripeError('No paid Stripe invoice with a refundable payment was found for this subscription');
		}
		const intentId = await this.billingRepository.actionIntents.create({
			userId: BigInt(syncedTargetUser.id),
			actorAdminId: BigInt(params.adminUserId),
			actionType: 'cancel_and_refund',
			subscriptionId: subscription.provider_id,
			invoiceId: refundTarget?.invoiceId ?? null,
			paymentIntentId: refundTarget?.paymentIntentId ?? null,
			refundAmount: refundDecision.amountCents !== null ? BigInt(refundDecision.amountCents) : null,
			refundReason: params.reason ?? null,
		});
		let refund: Stripe.Response<Stripe.Refund> | null = null;
		try {
			await this.stripeService.cancelSubscriptionImmediately(syncedTargetUser.id, params.reason);
			await this.billingRepository.actionIntents.markStage(intentId, 'sub_canceled', {
				sub_canceled_at: new Date(),
			});
			if (refundDecision.amountCents !== null && refundTarget) {
				refund = await this.stripe.refunds.create(
					{
						...(refundTarget.paymentIntentId
							? {payment_intent: refundTarget.paymentIntentId}
							: {charge: refundTarget.chargeId!}),
						amount: refundDecision.amountCents,
						...(params.reason ? {reason: 'requested_by_customer' as const} : {}),
						metadata: {
							intent_id: String(intentId),
							admin_user_id: params.adminUserId.toString(),
							target_user_id: syncedTargetUser.id.toString(),
							subscription_id: subscription.provider_id,
							invoice_id: refundTarget.invoiceId,
							refund_policy: refundDecision.policy,
							refund_policy_basis: refundDecision.basis,
							eu_withdrawal_waiver_required: refundTarget.euWithdrawalWaiverRequired ? 'true' : 'false',
							eu_withdrawal_waiver_accepted: refundTarget.euWithdrawalWaiverAccepted ? 'true' : 'false',
							...(refundTarget.purchaseGeoipCountryCode
								? {purchase_geoip_country_code: refundTarget.purchaseGeoipCountryCode}
								: {}),
							...(refundTarget.purchaseClientCountryCode
								? {purchase_client_country_code: refundTarget.purchaseClientCountryCode}
								: {}),
							...(refundTarget.euWithdrawalWaiverTextVersion
								? {eu_withdrawal_waiver_text_version: refundTarget.euWithdrawalWaiverTextVersion}
								: {}),
							...(refundTarget.stripeBillingCountryCode
								? {stripe_billing_country_code: refundTarget.stripeBillingCountryCode}
								: {}),
							...(refundTarget.stripePaymentMethodCountryCode
								? {stripe_payment_method_country_code: refundTarget.stripePaymentMethodCountryCode}
								: {}),
							...(refundTarget.stripeCustomerCountryCode
								? {stripe_customer_country_code: refundTarget.stripeCustomerCountryCode}
								: {}),
							...(refundTarget.stripeTermsOfServiceAccepted !== null
								? {stripe_terms_of_service_accepted: refundTarget.stripeTermsOfServiceAccepted ? 'true' : 'false'}
								: {}),
							...(params.reason ? {admin_reason: params.reason} : {}),
						},
					},
					{idempotencyKey: `admin-cancel-refund:${intentId}`},
				);
				try {
					await this.billingRepository.refunds.upsertFromStripe(refund, {
						invoiceId: refundTarget.invoiceId,
						customerId: subscription.customer_id ?? undefined,
						userId: BigInt(syncedTargetUser.id),
					});
				} catch (mirrorErr) {
					Logger.error(
						{mirrorErr, refundId: refund.id},
						'Mirror upsert failed after refund create; reconciler will heal',
					);
				}
				await this.billingRepository.actionIntents.markStage(intentId, 'refund_created', {
					refund_created_at: new Date(),
					refund_id: refund.id,
				});
			}
			await this.billingRepository.actionIntents.markStage(intentId, 'complete', {
				completed_at: new Date(),
			});
		} catch (err) {
			const message = err instanceof Error ? err.message : String(err);
			try {
				await this.billingRepository.actionIntents.markStage(intentId, 'failed', {
					error_message: message,
				});
			} catch (markErr) {
				Logger.error({markErr, intentId: String(intentId)}, 'Failed to mark action intent as failed');
			}
			throw err;
		}
		const auditMetadata = new Map<string, string>([
			['subscription_id', subscription.provider_id],
			['refund_policy', refundDecision.policy],
			['refund_policy_basis', refundDecision.basis],
			['cycle_elapsed_days', String(refundDecision.cycleElapsedDays)],
			['intent_id', String(intentId)],
		]);
		if (refundTarget) {
			auditMetadata.set('invoice_id', refundTarget.invoiceId);
			auditMetadata.set('invoice_amount_paid_cents', String(refundTarget.amountPaidCents));
			if (refundTarget.paymentIntentId) auditMetadata.set('payment_intent_id', refundTarget.paymentIntentId);
			if (refundTarget.chargeId) auditMetadata.set('charge_id', refundTarget.chargeId);
		}
		if (refund) {
			auditMetadata.set('refund_id', refund.id);
			auditMetadata.set('refunded_amount_cents', String(refund.amount));
		}
		if (params.reason) {
			auditMetadata.set('reason', params.reason);
		}
		await this.auditService.createAuditLog({
			adminUserId: params.adminUserId,
			targetType: 'user',
			targetId: BigInt(syncedTargetUser.id),
			action: 'billing_refund_policy_cancel_now',
			auditLogReason: params.reason ?? null,
			metadata: auditMetadata,
		});
		return {
			subscription_id: subscription.provider_id,
			invoice_id: refundTarget?.invoiceId ?? '',
			payment_intent_id: refundTarget?.paymentIntentId ?? null,
			charge_id: refundTarget?.chargeId ?? null,
			refund_policy: refundDecision.policy,
			refund_policy_basis: refundDecision.basis,
			refund_id: refund?.id ?? null,
			refunded_amount_cents: refund?.amount ?? 0,
			invoice_amount_paid_cents: refundTarget?.amountPaidCents ?? 0,
			currency: refundTarget?.currency ?? subscription.currency ?? 'usd',
			cycle_elapsed_days: refundDecision.cycleElapsedDays,
			purchase_geoip_country_code: refundTarget?.purchaseGeoipCountryCode ?? null,
			purchase_client_country_code: refundTarget?.purchaseClientCountryCode ?? null,
			stripe_payment_method_country_code: refundTarget?.stripePaymentMethodCountryCode ?? null,
			stripe_billing_country_code: refundTarget?.stripeBillingCountryCode ?? null,
			stripe_customer_country_code: refundTarget?.stripeCustomerCountryCode ?? null,
			stripe_terms_of_service_accepted: refundTarget?.stripeTermsOfServiceAccepted ?? null,
			eu_withdrawal_waiver_required: refundTarget?.euWithdrawalWaiverRequired ?? false,
			eu_withdrawal_waiver_accepted: refundTarget?.euWithdrawalWaiverAccepted ?? false,
			eu_withdrawal_waiver_accepted_at: refundTarget?.euWithdrawalWaiverAcceptedAt?.toISOString() ?? null,
			eu_withdrawal_waiver_text_version: refundTarget?.euWithdrawalWaiverTextVersion ?? null,
		};
	}

	private async ensureManageableSubscription(targetUser: User): Promise<BillingSubscriptionRow> {
		const sub = await this.resolveManageableStripeSubscription(targetUser);
		if (!sub) {
			throw new StripeNoActiveSubscriptionError();
		}
		await this.syncResolvedStripeState(targetUser, {
			customerId: sub.customer_id ?? undefined,
			subscriptionId: sub.provider_id,
			premiumWillCancel: sub.cancel_at_period_end ?? undefined,
		});
		return sub;
	}

	async cancelSubscription(adminUserId: UserID, targetUserId: UserID, auditLogReason: string | null): Promise<void> {
		if (!this.stripeService) {
			throw new StripePaymentNotAvailableError();
		}
		const targetUser = await getRequiredUser(this.userRepository, targetUserId);
		await this.ensureManageableSubscription(targetUser);
		await this.stripeService.cancelSubscriptionAtPeriodEnd(targetUserId);
		await this.auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(targetUserId),
			action: 'billing_cancel_subscription',
			auditLogReason,
		});
	}

	async cancelSubscriptionImmediately(
		adminUserId: UserID,
		targetUserId: UserID,
		auditLogReason: string | null,
	): Promise<void> {
		if (!this.stripeService) {
			throw new StripePaymentNotAvailableError();
		}
		const targetUser = await getRequiredUser(this.userRepository, targetUserId);
		await this.ensureManageableSubscription(targetUser);
		await this.stripeService.cancelSubscriptionImmediately(targetUserId, auditLogReason ?? undefined);
		await this.auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(targetUserId),
			action: 'billing_cancel_subscription_now',
			auditLogReason,
		});
	}

	async endPremiumGracePeriod(adminUserId: UserID, targetUserId: UserID, auditLogReason: string | null): Promise<void> {
		if (!this.stripeService) {
			throw new StripePaymentNotAvailableError();
		}
		const targetUser = await getRequiredUser(this.userRepository, targetUserId);
		const wasInGrace = await this.stripeService.endPremiumGracePeriod(targetUserId);
		const auditMetadata = new Map<string, string>();
		auditMetadata.set('was_in_grace', String(wasInGrace));
		if (targetUser.premiumGraceEndsAt) {
			auditMetadata.set('prior_grace_ends_at', targetUser.premiumGraceEndsAt.toISOString());
		}
		await this.auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(targetUserId),
			action: 'billing_end_premium_grace_period',
			auditLogReason,
			metadata: auditMetadata,
		});
	}

	async reactivateSubscription(
		adminUserId: UserID,
		targetUserId: UserID,
		auditLogReason: string | null,
	): Promise<void> {
		if (!this.stripeService) {
			throw new StripePaymentNotAvailableError();
		}
		const targetUser = await getRequiredUser(this.userRepository, targetUserId);
		await this.ensureManageableSubscription(targetUser);
		await this.stripeService.reactivateSubscription(targetUserId);
		await this.auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(targetUserId),
			action: 'billing_reactivate_subscription',
			auditLogReason,
		});
	}

	async getPriceForInvoice(invoice: BillingInvoiceRow): Promise<BillingPriceRow | null> {
		if (!invoice.subscription_id) return null;
		const sub = await this.billingRepository.subscriptions.findById(invoice.subscription_id);
		if (!sub?.primary_price_id) return null;
		return this.billingRepository.prices.findById(sub.primary_price_id);
	}
}

function createBillingService(ctx: {get: <K extends keyof HonoEnv['Variables']>(key: K) => HonoEnv['Variables'][K]}): {
	userRepository: IUserRepository;
	service: BillingAdminControllerService;
} {
	const userRepository = ctx.get('userRepository');
	const stripeService = (ctx.get('stripeService') as StripeService | undefined) ?? null;
	const snowflakeService = ctx.get('snowflakeService') as ISnowflakeService;
	const auditService = new AdminAuditService(new AdminRepository(), snowflakeService);
	const billingRepository = getBillingRepository();
	return {
		userRepository,
		service: new BillingAdminControllerService(userRepository, stripeService, auditService, billingRepository),
	};
}

function mapMirrorPaymentRecordToResponse(user: User, record: MirrorBillingPaymentRecord) {
	const checkoutSessionMetadata = getCheckoutSessionMetadata(record.checkoutSession);
	const localPayment = record.localPayment;
	const resolvedPaymentIntentId = record.primaryPayment?.payment_intent_id ?? null;
	const refundedAmountCents = getRefundAmount(record.refunds);
	const stripePaymentMethodCountryCode = normalizeCountryCode(record.charge?.card_country ?? null);
	const stripeBillingCountryCode: string | null = null;
	const stripeCustomerCountryCode: string | null = null;
	const stripeTermsOfServiceAccepted: boolean | null = null;
	const metadataWaiverRequired = parseStripeMetadataBoolean(
		getMetadataString(checkoutSessionMetadata, 'eu_withdrawal_waiver_required'),
	);
	const metadataWaiverAccepted = parseStripeMetadataBoolean(
		getMetadataString(checkoutSessionMetadata, 'eu_withdrawal_waiver_accepted'),
	);
	const stripeCountryRequiresWaiver = [
		stripeBillingCountryCode,
		stripeCustomerCountryCode,
		stripePaymentMethodCountryCode,
	].some((countryCode) => isEuEeaCountryCode(countryCode));
	const euWithdrawalWaiverRequired =
		(localPayment?.euWithdrawalWaiverRequired ?? false) ||
		metadataWaiverRequired === true ||
		stripeCountryRequiresWaiver;
	const euWithdrawalWaiverAccepted =
		(localPayment?.euWithdrawalWaiverAccepted ?? false) || metadataWaiverAccepted === true;
	const amountPaid = Number(record.invoice.amount_paid ?? 0n);
	return {
		checkout_session_id: record.checkoutSession?.provider_id ?? null,
		user_id: user.id.toString(),
		stripe_customer_id: record.invoice.customer_id ?? null,
		payment_intent_id: resolvedPaymentIntentId,
		resolved_payment_intent_id: resolvedPaymentIntentId,
		charge_id: record.primaryPayment?.charge_id ?? null,
		subscription_id: record.invoice.subscription_id ?? null,
		invoice_id: record.invoice.provider_id,
		price_id: null,
		product_type: getMetadataString(checkoutSessionMetadata, 'product_type') ?? record.invoice.billing_reason ?? null,
		amount_cents: amountPaid,
		currency: record.invoice.currency ?? 'usd',
		status: buildPaymentStatus(record.invoice, record.primaryPayment, record.refunds),
		stripe_source: 'invoice' as const,
		refundable_via_payment_intent: resolvedPaymentIntentId !== null && refundedAmountCents < amountPaid,
		refunded_amount_cents: refundedAmountCents,
		net_amount_cents: Math.max(0, amountPaid - refundedAmountCents),
		refunds: record.refunds.map((refund) => ({
			id: refund.provider_id,
			amount_cents: Number(refund.amount ?? 0n),
			currency: refund.currency ?? 'usd',
			status: refund.status ?? null,
			reason: refund.reason ?? null,
			created: refund.stripe_created_at ? Math.floor(refund.stripe_created_at.getTime() / 1000) : 0,
			payment_intent_id: refund.payment_intent_id ?? null,
			charge_id: refund.charge_id ?? null,
		})),
		payment_method_type: record.charge?.payment_method_type ?? null,
		payment_method_brand: record.charge?.card_brand ?? null,
		payment_method_last4: record.charge?.card_last4 ?? null,
		stripe_payment_method_country_code: stripePaymentMethodCountryCode,
		stripe_billing_country_code: stripeBillingCountryCode,
		stripe_customer_country_code: stripeCustomerCountryCode,
		stripe_terms_of_service_accepted: stripeTermsOfServiceAccepted,
		is_gift: getMetadataString(checkoutSessionMetadata, 'is_gift') === 'true',
		gift_code: getMetadataString(checkoutSessionMetadata, 'gift_code'),
		purchase_geoip_country_code: normalizeCountryCode(
			getMetadataString(checkoutSessionMetadata, 'purchase_geoip_country_code'),
		),
		purchase_client_country_code: normalizeCountryCode(
			getMetadataString(checkoutSessionMetadata, 'purchase_client_country_code'),
		),
		eu_withdrawal_waiver_required: euWithdrawalWaiverRequired,
		eu_withdrawal_waiver_accepted: euWithdrawalWaiverAccepted,
		eu_withdrawal_waiver_accepted_at: getCheckoutSessionAcceptedAt(record.checkoutSession)?.toISOString() ?? null,
		eu_withdrawal_waiver_text_version: getMetadataString(checkoutSessionMetadata, 'eu_withdrawal_waiver_text_version'),
		created_at: (record.invoice.stripe_created_at ?? new Date(0)).toISOString(),
		completed_at:
			record.primaryPayment?.paid_at?.toISOString() ??
			(record.invoice.status === 'paid'
				? ((record.invoice.paid_at ?? record.invoice.stripe_created_at ?? null)?.toISOString() ?? null)
				: null),
	};
}

function mapInvoiceRowToResponse(invoice: BillingInvoiceRow, primaryPayment: BillingPaymentRow | null) {
	return {
		id: invoice.provider_id,
		amount_due: Number(invoice.amount_due ?? 0n),
		amount_paid: Number(invoice.amount_paid ?? 0n),
		currency: invoice.currency ?? 'usd',
		status: invoice.status ?? null,
		created: invoice.stripe_created_at ? Math.floor(invoice.stripe_created_at.getTime() / 1000) : 0,
		billing_reason: invoice.billing_reason ?? null,
		subscription_id: invoice.subscription_id ?? null,
		payment_type: null,
		payment_status: primaryPayment?.status ?? null,
		payment_intent_id: primaryPayment?.payment_intent_id ?? null,
		charge_id: primaryPayment?.charge_id ?? null,
		paid_at: (primaryPayment?.paid_at ?? invoice.paid_at)?.toISOString() ?? null,
		hosted_invoice_url: invoice.hosted_invoice_url ?? null,
		invoice_pdf: invoice.invoice_pdf ?? null,
	};
}

function mapSubscriptionRowToResponse(sub: BillingSubscriptionRow, primaryPrice: BillingPriceRow | null) {
	return {
		id: sub.provider_id,
		status: sub.status ?? 'unknown',
		current_period_start: sub.current_period_start?.toISOString() ?? null,
		current_period_end: sub.current_period_end?.toISOString() ?? null,
		cancel_at_period_end: sub.cancel_at_period_end ?? false,
		cancel_at: sub.cancel_at?.toISOString() ?? null,
		canceled_at: sub.canceled_at?.toISOString() ?? null,
		plan_interval: primaryPrice?.interval ?? null,
		plan_amount_cents:
			primaryPrice?.unit_amount !== null && primaryPrice?.unit_amount !== undefined
				? Number(primaryPrice.unit_amount)
				: null,
		plan_currency: primaryPrice?.currency ?? sub.currency ?? null,
		default_payment_method_id: sub.default_payment_method ?? null,
	};
}

function mapPaymentMethodRowToResponse(pm: BillingPaymentMethodRow) {
	return {
		id: pm.provider_id,
		type: pm.type ?? 'card',
		card_brand: pm.card_brand ?? null,
		card_last4: pm.card_last4 ?? null,
		card_exp_month: pm.card_exp_month ?? null,
		card_exp_year: pm.card_exp_year ?? null,
		created: pm.stripe_created_at ? Math.floor(pm.stripe_created_at.getTime() / 1000) : 0,
	};
}

function buildEmptySubscriptionResponse() {
	return {
		id: '',
		status: 'none',
		current_period_start: null,
		current_period_end: null,
		cancel_at_period_end: false,
		cancel_at: null,
		canceled_at: null,
		plan_interval: null,
		plan_amount_cents: null,
		plan_currency: null,
		default_payment_method_id: null,
	};
}

function clampInvoiceLimit(rawLimit: string | undefined): number {
	if (!rawLimit) return 25;
	const parsed = parseInt(rawLimit, 10);
	if (!Number.isFinite(parsed)) return 25;
	return Math.max(1, Math.min(parsed, 100));
}

export function BillingAdminController(app: HonoApp) {
	app.get(
		'/admin/billing/users/:userId/overview',
		requireAdminACL(AdminACLs.BILLING_VIEW),
		OpenAPI({
			operationId: 'admin_billing_overview',
			summary: 'Get billing overview for a user',
			description: 'Retrieve subscription status, payment history, and Stripe payment methods for a user.',
			responseSchema: AdminBillingOverviewResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			ensureBillingFeatureAvailable();
			const userId = createUserID(BigInt(ctx.req.param('userId')));
			const {userRepository, service: billingService} = createBillingService(ctx);
			const user = await getRequiredUser(userRepository, userId);
			const [payments, subscription, paymentMethods, stripeCustomerId] = await Promise.all([
				billingService.getMirrorBillingPaymentRecords(user),
				billingService.getUserSubscription(user),
				billingService.getUserPaymentMethods(user),
				billingService.getResolvedStripeCustomerId(user),
			]);
			const subscriptionPrice = subscription
				? await getBillingRepository().prices.findById(subscription.primary_price_id ?? '')
				: null;
			return ctx.json({
				subscription: subscription ? mapSubscriptionRowToResponse(subscription, subscriptionPrice) : null,
				payments: payments.map((payment) => mapMirrorPaymentRecordToResponse(user, payment)),
				payment_methods: paymentMethods.map(mapPaymentMethodRowToResponse),
				stripe_customer_id: stripeCustomerId,
			});
		},
	);
	app.get(
		'/admin/billing/guilds/:guildId/overview',
		requireAdminACL(AdminACLs.BILLING_VIEW),
		OpenAPI({
			operationId: 'admin_billing_guild_overview',
			summary: 'Get billing overview for a guild',
			description:
				'Retrieve guild billing state. The current billing mirror is user-scoped, so guilds without persisted billing records return an empty overview.',
			responseSchema: AdminBillingOverviewResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			ensureBillingFeatureAvailable();
			const guildId = ctx.req.param('guildId');
			const adminService = ctx.get('adminService');
			const lookup = await adminService.guildServiceAggregate.lookupService.lookupGuild({guild_id: BigInt(guildId)});
			if (!lookup.guild) {
				throw new UnknownGuildError();
			}
			return ctx.json({
				subscription: null,
				payments: [],
				payment_methods: [],
				stripe_customer_id: null,
			});
		},
	);
	app.get(
		'/admin/billing/users/:userId/payments',
		requireAdminACL(AdminACLs.BILLING_VIEW),
		OpenAPI({
			operationId: 'admin_billing_list_payments',
			summary: 'List payments for a user',
			description: 'Retrieve the payment history stored for a user.',
			responseSchema: AdminPaymentListResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			ensureBillingFeatureAvailable();
			const userId = createUserID(BigInt(ctx.req.param('userId')));
			const {userRepository, service: billingService} = createBillingService(ctx);
			const user = await getRequiredUser(userRepository, userId);
			const payments = await billingService.getMirrorBillingPaymentRecords(user);
			return ctx.json({
				payments: payments.map((payment) => mapMirrorPaymentRecordToResponse(user, payment)),
			});
		},
	);
	app.get(
		'/admin/billing/users/:userId/subscription',
		requireAdminACL(AdminACLs.BILLING_VIEW),
		OpenAPI({
			operationId: 'admin_billing_get_subscription',
			summary: 'Get subscription for a user',
			description: 'Retrieve the current Stripe subscription details for a user.',
			responseSchema: AdminSubscriptionResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			ensureBillingFeatureAvailable();
			const userId = createUserID(BigInt(ctx.req.param('userId')));
			const {userRepository, service: billingService} = createBillingService(ctx);
			const user = await getRequiredUser(userRepository, userId);
			const subscription = await billingService.getUserSubscription(user);
			const subscriptionPrice = subscription?.primary_price_id
				? await getBillingRepository().prices.findById(subscription.primary_price_id)
				: null;
			return ctx.json(
				subscription ? mapSubscriptionRowToResponse(subscription, subscriptionPrice) : buildEmptySubscriptionResponse(),
			);
		},
	);
	app.get(
		'/admin/billing/users/:userId/payment-methods',
		requireAdminACL(AdminACLs.BILLING_VIEW),
		OpenAPI({
			operationId: 'admin_billing_list_payment_methods',
			summary: 'List payment methods for a user',
			description: 'Retrieve the Stripe payment methods associated with a user.',
			responseSchema: AdminPaymentMethodListResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			ensureBillingFeatureAvailable();
			const userId = createUserID(BigInt(ctx.req.param('userId')));
			const {userRepository, service: billingService} = createBillingService(ctx);
			const user = await getRequiredUser(userRepository, userId);
			const paymentMethods = await billingService.getUserPaymentMethods(user);
			return ctx.json({
				payment_methods: paymentMethods.map(mapPaymentMethodRowToResponse),
			});
		},
	);
	app.get(
		'/admin/billing/users/:userId/invoices',
		requireAdminACL(AdminACLs.BILLING_VIEW),
		OpenAPI({
			operationId: 'admin_billing_list_invoices',
			summary: 'List invoices for a user',
			description: 'Retrieve recent Stripe invoices for a user.',
			responseSchema: AdminInvoiceListResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			ensureBillingFeatureAvailable();
			const userId = createUserID(BigInt(ctx.req.param('userId')));
			const {userRepository, service: billingService} = createBillingService(ctx);
			const user = await getRequiredUser(userRepository, userId);
			const limit = clampInvoiceLimit(ctx.req.query('limit') ?? undefined);
			const result = await billingService.getUserInvoices(user, limit);
			const billingRepo = getBillingRepository();
			const invoiceResponses = await Promise.all(
				result.invoices.map(async (invoice) => {
					const primaryPayment = await billingRepo.payments.findPrimaryForInvoice(invoice.provider_id);
					return mapInvoiceRowToResponse(invoice, primaryPayment);
				}),
			);
			return ctx.json({
				invoices: invoiceResponses,
				has_more: result.has_more,
			});
		},
	);
	app.post(
		'/admin/billing/users/:userId/refund',
		requireAdminACL(AdminACLs.BILLING_REFUND),
		Validator('json', AdminBillingRefundRequest),
		OpenAPI({
			operationId: 'admin_billing_refund',
			summary: 'Issue a refund for a user payment',
			description: 'Issue a full or partial refund for a user payment through Stripe.',
			responseSchema: null,
			statusCode: 204,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			ensureBillingFeatureAvailable();
			const userId = createUserID(BigInt(ctx.req.param('userId')));
			const {userRepository, service: billingService} = createBillingService(ctx);
			const user = await getRequiredUser(userRepository, userId);
			const {payment_intent_id, amount_cents, reason} = ctx.req.valid('json');
			await billingService.issueRefund({
				adminUserId: ctx.get('adminUserId'),
				targetUser: user,
				paymentIntentId: payment_intent_id,
				amountCents: amount_cents,
				reason: reason ?? undefined,
			});
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/admin/billing/users/:userId/refund-policy-cancel-now',
		requireAdminACL(AdminACLs.BILLING_REFUND),
		requireAdminACL(AdminACLs.BILLING_MANAGE_SUBSCRIPTION),
		Validator('json', AdminBillingRefundLatestInvoiceCancelRequest),
		OpenAPI({
			operationId: 'admin_billing_refund_policy_cancel_now',
			summary: 'Apply refund policy and cancel subscription immediately',
			description:
				'Cancels a user subscription immediately and applies the support refund policy against the latest paid Stripe invoice.',
			responseSchema: AdminBillingRefundLatestInvoiceCancelResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			ensureBillingFeatureAvailable();
			const userId = createUserID(BigInt(ctx.req.param('userId')));
			const {userRepository, service: billingService} = createBillingService(ctx);
			const user = await getRequiredUser(userRepository, userId);
			const {reason} = ctx.req.valid('json');
			const result = await billingService.applyRefundPolicyAndCancelImmediately({
				adminUserId: ctx.get('adminUserId'),
				targetUser: user,
				reason: reason ?? undefined,
			});
			return ctx.json(result);
		},
	);
	app.post(
		'/admin/billing/users/:userId/cancel-subscription-now',
		requireAdminACL(AdminACLs.BILLING_MANAGE_SUBSCRIPTION),
		Validator('json', AdminBillingCancelImmediatelyRequest),
		OpenAPI({
			operationId: 'admin_billing_cancel_subscription_now',
			summary: 'Cancel a user subscription immediately',
			description: 'Cancel a user Stripe subscription immediately without issuing a refund.',
			responseSchema: null,
			statusCode: 204,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			ensureBillingFeatureAvailable();
			const userId = createUserID(BigInt(ctx.req.param('userId')));
			const {service: billingService} = createBillingService(ctx);
			const {reason} = ctx.req.valid('json');
			await billingService.cancelSubscriptionImmediately(ctx.get('adminUserId'), userId, reason ?? null);
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/admin/billing/users/:userId/cancel-subscription',
		requireAdminACL(AdminACLs.BILLING_MANAGE_SUBSCRIPTION),
		OpenAPI({
			operationId: 'admin_billing_cancel_subscription',
			summary: 'Cancel a user subscription',
			description: 'Set a user Stripe subscription to cancel at period end.',
			responseSchema: null,
			statusCode: 204,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			ensureBillingFeatureAvailable();
			const userId = createUserID(BigInt(ctx.req.param('userId')));
			const {service: billingService} = createBillingService(ctx);
			await billingService.cancelSubscription(ctx.get('adminUserId'), userId, ctx.get('auditLogReason'));
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/admin/billing/users/:userId/end-premium-grace-period',
		requireAdminACL(AdminACLs.BILLING_MANAGE_SUBSCRIPTION),
		OpenAPI({
			operationId: 'admin_billing_end_premium_grace_period',
			summary: "End a user's premium grace period",
			description:
				'End the post-cancel premium grace period for a user immediately, downgrading them and clearing premium_since. Idempotent: safe to call when not in grace. Use when investigating fraud or honoring a user request to opt out of the recovery window.',
			responseSchema: null,
			statusCode: 204,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			ensureBillingFeatureAvailable();
			const userId = createUserID(BigInt(ctx.req.param('userId')));
			const {service: billingService} = createBillingService(ctx);
			await billingService.endPremiumGracePeriod(ctx.get('adminUserId'), userId, ctx.get('auditLogReason'));
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/admin/billing/users/:userId/reactivate-subscription',
		requireAdminACL(AdminACLs.BILLING_MANAGE_SUBSCRIPTION),
		OpenAPI({
			operationId: 'admin_billing_reactivate_subscription',
			summary: 'Reactivate a user subscription',
			description: 'Remove a period-end cancellation from a user Stripe subscription.',
			responseSchema: null,
			statusCode: 204,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			ensureBillingFeatureAvailable();
			const userId = createUserID(BigInt(ctx.req.param('userId')));
			const {service: billingService} = createBillingService(ctx);
			await billingService.reactivateSubscription(ctx.get('adminUserId'), userId, ctx.get('auditLogReason'));
			return ctx.body(null, 204);
		},
	);
}
