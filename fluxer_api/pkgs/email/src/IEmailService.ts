// SPDX-License-Identifier: AGPL-3.0-or-later

import type {EmailAttachment} from '@pkgs/email/src/EmailProviderTypes';
import type {
	BillingPaymentProblem,
	EmailTemplateVariables,
	PlutoniumPlan,
	ReporterOutcome,
	ReportReceivedTargetKind,
	ReportReference,
} from '@pkgs/email/src/email_i18n/EmailI18nTypes';

export interface OperatorPassWaitlistVerificationEmail {
	confirmUrl: string;
}

interface OperatorPassSaleOpenEmail {
	checkoutUrl: string;
	priceFormatted: string;
	priceRisesAt: Date;
	unsubscribeUrl: string;
	unsubscribePageUrl: string;
}

interface OperatorPassWelcomeEmail {
	labsInviteUrl: string;
	invoiceNumber: string | null;
	deliveredAt: Date;
	wallName: string | null;
	passCount: number;
	timeZone?: string | null;
}

interface OperatorPassPurchaseConfirmationEmail {
	invoiceNumber: string | null;
	orderDate: Date;
	quantity: number;
	amountMinor: number;
	currency: string;
	waiverConsentAt: Date | null;
	countryConsent: boolean;
	withdrawalDays: number | null;
	withdrawalEndsOn: string | null;
	wallName: string | null;
	unitAmountMinor: number;
	paymentMethod: string | null;
	consentText: string | null;
	timeZone?: string | null;
}

interface OperatorPassWithdrawalReceiptEmail {
	cancellation: boolean;
	name: string;
	receivedAt: Date;
	invoiceNumber: string | null;
	orderDate: Date;
	amountMinor: number;
	currency: string;
	lastPass: boolean;
	hasDonation: boolean;
	refundViaDispute: boolean;
	timeZone?: string | null;
}

export type OperatorPassOfferEnd = 'withdrawn' | 'declined' | 'cancelled';

interface OperatorPassOfferReceivedEmail {
	senderName: string;
	arrivesAt: Date | null;
	timeZone?: string | null;
}

interface OperatorPassOfferAcceptedEmail {
	recipientName: string;
	remainingCount: number;
}

interface OperatorPassOfferEndedEmail {
	otherName: string;
	ended: OperatorPassOfferEnd;
}

interface OperatorPassRevokedEmail {
	cause: 'refund' | 'dispute' | 'withdrawal';
	lastPass: boolean;
}

interface OperatorPassRemovalNoticeEmail {
	rule: string;
	facts: string;
	replyBy: Date;
}

interface OperatorPassPerksRemovedEmail {
	until: Date | null;
	rule: string;
	facts: string;
	automated: boolean;
}

interface OperatorPassWallHiddenEmail {
	reason: 'guidelines' | 'impersonation' | 'rights' | 'age_suitability';
	note: string;
}

interface OperatorPassWallNameReplacedEmail {
	target: 'name' | 'avatar' | 'both';
	shown: 'previous' | 'username' | 'operator';
	reason: 'guidelines' | 'impersonation' | 'rights' | 'age_suitability';
	note: string;
}

export interface PaymentDisputeWarningEmail {
	product: 'operator_pass' | 'plutonium';
	invoiceNumber: string | null;
	orderDate: Date;
	amountMinor: number;
	currency: string;
	warningDate: Date;
	note: string | null;
}

interface PaymentDisputeReviewAlertEmail {
	disputeId: string;
	product: 'operator_pass' | 'plutonium';
	invoiceNumber: string | null;
	purchaserId: string;
	reason: string;
	amountMinor: number;
	currency: string;
	evidenceDueBy: Date | null;
	blockers: ReadonlyArray<string>;
	reviewUrl: string;
}

export interface BillingTax {
	amountMinor: number;
	behavior: 'inclusive' | 'exclusive';
}

export interface BillingTotals {
	totalMinor: number;
	paidMinor: number;
	currency: string;
	tax: BillingTax | null;
	reverseCharge: boolean;
}

export interface BillingInvoiceDocument {
	url: string;
	linkUntil: Date;
	pdf: EmailAttachment | null;
}

export interface PlutoniumPurchaseTerms {
	renewal: {interval: 'month' | 'year'; amountMinor: number; tax: 'included' | 'added' | 'none'} | null;
	consentAt: Date | null;
	withdrawalEndsAt: Date | null;
	seller: boolean;
}

interface PlutoniumPaymentReceiptEmail {
	premiumName: string;
	plan: PlutoniumPlan;
	invoiceNumber: string | null;
	paidAt: Date;
	period: {start: Date; end: Date} | null;
	totals: BillingTotals;
	invoice: BillingInvoiceDocument | null;
	purchase: PlutoniumPurchaseTerms | null;
	timeZone?: string | null;
}

interface PlutoniumRefundReceiptEmail {
	premiumName: string;
	plan: PlutoniumPlan;
	invoiceNumber: string | null;
	refundedAt: Date;
	amountMinor: number;
	paidMinor: number;
	paidAt: Date;
	currency: string;
	refundUrl: string | null;
	subscriptionEnded?: {requestedAt: Date; reference: string} | null;
	timeZone?: string | null;
}

interface PlutoniumPaymentFailedEmail {
	premiumName: string;
	reason: BillingPaymentProblem;
	invoiceNumber: string | null;
	attemptedAt: Date;
	amountMinor: number;
	currency: string;
	invoiceUrl: string | null;
	accessEndsAt: Date | null;
	reminder?: boolean;
	planChange?: boolean;
	timeZone?: string | null;
}

interface DonationReceiptEmail {
	interval: 'month' | 'year';
	invoiceNumber: string | null;
	paidAt: Date;
	period: {start: Date; end: Date} | null;
	totals: BillingTotals;
	invoice: BillingInvoiceDocument | null;
	manageUrl: string;
}

interface DonationRefundReceiptEmail {
	reference: string | null;
	refundedAt: Date;
	amountMinor: number;
	currency: string;
	refundUrl: string | null;
}

interface DonationPaymentFailedEmail {
	reason: BillingPaymentProblem;
	invoiceNumber: string | null;
	attemptedAt: Date;
	amountMinor: number;
	currency: string;
	invoiceUrl: string | null;
	manageUrl: string;
	reminder?: boolean;
}

export type BillingPaymentMethod =
	| {kind: 'card'; brand: string; last4: string}
	| {kind: 'named'; name: string}
	| {kind: 'none'};

export type BillingRecurringPlan = 'monthly' | 'yearly';

type BillingNoticeVariant = 'renewal' | 'annual' | 'gift';

interface UpcomingCharge {
	chargeAt: Date;
	next: BillingTotals | null;
	regular: BillingTotals;
	taxIncluded: boolean;
	paymentMethod: BillingPaymentMethod;
	seller: boolean;
	timeZone?: string | null;
}

interface PlutoniumRenewalNoticeEmail extends UpcomingCharge {
	variant: BillingNoticeVariant;
	premiumName: string;
	plan: BillingRecurringPlan;
	giftEndsAt: Date | null;
	yearStartsAt: Date | null;
}

interface DonationRenewalNoticeEmail extends UpcomingCharge {
	manageUrl: string;
}

interface CardExpiringEmail {
	brand: string;
	last4: string;
	expMonth: number;
	expYear: number;
	chargeAt: Date;
	amount: BillingTotals;
	taxIncluded: boolean;
	seller: boolean;
	timeZone?: string | null;
}

interface PlutoniumCardExpiringEmail extends CardExpiringEmail {
	premiumName: string;
}

interface DonationCardExpiringEmail extends CardExpiringEmail {
	manageUrl: string;
}

interface CancellationConfirmation {
	receivedAt: Date;
	endsAt: Date;
	reference: string;
	unpaid: BillingTotals | null;
	seller: boolean;
	timeZone?: string | null;
}

interface PlutoniumCancellationConfirmationEmail extends CancellationConfirmation {
	premiumName: string;
	plan: BillingRecurringPlan;
}

interface DonationCancellationConfirmationEmail extends CancellationConfirmation {
	manageUrl: string;
}

export type BillingEndedInvoiceStatus = 'void' | 'uncollectible' | 'open' | 'paid' | 'other';

interface PlutoniumEndedUnpaidEmail {
	premiumName: string;
	invoiceNumber: string | null;
	invoiceStatus: BillingEndedInvoiceStatus;
	amountMinor: number;
	currency: string;
	invoiceUrl: string | null;
	seller: boolean;
}

interface PlutoniumGiftAppliedEmail {
	premiumName: string;
	plan: BillingRecurringPlan;
	trialEnd: Date;
	amount: BillingTotals;
	taxIncluded: boolean;
	paymentMethod: BillingPaymentMethod;
	seller: boolean;
	timeZone?: string | null;
}

interface BillingCreditNoteEmail {
	amountMinor: number;
	currency: string;
	invoiceNumber: string | null;
	pdfUrl: string | null;
	upcomingInvoice: boolean;
	seller: boolean;
}

interface GiftChargebackNotificationEmail {
	billingResumed?: {premiumName: string; plan: BillingRecurringPlan} | null;
}

interface DonationConfirmationEmail {
	amountCents: number;
	currency: string;
	interval: string;
	manageUrl: string;
	paidAt?: Date | null;
	reference?: string | null;
	receiptUrl?: string | null;
}

interface OperatorPassInvoiceEmail {
	invoiceNumber: string | null;
	paidAt: Date;
	totals: BillingTotals;
	invoice: BillingInvoiceDocument;
	timeZone?: string | null;
}

interface OperatorPassRefundReceiptEmail {
	invoiceNumber: string | null;
	orderDate: Date;
	refundedAt: Date;
	amountMinor: number;
	currency: string;
	full: boolean;
	timeZone?: string | null;
}

export interface IBulkEmailSession {
	send(
		templateKey: 'operator_pass_sale_open',
		email: string,
		params: OperatorPassSaleOpenEmail,
		locale?: string | null,
	): Promise<boolean>;
	close(): Promise<void>;
}

interface EmailCodeRequester {
	location: string;
	device: string;
}

export interface EmailSendParams {
	password_change_verification: {code: string};
	mfa_backup_codes_view: {code: string};
	sign_in_code: {code: string; expiresAt: Date; requester: EmailCodeRequester};
	sign_up_code: {code: string; expiresAt: Date; requester: EmailCodeRequester};
	email_verification_code: {code: string; expiresAt: Date};
	reauth_code: {code: string; expiresAt: Date};
	email_change_original: {code: string};
	email_change_new: {code: string};
	password_reset: {resetToken: string; ipAddress?: string; location?: string};
	email_verification: {verificationToken: string};
	ip_authorization: {authorizationToken: string; ipAddress: string; location: string};
	email_change_revert: {newEmail: string; token: string};
	registration_approved: Record<string, never>;
	registration_pending: {reviewUrl: string};
	self_deletion_scheduled: {deletionDate: Date};
	account_deletion_scheduled_requested: {deletionDate: string};
	account_deletion_scheduled_inactivity: {deletionDate: string};
	account_deletion_cancelled: Record<string, never>;
	inactivity_warning: {deletionDate: Date; lastActiveDate: Date};
	enforcement_notice: EmailTemplateVariables['enforcement_notice'];
	enforcement_ended: EmailTemplateVariables['enforcement_ended'];
	report_resolved: {reportId: string; publicComment: string; outcome: ReporterOutcome; reference: ReportReference};
	dsa_report_resolved: {reportId: string; publicComment: string; outcome: ReporterOutcome; reference: ReportReference};
	report_received: {reportId: string; targetKind: ReportReceivedTargetKind};
	dsa_report_verification: {code: string; expiresAt: Date};
	donation_magic_link: {manageUrl: string; expiresAt: Date};
	donation_confirmation: DonationConfirmationEmail;
	donation_receipt: DonationReceiptEmail;
	donation_refund_receipt: DonationRefundReceiptEmail;
	donation_payment_failed: DonationPaymentFailedEmail;
	harvest_completed: {downloadUrl: string; totalMessages: number; fileSize: number; expiresAt: Date};
	bulk_message_deletion_completed: EmailTemplateVariables['bulk_message_deletion_completed'];
	gift_chargeback_notification: GiftChargebackNotificationEmail;
	operator_pass_welcome: OperatorPassWelcomeEmail;
	operator_pass_purchase_confirmation: OperatorPassPurchaseConfirmationEmail;
	operator_pass_withdrawal_receipt: OperatorPassWithdrawalReceiptEmail;
	operator_pass_refund_receipt: OperatorPassRefundReceiptEmail;
	operator_pass_invoice: OperatorPassInvoiceEmail;
	operator_pass_offer_received: OperatorPassOfferReceivedEmail;
	operator_pass_offer_accepted: OperatorPassOfferAcceptedEmail;
	operator_pass_offer_ended: OperatorPassOfferEndedEmail;
	operator_pass_revoked: OperatorPassRevokedEmail;
	operator_pass_removal_notice: OperatorPassRemovalNoticeEmail;
	operator_pass_wall_hidden: OperatorPassWallHiddenEmail;
	operator_pass_wall_name_replaced: OperatorPassWallNameReplacedEmail;
	operator_pass_perks_removed: OperatorPassPerksRemovedEmail;
	payment_dispute_warning: PaymentDisputeWarningEmail;
	payment_dispute_review_alert: PaymentDisputeReviewAlertEmail;
	plutonium_payment_receipt: PlutoniumPaymentReceiptEmail;
	plutonium_refund_receipt: PlutoniumRefundReceiptEmail;
	plutonium_payment_failed: PlutoniumPaymentFailedEmail;
	plutonium_renewal_notice: PlutoniumRenewalNoticeEmail;
	donation_renewal_notice: DonationRenewalNoticeEmail;
	plutonium_card_expiring: PlutoniumCardExpiringEmail;
	donation_card_expiring: DonationCardExpiringEmail;
	plutonium_cancellation_confirmation: PlutoniumCancellationConfirmationEmail;
	donation_cancellation_confirmation: DonationCancellationConfirmationEmail;
	plutonium_ended_unpaid: PlutoniumEndedUnpaidEmail;
	plutonium_gift_applied: PlutoniumGiftAppliedEmail;
	billing_credit_note: BillingCreditNoteEmail;
	operator_pass_waitlist_verification: OperatorPassWaitlistVerificationEmail;
	operator_pass_sale_open: OperatorPassSaleOpenEmail;
}

export interface IEmailService {
	send<K extends keyof EmailSendParams>(
		templateKey: K,
		email: string,
		params: EmailSendParams[K],
		locale?: string | null,
	): Promise<boolean>;
	isEmailBounced(email: string): Promise<boolean>;
	openBulkSession(): Promise<IBulkEmailSession>;
}
