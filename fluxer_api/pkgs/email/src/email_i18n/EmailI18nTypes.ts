// SPDX-License-Identifier: AGPL-3.0-or-later

export type PlutoniumPlan = 'monthly' | 'yearly' | 'gift_1_month' | 'gift_1_year';

export type BillingPaymentProblem = 'declined' | 'action_required';

export type YesNo = 'yes' | 'no';

type BillingRecurringPlanName = 'monthly' | 'yearly';

type BillingPaymentMethodKind = 'card' | 'plain_card' | 'named' | 'none';

interface SellerVariables {
	sellerAddress: string;
	sellerEmail: string;
}

interface PaymentMethodVariables {
	method: BillingPaymentMethodKind;
	brand: string;
	last4: string;
	methodName: string;
}

interface UpcomingChargeVariables extends SellerVariables, PaymentMethodVariables {
	chargeAt: Date;
	nextFormatted: string;
	regularFormatted: string;
	tax: YesNo;
	differs: YesNo;
	cancelByFormatted: string;
}

interface CardExpiringVariables extends SellerVariables {
	branded: YesNo;
	brand: string;
	last4: string;
	expiryFormatted: string;
	chargeAt: Date;
	amountFormatted: string;
	tax: YesNo;
}

interface CancellationVariables extends SellerVariables {
	receivedFormatted: string;
	endsAt: Date;
	reference: string;
	unpaid: YesNo;
	unpaidFormatted: string;
}

export type ReportReceivedTargetKind = 'message' | 'user' | 'guild';

export type ReporterOutcome = 'removed' | 'restricted' | 'action_taken' | 'no_violation';

export type ReportReferenceTarget = 'message_community' | 'message_dm' | 'message' | 'user' | 'community' | 'other';

export interface ReportReference {
	target: ReportReferenceTarget;
	reportedAt: Date;
	username: string;
	communityName: string;
	channelName: string;
}

export type EnforcementNoticeKindLabel =
	| 'warning'
	| 'suspension'
	| 'ban'
	| 'closure'
	| 'feature_limit'
	| 'content_removal'
	| 'profile_reset'
	| 'community_removal'
	| 'bot';

export interface EmailTemplateVariables {
	account_deletion_cancelled: Record<string, never>;
	account_deletion_scheduled_inactivity: {
		deletion_date: string;
	};
	account_deletion_scheduled_requested: {
		deletion_date: string;
	};
	bulk_message_deletion_completed: {
		message_count: number;
		channel_count: number;
	};
	billing_credit_note: SellerVariables & {
		amountFormatted: string;
		numbered: YesNo;
		invoiceNumber: string;
		upcoming: YesNo;
	};
	donation_confirmation: {
		amount: string;
		interval: string;
		paidAt: Date;
	};
	donation_magic_link: {
		minutes: number;
	};
	donation_cancellation_confirmation: CancellationVariables;
	donation_card_expiring: CardExpiringVariables;
	donation_payment_failed: {
		reason: BillingPaymentProblem;
		attemptedAt: Date;
		amountFormatted: string;
	};
	donation_receipt: {
		interval: 'month' | 'year';
		paidAt: Date;
		periodStart: Date;
		periodEnd: Date;
		amountFormatted: string;
		pdf: YesNo;
		invoiceLinkUntil: Date;
	};
	donation_renewal_notice: UpcomingChargeVariables;
	donation_refund_receipt: {
		refundedAt: Date;
		amountFormatted: string;
	};
	dsa_report_resolved: ReportReference & {
		outcome: ReporterOutcome;
		appeals_email: string | null;
	};
	dsa_report_verification: {
		minutes: number;
	};
	email_change_new: {
		minutes: number;
	};
	email_change_original: {
		minutes: number;
	};
	email_change_revert: Record<string, never>;
	email_verification: Record<string, never>;
	email_verification_code: {
		minutes: number;
	};
	gift_chargeback_notification: {
		premiumName: string;
		plan: BillingRecurringPlanName;
	};
	enforcement_ended: {
		outcome: 'lifted' | 'reversed';
		decision_id: string | null;
		restores_access: 'yes' | 'no';
		statement: string | null;
		content_removed: 'yes' | 'no';
	};
	enforcement_notice: {
		kind_label: EnforcementNoticeKindLabel;
		neutral: 'yes' | 'no';
		summary: string;
		effects: string;
		rule_title: string;
		rule_url: string | null;
		facts: string;
		statement: string | null;
		how_we_decided: string;
		appeals_email: string | null;
		appeal_deadline: string;
		redress: string | null;
		decision_id: string;
	};
	harvest_completed: {
		totalMessages: number;
		fileSizeMB: number;
		expiresAt: Date;
	};
	inactivity_warning: {
		deletionDate: Date;
		lastActiveDate: Date;
	};
	ip_authorization: Record<string, never>;
	mfa_backup_codes_view: {
		minutes: number;
	};
	operator_pass_invoice: {
		paidAt: Date;
		amountFormatted: string;
		taxFormatted: string;
		pdf: YesNo;
		invoiceLinkUntil: Date;
	};
	operator_pass_offer_accepted: {
		recipientName: string;
		remainingCount: number;
	};
	operator_pass_offer_ended: {
		otherName: string;
		ended: 'withdrawn' | 'declined' | 'cancelled';
	};
	operator_pass_offer_received: {
		senderName: string;
		arrivesAt: string;
	};
	operator_pass_perks_removed: {
		scope: 'permanent' | 'temporary';
		until: Date;
		automated: 'yes' | 'no';
	};
	operator_pass_purchase_confirmation: {
		termsVersion: string;
		orderDate: Date;
		quantity: number;
		priceFormatted: string;
		withdrawalEndsOn: string;
		waiverConsentAt: Date;
		consentText: string;
		wallName: string;
		wallOptOut: 'yes' | 'no';
		sellerEmail: string;
	};
	operator_pass_refund_receipt: {
		orderDate: Date;
		refundedAt: Date;
		amountFormatted: string;
		full: YesNo;
	};
	operator_pass_removal_notice: {
		replyDays: number;
	};
	operator_pass_revoked: {
		cause: 'refund' | 'dispute' | 'withdrawal';
		lastPass: 'yes' | 'no';
	};
	operator_pass_sale_open: {
		priceFormatted: string;
		priceEndsOn: string;
	};
	operator_pass_waitlist_verification: Record<string, never>;
	operator_pass_wall_hidden: {
		reason: 'guidelines' | 'impersonation' | 'rights' | 'age_suitability';
	};
	operator_pass_wall_name_replaced: {
		target: 'name' | 'avatar' | 'both';
		shown: 'previous' | 'username' | 'operator';
		reason: 'guidelines' | 'impersonation' | 'rights' | 'age_suitability';
	};
	operator_pass_welcome: {
		founderName: string;
		deliveredAt: Date;
		wallName: string;
		wallOptOut: 'yes' | 'no';
	};
	operator_pass_withdrawal_receipt: {
		cancellation: 'yes' | 'no';
		receivedAt: Date;
		orderDate: Date;
		amountFormatted: string;
		lastPass: 'yes' | 'no';
		refundViaDispute: 'yes' | 'no';
	};
	password_change_verification: {
		minutes: number;
	};
	password_reset: Record<string, never>;
	payment_dispute_warning: {
		appeals_email: string | null;
		privacy_email: string | null;
		product: 'operator_pass' | 'plutonium';
		orderDate: Date;
		warningDate: Date;
	};
	plutonium_cancellation_confirmation: CancellationVariables & {
		premiumName: string;
		plan: BillingRecurringPlanName;
	};
	plutonium_card_expiring: CardExpiringVariables & {
		premiumName: string;
	};
	plutonium_ended_unpaid: SellerVariables & {
		premiumName: string;
		numbered: YesNo;
		invoiceNumber: string;
		owed: 'none' | 'stopped' | 'open' | 'unknown';
		amountFormatted: string;
	};
	plutonium_gift_applied: SellerVariables &
		PaymentMethodVariables & {
			premiumName: string;
			plan: BillingRecurringPlanName;
			trialEnd: Date;
			amountFormatted: string;
			tax: YesNo;
		};
	plutonium_payment_failed: {
		premiumName: string;
		reason: BillingPaymentProblem;
		attemptedAt: Date;
		amountFormatted: string;
		access: YesNo;
		accessEndsAt: Date;
	};
	plutonium_payment_receipt: {
		premiumName: string;
		plan: PlutoniumPlan;
		gift: YesNo;
		paidAt: Date;
		periodStart: Date;
		periodEnd: Date;
		amountFormatted: string;
		taxFormatted: string;
		pdf: YesNo;
		invoiceLinkUntil: Date;
		renewal: 'month' | 'year' | 'none';
		renewalFormatted: string;
		renewalTax: 'included' | 'added' | 'none';
		consentAt: Date;
		withdrawalEndsAt: Date;
		invoiceNumber: string;
		sellerAddress: string;
		sellerEmail: string;
	};
	plutonium_refund_receipt: {
		premiumName: string;
		plan: PlutoniumPlan;
		refundedAt: Date;
		amountFormatted: string;
		paidFormatted: string;
		paidAt: Date;
		partial: YesNo;
		requestedFormatted: string;
		reference: string;
	};
	plutonium_renewal_notice: UpcomingChargeVariables & {
		premiumName: string;
		gift: YesNo;
		annual: YesNo;
		plan: BillingRecurringPlanName;
		giftEndsAt: Date;
		yearStartsAt: Date;
	};
	reauth_code: {
		minutes: number;
	};
	registration_approved: Record<string, never>;
	registration_pending: Record<string, never>;
	report_received: {
		targetKind: ReportReceivedTargetKind;
	};
	report_resolved: ReportReference & {
		outcome: ReporterOutcome;
		appeals_email: string | null;
	};
	self_deletion_scheduled: {
		deletionDate: Date;
	};
	sign_in_code: {
		minutes: number;
	};
	sign_up_code: {
		minutes: number;
	};
}
