// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	EmailTemplateVariables,
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
	donation_confirmation: {amountCents: number; currency: string; interval: string; manageUrl: string};
	harvest_completed: {downloadUrl: string; totalMessages: number; fileSize: number; expiresAt: Date};
	bulk_message_deletion_completed: EmailTemplateVariables['bulk_message_deletion_completed'];
	gift_chargeback_notification: Record<string, never>;
	operator_pass_welcome: OperatorPassWelcomeEmail;
	operator_pass_purchase_confirmation: OperatorPassPurchaseConfirmationEmail;
	operator_pass_withdrawal_receipt: OperatorPassWithdrawalReceiptEmail;
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
	openBulkSession(): Promise<IBulkEmailSession>;
}
