// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	BillingPaymentMethod,
	BillingTotals,
	EmailSendParams,
	IBulkEmailSession,
	IEmailService,
} from '@pkgs/email/src/IEmailService';
import {OPERATOR_PASS_TERMS_VERSION} from '@pkgs/email/src/operator_pass_terms/OperatorPassTerms';

export interface SentEmailRecord {
	to: string;
	type: string;
	timestamp: Date;
	metadata: Record<string, string>;
}

function totalsMetadata(totals: BillingTotals): Record<string, string> {
	return {
		total_minor: String(totals.totalMinor),
		paid_minor: String(totals.paidMinor),
		currency: totals.currency,
		tax_minor: totals.tax ? String(totals.tax.amountMinor) : '',
		tax_behavior: totals.tax?.behavior ?? '',
		reverse_charge: totals.reverseCharge ? 'true' : 'false',
	};
}

function paymentMethodMetadata(method: BillingPaymentMethod): Record<string, string> {
	return {
		payment_method: method.kind,
		card_brand: method.kind === 'card' ? method.brand : '',
		card_last4: method.kind === 'card' ? method.last4 : '',
		payment_method_name: method.kind === 'named' ? method.name : '',
	};
}

function prefixed(prefix: string, metadata: Record<string, string>): Record<string, string> {
	return Object.fromEntries(Object.entries(metadata).map(([key, value]) => [`${prefix}_${key}`, value]));
}

function stringMetadata(variables: Record<string, string | null>): Record<string, string> {
	return Object.fromEntries(Object.entries(variables).map(([key, value]) => [key, value ?? '']));
}

export class TestEmailService implements IEmailService {
	private readonly sentEmails: Array<SentEmailRecord> = [];

	readonly bouncedEmails = new Set<string>();

	async isEmailBounced(email: string): Promise<boolean> {
		return this.bouncedEmails.has(email);
	}

	async openBulkSession(): Promise<IBulkEmailSession> {
		return {
			send: (templateKey, email, params, locale) => this.send(templateKey, email, params, locale),
			close: async () => {},
		};
	}

	listSentEmails(): Array<SentEmailRecord> {
		return [...this.sentEmails];
	}

	clearSentEmails(): void {
		this.sentEmails.length = 0;
	}

	send<K extends keyof EmailSendParams>(
		templateKey: K,
		email: string,
		params: EmailSendParams[K],
		locale: string | null = null,
	): Promise<boolean> {
		return this.handlers[templateKey](email, params, locale);
	}

	private readonly handlers: {
		[K in keyof EmailSendParams]: (
			email: string,
			params: EmailSendParams[K],
			locale: string | null,
		) => Promise<boolean>;
	} = {
		password_change_verification: async (email, {code}) => {
			return this.record(email, 'password_change_verification', {code});
		},
		mfa_backup_codes_view: async (email, {code}) => {
			return this.record(email, 'mfa_backup_codes_view', {code});
		},
		sign_in_code: async (email, {code, expiresAt, requester}) => {
			return this.record(email, 'sign_in_code', {code, expires_at: expiresAt.toISOString(), ...requester});
		},
		sign_up_code: async (email, {code, expiresAt, requester}) => {
			return this.record(email, 'sign_up_code', {code, expires_at: expiresAt.toISOString(), ...requester});
		},
		email_verification_code: async (email, {code, expiresAt}) => {
			return this.record(email, 'email_verification_code', {code, expires_at: expiresAt.toISOString()});
		},
		reauth_code: async (email, {code, expiresAt}) => {
			return this.record(email, 'reauth_code', {code, expires_at: expiresAt.toISOString()});
		},
		email_change_original: async (email, {code}) => {
			return this.record(email, 'email_change_original', {code});
		},
		email_change_new: async (email, {code}) => {
			return this.record(email, 'email_change_new', {code});
		},
		password_reset: async (email, {resetToken, ipAddress, location}) => {
			return this.record(email, 'password_reset', {
				token: resetToken,
				...(ipAddress ? {ip: ipAddress} : {}),
				...(location ? {location} : {}),
			});
		},
		email_verification: async (email, {verificationToken}) => {
			return this.record(email, 'email_verification', {token: verificationToken});
		},
		ip_authorization: async (email, {authorizationToken, ipAddress, location}) => {
			return this.record(email, 'ip_authorization', {token: authorizationToken, ip: ipAddress, location});
		},
		email_change_revert: async (email, {newEmail, token}) => {
			return this.record(email, 'email_change_revert', {token, new_email: newEmail});
		},
		registration_approved: async (email, _params) => {
			return this.record(email, 'registration_approved');
		},
		registration_pending: async (email, {reviewUrl}) => {
			return this.record(email, 'registration_pending', {review_url: reviewUrl});
		},
		self_deletion_scheduled: async (email, {deletionDate}) => {
			return this.record(email, 'self_deletion_scheduled', {deletion_date: deletionDate.toISOString()});
		},
		account_deletion_scheduled_requested: async (email, {deletionDate}) => {
			return this.record(email, 'account_deletion_scheduled_requested', {deletion_date: deletionDate});
		},
		account_deletion_scheduled_inactivity: async (email, {deletionDate}) => {
			return this.record(email, 'account_deletion_scheduled_inactivity', {deletion_date: deletionDate});
		},
		account_deletion_cancelled: async (email, _params) => {
			return this.record(email, 'account_deletion_cancelled');
		},
		inactivity_warning: async (email, {deletionDate, lastActiveDate}) => {
			return this.record(email, 'inactivity_warning', {
				deletion_date: deletionDate.toISOString(),
				last_active_date: lastActiveDate.toISOString(),
			});
		},
		enforcement_notice: async (email, variables) => {
			return this.record(email, 'enforcement_notice', stringMetadata(variables));
		},
		enforcement_ended: async (email, variables) => {
			return this.record(email, 'enforcement_ended', stringMetadata(variables));
		},
		report_resolved: async (email, {reportId, publicComment, outcome}) => {
			return this.record(email, 'report_resolved', {report_id: reportId, public_comment: publicComment, outcome});
		},
		dsa_report_resolved: async (email, {reportId, publicComment, outcome}) => {
			return this.record(email, 'dsa_report_resolved', {report_id: reportId, public_comment: publicComment, outcome});
		},
		report_received: async (email, {reportId, targetKind}) => {
			return this.record(email, 'report_received', {report_id: reportId, target_kind: targetKind});
		},
		dsa_report_verification: async (email, {code, expiresAt}) => {
			return this.record(email, 'dsa_report_verification', {code, expires_at: expiresAt.toISOString()});
		},
		donation_magic_link: async (email, {manageUrl, expiresAt}) => {
			return this.record(email, 'donation_magic_link', {
				manage_url: manageUrl,
				expires_at: expiresAt.toISOString(),
			});
		},
		donation_confirmation: async (email, params) => {
			return this.record(email, 'donation_confirmation', {
				amount: (params.amountCents / 100).toString(),
				currency: params.currency.toUpperCase(),
				interval: params.interval,
				manage_url: params.manageUrl,
				paid_at: params.paidAt?.toISOString() ?? '',
				reference: params.reference ?? '',
				receipt_url: params.receiptUrl ?? '',
			});
		},
		donation_receipt: async (email, params, locale) => {
			return this.record(email, 'donation_receipt', {
				interval: params.interval,
				invoice_number: params.invoiceNumber ?? '',
				invoice_url: params.invoice?.url ?? '',
				...totalsMetadata(params.totals),
				locale: locale ?? '',
			});
		},
		donation_refund_receipt: async (email, params, locale) => {
			return this.record(email, 'donation_refund_receipt', {
				reference: params.reference ?? '',
				amount_minor: String(params.amountMinor),
				currency: params.currency,
				locale: locale ?? '',
			});
		},
		donation_payment_failed: async (email, params, locale) => {
			return this.record(email, 'donation_payment_failed', {
				reason: params.reason,
				invoice_number: params.invoiceNumber ?? '',
				amount_minor: String(params.amountMinor),
				reminder: params.reminder ? 'true' : 'false',
				locale: locale ?? '',
			});
		},
		harvest_completed: async (email, {downloadUrl, totalMessages, fileSize, expiresAt}) => {
			return this.record(email, 'harvest_completed', {
				download_url: downloadUrl,
				total_messages: totalMessages.toString(10),
				file_size: fileSize.toString(10),
				expires_at: expiresAt.toISOString(),
			});
		},
		bulk_message_deletion_completed: async (email, totals) => {
			return this.record(email, 'bulk_message_deletion_completed', {
				message_count: totals.message_count.toString(10),
				channel_count: totals.channel_count.toString(10),
			});
		},
		gift_chargeback_notification: async (email, params) => {
			return this.record(email, 'gift_chargeback_notification', {
				billing_resumed: params.billingResumed ? 'true' : 'false',
				plan: params.billingResumed?.plan ?? '',
			});
		},
		operator_pass_welcome: async (email, params) => {
			return this.record(email, 'operator_pass_welcome', {
				labs_invite_url: params.labsInviteUrl,
				invoice_number: params.invoiceNumber ?? '',
				delivered_at: params.deliveredAt.toISOString(),
				wall_name: params.wallName ?? '',
				pass_count: String(params.passCount),
			});
		},
		operator_pass_purchase_confirmation: async (email, params) => {
			return this.record(email, 'operator_pass_purchase_confirmation', {
				invoice_number: params.invoiceNumber ?? '',
				order_date: params.orderDate.toISOString(),
				quantity: String(params.quantity),
				amount_minor: String(params.amountMinor),
				currency: params.currency,
				withdrawal_days: params.withdrawalDays === null ? '' : String(params.withdrawalDays),
				withdrawal_ends_on: params.withdrawalEndsOn ?? '',
				waiver_consent_at: params.waiverConsentAt?.toISOString() ?? '',
				wall_name: params.wallName ?? '',
				unit_amount_minor: String(params.unitAmountMinor),
				payment_method: params.paymentMethod ?? '',
				consent_text: params.consentText ?? '',
				country_consent: params.countryConsent ? 'yes' : 'no',
				terms_version: OPERATOR_PASS_TERMS_VERSION,
				attachment: 'application/pdf',
			});
		},
		operator_pass_withdrawal_receipt: async (email, params) => {
			return this.record(email, 'operator_pass_withdrawal_receipt', {
				cancellation: String(params.cancellation),
				name: params.name,
				received_at: params.receivedAt.toISOString(),
				invoice_number: params.invoiceNumber ?? '',
				order_date: params.orderDate.toISOString(),
				amount_minor: String(params.amountMinor),
				currency: params.currency,
				last_pass: String(params.lastPass),
				has_donation: String(params.hasDonation),
				refund_via_dispute: String(params.refundViaDispute),
			});
		},
		operator_pass_offer_received: async (email, params) => {
			return this.record(email, 'operator_pass_offer_received', {
				sender_name: params.senderName,
				arrives_at: params.arrivesAt?.toISOString() ?? '',
			});
		},
		operator_pass_offer_accepted: async (email, params) => {
			return this.record(email, 'operator_pass_offer_accepted', {
				recipient_name: params.recipientName,
				remaining_count: String(params.remainingCount),
			});
		},
		operator_pass_offer_ended: async (email, params) => {
			return this.record(email, 'operator_pass_offer_ended', {other_name: params.otherName, ended: params.ended});
		},
		operator_pass_revoked: async (email, params) => {
			return this.record(email, 'operator_pass_revoked', {
				cause: params.cause,
				last_pass: params.lastPass ? 'yes' : 'no',
			});
		},
		operator_pass_removal_notice: async (email, params) => {
			return this.record(email, 'operator_pass_removal_notice', {
				rule: params.rule,
				facts: params.facts,
				reply_by: params.replyBy.toISOString(),
			});
		},
		operator_pass_wall_hidden: async (email, params) => {
			return this.record(email, 'operator_pass_wall_hidden', {
				reason: params.reason,
				note: params.note,
			});
		},
		operator_pass_wall_name_replaced: async (email, params) => {
			return this.record(email, 'operator_pass_wall_name_replaced', {
				target: params.target,
				shown: params.shown,
				reason: params.reason,
				note: params.note,
			});
		},
		operator_pass_perks_removed: async (email, params) => {
			return this.record(email, 'operator_pass_perks_removed', {
				rule: params.rule,
				facts: params.facts,
				automated: String(params.automated),
				until: params.until?.toISOString() ?? '',
			});
		},
		payment_dispute_warning: async (email, params) => {
			return this.record(email, 'payment_dispute_warning', {
				product: params.product,
				invoice_number: params.invoiceNumber ?? '',
				amount_minor: String(params.amountMinor),
				currency: params.currency,
				warning_date: params.warningDate.toISOString(),
			});
		},
		payment_dispute_review_alert: async (email, params) => {
			return this.record(email, 'payment_dispute_review_alert', {
				dispute_id: params.disputeId,
				product: params.product,
				purchaser_id: params.purchaserId,
				reason: params.reason,
				blockers: params.blockers.join(','),
			});
		},
		plutonium_payment_receipt: async (email, params, locale) => {
			return this.record(email, 'plutonium_payment_receipt', {
				premium_name: params.premiumName,
				plan: params.plan,
				invoice_number: params.invoiceNumber ?? '',
				paid_at: params.paidAt.toISOString(),
				period_start: params.period?.start.toISOString() ?? '',
				period_end: params.period?.end.toISOString() ?? '',
				...totalsMetadata(params.totals),
				invoice_url: params.invoice?.url ?? '',
				invoice_pdf: params.invoice?.pdf?.filename ?? '',
				renewal: params.purchase?.renewal?.interval ?? '',
				withdrawal_ends_at: params.purchase?.withdrawalEndsAt?.toISOString() ?? '',
				consent_at: params.purchase?.consentAt?.toISOString() ?? '',
				locale: locale ?? '',
			});
		},
		plutonium_refund_receipt: async (email, params, locale) => {
			return this.record(email, 'plutonium_refund_receipt', {
				premium_name: params.premiumName,
				plan: params.plan,
				invoice_number: params.invoiceNumber ?? '',
				refunded_at: params.refundedAt.toISOString(),
				amount_minor: String(params.amountMinor),
				paid_minor: String(params.paidMinor),
				currency: params.currency,
				refund_url: params.refundUrl ?? '',
				subscription_ended_at: params.subscriptionEnded?.requestedAt.toISOString() ?? '',
				subscription_reference: params.subscriptionEnded?.reference ?? '',
				locale: locale ?? '',
			});
		},
		plutonium_payment_failed: async (email, params, locale) => {
			return this.record(email, 'plutonium_payment_failed', {
				premium_name: params.premiumName,
				reason: params.reason,
				invoice_number: params.invoiceNumber ?? '',
				attempted_at: params.attemptedAt.toISOString(),
				amount_minor: String(params.amountMinor),
				currency: params.currency,
				invoice_url: params.invoiceUrl ?? '',
				access_ends_at: params.accessEndsAt?.toISOString() ?? '',
				reminder: params.reminder ? 'true' : 'false',
				plan_change: params.planChange ? 'true' : 'false',
				locale: locale ?? '',
			});
		},
		plutonium_renewal_notice: async (email, params, locale) => {
			return this.record(email, 'plutonium_renewal_notice', {
				variant: params.variant,
				plan: params.plan,
				...this.upcomingChargeMetadata(params),
				gift_ends_at: params.giftEndsAt?.toISOString() ?? '',
				year_starts_at: params.yearStartsAt?.toISOString() ?? '',
				locale: locale ?? '',
			});
		},
		donation_renewal_notice: async (email, params, locale) => {
			return this.record(email, 'donation_renewal_notice', {
				...this.upcomingChargeMetadata(params),
				manage_url: params.manageUrl,
				locale: locale ?? '',
			});
		},
		plutonium_card_expiring: async (email, params, locale) => {
			return this.record(email, 'plutonium_card_expiring', {
				...this.cardExpiringMetadata(params),
				locale: locale ?? '',
			});
		},
		donation_card_expiring: async (email, params, locale) => {
			return this.record(email, 'donation_card_expiring', {
				...this.cardExpiringMetadata(params),
				manage_url: params.manageUrl,
				locale: locale ?? '',
			});
		},
		plutonium_cancellation_confirmation: async (email, params, locale) => {
			return this.record(email, 'plutonium_cancellation_confirmation', {
				plan: params.plan,
				...this.cancellationMetadata(params),
				locale: locale ?? '',
			});
		},
		donation_cancellation_confirmation: async (email, params, locale) => {
			return this.record(email, 'donation_cancellation_confirmation', {
				...this.cancellationMetadata(params),
				manage_url: params.manageUrl,
				locale: locale ?? '',
			});
		},
		plutonium_ended_unpaid: async (email, params, locale) => {
			return this.record(email, 'plutonium_ended_unpaid', {
				invoice_number: params.invoiceNumber ?? '',
				invoice_status: params.invoiceStatus,
				amount_minor: String(params.amountMinor),
				invoice_url: params.invoiceUrl ?? '',
				locale: locale ?? '',
			});
		},
		plutonium_gift_applied: async (email, params, locale) => {
			return this.record(email, 'plutonium_gift_applied', {
				plan: params.plan,
				trial_end: params.trialEnd.toISOString(),
				...prefixed('amount', totalsMetadata(params.amount)),
				tax_included: params.taxIncluded ? 'true' : 'false',
				...paymentMethodMetadata(params.paymentMethod),
				locale: locale ?? '',
			});
		},
		billing_credit_note: async (email, params, locale) => {
			return this.record(email, 'billing_credit_note', {
				amount_minor: String(params.amountMinor),
				currency: params.currency,
				invoice_number: params.invoiceNumber ?? '',
				pdf_url: params.pdfUrl ?? '',
				upcoming_invoice: params.upcomingInvoice ? 'true' : 'false',
				locale: locale ?? '',
			});
		},
		operator_pass_invoice: async (email, params) => {
			return this.record(email, 'operator_pass_invoice', {
				invoice_number: params.invoiceNumber ?? '',
				invoice_url: params.invoice.url,
				...totalsMetadata(params.totals),
			});
		},
		operator_pass_refund_receipt: async (email, params) => {
			return this.record(email, 'operator_pass_refund_receipt', {
				invoice_number: params.invoiceNumber ?? '',
				amount_minor: String(params.amountMinor),
				currency: params.currency,
				full: params.full ? 'true' : 'false',
			});
		},
		operator_pass_waitlist_verification: async (email, params) => {
			return this.record(email, 'operator_pass_waitlist_verification', {
				confirm_url: params.confirmUrl,
			});
		},
		operator_pass_sale_open: async (email, params) => {
			return this.record(email, 'operator_pass_sale_open', {
				checkout_url: params.checkoutUrl,
				price_formatted: params.priceFormatted,
				price_rises_at: params.priceRisesAt.toISOString(),
				unsubscribe_url: params.unsubscribeUrl,
				unsubscribe_page_url: params.unsubscribePageUrl,
			});
		},
	};

	private upcomingChargeMetadata(
		params: EmailSendParams['donation_renewal_notice'] | EmailSendParams['plutonium_renewal_notice'],
	): Record<string, string> {
		return {
			charge_at: params.chargeAt.toISOString(),
			...prefixed('next', params.next ? totalsMetadata(params.next) : {total_minor: ''}),
			...prefixed('regular', totalsMetadata(params.regular)),
			regular_differs: params.next && params.next.totalMinor !== params.regular.totalMinor ? 'true' : 'false',
			tax_included: params.taxIncluded ? 'true' : 'false',
			...paymentMethodMetadata(params.paymentMethod),
		};
	}

	private cardExpiringMetadata(
		params: EmailSendParams['donation_card_expiring'] | EmailSendParams['plutonium_card_expiring'],
	): Record<string, string> {
		return {
			card_brand: params.brand,
			card_last4: params.last4,
			card_expiry: `${params.expMonth}/${params.expYear}`,
			charge_at: params.chargeAt.toISOString(),
			...prefixed('amount', totalsMetadata(params.amount)),
		};
	}

	private cancellationMetadata(
		params:
			| EmailSendParams['donation_cancellation_confirmation']
			| EmailSendParams['plutonium_cancellation_confirmation'],
	): Record<string, string> {
		return {
			received_at: params.receivedAt.toISOString(),
			ends_at: params.endsAt.toISOString(),
			reference: params.reference,
			time_zone: params.timeZone ?? '',
			unpaid_minor: params.unpaid ? String(params.unpaid.totalMinor) : '',
		};
	}

	private record(to: string, type: string, metadata: Record<string, string> = {}): boolean {
		this.sentEmails.push({to, type, timestamp: new Date(), metadata});
		return true;
	}
}
