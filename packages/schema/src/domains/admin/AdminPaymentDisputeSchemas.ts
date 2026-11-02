// SPDX-License-Identifier: AGPL-3.0-or-later

import {createNamedStringLiteralUnion, SnowflakeStringType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

export const AdminPaymentDisputeProductSchema = createNamedStringLiteralUnion(
	[
		['operator_pass', 'OPERATOR_PASS', 'An Operator Pass purchase or gift, under section 9 of the Operator Pass terms'],
		['plutonium', 'PLUTONIUM', 'A Plutonium subscription, purchase or gift, under section 4.4 of the terms of service'],
	],
	'Product the disputed payment was for',
);

export const AdminPaymentDisputeReviewBlockerSchema = createNamedStringLiteralUnion(
	[
		[
			'exempt_reason',
			'EXEMPT_REASON',
			'The dispute reason is fraudulent, unrecognized, debit_not_authorized, duplicate, credit_not_processed, product_not_received or product_unacceptable',
		],
		['during_withdrawal', 'DURING_WITHDRAWAL', 'The buyer disputed while the withdrawal right was live'],
		['withdrawn', 'WITHDRAWN', 'The buyer withdrew from the purchase'],
		['refunded_before_dispute', 'REFUNDED_BEFORE_DISPUTE', 'The payment was refunded before the dispute'],
		['perks_not_provided', 'PERKS_NOT_PROVIDED', 'What the payment bought, or a working gift code, was never provided'],
		[
			'perks_removed',
			'PERKS_REMOVED',
			'The Operator Pass perks were removed under section 8 on or before the dispute date',
		],
		[
			'before_terms_effective',
			'BEFORE_TERMS_EFFECTIVE',
			'A Plutonium payment or its dispute came before the terms that allow closing an account for a dispute took effect',
		],
	],
	'Why the terms rule out finding the dispute unjustified',
);

export const AdminPaymentDisputeReviewStatusSchema = createNamedStringLiteralUnion(
	[
		['open', 'OPEN', 'Waiting for a person to review it'],
		['justified', 'JUSTIFIED', 'Found justified, no action against the account'],
		[
			'unjustified',
			'UNJUSTIFIED',
			'Found unjustified, the account was warned and, if the dispute ends with the money returned, is closed for Plutonium or barred from the Operator Pass',
		],
	],
	'Payment dispute review status',
);

export const AdminPaymentDisputeReviewResponse = z.object({
	dispute_id: z.string().describe('Stripe dispute ID'),
	product: AdminPaymentDisputeProductSchema,
	order_id: z.string().describe('Disputed order: the checkout session, or the invoice or payment intent of a renewal'),
	invoice_number: z
		.string()
		.nullable()
		.describe("Stripe's invoice number of the disputed payment, or the charge receipt number when it has no invoice"),
	purchaser_id: SnowflakeStringType.describe('Account that made the disputed payment'),
	is_gift: z.boolean().describe('Whether the purchase was a gift'),
	reason: z.string().describe('Stripe dispute reason code, read as the ground the bank gave'),
	amount_minor: z.number().int().describe('Disputed amount in minor units'),
	currency: z.string().describe('Currency of the disputed amount'),
	evidence_submitted: z.boolean().describe('Whether evidence was submitted to the bank'),
	evidence_due_by: z.iso.datetime().nullable().describe('Deadline Stripe gives for evidence to the bank'),
	disputed_at: z.iso.datetime().nullable().describe('When the dispute opened'),
	closed_at: z.iso.datetime().nullable().describe('When the dispute closed, null while it is open'),
	outcome: z
		.string()
		.nullable()
		.describe(
			'Stripe status the dispute closed with: won also covers a dispute the buyer withdrew, lost means the money was returned',
		),
	purchased_at: z.iso.datetime().nullable().describe('When the payment was made'),
	perks_started_at: z.iso.datetime().nullable().describe('When what the payment bought, or the gift code, started'),
	withdrawal_ends_at: z.iso.datetime().nullable().describe('When the withdrawal period ended'),
	withdrawn_at: z.iso.datetime().nullable().describe('When the buyer withdrew'),
	refunded_at: z.iso.datetime().nullable().describe('When the payment was first refunded'),
	perks_removed_at: z.iso
		.datetime()
		.nullable()
		.describe('First Operator Pass perk removal under section 8 on or before the dispute date'),
	status: AdminPaymentDisputeReviewStatusSchema,
	blockers: z.array(AdminPaymentDisputeReviewBlockerSchema).describe('Grounds that rule out finding it unjustified'),
	warnable: z
		.boolean()
		.describe(
			'Whether it can be marked unjustified now: open, unreviewed, no blockers and enough time before the warning date',
		),
	warning_deadline: z.iso
		.datetime()
		.nullable()
		.describe('Date the warning names, 3 days before evidence_due_by, or the date it would name while unreviewed'),
	warning_email_sent_at: z.iso.datetime().nullable().describe('When the warning email was sent'),
	warning_dm_sent_at: z.iso.datetime().nullable().describe('When the warning system message was sent'),
	warned_at: z.iso
		.datetime()
		.nullable()
		.describe('When the warning was complete, once both the email and the system message were sent'),
	enforcement_decision_id: SnowflakeStringType.nullable().describe(
		'Enforcement decision that closed the account after the dispute ended with the money returned',
	),
	awaited_decision_id: SnowflakeStringType.nullable().describe(
		'Other closure decision that holds the account while the closure for this dispute waits. The closure is recorded when that decision ends without the account being deleted',
	),
	decided_at: z.iso.datetime().nullable().describe('When a person decided'),
	decided_by: SnowflakeStringType.nullable().describe('Staff member who decided'),
	decision_note: z
		.string()
		.nullable()
		.describe('Note to the account holder given with the decision, shown in the warning and in the enforcement notice'),
	reversed_at: z.iso.datetime().nullable().describe('When the unjustified finding was reversed after an appeal'),
	reversed_by: SnowflakeStringType.nullable().describe('Staff member who reversed it'),
	reversal_note: z.string().nullable().describe('Why it was reversed'),
	dispute_purchase_at: z.iso
		.datetime()
		.nullable()
		.describe('When a purchase from a store or an older Stripe checkout was first applied while the dispute was open'),
});

export type AdminPaymentDisputeReviewResponse = z.infer<typeof AdminPaymentDisputeReviewResponse>;

export const AdminPaymentDisputeReviewListResponse = z.object({
	reviews: z.array(AdminPaymentDisputeReviewResponse).describe('Dispute reviews, newest first'),
});

export type AdminPaymentDisputeReviewListResponse = z.infer<typeof AdminPaymentDisputeReviewListResponse>;

export const AdminPaymentDisputeIdParam = z.object({
	dispute_id: z.string().trim().min(1).max(128).describe('Stripe dispute ID'),
});

export const AdminPaymentDisputeDecisionRequest = z.object({
	decision: z
		.enum(['unjustified', 'justified'])
		.describe('Find the dispute unjustified, which warns the account by email and system message, or justified'),
	note: z
		.string()
		.trim()
		.max(500)
		.nullable()
		.default(null)
		.describe(
			'Note to the account holder, shown in the warning and in the enforcement notice if the account is closed',
		),
});

export type AdminPaymentDisputeDecisionRequest = z.infer<typeof AdminPaymentDisputeDecisionRequest>;

export const AdminPaymentDisputeReversalRequest = z.object({
	note: z.string().trim().min(1).max(2000).describe('What the appeal showed'),
});

export type AdminPaymentDisputeReversalRequest = z.infer<typeof AdminPaymentDisputeReversalRequest>;
