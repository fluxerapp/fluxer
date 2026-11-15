// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminRepository} from '@app/api/admin/AdminRepository';
import {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import {createUserID} from '@app/api/BrandedTypes';
import {Config, getConfig} from '@app/api/Config';
import {setCassandraQueryExecutorForTesting} from '@app/api/database/CassandraQueryExecution';
import type {OperatorPassPurchaseRow, OperatorPassUnitRow} from '@app/api/database/types/OperatorPassTypes';
import type {PaymentDisputeReviewRow} from '@app/api/database/types/PaymentDisputeTypes';
import {getSnowflakeService} from '@app/api/middleware/ServiceRegistry';
import {getEmailService, getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import {shouldCloseAfterPaymentDispute} from '@app/api/payment_disputes/PaymentDisputeEnforcement';
import {PaymentDisputeRepository} from '@app/api/payment_disputes/PaymentDisputeRepository';
import {
	getOperatorPassDisputeSubject,
	getPaymentDisputeReviewBlockers,
	PaymentDisputeReviewService,
	type PaymentDisputeSubject,
} from '@app/api/payment_disputes/PaymentDisputeReviews';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {createRecordingSmtpServer} from '@app/api/test/RecordingSmtpServer';
import {EmailDeliveryModes} from '@fluxer/constants/src/AccountConfigConstants';
import {SmtpTlsModes} from '@fluxer/constants/src/SmtpConstants';
import {MS_PER_DAY, MS_PER_SECOND} from '@fluxer/date_utils/src/DateConstants';
import type Stripe from 'stripe';
import {afterEach, assert, beforeEach, describe, expect, test} from 'vitest';

const PAID_AT = new Date('2026-11-03T16:00:00Z');
const DISPUTED_AT = new Date(PAID_AT.getTime() + 30 * MS_PER_DAY);
const WARNING_DEADLINE = new Date(DISPUTED_AT.getTime() + 7 * MS_PER_DAY);
const PURCHASER_ID = createUserID(1001n);

function review(overrides: Partial<PaymentDisputeReviewRow> = {}): PaymentDisputeReviewRow {
	return {
		dispute_id: 'dp_test_1',
		product: 'plutonium',
		order_id: 'cs_test_1',
		invoice_number: 'ABCD1234-0001',
		purchaser_id: PURCHASER_ID,
		is_gift: false,
		reason: 'general',
		payment_intent_id: 'pi_test_1',
		charge_id: 'ch_test_1',
		amount_minor: 499,
		currency: 'USD',
		evidence_submitted: false,
		evidence_due_by: new Date(DISPUTED_AT.getTime() + 10 * MS_PER_DAY),
		disputed_at: DISPUTED_AT,
		closed_at: null,
		outcome: null,
		purchased_at: PAID_AT,
		perks_started_at: PAID_AT,
		withdrawal_ends_at: new Date(PAID_AT.getTime() + 15 * MS_PER_DAY),
		withdrawn_at: null,
		refunded_at: null,
		perks_removed_at: null,
		status: 'open',
		created_at: DISPUTED_AT,
		decided_at: null,
		decided_by: null,
		decision_note: null,
		warning_deadline: null,
		warning_email_sent_at: null,
		warning_dm_sent_at: null,
		warned_at: null,
		enforcement_decision_id: null,
		reversed_at: null,
		reversed_by: null,
		reversal_note: null,
		dispute_purchase_at: null,
		version: 1,
		...overrides,
	};
}

function warnedLostReview(closedAt: Date): PaymentDisputeReviewRow {
	return review({
		status: 'unjustified',
		decided_at: DISPUTED_AT,
		warning_deadline: WARNING_DEADLINE,
		warning_email_sent_at: DISPUTED_AT,
		warning_dm_sent_at: DISPUTED_AT,
		warned_at: DISPUTED_AT,
		closed_at: closedAt,
		outcome: 'lost',
	});
}

const SUBJECT: PaymentDisputeSubject = {
	product: 'plutonium',
	orderId: 'cs_test_1',
	invoiceNumber: 'ABCD1234-0001',
	purchaserId: PURCHASER_ID,
	isGift: false,
	paymentIntentId: 'pi_test_1',
	amountMinor: 499,
	currency: 'USD',
	disputedAt: DISPUTED_AT,
	purchasedAt: PAID_AT,
	perksStartedAt: PAID_AT,
	withdrawalEndsAt: new Date(PAID_AT.getTime() + 15 * MS_PER_DAY),
	withdrawnAt: null,
	refundedAt: null,
	perksRemovedAt: null,
};

function lostDispute(): Stripe.Dispute {
	return {
		id: 'dp_test_1',
		object: 'dispute',
		status: 'lost',
		reason: 'general',
		amount: 499,
		currency: 'usd',
		charge: 'ch_test_1',
		payment_intent: 'pi_test_1',
		created: Math.floor(DISPUTED_AT.getTime() / MS_PER_SECOND),
		balance_transactions: [],
		enhanced_eligibility_types: [],
		is_charge_refundable: false,
		livemode: false,
		metadata: {},
		evidence: {
			access_activity_log: null,
			billing_address: null,
			cancellation_policy: null,
			cancellation_policy_disclosure: null,
			cancellation_rebuttal: null,
			customer_communication: null,
			customer_email_address: null,
			customer_name: null,
			customer_purchase_ip: null,
			customer_signature: null,
			duplicate_charge_documentation: null,
			duplicate_charge_explanation: null,
			duplicate_charge_id: null,
			enhanced_evidence: {},
			product_description: null,
			receipt: null,
			refund_policy: null,
			refund_policy_disclosure: null,
			refund_refusal_explanation: null,
			service_date: null,
			service_documentation: null,
			shipping_address: null,
			shipping_carrier: null,
			shipping_date: null,
			shipping_documentation: null,
			shipping_tracking_number: null,
			uncategorized_file: null,
			uncategorized_text: null,
		},
		evidence_details: {
			due_by: null,
			enhanced_eligibility: {},
			has_evidence: false,
			past_due: false,
			submission_count: 0,
		},
	};
}

let originalClosureEffectiveAt = Config.paymentDisputes.closureEffectiveAt;

beforeEach(() => {
	originalClosureEffectiveAt = Config.paymentDisputes.closureEffectiveAt;
	Config.paymentDisputes.closureEffectiveAt = new Date('2000-01-01T00:00:00Z');
});

afterEach(() => {
	Config.paymentDisputes.closureEffectiveAt = originalClosureEffectiveAt;
});

describe('the closure terms effective date', () => {
	test('blocks every Plutonium review while the date is unset, and no closure follows a loss', () => {
		Config.paymentDisputes.closureEffectiveAt = null;
		expect(getPaymentDisputeReviewBlockers(review())).toEqual(['before_terms_effective']);
		expect(shouldCloseAfterPaymentDispute(warnedLostReview(WARNING_DEADLINE))).toBe(false);
	});

	test('blocks a payment and dispute from before the date', () => {
		Config.paymentDisputes.closureEffectiveAt = new Date(DISPUTED_AT.getTime() + MS_PER_DAY);
		expect(getPaymentDisputeReviewBlockers(review())).toEqual(['before_terms_effective']);
	});

	test('lets a payment and dispute from on or after the date through', () => {
		Config.paymentDisputes.closureEffectiveAt = PAID_AT;
		expect(getPaymentDisputeReviewBlockers(review())).toEqual([]);
		expect(shouldCloseAfterPaymentDispute(warnedLostReview(WARNING_DEADLINE))).toBe(true);
	});

	test('blocks a dispute after the date about a payment made before it', () => {
		Config.paymentDisputes.closureEffectiveAt = new Date(PAID_AT.getTime() + MS_PER_DAY);
		expect(getPaymentDisputeReviewBlockers(review())).toEqual(['before_terms_effective']);
	});
});

describe('an Operator Pass dispute after a partial refund', () => {
	const purchase = {
		checkout_session_id: 'cs_op_partial',
		purchaser_id: PURCHASER_ID,
		payment_intent_id: 'pi_op_partial',
		amount_minor: 3000,
		currency: 'EUR',
		quantity: 3,
		status: 'disputed',
		purchased_at: PAID_AT,
		delivered_at: PAID_AT,
		disputed_at: DISPUTED_AT,
		refunded_at: null,
		withdrawn_at: null,
	} as OperatorPassPurchaseRow;
	const refundedAt = new Date(DISPUTED_AT.getTime() - 5 * MS_PER_DAY);
	const unit = (index: number, ended: boolean): OperatorPassUnitRow =>
		({
			pass_id: `op_partial_${index}`,
			purchase_id: purchase.checkout_session_id,
			holder_id: PURCHASER_ID,
			status: ended ? 'revoked' : 'held',
			ended_at: ended ? refundedAt : null,
			end_reason: ended ? 'refund' : null,
		}) as OperatorPassUnitRow;

	function blockersFor(units: ReadonlyArray<OperatorPassUnitRow>) {
		const subject = getOperatorPassDisputeSubject(purchase, lostDispute(), [], units);
		return {
			subject,
			blockers: getPaymentDisputeReviewBlockers(
				review({
					product: 'operator_pass',
					perks_started_at: subject.perksStartedAt,
					withdrawal_ends_at: subject.withdrawalEndsAt,
					refunded_at: subject.refundedAt,
				}),
			),
		};
	}

	test('one of three passes refunded before the dispute blocks an unjustified decision', () => {
		const {subject, blockers} = blockersFor([unit(0, true), unit(1, false), unit(2, false)]);
		expect(subject.refundedAt).toEqual(refundedAt);
		expect(blockers).toEqual(['refunded_before_dispute']);
	});

	test('no refunded pass leaves the review open to a decision', () => {
		const {subject, blockers} = blockersFor([unit(0, false), unit(1, false), unit(2, false)]);
		expect(subject.refundedAt).toBeNull();
		expect(blockers).toEqual([]);
	});
});

describe('shouldCloseAfterPaymentDispute', () => {
	test('a loss before the warning date closes nothing', () => {
		expect(shouldCloseAfterPaymentDispute(warnedLostReview(new Date(WARNING_DEADLINE.getTime() - 1)))).toBe(false);
	});

	test('a loss on or after the warning date closes the account', () => {
		expect(shouldCloseAfterPaymentDispute(warnedLostReview(WARNING_DEADLINE))).toBe(true);
		expect(shouldCloseAfterPaymentDispute(warnedLostReview(new Date(WARNING_DEADLINE.getTime() + MS_PER_DAY)))).toBe(
			true,
		);
	});

	test('a loss without a warning date closes nothing', () => {
		expect(
			shouldCloseAfterPaymentDispute({
				...warnedLostReview(new Date(WARNING_DEADLINE.getTime() + 1)),
				warning_deadline: null,
			}),
		).toBe(false);
	});

	test('a review that already closed the account closes nothing again', () => {
		expect(shouldCloseAfterPaymentDispute({...warnedLostReview(WARNING_DEADLINE), enforcement_decision_id: 1n})).toBe(
			false,
		);
	});
});

describe('PaymentDisputeRepository', () => {
	let repository: PaymentDisputeRepository;

	beforeEach(() => {
		setCassandraQueryExecutorForTesting(new InMemoryCassandraQueryExecutor());
		repository = new PaymentDisputeRepository();
	});

	test('a second create for the same dispute changes nothing and the purchaser lookup finds the review', async () => {
		expect(await repository.createReview(review())).toBe(true);
		expect(await repository.createReview(review({reason: 'fraudulent'}))).toBe(false);
		const reviews = await repository.listReviewsByPurchaser(PURCHASER_ID);
		expect(reviews.map((entry) => [entry.dispute_id, entry.reason])).toEqual([['dp_test_1', 'general']]);
	});
});

const AUDIT_PAGE_SIZE = 10;

describe('payment dispute review alerts through SMTP', () => {
	let harness: ApiTestHarness;
	let repository: PaymentDisputeRepository;
	let adminRepository: AdminRepository;
	let service: PaymentDisputeReviewService;
	let originalSelfHosted: boolean;
	let originalAdminEndpoint: string;
	const servers = new Set<Awaited<ReturnType<typeof createRecordingSmtpServer>>>();
	const pendingOpens = new Set<Promise<PaymentDisputeReviewRow>>();

	beforeEach(async () => {
		originalSelfHosted = Config.instance.selfHosted;
		originalAdminEndpoint = Config.endpoints.admin;
		harness = await createApiTestHarness();
		Config.instance.selfHosted = false;
		Config.endpoints.admin = 'https://admin.example.test';
		const config = getConfig();
		const testMode = config.dev.testModeEnabled;
		try {
			config.dev.testModeEnabled = false;
			getEmailService();
		} finally {
			config.dev.testModeEnabled = testMode;
		}
		repository = new PaymentDisputeRepository();
		adminRepository = new AdminRepository();
		service = new PaymentDisputeReviewService(
			repository,
			new AdminAuditService(adminRepository, getSnowflakeService()),
			getEmailService(),
		);
	});

	afterEach(async () => {
		for (const smtp of servers) smtp.release();
		try {
			await Promise.all(pendingOpens);
		} finally {
			try {
				await Promise.all([...servers].map((smtp) => smtp.close()));
			} finally {
				servers.clear();
				pendingOpens.clear();
				Config.instance.selfHosted = originalSelfHosted;
				Config.endpoints.admin = originalAdminEndpoint;
				await harness.shutdown();
			}
		}
	});

	test.each([
		{name: 'accepts', firstDelivery: 'hold', accepted: 1},
		{name: 'rejects', firstDelivery: 'hold-and-reject', accepted: 0},
	] as const)(
		'keeps the review when SMTP $name the alert and does not resend on a repeat event',
		async ({firstDelivery, accepted}) => {
			const smtp = await createRecordingSmtpServer({firstDelivery});
			servers.add(smtp);
			await getInstanceConfigRepository().setInstanceIntegrationsConfig({
				email: {
					delivery: EmailDeliveryModes.SMTP,
					from_email: 'reviews@example.com',
					from_name: 'Payment reviews',
					smtp: {host: '127.0.0.1', port: smtp.port, username: '', password: '', tls_mode: SmtpTlsModes.NONE},
				},
			});
			expect(await getInstanceConfigRepository().isEmailEnabled()).toBe(true);
			const dispute: Stripe.Dispute = {...lostDispute(), status: 'needs_response'};
			const pending = service.openReview(SUBJECT, dispute);
			pendingOpens.add(pending);
			await smtp.entered;

			const opened = await repository.findReview('dp_test_1');
			assert(opened !== null);
			const openedAt = opened.created_at;
			expect(opened).toEqual(review({created_at: openedAt, evidence_due_by: null}));
			expect(await adminRepository.listAllAuditLogsPaginated(AUDIT_PAGE_SIZE)).toMatchObject([
				{
					targetType: 'user',
					targetId: 1001n,
					action: 'open_payment_dispute_review',
					auditLogReason: 'Payment dispute opened, purchases are blocked until it closes and a person reviews it',
					metadata: new Map([
						['dispute_id', 'dp_test_1'],
						['product', 'plutonium'],
						['order_id', 'cs_test_1'],
						['reason', 'general'],
						['evidence_due_by', ''],
						['blockers', ''],
					]),
					createdAt: expect.any(Date),
				},
			]);
			expect(smtp.delivered).toEqual([]);
			expect(smtp.attempts).toMatchObject([{from: '<reviews@example.com>', to: '<support@fluxer.com>'}]);
			expect(smtp.attempts[0].body).toContain('Subject: Plutonium payment dispute dp_test_1');
			expect(smtp.attempts[0].body).toContain(
				'A Plutonium payment dispute opened and needs a review.\r\n\r\nDispute: dp_test_1\r\nInvoice: ABCD1234-0001\r\nAccount: 1001\r\nAmount: $4.99\r\nReason: general\r\nEvidence due: not given\r\nNothing in the terms rules out finding it unjustified.\r\n\r\nReview it: https://admin.example.test/payment-disputes',
			);

			smtp.release();
			expect(await pending).toEqual(review({created_at: openedAt, evidence_due_by: null}));
			expect(smtp.delivered).toHaveLength(accepted);
			expect(await service.openReview(SUBJECT, dispute)).toEqual(
				review({created_at: openedAt, evidence_due_by: null, version: 2}),
			);
			expect(await repository.findReview('dp_test_1')).toEqual(
				review({created_at: openedAt, evidence_due_by: null, version: 2}),
			);
			expect(smtp.attempts).toHaveLength(1);
			expect(smtp.delivered).toHaveLength(accepted);
			expect(await adminRepository.listAllAuditLogsPaginated(AUDIT_PAGE_SIZE)).toHaveLength(1);
		},
	);
});
