// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {createBillingDocumentService} from '@app/api/billing_documents/BillingDocumentService';
import {Config} from '@app/api/Config';
import {Db} from '@app/api/database/CassandraTypes';
import type {OperatorPassPurchaseRow, OperatorPassUnitRow} from '@app/api/database/types/OperatorPassTypes';
import type {OperatorPassServices} from '@app/api/middleware/OperatorPassServiceFactory';
import {getKVClient, setInjectedWorkerService} from '@app/api/middleware/ServiceRegistry';
import {getCacheService, getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {OperatorPassCheckoutService} from '@app/api/operator_pass/OperatorPassCheckoutService';
import {OperatorPassEntitlementService} from '@app/api/operator_pass/OperatorPassEntitlementService';
import {
	decodeOperatorPassWithdrawalReceipt,
	OPERATOR_PASS_DELIVERY_RECORD_TASK,
	OPERATOR_PASS_WITHDRAWAL_RECEIPT_TASK,
	OperatorPassFulfilmentService,
	resolveOperatorPassSettledAt,
	syncOperatorPassDeliveryRecord,
} from '@app/api/operator_pass/OperatorPassFulfilmentService';
import {endOperatorPassesForDeletion} from '@app/api/operator_pass/OperatorPassHoldingService';
import {operatorPassHolderLockKey, operatorPassPurchaseLockKey} from '@app/api/operator_pass/OperatorPassLocks';
import {
	getOperatorPassWithdrawalDeadline,
	getOperatorPassWithdrawalEndsAt,
	getOperatorPassWithdrawalEndsOn,
	mapOperatorPassPurchaseToResponse,
} from '@app/api/operator_pass/OperatorPassPurchaseRules';
import {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import {stripeInvoice} from '@app/api/stripe/tests/BillingEmailTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopWorkerService} from '@app/api/test/NoopWorkerService';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {runOperatorPassFulfilmentSweep} from '@app/api/worker/tasks/OperatorPassFulfilmentSweep';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {OperatorPassWithdrawalUnavailableError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassWithdrawalUnavailableError';
import {formatLockKey} from '@pkgs/cache/src/CacheLockValidation';
import {TestEmailService} from '@pkgs/email/src/TestEmailService';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {PDFDocument} from 'pdf-lib';
import type Stripe from 'stripe';
import {afterAll, beforeAll, expect, test, vi} from 'vitest';

const {loggerError} = vi.hoisted(() => ({loggerError: vi.fn()}));

vi.mock('@app/api/Logger', async (importOriginal) => {
	const actual = await importOriginal<typeof import('@app/api/Logger')>();
	return {
		...actual,
		Logger: new Proxy(actual.Logger, {
			get: (target, prop) => (prop === 'error' ? loggerError : Reflect.get(target, prop)),
		}),
	};
});

const DAY_MS = 24 * 60 * 60 * 1000;

let harness: ApiTestHarness;
let priceRisesAt: Date | null;

beforeAll(async () => {
	harness = await createApiTestHarness();
	priceRisesAt = Config.operatorPass.priceRisesAt;
	Config.operatorPass.priceRisesAt = new Date(Date.now() + 30 * DAY_MS);
});

afterAll(async () => {
	Config.operatorPass.priceRisesAt = priceRisesAt;
	await harness.shutdown();
});

async function startPurchase(certificateNames: Array<string>, quantity: number, geoipCountryCode = 'SE') {
	const account = await createTestAccount(harness);
	await createBuilder(harness, account.token)
		.post(`/test/users/${account.userId}/security-flags`)
		.body({email_verified: true})
		.execute();
	const user = await getUserRepository().findUniqueAssert(createUserID(BigInt(account.userId)));
	const create = vi.fn(async (params: Stripe.Checkout.SessionCreateParams) => ({
		id: `cs_test_fulfil_${account.userId}`,
		url: 'https://checkout.stripe.test/session',
		params,
	}));
	const repository = new OperatorPassRepository();
	await new OperatorPassCheckoutService(
		{checkout: {sessions: {create}}} as unknown as Stripe,
		repository,
		new OperatorPassEntitlementService(repository),
	).createCheckoutSession({
		user,
		request: {
			quantity,
			certificate_names: certificateNames,
			wall_opt_out: true,
			delivery_consent: true,
			age_confirmed: true,
		},
		geoipCountryCode,
		clientIp: '203.0.113.7',
		userAgent: 'vitest',
	});
	const params = create.mock.calls[0]![0];
	const startedAt = new Date('2026-12-30T10:00:00Z');
	const intentMetadata: Record<string, string> = {};
	const refunds = new Map<string, Stripe.Refund>();
	const amount = params.line_items?.[0]?.price_data?.unit_amount ?? 0;
	const stripe = {
		paymentIntents: {
			retrieve: vi.fn(async () => ({
				latest_charge: {
					created: Math.floor(startedAt.getTime() / 1000),
					receipt_number: null,
					payment_method_details: {type: 'sepa_debit'},
					...(refunds.size > 0 ? {refunded: true, amount_refunded: amount * quantity} : {}),
				},
			})),
			update: vi.fn(async (_id: string, update: Stripe.PaymentIntentUpdateParams) => {
				Object.assign(intentMetadata, update.metadata);
				return {} as Stripe.PaymentIntent;
			}),
		},
		refunds: {
			create: vi.fn(async (refund: Stripe.RefundCreateParams, options?: Stripe.RequestOptions) => {
				const key = options?.idempotencyKey ?? `re_test_${refunds.size}`;
				const created =
					refunds.get(key) ??
					({id: `re_test_${refunds.size}`, object: 'refund', metadata: refund.metadata} as Stripe.Refund);
				refunds.set(key, created);
				return created;
			}),
			list: vi.fn(async () => ({data: [...refunds.values()]})),
		},
		invoices: {
			retrieve: vi.fn(async (id: string) => ({
				...stripeInvoice({
					id,
					total: amount * quantity + 2000,
					tax: {amount: 2000, behavior: 'exclusive'},
					subscriptionId: null,
					invoicePdf: true,
				}),
				currency: params.line_items?.[0]?.price_data?.currency ?? 'usd',
			})),
		},
	} as unknown as Stripe;
	const deliverPurchase = vi.fn(async () => {});
	const setPurchasePaused = vi.fn(async () => {});
	const services = {
		repository,
		wallService: {getEntry: async () => null},
		holdingService: {
			deliverPurchase,
			setPurchasePaused,
			endPurchaseUnits: vi.fn(async () => {}),
			queries: {listHeldUnits: async () => []},
		},
		roleService: {isLabsMember: async () => true},
		removalService: {},
	} as unknown as OperatorPassServices;
	const emails = new TestEmailService();
	const fulfilment = new OperatorPassFulfilmentService(
		stripe,
		repository,
		getUserRepository(),
		emails,
		getCacheService(),
		getKVClient(),
		services,
		null,
	);
	const session = {
		id: `cs_test_fulfil_${account.userId}`,
		created: Math.floor(startedAt.getTime() / 1000),
		metadata: params.metadata,
		payment_intent: `pi_test_${account.userId}`,
		invoice: null,
		payment_status: 'unpaid',
		customer_details: {address: {country: 'SE'}, business_name: null, tax_ids: []},
		custom_text: params.custom_text,
		consent: {terms_of_service: 'accepted'},
	} as unknown as Stripe.Checkout.Session;
	return {
		repository,
		fulfilment,
		session,
		stripe,
		intentMetadata,
		deliverPurchase,
		setPurchasePaused,
		startedAt,
		emails,
		params,
	};
}

test('dates a delayed payment from when it succeeded, not from when it started', async () => {
	const {repository, fulfilment, session, intentMetadata, startedAt} = await startPurchase(['Ada Lovelace'], 1);
	await fulfilment.handleCheckoutSessionCompleted(session);
	expect((await repository.findPurchase(session.id))?.purchased_at ?? null).toBeNull();

	const settledAt = new Date(startedAt.getTime() + 3 * DAY_MS);
	await fulfilment.handleAsyncPaymentSucceeded({...session, payment_status: 'paid'}, settledAt);
	const purchase = await repository.findPurchase(session.id);
	expect(purchase).toMatchObject({status: 'completed', purchased_at: settledAt});
	expect(intentMetadata.operator_pass_paid_at).toBe(settledAt.toISOString());
	const [certificate] = await repository.listCertificatesByPurchase(session.id);
	expect(certificate?.paid_at).toEqual(settledAt);
});

test('a delivery waits while another delivery to the same buyer is under way, so only one can count as the first pass', async () => {
	const {repository, fulfilment, session, deliverPurchase, startedAt} = await startPurchase(['Ada Lovelace'], 1);
	await fulfilment.handleCheckoutSessionCompleted(session);
	const paid = {...session, payment_status: 'paid'} as Stripe.Checkout.Session;
	const purchaserId = (await repository.findPurchase(session.id))!.purchaser_id;
	const lockKey = operatorPassHolderLockKey(purchaserId);
	const token = await getCacheService().acquireLock(lockKey, 60);
	expect(token).toBeTruthy();

	await expect(fulfilment.handleAsyncPaymentSucceeded(paid, startedAt)).rejects.toThrow('STRIPE_ERROR');
	expect(deliverPurchase).not.toHaveBeenCalled();
	expect((await repository.findPurchase(session.id))?.holder_had_pass ?? null).toBeNull();

	await getCacheService().releaseLock(lockKey, token!);
	await fulfilment.handleAsyncPaymentSucceeded(paid, startedAt);
	expect(deliverPurchase).toHaveBeenCalledOnce();
	expect((await repository.findPurchase(session.id))?.holder_had_pass).toBe(false);
});

test('refunds when the purchase row with the names was never written', async () => {
	const {repository, fulfilment, session, stripe, emails} = await startPurchase(['Ada Lovelace'], 1);
	await repository.deletePurchase((await repository.findPurchase(session.id))!);
	await fulfilment.handleCheckoutSessionCompleted({...session, payment_status: 'paid'});
	expect(await repository.findPurchase(session.id)).toMatchObject({
		status: 'duplicate_refunded',
		certificate_names: null,
	});
	expect(emails.listSentEmails()).toContainEqual(
		expect.objectContaining({type: 'operator_pass_purchase_refused', metadata: {}}),
	);
	expect(stripe.refunds.create).toHaveBeenCalledWith(
		expect.objectContaining({
			payment_intent: session.payment_intent,
			metadata: expect.objectContaining({rejection_reason: 'operator_pass_names_missing'}),
		}),
		expect.anything(),
	);
	expect(await repository.listCertificatesByPurchase(session.id)).toEqual([]);
});

test('keeps a refused purchase pending until the refusal email goes out', async () => {
	const {repository, fulfilment, session, stripe, emails} = await startPurchase(['Ada Lovelace'], 1);
	await repository.deletePurchase((await repository.findPurchase(session.id))!);
	const paid = {...session, payment_status: 'paid'} as Stripe.Checkout.Session;
	const send = vi.spyOn(emails, 'send').mockResolvedValueOnce(false);
	await expect(fulfilment.handleCheckoutSessionCompleted(paid)).rejects.toThrow();
	expect(await repository.findPurchase(session.id)).toMatchObject({status: 'pending'});

	await fulfilment.handleCheckoutSessionCompleted(paid);
	expect(send).toHaveBeenCalledTimes(2);
	expect(await repository.findPurchase(session.id)).toMatchObject({
		status: 'duplicate_refunded',
		certificate_names: null,
		wall_name: null,
	});
	expect(emails.listSentEmails()).toEqual([
		expect.objectContaining({type: 'operator_pass_purchase_refused', metadata: {}}),
	]);
	expect(new Set(vi.mocked(stripe.refunds.create).mock.calls.map(([, options]) => options?.idempotencyKey))).toEqual(
		new Set([`operator-pass-duplicate-refund:${session.id}`]),
	);
});

test('sends the refusal email once when marking the refused purchase fails after the email went out', async () => {
	const {repository, fulfilment, session, emails} = await startPurchase(['Ada Lovelace'], 1);
	await repository.deletePurchase((await repository.findPurchase(session.id))!);
	const paid = {...session, payment_status: 'paid'} as Stripe.Checkout.Session;
	const updatePurchase = repository.updatePurchase.bind(repository);
	vi.spyOn(repository, 'updatePurchase').mockImplementation(async (purchase, patch) => {
		if ('status' in patch) {
			vi.mocked(repository.updatePurchase).mockImplementation(updatePurchase);
			throw new Error('write failed');
		}
		return updatePurchase(purchase, patch);
	});
	await expect(fulfilment.handleCheckoutSessionCompleted(paid)).rejects.toThrow('write failed');

	await fulfilment.handleCheckoutSessionCompleted(paid);
	expect(await repository.findPurchase(session.id)).toMatchObject({status: 'duplicate_refunded'});
	expect(emails.listSentEmails()).toEqual([
		expect.objectContaining({type: 'operator_pass_purchase_refused', metadata: {}}),
	]);
});

test('a refusal refund webhook that arrives before the checkout retry is retried and leaves the refusal email as the only email', async () => {
	const {repository, fulfilment, session, stripe, emails} = await startPurchase(['Ada Lovelace'], 1);
	await repository.deletePurchase((await repository.findPurchase(session.id))!);
	const paid = {...session, payment_status: 'paid'} as Stripe.Checkout.Session;
	vi.spyOn(emails, 'send').mockResolvedValueOnce(false);
	await expect(fulfilment.handleCheckoutSessionCompleted(paid)).rejects.toThrow();
	const pending = (await repository.findPurchase(session.id))!;
	expect(pending.status).toBe('pending');
	const [refusalRefund] = (await stripe.refunds.list()).data;

	const refundCharge = {
		id: 'ch_test_refusal',
		refunded: true,
		amount_refunded: pending.amount_minor,
	} as Stripe.Charge;
	await expect(fulfilment.handleChargeRefunded(pending, refundCharge)).rejects.toThrow();
	expect(await repository.findPurchase(session.id)).toMatchObject({status: 'pending'});
	expect(await fulfilment.prepareRefundReceipt(pending, refusalRefund!)).toBeNull();

	await fulfilment.handleCheckoutSessionCompleted(paid);
	expect(await repository.findPurchase(session.id)).toMatchObject({
		status: 'duplicate_refunded',
		certificate_names: null,
		wall_name: null,
	});
	await fulfilment.handleChargeRefunded(pending, refundCharge);
	expect((await repository.findPurchase(session.id))?.status).toBe('duplicate_refunded');
	expect(await fulfilment.prepareRefundReceipt(pending, refusalRefund!)).toBeNull();
	expect(emails.listSentEmails()).toEqual([
		expect.objectContaining({type: 'operator_pass_purchase_refused', metadata: {}}),
	]);
});

test('a single pass can not be withdrawn while the payment is disputed, so the dispute is not accepted for it', async () => {
	const buyer = await createTestAccount(harness);
	const holder = await getUserRepository().findUniqueAssert(createUserID(BigInt(buyer.userId)));
	const purchase = {
		checkout_session_id: 'cs_test_disputed_unit',
		purchaser_id: holder.id,
		payment_intent_id: 'pi_test_disputed_unit',
		status: 'disputed',
		dispute_id: 'dp_test_disputed_unit',
		quantity: 3,
		amount_minor: 3000,
		withdrawal_days: 7,
		withdrawal_ends_at: new Date(Date.now() + 5 * DAY_MS),
	} as OperatorPassPurchaseRow;
	const unit = {
		pass_id: `${purchase.checkout_session_id}:0`,
		purchase_id: purchase.checkout_session_id,
		holder_id: holder.id,
		status: 'held',
		source: 'purchase',
	} as OperatorPassUnitRow;
	const repository = {
		findPurchase: vi.fn(async () => purchase),
		findUnit: vi.fn(async () => unit),
		listUnitsByPurchase: vi.fn(async () => [unit]),
	};
	const stripe = {
		paymentIntents: {retrieve: vi.fn(async () => ({latest_charge: {amount_refunded: 0}}))},
		refunds: {create: vi.fn()},
		disputes: {close: vi.fn()},
	};
	const fulfilment = new OperatorPassFulfilmentService(
		stripe as unknown as Stripe,
		repository as unknown as OperatorPassRepository,
		getUserRepository(),
		new TestEmailService(),
		getCacheService(),
		getKVClient(),
		{repository, wallService: {}, holdingService: {}, removalService: {}} as unknown as OperatorPassServices,
		null,
	);

	await expect(
		fulfilment.withdrawUnit({holder, passId: unit.pass_id, name: 'Ada Lovelace', receivedAt: new Date()}),
	).rejects.toBeInstanceOf(OperatorPassWithdrawalUnavailableError);
	expect(stripe.disputes.close).not.toHaveBeenCalled();
	expect(stripe.refunds.create).not.toHaveBeenCalled();
});

test('a failed delayed payment deletes the pending purchase with its names', async () => {
	const {repository, fulfilment, session} = await startPurchase(['Ada Lovelace'], 1);
	await fulfilment.handleCheckoutSessionCompleted(session);
	expect(await repository.findPurchase(session.id)).toMatchObject({status: 'pending', purchased_at: null});

	await fulfilment.handleAsyncPaymentFailed(session);
	expect(await repository.findPurchase(session.id)).toBeNull();
});

test('the fulfilment sweep delivers a paid purchase whose confirmation email failed the first time', async () => {
	const {repository, fulfilment, session, emails, deliverPurchase} = await startPurchase(['Ada Lovelace'], 1);
	deliverPurchase.mockImplementation((async ({
		purchase,
		deliveredAt,
	}: {
		purchase: OperatorPassPurchaseRow;
		deliveredAt: Date;
	}) => {
		await repository.createUnit({
			pass_id: `${purchase.checkout_session_id}:1`,
			purchase_id: purchase.checkout_session_id,
			buyer_id: purchase.purchaser_id,
			holder_id: purchase.purchaser_id,
			source: 'purchase',
			status: 'held',
			transferable: true,
			delivered_at: deliveredAt,
			first_delivered_at: deliveredAt,
			paused_at: null,
			ended_at: null,
			end_reason: null,
			withdrawal_country: null,
			withdrawal_days: null,
			version: 1,
		});
	}) as never);
	const paid = {...session, payment_status: 'paid'} as Stripe.Checkout.Session;
	const send = vi.spyOn(emails, 'send').mockResolvedValueOnce(false);
	await expect(fulfilment.handleCheckoutSessionCompleted(paid)).rejects.toThrow();
	expect(send).toHaveBeenCalledTimes(1);
	expect(await repository.findPurchase(session.id)).toMatchObject({status: 'pending', delivered_at: null});

	const kvClient = {
		acquireLock: async () => true,
		extendLock: async () => true,
		releaseLock: async () => true,
	} as unknown as IKVProvider;
	const sweep = (completedIds: ReadonlySet<string>, retrieveSession: () => Promise<Stripe.Checkout.Session>) =>
		runOperatorPassFulfilmentSweep({
			kvClient,
			now: () => Date.now() + 31 * 60 * 1000,
			repository: {
				scanPurchases: async () => ({rows: [(await repository.findPurchase(session.id))!], pageState: null}),
			},
			listCompletedSessionIds: async () => completedIds,
			retrieveSession,
			resolvePaidAt: async (_session, fallback) => fallback,
			checkout: fulfilment,
			recordDelivery: async () => {},
			requeueWithdrawalReceipts: async () => {},
		});
	const notRetrieved = vi.fn(async () => paid);
	expect(await sweep(new Set(), notRetrieved)).toEqual({ran: true, checked: 0, failed: 0});
	expect(notRetrieved).not.toHaveBeenCalled();
	const result = await sweep(new Set([session.id]), async () => paid);
	expect(result).toEqual({ran: true, checked: 1, failed: 0});
	const purchase = await repository.findPurchase(session.id);
	expect(purchase).toMatchObject({status: 'completed', wall_name: null});
	expect(purchase?.delivered_at).not.toBeNull();
	expect(purchase?.welcome_sent_at).not.toBeNull();
	expect(deliverPurchase).toHaveBeenCalledTimes(1);
	expect(emails.listSentEmails().map((email) => email.type)).toEqual(
		expect.arrayContaining(['operator_pass_purchase_confirmation', 'operator_pass_welcome']),
	);
	expect(emails.listSentEmails().find((email) => email.type === 'operator_pass_welcome')?.metadata).toMatchObject({
		certificate: expect.stringMatching(/\.pdf$/),
	});
	expect(await repository.listCertificatesByPurchase(session.id)).toHaveLength(1);
});

function sweepOnce(
	repository: OperatorPassRepository,
	fulfilment: OperatorPassFulfilmentService,
	session: Stripe.Checkout.Session,
) {
	const kvClient = {
		acquireLock: async () => true,
		extendLock: async () => true,
		releaseLock: async () => true,
	} as unknown as IKVProvider;
	return runOperatorPassFulfilmentSweep({
		kvClient,
		now: () => Date.now() + 31 * 60 * 1000,
		repository: {
			scanPurchases: async () => ({rows: [(await repository.findPurchase(session.id))!], pageState: null}),
		},
		listCompletedSessionIds: async () => new Set([session.id]),
		retrieveSession: async () => session,
		resolvePaidAt: async (_session, fallback) => fallback,
		checkout: fulfilment,
		recordDelivery: async () => {},
		requeueWithdrawalReceipts: async () => {},
	});
}

test('the fulfilment sweep does not deliver a payment that was refunded before the purchase was fulfilled', async () => {
	const {repository, fulfilment, session, stripe, emails, deliverPurchase} = await startPurchase(['Ada Lovelace'], 1);
	await fulfilment.handleCheckoutSessionCompleted(session);
	expect(await fulfilment.findPurchaseByPaymentIntent(session.payment_intent as string)).toBeNull();
	const pending = (await repository.findPurchase(session.id))!;
	vi.mocked(stripe.paymentIntents.retrieve).mockResolvedValue({
		latest_charge: {
			id: 'ch_test_refunded_before_fulfilment',
			created: Math.floor(Date.now() / 1000),
			refunded: true,
			amount_refunded: pending.amount_minor,
			disputed: false,
			payment_method_details: {type: 'card'},
		},
	} as never);

	const paid = {...session, payment_status: 'paid'} as Stripe.Checkout.Session;
	expect(await sweepOnce(repository, fulfilment, paid)).toEqual({ran: true, checked: 1, failed: 0});
	expect(await repository.findPurchase(session.id)).toMatchObject({status: 'refunded', delivered_at: null});
	expect(deliverPurchase).not.toHaveBeenCalled();
	expect(stripe.refunds.create).not.toHaveBeenCalled();
	expect(await repository.listCertificatesByPurchase(session.id)).toEqual([]);
	expect(emails.listSentEmails()).toEqual([]);
});

test('the fulfilment sweep pauses a pass whose payment was already disputed and holds back the welcome email', async () => {
	const {repository, fulfilment, session, stripe, emails, setPurchasePaused} = await startPurchase(['Ada Lovelace'], 1);
	await fulfilment.handleCheckoutSessionCompleted(session);
	vi.mocked(stripe.paymentIntents.retrieve).mockResolvedValue({
		latest_charge: {
			id: 'ch_test_disputed_before_fulfilment',
			created: Math.floor(Date.now() / 1000),
			refunded: false,
			amount_refunded: 0,
			disputed: true,
			payment_method_details: {type: 'card'},
		},
	} as never);
	const dispute = {
		id: 'dp_test_before_fulfilment',
		status: 'needs_response',
		reason: 'fraudulent',
		created: Math.floor(Date.now() / 1000),
	} as Stripe.Dispute;
	(stripe as unknown as {disputes: unknown}).disputes = {list: vi.fn(async () => ({data: [dispute]}))};

	const paid = {...session, payment_status: 'paid'} as Stripe.Checkout.Session;
	expect(await sweepOnce(repository, fulfilment, paid)).toEqual({ran: true, checked: 1, failed: 0});
	const purchase = await repository.findPurchase(session.id);
	expect(purchase).toMatchObject({status: 'disputed', dispute_id: dispute.id, dispute_status: 'needs_response'});
	expect(purchase?.welcome_sent_at ?? null).toBeNull();
	expect(setPurchasePaused).toHaveBeenCalledWith(expect.objectContaining({checkout_session_id: session.id}), true);
	expect(emails.listSentEmails().some((email) => email.type === 'operator_pass_welcome')).toBe(false);
});

test('a pass delivered while a dispute is open gets its certificate email once the dispute is released', async () => {
	const {repository, fulfilment, session, stripe, emails, deliverPurchase} = await startPurchase(['Ada Lovelace'], 1);
	deliverPurchase.mockImplementation((async ({
		purchase,
		deliveredAt,
	}: {
		purchase: OperatorPassPurchaseRow;
		deliveredAt: Date;
	}) => {
		await repository.createUnit({
			pass_id: `${purchase.checkout_session_id}:1`,
			purchase_id: purchase.checkout_session_id,
			buyer_id: purchase.purchaser_id,
			holder_id: purchase.purchaser_id,
			source: 'purchase',
			status: 'held',
			transferable: true,
			delivered_at: deliveredAt,
			first_delivered_at: deliveredAt,
			paused_at: null,
			ended_at: null,
			end_reason: null,
			withdrawal_country: null,
			withdrawal_days: null,
			version: 1,
		});
	}) as never);
	vi.mocked(stripe.paymentIntents.retrieve).mockResolvedValue({
		latest_charge: {
			id: 'ch_test_disputed_then_released',
			created: Math.floor(Date.now() / 1000),
			refunded: false,
			amount_refunded: 0,
			disputed: true,
			payment_method_details: {type: 'card'},
		},
	} as never);
	const dispute = {
		id: 'dp_test_released_after_delivery',
		status: 'needs_response',
		reason: 'fraudulent',
		created: Math.floor(Date.now() / 1000),
	} as Stripe.Dispute;
	(stripe as unknown as {disputes: unknown}).disputes = {list: vi.fn(async () => ({data: [dispute]}))};
	const paid = {...session, payment_status: 'paid'} as Stripe.Checkout.Session;
	const welcomes = () => emails.listSentEmails().filter((email) => email.type === 'operator_pass_welcome');

	await fulfilment.handleCheckoutSessionCompleted(paid);
	const paused = (await repository.findPurchase(session.id))!;
	expect(paused).toMatchObject({status: 'disputed'});
	expect(paused.delivered_at).not.toBeNull();
	expect(welcomes()).toHaveLength(0);

	const won = {...dispute, status: 'won'} as Stripe.Dispute;
	vi.spyOn(emails, 'send').mockResolvedValueOnce(false);
	await fulfilment.handleDisputeReleased(paused, won);
	expect(await repository.findPurchase(session.id)).toMatchObject({status: 'completed', welcome_sent_at: null});
	await fulfilment.handleDisputeReleased(paused, won);
	await fulfilment.handleCheckoutSessionCompleted(paid);
	const restored = await repository.findPurchase(session.id);
	expect(restored).toMatchObject({status: 'completed', dispute_status: 'won'});
	expect(restored?.welcome_sent_at).not.toBeNull();
	expect(welcomes()).toHaveLength(1);
	expect(welcomes()[0]?.metadata).toMatchObject({certificate: expect.stringMatching(/\.pdf$/)});
});

test('a purchase whose address bounced before delivery is refunded without a confirmation or a certificate', async () => {
	const {repository, fulfilment, session, stripe, emails} = await startPurchase(['Ada Lovelace'], 1);
	const purchaser = await getUserRepository().findUniqueAssert(
		(await repository.findPurchase(session.id))!.purchaser_id,
	);
	emails.bouncedEmails.add(purchaser.email!);
	await fulfilment.handleCheckoutSessionCompleted({...session, payment_status: 'paid'} as Stripe.Checkout.Session);
	expect(await repository.findPurchase(session.id)).toMatchObject({
		status: 'duplicate_refunded',
		certificate_names: null,
		delivered_at: null,
	});
	expect(stripe.refunds.create).toHaveBeenCalledWith(
		expect.objectContaining({
			metadata: expect.objectContaining({rejection_reason: 'operator_pass_email_undeliverable'}),
		}),
		expect.anything(),
	);
	expect(emails.listSentEmails()).toEqual([]);
	expect(await repository.listCertificatesByPurchase(session.id)).toEqual([]);
});

test('a certificate email to an address that bounced after delivery is marked skipped, not emailed, and not retried', async () => {
	const {repository, fulfilment, session, emails, deliverPurchase} = await startPurchase(['Ada Lovelace'], 1);
	deliverPurchase.mockImplementation((async ({
		purchase,
		deliveredAt,
	}: {
		purchase: OperatorPassPurchaseRow;
		deliveredAt: Date;
	}) => {
		await repository.createUnit({
			pass_id: `${purchase.checkout_session_id}:1`,
			purchase_id: purchase.checkout_session_id,
			buyer_id: purchase.purchaser_id,
			holder_id: purchase.purchaser_id,
			source: 'purchase',
			status: 'held',
			transferable: true,
			delivered_at: deliveredAt,
			first_delivered_at: deliveredAt,
			paused_at: null,
			ended_at: null,
			end_reason: null,
			withdrawal_country: null,
			withdrawal_days: null,
			version: 1,
		});
	}) as never);
	const paid = {...session, payment_status: 'paid'} as Stripe.Checkout.Session;
	const send = emails.send.bind(emails);
	vi.spyOn(emails, 'send').mockImplementation(async (...args) =>
		args[0] === 'operator_pass_welcome' ? false : Reflect.apply(send, emails, args),
	);
	await expect(fulfilment.handleCheckoutSessionCompleted(paid)).rejects.toThrow();
	const delivered = (await repository.findPurchase(session.id))!;
	expect(delivered).toMatchObject({status: 'completed', welcome_sent_at: null});

	const purchaser = await getUserRepository().findUniqueAssert(delivered.purchaser_id);
	emails.bouncedEmails.add(purchaser.email!);
	await fulfilment.handleCheckoutSessionCompleted(paid);
	const settled = (await repository.findPurchase(session.id))!;
	expect(settled.welcome_sent_at ?? null).toBeNull();
	expect(settled.welcome_skipped_at).not.toBeNull();
	expect(emails.listSentEmails().some((email) => email.type === 'operator_pass_welcome')).toBe(false);
	const response = mapOperatorPassPurchaseToResponse(
		settled,
		await repository.listUnitsByPurchase(session.id),
		new Date(),
		new Map(),
	);
	expect(response).toMatchObject({certificate_available: true, certificate_emailed: false});
	await fulfilment.handleCheckoutSessionCompleted(paid);
	expect(emails.listSentEmails().some((email) => email.type === 'operator_pass_welcome')).toBe(false);
});

test('the welcome email waits for the certificate file instead of going out without it', async () => {
	const {repository, fulfilment, session, emails, deliverPurchase} = await startPurchase(['Ada Lovelace'], 1);
	deliverPurchase.mockImplementation((async ({
		purchase,
		deliveredAt,
	}: {
		purchase: OperatorPassPurchaseRow;
		deliveredAt: Date;
	}) => {
		await repository.createUnit({
			pass_id: `${purchase.checkout_session_id}:1`,
			purchase_id: purchase.checkout_session_id,
			buyer_id: purchase.purchaser_id,
			holder_id: purchase.purchaser_id,
			source: 'purchase',
			status: 'held',
			transferable: true,
			delivered_at: deliveredAt,
			first_delivered_at: deliveredAt,
			paused_at: null,
			ended_at: null,
			end_reason: null,
			withdrawal_country: null,
			withdrawal_days: null,
			version: 1,
		});
	}) as never);
	const paid = {...session, payment_status: 'paid'} as Stripe.Checkout.Session;
	vi.spyOn(fulfilment.certificates, 'loadPurchaseFile').mockResolvedValueOnce(null);
	await expect(fulfilment.handleCheckoutSessionCompleted(paid)).rejects.toThrow();
	expect((await repository.findPurchase(session.id))?.welcome_sent_at ?? null).toBeNull();
	expect(emails.listSentEmails().some((email) => email.type === 'operator_pass_welcome')).toBe(false);

	await fulfilment.handleCheckoutSessionCompleted(paid);
	expect((await repository.findPurchase(session.id))?.welcome_sent_at).not.toBeNull();
	expect(emails.listSentEmails().find((email) => email.type === 'operator_pass_welcome')?.metadata).toMatchObject({
		certificate: expect.stringMatching(/\.pdf$/),
	});
});

test('the fulfilment sweep lists Stripe sessions only when a pending purchase is waiting, from its creation time', async () => {
	const {repository, fulfilment, session} = await startPurchase(['Ada Lovelace'], 1);
	await fulfilment.handleCheckoutSessionCompleted(session);
	const pending = (await repository.findPurchase(session.id))!;
	expect(pending.status).toBe('pending');

	const kvClient = {
		acquireLock: async () => true,
		extendLock: async () => true,
		releaseLock: async () => true,
	} as unknown as IKVProvider;
	const listCompletedSessionIds = vi.fn(async (_createdSince: Date) => new Set<string>());
	const retrieveSession = vi.fn(async () => session);
	const sweep = (offsetMs: number) =>
		runOperatorPassFulfilmentSweep({
			kvClient,
			now: () => pending.created_at.getTime() + offsetMs,
			repository: {scanPurchases: async () => ({rows: [pending], pageState: null})},
			listCompletedSessionIds,
			retrieveSession,
			resolvePaidAt: async (_session, fallback) => fallback,
			checkout: fulfilment,
			recordDelivery: async () => {},
			requeueWithdrawalReceipts: async () => {},
		});

	expect(await sweep(5 * 60 * 1000)).toEqual({ran: true, checked: 0, failed: 0});
	expect(listCompletedSessionIds).not.toHaveBeenCalled();

	expect(await sweep(31 * 60 * 1000)).toEqual({ran: true, checked: 0, failed: 0});
	expect(listCompletedSessionIds).toHaveBeenCalledTimes(1);
	expect(listCompletedSessionIds.mock.calls[0][0]).toEqual(new Date(pending.created_at.getTime() - 10 * 60 * 1000));
	expect(retrieveSession).not.toHaveBeenCalled();
});

test('the fulfilment sweep alerts once for a paid purchase that never got far enough to record its payment, and once for a lapsed certificate email', async () => {
	const createdAt = new Date('2026-11-02T10:00:00Z');
	const stuck = {
		checkout_session_id: 'cs_test_sweep_stuck',
		status: 'pending',
		created_at: createdAt,
		purchased_at: null,
		delivered_at: null,
	} as unknown as OperatorPassPurchaseRow;
	const lapsed = {
		checkout_session_id: 'cs_test_sweep_lapsed',
		status: 'completed',
		created_at: createdAt,
		completed_at: createdAt,
		purchased_at: createdAt,
		delivered_at: createdAt,
		welcome_sent_at: null,
		welcome_skipped_at: null,
	} as unknown as OperatorPassPurchaseRow;
	const markers = new Set<string>();
	const kvClient = {
		acquireLock: async () => true,
		extendLock: async () => true,
		releaseLock: async () => true,
		setnx: async (key: string) => !markers.has(key) && Boolean(markers.add(key)),
	} as unknown as IKVProvider;
	const handleAsyncPaymentSucceeded = vi.fn(async () => {
		throw new Error('The purchaser was not found');
	});
	const sweep = () =>
		runOperatorPassFulfilmentSweep({
			kvClient,
			now: () => createdAt.getTime() + 8 * DAY_MS,
			repository: {scanPurchases: async () => ({rows: [stuck, lapsed], pageState: null})},
			listCompletedSessionIds: async () => new Set<string>(),
			retrieveSession: async () => ({payment_status: 'paid'}) as Stripe.Checkout.Session,
			resolvePaidAt: async (_session, fallback) => fallback,
			checkout: {
				handleAsyncPaymentSucceeded,
				handleCheckoutSessionExpired: async () => {},
				handleAsyncPaymentFailed: async () => {},
			},
			recordDelivery: async () => {},
			requeueWithdrawalReceipts: async () => {},
		});
	loggerError.mockClear();
	expect(await sweep()).toEqual({ran: true, checked: 1, failed: 1});
	await sweep();
	const messages = loggerError.mock.calls.map((call) => call.at(-1));
	expect(
		messages.filter((message) => message === 'Operator Pass purchase is still not delivered 24 hours after payment'),
	).toHaveLength(2);
	expect(
		messages.filter(
			(message) => message === 'Operator Pass certificate email was never sent, the sweep stopped retrying it',
		),
	).toHaveLength(1);
});

test('counts the Brazil withdrawal days from delivery when delivery crosses midnight after payment', async () => {
	const {repository, fulfilment, session, emails} = await startPurchase(['Ada Lovelace'], 1, 'US');
	const clockOffsetMs = Config.operatorPass.clockOffsetMs;
	const paidAt = new Date('2026-11-10T21:50:00-03:00');
	const deliveredAt = new Date('2026-11-11T00:20:00-03:00');
	const brazilian = {
		...session,
		payment_status: 'paid',
		customer_details: {address: {country: 'BR'}, business_name: null, tax_ids: []},
	} as unknown as Stripe.Checkout.Session;
	try {
		Config.operatorPass.clockOffsetMs = paidAt.getTime() + 60_000 - Date.now();
		vi.spyOn(emails, 'send').mockResolvedValueOnce(false);
		await expect(fulfilment.handleAsyncPaymentSucceeded(brazilian, paidAt)).rejects.toThrow();
		const recorded = await repository.findPurchase(session.id);
		expect(recorded).toMatchObject({status: 'pending', purchased_at: paidAt, withdrawal_days: 7, delivered_at: null});
		expect(getOperatorPassWithdrawalEndsOn(recorded!.withdrawal_ends_at!)).toBe('2026-11-17');

		Config.operatorPass.clockOffsetMs = deliveredAt.getTime() - Date.now();
		await fulfilment.handleAsyncPaymentSucceeded(brazilian, paidAt);
		const purchase = await repository.findPurchase(session.id);
		expect(purchase).toMatchObject({status: 'completed', purchased_at: paidAt});
		expect(getOperatorPassWithdrawalEndsOn(purchase!.withdrawal_ends_at!)).toBe('2026-11-18');
		expect(getOperatorPassWithdrawalDeadline(purchase!)).toEqual(new Date('2026-11-19T00:00:00-05:00'));
		expect(emails.listSentEmails()).toContainEqual(
			expect.objectContaining({
				type: 'operator_pass_purchase_confirmation',
				metadata: expect.objectContaining({withdrawal_ends_on: '2026-11-18'}),
			}),
		);
	} finally {
		Config.operatorPass.clockOffsetMs = clockOffsetMs;
	}
});

function mockedWithdrawal(purchase: OperatorPassPurchaseRow, units: Array<OperatorPassUnitRow>) {
	const updatePurchase = vi.fn(async (row: OperatorPassPurchaseRow, _update: Record<string, unknown>) => row);
	const repository = {
		findPurchase: vi.fn(async () => purchase),
		listUnitsByPurchase: vi.fn(async () => units),
		listUnitEvents: vi.fn(async () => [] as Array<unknown>),
		updatePurchase,
	};
	const endPurchaseUnits = vi.fn(
		async (
			_purchase: OperatorPassPurchaseRow,
			cause: string,
			status: string,
			_limit?: number,
			only: Set<string> | null = null,
		) => {
			for (const unit of units) {
				if (unit.status === 'held' && (only === null || only.has(unit.pass_id))) {
					unit.status = status as OperatorPassUnitRow['status'];
					unit.end_reason = cause;
				}
			}
			return [];
		},
	);
	const stripe = {
		paymentIntents: {retrieve: vi.fn(async () => ({latest_charge: {amount_refunded: 0}}))},
		refunds: {create: vi.fn(async () => ({id: 're_test_withdrawal', object: 'refund'}))},
		checkout: {sessions: {retrieve: vi.fn(async () => ({customer_details: {email: 'checkout@example.com'}}))}},
	};
	const emails = new TestEmailService();
	const fulfilment = new OperatorPassFulfilmentService(
		stripe as unknown as Stripe,
		repository as unknown as OperatorPassRepository,
		getUserRepository(),
		emails,
		getCacheService(),
		getKVClient(),
		{
			repository,
			wallService: {getEntry: async () => null},
			holdingService: {endPurchaseUnits, queries: {listHeldUnits: async () => []}},
			removalService: {},
		} as unknown as OperatorPassServices,
		createBillingDocumentService(stripe as unknown as Stripe),
	);
	vi.spyOn(fulfilment.certificates, 'syncCancellations').mockResolvedValue(undefined as never);
	const jobs: Array<{task: string; payload: Record<string, unknown>; jobKey: string | undefined}> = [];
	setInjectedWorkerService({
		addJob: async (task: string, payload: Record<string, unknown>, options?: {jobKey?: string}) => {
			jobs.push({task, payload, jobKey: options?.jobKey});
			return 1n;
		},
		cancelJob: async () => false,
	} as never);
	return {fulfilment, repository, stripe, emails, endPurchaseUnits, updatePurchase, jobs};
}

test('the withdrawal receipt is queued and sent again when the first send fails', async () => {
	const account = await createTestAccount(harness);
	const purchaser = await getUserRepository().findUniqueAssert(createUserID(BigInt(account.userId)));
	const receivedAt = new Date();
	const purchase = {
		checkout_session_id: `cs_test_receipt_${account.userId}`,
		purchaser_id: purchaser.id,
		payment_intent_id: 'pi_test_receipt',
		invoice_id: 'in_test_receipt',
		status: 'pending',
		quantity: 1,
		amount_minor: 1000,
		currency: 'eur',
		purchased_at: new Date(receivedAt.getTime() - DAY_MS),
		created_at: new Date(receivedAt.getTime() - DAY_MS),
		withdrawal_days: null,
		withdrawal_ends_at: new Date(receivedAt.getTime() + 13 * DAY_MS),
	} as OperatorPassPurchaseRow;
	const {fulfilment, stripe, emails, jobs} = mockedWithdrawal(purchase, []);
	Object.assign(stripe.refunds, {
		list: vi.fn(async () => ({data: []})),
		retrieve: vi.fn(async (id: string) => ({id, status: 'succeeded', metadata: {}})),
	});
	try {
		await fulfilment.withdraw({
			purchaser,
			purchaseId: purchase.checkout_session_id,
			name: 'Ada',
			channel: 'button',
			receivedAt,
		});
		expect(jobs).toHaveLength(1);
		expect(jobs[0]).toMatchObject({
			task: OPERATOR_PASS_WITHDRAWAL_RECEIPT_TASK,
			jobKey: `withdrawal-receipt:${purchase.checkout_session_id}:all:${receivedAt.toISOString()}`,
		});
		const receipt = decodeOperatorPassWithdrawalReceipt(jobs[0]!.payload);
		expect(receipt).toMatchObject({
			purchaseId: purchase.checkout_session_id,
			passId: null,
			amountMinor: 1000,
			receivedAt,
			passCount: 1,
			refundId: 're_test_withdrawal',
		});

		vi.spyOn(emails, 'send').mockResolvedValueOnce(false);
		await expect(fulfilment.sendWithdrawalReceipt(receipt)).rejects.toThrow();
		await fulfilment.sendWithdrawalReceipt(receipt);
		const sent = emails.listSentEmails().filter((email) => email.type === 'operator_pass_withdrawal_receipt');
		expect(sent).toHaveLength(1);
	} finally {
		setInjectedWorkerService(new NoopWorkerService());
	}
});

test('a withdrawal that accepts a dispute and then fails is finished by the retry with one close and one receipt', async () => {
	const account = await createTestAccount(harness);
	const purchaser = await getUserRepository().findUniqueAssert(createUserID(BigInt(account.userId)));
	const receivedAt = new Date();
	const purchase = {
		checkout_session_id: `cs_test_dispute_withdrawal_${account.userId}`,
		purchaser_id: purchaser.id,
		payment_intent_id: 'pi_test_dispute_withdrawal',
		status: 'disputed',
		dispute_id: `dp_test_dispute_withdrawal_${account.userId}`,
		quantity: 1,
		amount_minor: 2000,
		currency: 'eur',
		purchased_at: new Date(receivedAt.getTime() - DAY_MS),
		created_at: new Date(receivedAt.getTime() - DAY_MS),
		withdrawal_days: null,
		withdrawal_ends_at: new Date(receivedAt.getTime() + 13 * DAY_MS),
	} as OperatorPassPurchaseRow;
	const {fulfilment, stripe, endPurchaseUnits, updatePurchase, jobs} = mockedWithdrawal(purchase, []);
	stripe.refunds.create.mockRejectedValue(Object.assign(new Error('charge disputed'), {code: 'charge_disputed'}));
	const disputes = {
		retrieve: vi.fn(async (id: string) => ({id, status: 'needs_response', amount: 2000})),
		close: vi.fn(async (id: string) => ({id, status: 'lost', amount: 2000})),
	};
	Object.assign(stripe, {disputes});
	endPurchaseUnits.mockRejectedValueOnce(new Error('unit write failed'));
	try {
		await expect(
			fulfilment.withdraw({
				purchaser,
				purchaseId: purchase.checkout_session_id,
				name: 'Ada',
				channel: 'button',
				receivedAt,
			}),
		).rejects.toThrow('unit write failed');
		expect(disputes.close).toHaveBeenCalledTimes(1);
		expect(updatePurchase).not.toHaveBeenCalled();

		purchase.status = 'dispute_lost';
		const retriedAt = new Date(receivedAt.getTime() + 60_000);
		await fulfilment.withdraw({
			purchaser,
			purchaseId: purchase.checkout_session_id,
			name: 'Ada',
			channel: 'button',
			receivedAt: retriedAt,
		});
		expect(disputes.close).toHaveBeenCalledTimes(1);
		expect(updatePurchase).toHaveBeenCalledTimes(1);
		expect(updatePurchase.mock.calls[0]![1]).toMatchObject({
			status: Db.set('withdrawn'),
			withdrawn_at: Db.set(receivedAt),
			withdrawal_refund_id: Db.set(null),
		});
		expect(jobs).toHaveLength(1);
		expect(decodeOperatorPassWithdrawalReceipt(jobs[0]!.payload)).toMatchObject({
			purchaseId: purchase.checkout_session_id,
			receivedAt,
			amountMinor: 2000,
			passCount: 1,
			refund: 'dispute',
			refundId: null,
		});
	} finally {
		setInjectedWorkerService(new NoopWorkerService());
	}
});

test('a withdrawal receipt stays owed until it is sent, even when Stripe fails on every attempt', async () => {
	const account = await createTestAccount(harness);
	const purchaser = await getUserRepository().findUniqueAssert(createUserID(BigInt(account.userId)));
	const receivedAt = new Date();
	const purchase = {
		checkout_session_id: `cs_test_receipt_owed_${account.userId}`,
		purchaser_id: purchaser.id,
		payment_intent_id: 'pi_test_receipt_owed',
		invoice_id: 'in_test_receipt_owed',
		status: 'pending',
		quantity: 1,
		amount_minor: 1000,
		currency: 'eur',
		purchased_at: new Date(receivedAt.getTime() - DAY_MS),
		created_at: new Date(receivedAt.getTime() - DAY_MS),
		withdrawal_days: null,
		withdrawal_ends_at: new Date(receivedAt.getTime() + 13 * DAY_MS),
	} as OperatorPassPurchaseRow;
	const {fulfilment, repository, stripe, emails, jobs} = mockedWithdrawal(purchase, []);
	repository.updatePurchase.mockImplementation(async (_row, patch) => {
		for (const [column, op] of Object.entries(patch) as Array<[string, {kind: string; value?: unknown}]>) {
			Object.assign(purchase, {[column]: op.kind === 'set' ? op.value : null});
		}
		return purchase;
	});
	Object.assign(stripe.refunds, {
		list: vi.fn(async () => {
			throw new Error('Stripe is unavailable');
		}),
		retrieve: vi.fn(async () => {
			throw new Error('Stripe is unavailable');
		}),
	});
	try {
		await fulfilment.withdraw({
			purchaser,
			purchaseId: purchase.checkout_session_id,
			name: 'Ada',
			channel: 'button',
			receivedAt,
		});
		const key = `withdrawal-receipt:${purchase.checkout_session_id}:all:${receivedAt.toISOString()}`;
		expect([...(purchase.withdrawal_receipts_owed?.keys() ?? [])]).toEqual([key]);
		const receipt = decodeOperatorPassWithdrawalReceipt(jobs[0]!.payload);

		await expect(fulfilment.sendWithdrawalReceipt(receipt, true)).rejects.toThrow('Stripe is unavailable');
		expect(purchase.withdrawal_receipts_owed?.has(key)).toBe(true);
		await fulfilment.requeueOwedWithdrawalReceipts(purchase, new Date(receivedAt.getTime() + 5 * 60 * 1000));
		expect(jobs).toHaveLength(1);
		await fulfilment.requeueOwedWithdrawalReceipts(purchase, new Date(receivedAt.getTime() + 11 * 60 * 1000));
		expect(jobs).toHaveLength(2);
		expect(jobs[1]).toMatchObject({task: OPERATOR_PASS_WITHDRAWAL_RECEIPT_TASK, jobKey: key});

		await fulfilment.sendWithdrawalReceipt(decodeOperatorPassWithdrawalReceipt(jobs[1]!.payload), false);
		await fulfilment.sendWithdrawalReceipt(receipt, false);
		const sent = emails.listSentEmails().filter((email) => email.type === 'operator_pass_withdrawal_receipt');
		expect(sent).toHaveLength(1);
		expect(sent[0]!.metadata).toMatchObject({credit_note: '', credit_note_pdf: ''});
		expect(purchase.withdrawal_receipts_owed ?? null).toBeNull();
		expect(jobs.slice(2)).toMatchObject([{task: 'sendBillingEmail', payload: {kind: 'refund'}}]);
		await fulfilment.requeueOwedWithdrawalReceipts(purchase, new Date(receivedAt.getTime() + 11 * 60 * 1000));
		expect(jobs).toHaveLength(3);
	} finally {
		setInjectedWorkerService(new NoopWorkerService());
	}
});

test('a withdrawal receipt is retried for a week from when support recorded it, then dropped with one error', async () => {
	const account = await createTestAccount(harness);
	const purchaser = await getUserRepository().findUniqueAssert(createUserID(BigInt(account.userId)));
	const recordedAt = new Date();
	const receivedAt = new Date(recordedAt.getTime() - 10 * DAY_MS);
	const purchase = {
		checkout_session_id: `cs_test_receipt_late_${account.userId}`,
		purchaser_id: purchaser.id,
		payment_intent_id: 'pi_test_receipt_late',
		invoice_id: null,
		status: 'pending',
		quantity: 1,
		amount_minor: 1000,
		currency: 'eur',
		purchased_at: new Date(receivedAt.getTime() - DAY_MS),
		created_at: new Date(receivedAt.getTime() - DAY_MS),
		withdrawal_days: null,
		withdrawal_ends_at: new Date(recordedAt.getTime() + 3 * DAY_MS),
	} as OperatorPassPurchaseRow;
	const {fulfilment, repository, jobs} = mockedWithdrawal(purchase, []);
	repository.updatePurchase.mockImplementation(async (_row, patch) => {
		for (const [column, op] of Object.entries(patch) as Array<[string, {kind: string; value?: unknown}]>) {
			Object.assign(purchase, {[column]: op.kind === 'set' ? op.value : null});
		}
		return purchase;
	});
	try {
		await fulfilment.withdraw({
			purchaser,
			purchaseId: purchase.checkout_session_id,
			name: 'Ada',
			channel: 'email',
			receivedAt,
		});
		expect(jobs).toHaveLength(1);
		loggerError.mockClear();
		await fulfilment.requeueOwedWithdrawalReceipts(purchase, new Date(recordedAt.getTime() + 11 * 60 * 1000));
		expect(jobs).toHaveLength(2);
		expect(loggerError).not.toHaveBeenCalled();

		await fulfilment.requeueOwedWithdrawalReceipts(purchase, new Date(recordedAt.getTime() + 8 * DAY_MS));
		expect(jobs).toHaveLength(2);
		expect(loggerError).toHaveBeenCalledOnce();
		expect(purchase.withdrawal_receipts_owed ?? null).toBeNull();
	} finally {
		setInjectedWorkerService(new NoopWorkerService());
	}
});

test('a Brazil withdrawal received on time refunds a pass that moved to a friend after it was sent', async () => {
	const account = await createTestAccount(harness);
	const purchaser = await getUserRepository().findUniqueAssert(createUserID(BigInt(account.userId)));
	const recipient = await createTestAccount(harness);
	const deliveredAt = new Date('2026-11-10T15:00:00Z');
	const purchase = {
		checkout_session_id: `cs_test_late_move_${account.userId}`,
		purchaser_id: purchaser.id,
		payment_intent_id: 'pi_test_late_move',
		status: 'completed',
		quantity: 1,
		amount_minor: 1000,
		currency: 'brl',
		purchased_at: deliveredAt,
		delivered_at: deliveredAt,
		created_at: deliveredAt,
		withdrawal_country: 'BR',
		withdrawal_days: 7,
		withdrawal_ends_at: getOperatorPassWithdrawalEndsAt(deliveredAt, 7),
	} as OperatorPassPurchaseRow;
	const deadline = getOperatorPassWithdrawalDeadline(purchase)!;
	const receivedAt = new Date(deadline.getTime() - DAY_MS);
	const movedAt = new Date(deadline.getTime() + 60_000);
	const unit = {
		pass_id: `${purchase.checkout_session_id}:0`,
		purchase_id: purchase.checkout_session_id,
		buyer_id: purchaser.id,
		holder_id: createUserID(BigInt(recipient.userId)),
		status: 'held',
		source: 'transfer',
	} as OperatorPassUnitRow;
	const {fulfilment, repository, stripe, endPurchaseUnits, updatePurchase, jobs} = mockedWithdrawal(purchase, [unit]);
	repository.listUnitEvents.mockResolvedValue([
		{pass_id: unit.pass_id, kind: 'transferred', sender_id: purchaser.id, occurred_at: movedAt},
	]);
	try {
		await expect(
			fulfilment.withdraw({
				purchaser,
				purchaseId: purchase.checkout_session_id,
				name: 'Ada',
				channel: 'button',
				receivedAt,
			}),
		).rejects.toBeInstanceOf(OperatorPassWithdrawalUnavailableError);

		await fulfilment.withdraw({
			purchaser,
			purchaseId: purchase.checkout_session_id,
			name: 'Ada',
			channel: 'email',
			receivedAt,
		});
		const scope = `operator_pass_withdrawal:all:${receivedAt.toISOString()}`;
		expect(stripe.refunds.create).toHaveBeenCalledWith(
			expect.objectContaining({amount: 1000, metadata: expect.objectContaining({rejection_reason: scope})}),
			{idempotencyKey: `operator-pass-${scope}:${purchase.checkout_session_id}`},
		);
		expect(endPurchaseUnits).toHaveBeenCalledWith(
			purchase,
			'withdrawal',
			'revoked',
			Number.POSITIVE_INFINITY,
			new Set([unit.pass_id]),
		);
		expect(unit.status).toBe('revoked');
		expect(updatePurchase.mock.calls.at(-1)![1]).toMatchObject({status: Db.set('withdrawn')});
		expect(decodeOperatorPassWithdrawalReceipt(jobs[0]!.payload)).toMatchObject({amountMinor: 1000});
	} finally {
		setInjectedWorkerService(new NoopWorkerService());
	}
});

test('a withdrawal sent to support before delivery is refunded in full even when delivery came before support recorded it', async () => {
	const account = await createTestAccount(harness);
	const purchaser = await getUserRepository().findUniqueAssert(createUserID(BigInt(account.userId)));
	const recipient = await createTestAccount(harness);
	const receivedAt = new Date(Date.now() - 2 * 60 * 60 * 1000);
	const deliveredAt = new Date(receivedAt.getTime() + 60 * 60 * 1000);
	const purchase = {
		checkout_session_id: `cs_test_late_delivery_${account.userId}`,
		purchaser_id: purchaser.id,
		payment_intent_id: 'pi_test_late_delivery',
		status: 'completed',
		quantity: 2,
		amount_minor: 2000,
		currency: 'eur',
		purchased_at: new Date(receivedAt.getTime() - DAY_MS),
		created_at: new Date(receivedAt.getTime() - DAY_MS),
		delivered_at: deliveredAt,
		perks_granted_at: deliveredAt,
		withdrawal_days: null,
		withdrawal_ends_at: getOperatorPassWithdrawalEndsAt(new Date(receivedAt.getTime() - DAY_MS), null),
	} as OperatorPassPurchaseRow;
	const units = [
		{holder_id: purchaser.id, source: 'purchase'},
		{holder_id: createUserID(BigInt(recipient.userId)), source: 'transfer'},
	].map(
		({holder_id, source}, index) =>
			({
				pass_id: `${purchase.checkout_session_id}:${index}`,
				purchase_id: purchase.checkout_session_id,
				buyer_id: purchaser.id,
				holder_id,
				status: 'held',
				source,
				end_reason: null,
			}) as OperatorPassUnitRow,
	);
	const {fulfilment, stripe, endPurchaseUnits, updatePurchase, jobs} = mockedWithdrawal(purchase, units);
	try {
		await expect(
			fulfilment.withdraw({
				purchaser,
				purchaseId: purchase.checkout_session_id,
				name: 'Ada',
				channel: 'button',
				receivedAt: new Date(),
			}),
		).rejects.toBeInstanceOf(OperatorPassWithdrawalUnavailableError);

		await fulfilment.withdraw({
			purchaser,
			purchaseId: purchase.checkout_session_id,
			name: 'Ada',
			channel: 'email',
			receivedAt,
		});
		expect(stripe.refunds.create).toHaveBeenCalledWith(expect.objectContaining({amount: 2000}), expect.anything());
		expect(endPurchaseUnits).toHaveBeenCalledWith(
			purchase,
			'withdrawal',
			'withdrawn',
			Number.POSITIVE_INFINITY,
			new Set([units[0]!.pass_id]),
		);
		expect(endPurchaseUnits).toHaveBeenCalledWith(
			purchase,
			'withdrawal',
			'revoked',
			Number.POSITIVE_INFINITY,
			new Set([units[1]!.pass_id]),
		);
		expect(units.map((unit) => unit.status)).toEqual(['withdrawn', 'revoked']);
		expect(updatePurchase.mock.calls.at(-1)![1]).toMatchObject({
			status: Db.set('withdrawn'),
			withdrawn_at: Db.set(receivedAt),
		});
		expect(decodeOperatorPassWithdrawalReceipt(jobs[0]!.payload)).toMatchObject({amountMinor: 2000, receivedAt});
	} finally {
		setInjectedWorkerService(new NoopWorkerService());
	}
});

test('a withdrawal sent to support after delivery is still refused', async () => {
	const account = await createTestAccount(harness);
	const purchaser = await getUserRepository().findUniqueAssert(createUserID(BigInt(account.userId)));
	const deliveredAt = new Date(Date.now() - 2 * 60 * 60 * 1000);
	const purchase = {
		checkout_session_id: `cs_test_after_delivery_${account.userId}`,
		purchaser_id: purchaser.id,
		payment_intent_id: 'pi_test_after_delivery',
		status: 'completed',
		quantity: 1,
		amount_minor: 1000,
		currency: 'eur',
		purchased_at: new Date(deliveredAt.getTime() - 60_000),
		created_at: new Date(deliveredAt.getTime() - 60_000),
		delivered_at: deliveredAt,
		perks_granted_at: deliveredAt,
		withdrawal_days: null,
		withdrawal_ends_at: getOperatorPassWithdrawalEndsAt(new Date(deliveredAt.getTime() - 60_000), null),
	} as OperatorPassPurchaseRow;
	const unit = {
		pass_id: `${purchase.checkout_session_id}:0`,
		purchase_id: purchase.checkout_session_id,
		buyer_id: purchaser.id,
		holder_id: purchaser.id,
		status: 'held',
		source: 'purchase',
		end_reason: null,
	} as OperatorPassUnitRow;
	const {fulfilment, stripe} = mockedWithdrawal(purchase, [unit]);
	try {
		await expect(
			fulfilment.withdraw({
				purchaser,
				purchaseId: purchase.checkout_session_id,
				name: 'Ada',
				channel: 'email',
				receivedAt: new Date(deliveredAt.getTime() + 60 * 60 * 1000),
			}),
		).rejects.toBeInstanceOf(OperatorPassWithdrawalUnavailableError);
		expect(stripe.refunds.create).not.toHaveBeenCalled();
		expect(unit.status).toBe('held');
	} finally {
		setInjectedWorkerService(new NoopWorkerService());
	}
});

test('a withdrawal recorded after support refunded the payment elsewhere gets a receipt for the amount returned', async () => {
	const account = await createTestAccount(harness);
	const purchaser = await getUserRepository().findUniqueAssert(createUserID(BigInt(account.userId)));
	const deliveredAt = new Date(Date.now() - DAY_MS);
	const purchase = {
		checkout_session_id: `cs_test_refunded_first_${account.userId}`,
		purchaser_id: purchaser.id,
		payment_intent_id: 'pi_test_refunded_first',
		status: 'completed',
		quantity: 2,
		amount_minor: 36000,
		currency: 'brl',
		purchased_at: deliveredAt,
		delivered_at: deliveredAt,
		created_at: deliveredAt,
		withdrawal_country: 'BR',
		withdrawal_days: 7,
		withdrawal_ends_at: getOperatorPassWithdrawalEndsAt(deliveredAt, 7),
	} as OperatorPassPurchaseRow;
	const units = [0, 1].map(
		(index) =>
			({
				pass_id: `${purchase.checkout_session_id}:${index}`,
				purchase_id: purchase.checkout_session_id,
				buyer_id: purchaser.id,
				holder_id: purchaser.id,
				status: 'held',
				source: 'purchase',
				end_reason: null,
			}) as OperatorPassUnitRow,
	);
	const {fulfilment, stripe, updatePurchase, jobs} = mockedWithdrawal(purchase, units);
	updatePurchase.mockImplementation(async (row, update) => {
		const status = update.status as {kind: string; value: OperatorPassPurchaseRow['status']} | undefined;
		if (status?.kind === 'set') row.status = status.value;
		return row;
	});
	stripe.paymentIntents.retrieve.mockResolvedValue({
		latest_charge: {id: 'ch_test_refunded_first', refunded: true, amount_refunded: 36000},
	} as never);
	try {
		await fulfilment.withdraw({
			purchaser,
			purchaseId: purchase.checkout_session_id,
			name: 'Ada',
			channel: 'email',
			receivedAt: new Date(),
		});
		expect(units.map((unit) => unit.end_reason)).toEqual(['refund', 'refund']);
		expect(stripe.refunds.create).not.toHaveBeenCalled();
		expect(jobs).toHaveLength(1);
		expect(decodeOperatorPassWithdrawalReceipt(jobs[0]!.payload)).toMatchObject({
			amountMinor: purchase.amount_minor,
			refund: 'already',
		});
	} finally {
		setInjectedWorkerService(new NoopWorkerService());
	}
});

test('a new certificate copy is refused once the pass ended in a withdrawal, even before the pages are cancelled', async () => {
	const {repository, fulfilment, session} = await startPurchase(['Ada Lovelace'], 1);
	await fulfilment.handleCheckoutSessionCompleted({...session, payment_status: 'paid'} as Stripe.Checkout.Session);
	const purchase = (await repository.findPurchase(session.id))!;
	const passId = `${session.id}:1`;
	const deliveredAt = purchase.delivered_at ?? new Date();
	await repository.createUnit({
		pass_id: passId,
		purchase_id: session.id,
		buyer_id: purchase.purchaser_id,
		holder_id: purchase.purchaser_id,
		source: 'purchase',
		status: 'held',
		transferable: false,
		delivered_at: deliveredAt,
		first_delivered_at: deliveredAt,
		paused_at: null,
		ended_at: null,
		end_reason: null,
		withdrawal_country: null,
		withdrawal_days: null,
		version: 1,
	});
	const copy = await fulfilment.reissueCertificatePage({passId, holderName: 'Ada King', reason: 'name_change'});
	expect(copy.version).toBe(2);

	const unit = (await repository.findUnit(passId))!;
	await repository.updateUnit(unit, {
		status: Db.set('withdrawn'),
		ended_at: Db.set(new Date()),
		end_reason: Db.set('withdrawal'),
	});
	await expect(
		fulfilment.reissueCertificatePage({passId, holderName: 'Ada Byron', reason: 'name_change'}),
	).rejects.toBeInstanceOf(InputValidationError);
	expect((await repository.listCertificateVersions(passId)).map((row) => row.version)).toEqual([1, 2]);
	expect(await fulfilment.certificates.loadPurchaseFile((await repository.findPurchase(session.id))!)).toBeNull();
});

test('a buyer can download one page of a three-name purchase, and another account cannot', async () => {
	const {repository, fulfilment, session} = await startPurchase(['Ana Souza', 'Bruno Lima', 'Ada Lovelace'], 3);
	await fulfilment.handleCheckoutSessionCompleted({...session, payment_status: 'paid'} as Stripe.Checkout.Session);
	const purchase = (await repository.findPurchase(session.id))!;
	const deliveredAt = purchase.delivered_at ?? new Date();
	for (const index of [1, 2, 3]) {
		await repository.createUnit({
			pass_id: `${session.id}:${index}`,
			purchase_id: session.id,
			buyer_id: purchase.purchaser_id,
			holder_id: purchase.purchaser_id,
			source: 'purchase',
			status: 'held',
			transferable: false,
			delivered_at: deliveredAt,
			first_delivered_at: deliveredAt,
			paused_at: null,
			ended_at: null,
			end_reason: null,
			withdrawal_country: null,
			withdrawal_days: null,
			version: 1,
		});
	}
	const listed = mapOperatorPassPurchaseToResponse(
		(await repository.findPurchase(session.id))!,
		await repository.listUnitsByPurchase(session.id),
		new Date(),
		new Map(),
	);
	expect(listed.pages).toEqual([
		{pass_id: `${session.id}:1`, name: 'Ana Souza', certificate_url: null},
		{pass_id: `${session.id}:2`, name: 'Bruno Lima', certificate_url: null},
		{pass_id: `${session.id}:3`, name: 'Ada Lovelace', certificate_url: null},
	]);
	const page = (await repository.findLatestCertificate(`${session.id}:2`))!;
	expect(page.holder_name).toBe('Bruno Lima');
});

async function deliverHeldUnits(
	repository: Awaited<ReturnType<typeof startPurchase>>['repository'],
	purchase: OperatorPassPurchaseRow,
	quantity: number,
): Promise<void> {
	const deliveredAt = purchase.delivered_at ?? new Date();
	for (let index = 1; index <= quantity; index++) {
		await repository.createUnit({
			pass_id: `${purchase.checkout_session_id}:${index}`,
			purchase_id: purchase.checkout_session_id,
			buyer_id: purchase.purchaser_id,
			holder_id: purchase.purchaser_id,
			source: 'purchase',
			status: 'held',
			transferable: false,
			delivered_at: deliveredAt,
			first_delivered_at: deliveredAt,
			paused_at: null,
			ended_at: null,
			end_reason: null,
			withdrawal_country: null,
			withdrawal_days: null,
			version: 1,
		});
	}
}

test('a pass that was passed on keeps its page in the list, in the same place as in the PDF', async () => {
	const {repository, fulfilment, session} = await startPurchase(['Ana Souza', 'Bruno Lima', 'Ada Lovelace'], 3);
	await fulfilment.handleCheckoutSessionCompleted({...session, payment_status: 'paid'} as Stripe.Checkout.Session);
	const purchase = (await repository.findPurchase(session.id))!;
	await deliverHeldUnits(repository, purchase, 3);
	const friend = await createTestAccount(harness);
	const moved = (await repository.findUnit(`${session.id}:2`))!;
	await repository.updateUnit(moved, {
		holder_id: Db.set(createUserID(BigInt(friend.userId))),
		source: Db.set('transfer'),
	});

	const listed = mapOperatorPassPurchaseToResponse(
		(await repository.findPurchase(session.id))!,
		await repository.listUnitsByPurchase(session.id),
		new Date(),
		new Map(),
	);
	expect(listed.pages).toEqual([
		{pass_id: `${session.id}:1`, name: 'Ana Souza', certificate_url: null},
		{pass_id: `${session.id}:2`, name: 'Bruno Lima', certificate_url: null},
		{pass_id: `${session.id}:3`, name: 'Ada Lovelace', certificate_url: null},
	]);
	const whole = (await fulfilment.certificates.loadPurchaseFile((await repository.findPurchase(session.id))!))!;
	expect((await PDFDocument.load(whole.bytes)).getPageCount()).toBe(3);
});

test('a new copy of a page shows the new name in the purchase', async () => {
	const {repository, fulfilment, session} = await startPurchase(['Ana Souza', 'Bruno Lima'], 2);
	await fulfilment.handleCheckoutSessionCompleted({...session, payment_status: 'paid'} as Stripe.Checkout.Session);
	await deliverHeldUnits(repository, (await repository.findPurchase(session.id))!, 2);

	await fulfilment.reissueCertificatePage({passId: `${session.id}:2`, holderName: 'Bruna Lima', reason: 'name_change'});

	const purchase = (await repository.findPurchase(session.id))!;
	expect(purchase.certificate_names).toEqual(['Ana Souza', 'Bruna Lima']);
	const listed = mapOperatorPassPurchaseToResponse(
		purchase,
		await repository.listUnitsByPurchase(session.id),
		new Date(),
		new Map(),
	);
	expect(listed.pages.map((page) => page.name)).toEqual(['Ana Souza', 'Bruna Lima']);
});

test('after an objection, earlier copies of the page lose the name and the PDF but keep the bookkeeping', async () => {
	const {repository, fulfilment, session} = await startPurchase(['Ana Souza', 'Bruno Lima'], 2);
	await fulfilment.handleCheckoutSessionCompleted({...session, payment_status: 'paid'} as Stripe.Checkout.Session);
	await deliverHeldUnits(repository, (await repository.findPurchase(session.id))!, 2);
	const passId = `${session.id}:2`;
	await fulfilment.reissueCertificatePage({passId, holderName: 'Bruno Lima Neto', reason: 'correction'});
	const before = await repository.listCertificateVersions(passId);

	const copy = await fulfilment.reissueCertificatePage({passId, holderName: 'Bea Lima', reason: 'objection'});

	expect(copy.version).toBe(3);
	const versions = await repository.listCertificateVersions(passId);
	expect(versions.map((row) => [row.version, row.holder_name, row.pdf.length > 0])).toEqual([
		[1, '', false],
		[2, '', false],
		[3, 'Bea Lima', true],
	]);
	expect(versions.slice(0, 2).map((row) => [row.number, row.sha256, row.issued_at.getTime()])).toEqual(
		before.map((row) => [row.number, row.sha256, row.issued_at.getTime()]),
	);
	expect((await repository.listCertificateVersions(`${session.id}:1`))[0]!.holder_name).toBe('Ana Souza');
	expect((await repository.findPurchase(session.id))!.certificate_names).toEqual(['Ana Souza', 'Bea Lima']);
});

test('a failure right after the certificate is created leaves the purchase undelivered and withdrawable until a retry delivers it', async () => {
	const {repository, fulfilment, session, deliverPurchase} = await startPurchase(['Ada Lovelace'], 1);
	const paid = {...session, payment_status: 'paid'} as Stripe.Checkout.Session;
	vi.spyOn(repository, 'indexCertificate').mockRejectedValueOnce(new Error('index write failed'));
	await expect(fulfilment.handleCheckoutSessionCompleted(paid)).rejects.toThrow('index write failed');
	const stuck = (await repository.findPurchase(session.id))!;
	expect(stuck.delivered_at ?? null).toBeNull();
	expect(mapOperatorPassPurchaseToResponse(stuck, [], new Date(), new Map())).toMatchObject({
		can_withdraw: true,
		certificate_available: false,
	});
	expect(deliverPurchase).not.toHaveBeenCalled();

	await fulfilment.handleCheckoutSessionCompleted(paid);
	const delivered = (await repository.findPurchase(session.id))!;
	expect(delivered).toMatchObject({status: 'completed'});
	expect(delivered.delivered_at).not.toBeNull();
	expect(await repository.listCertificatesByPurchase(session.id)).toHaveLength(1);
});

test('a payment metadata failure after delivery keeps the certificate downloadable and retries the metadata on its own', async () => {
	const {repository, fulfilment, session, stripe, intentMetadata} = await startPurchase(['Ada Lovelace'], 1);
	vi.mocked(stripe.paymentIntents.update).mockRejectedValue(new Error('Stripe is down'));
	const jobs: Array<{task: string; payload: Record<string, unknown>}> = [];
	setInjectedWorkerService({
		addJob: async (task: string, payload: Record<string, unknown>) => {
			jobs.push({task, payload});
			return 1n;
		},
		cancelJob: async () => false,
	} as never);
	try {
		await fulfilment.handleCheckoutSessionCompleted({...session, payment_status: 'paid'} as Stripe.Checkout.Session);
		const purchase = (await repository.findPurchase(session.id))!;
		expect(purchase.status).toBe('completed');
		expect(mapOperatorPassPurchaseToResponse(purchase, [], new Date(), new Map())).toMatchObject({
			certificate_available: true,
			can_withdraw: false,
		});
		const file = (await fulfilment.certificates.loadPurchaseFile(purchase))!;
		expect((await PDFDocument.load(file.bytes)).getPageCount()).toBe(1);
		expect(jobs).toEqual([{task: OPERATOR_PASS_DELIVERY_RECORD_TASK, payload: {purchase_id: session.id}}]);
		expect(intentMetadata.operator_pass_delivered_at).toBeUndefined();

		vi.mocked(stripe.paymentIntents.update).mockImplementation(async (_id, update) => {
			Object.assign(intentMetadata, update?.metadata);
			return {} as Stripe.Response<Stripe.PaymentIntent>;
		});
		await syncOperatorPassDeliveryRecord(stripe, repository, purchase);
		expect(intentMetadata.operator_pass_delivered_at).toBe(purchase.delivered_at!.toISOString());
		expect((await repository.findPurchase(session.id))?.delivery_recorded_at).not.toBeNull();
	} finally {
		setInjectedWorkerService(new NoopWorkerService());
	}
});

test('when the payment metadata and its retry job both fail, the fulfilment sweep records the delivery time later', async () => {
	const {repository, fulfilment, session, stripe, intentMetadata} = await startPurchase(['Ada Lovelace'], 1);
	vi.mocked(stripe.paymentIntents.update).mockRejectedValue(new Error('Stripe is down'));
	setInjectedWorkerService({
		addJob: async () => {
			throw new Error('queue is down');
		},
		cancelJob: async () => false,
	} as never);
	try {
		await fulfilment.handleCheckoutSessionCompleted({...session, payment_status: 'paid'} as Stripe.Checkout.Session);
	} finally {
		setInjectedWorkerService(new NoopWorkerService());
	}
	const delivered = (await repository.findPurchase(session.id))!;
	expect(delivered).toMatchObject({status: 'completed', delivery_recorded_at: null});
	expect(delivered.welcome_sent_at).not.toBeNull();
	expect(intentMetadata.operator_pass_delivered_at).toBeUndefined();

	const kvClient = {
		acquireLock: async () => true,
		extendLock: async () => true,
		releaseLock: async () => true,
	} as unknown as IKVProvider;
	const retrieveSession = vi.fn(async () => session);
	const sweep = () =>
		runOperatorPassFulfilmentSweep({
			kvClient,
			repository: {
				scanPurchases: async () => ({rows: [(await repository.findPurchase(session.id))!], pageState: null}),
			},
			listCompletedSessionIds: async () => new Set([session.id]),
			retrieveSession,
			resolvePaidAt: async (_session, fallback) => fallback,
			checkout: fulfilment,
			recordDelivery: async (purchase) => {
				await syncOperatorPassDeliveryRecord(stripe, repository, purchase);
			},
			requeueWithdrawalReceipts: async () => {},
		});
	await sweep();
	expect((await repository.findPurchase(session.id))?.delivery_recorded_at ?? null).toBeNull();

	vi.mocked(stripe.paymentIntents.update).mockImplementation(async (_id, update) => {
		Object.assign(intentMetadata, update?.metadata);
		return {} as Stripe.Response<Stripe.PaymentIntent>;
	});
	await sweep();
	expect(intentMetadata.operator_pass_delivered_at).toBe(delivered.delivered_at!.toISOString());
	expect((await repository.findPurchase(session.id))?.delivery_recorded_at).not.toBeNull();
	expect(retrieveSession).not.toHaveBeenCalled();
	vi.mocked(stripe.paymentIntents.update).mockClear();
	await sweep();
	expect(stripe.paymentIntents.update).not.toHaveBeenCalled();
});

test('a bonus delivered on a later retry keeps the certificate delivery time as the pass date', async () => {
	const {repository, fulfilment, session, deliverPurchase} = await startPurchase(['Ada Lovelace'], 1);
	const paid = {...session, payment_status: 'paid'} as Stripe.Checkout.Session;
	deliverPurchase.mockRejectedValueOnce(new Error('bonus store is down'));
	await expect(fulfilment.handleCheckoutSessionCompleted(paid)).rejects.toThrow('bonus store is down');
	const certificateDeliveredAt = (await repository.findPurchase(session.id))!.delivered_at!;
	expect(certificateDeliveredAt).not.toBeNull();

	vi.useFakeTimers({toFake: ['Date'], now: certificateDeliveredAt.getTime() + 2 * DAY_MS});
	try {
		await fulfilment.handleCheckoutSessionCompleted(paid);
	} finally {
		vi.useRealTimers();
	}
	expect(deliverPurchase).toHaveBeenCalledTimes(2);
	expect(deliverPurchase).toHaveBeenLastCalledWith(expect.objectContaining({deliveredAt: certificateDeliveredAt}));
	const purchase = (await repository.findPurchase(session.id))!;
	expect(purchase.delivered_at).toEqual(certificateDeliveredAt);
	expect(purchase.perks_granted_at!.getTime()).toBeGreaterThanOrEqual(certificateDeliveredAt.getTime() + 2 * DAY_MS);
});

test('account deletion waits for a reissue in progress, and a reissue after deletion started is refused', async () => {
	const {repository, fulfilment, session} = await startPurchase(['Ada Lovelace'], 1);
	await fulfilment.handleCheckoutSessionCompleted({...session, payment_status: 'paid'} as Stripe.Checkout.Session);
	const purchase = (await repository.findPurchase(session.id))!;
	const passId = `${session.id}:1`;
	await repository.createUnit({
		pass_id: passId,
		purchase_id: session.id,
		buyer_id: purchase.purchaser_id,
		holder_id: purchase.purchaser_id,
		source: 'purchase',
		status: 'held',
		transferable: true,
		delivered_at: purchase.delivered_at!,
		first_delivered_at: purchase.delivered_at!,
		paused_at: null,
		ended_at: null,
		end_reason: null,
		withdrawal_country: null,
		withdrawal_days: null,
		version: 1,
	});
	const reissue = fulfilment.certificates.reissuePage.bind(fulfilment.certificates);
	let release: () => void = () => {};
	const rendering = new Promise<void>((resolve) => {
		release = resolve;
	});
	vi.spyOn(fulfilment.certificates, 'reissuePage').mockImplementationOnce(async (params) => {
		await rendering;
		return reissue(params);
	});
	const reissued = fulfilment.reissueCertificatePage({passId, holderName: 'Ada King', reason: 'name_change'});
	await vi.waitFor(() => expect(fulfilment.certificates.reissuePage).toHaveBeenCalled());
	await expect(endOperatorPassesForDeletion(repository, getCacheService(), purchase.purchaser_id)).rejects.toThrow(
		'busy',
	);
	release();
	await reissued;
	await endOperatorPassesForDeletion(repository, getCacheService(), purchase.purchaser_id);
	expect((await repository.listCertificateVersions(passId)).map((row) => [row.holder_name, row.pdf.length])).toEqual([
		['', 0],
		['', 0],
	]);
	expect((await repository.findPurchase(session.id))?.certificate_names ?? null).toBeNull();

	const buyer = await getUserRepository().findUniqueAssert(purchase.purchaser_id);
	vi.spyOn(getUserRepository(), 'findUnique').mockResolvedValueOnce(
		Object.assign(Object.create(Object.getPrototypeOf(buyer)), buyer, {deletionStartedAt: new Date()}),
	);
	await expect(
		fulfilment.reissueCertificatePage({passId, holderName: 'Grace Hopper', reason: 'name_change'}),
	).rejects.toBeInstanceOf(InputValidationError);
	expect(await repository.listCertificateVersions(passId)).toHaveLength(2);
});

test('recovery dates a debit started on 31 December from when it settled in January', async () => {
	const {repository, fulfilment, session} = await startPurchase(['Ada Lovelace'], 1);
	const startedAt = new Date('2026-12-31T22:00:00Z');
	const settledAt = new Date('2027-01-03T09:00:00Z');
	const intent = {
		id: String(session.payment_intent),
		created: startedAt.getTime() / 1000,
		latest_charge: {created: startedAt.getTime() / 1000, payment_method_details: {type: 'sepa_debit'}},
	};
	const events = [
		{created: settledAt.getTime() / 1000 + 60, data: {object: {id: 'pi_someone_else'}}},
		{created: settledAt.getTime() / 1000, data: {object: {id: intent.id}}},
	];
	const lookup = {
		invoices: {retrieve: vi.fn()},
		events: {
			list: vi.fn(() => ({
				async *[Symbol.asyncIterator]() {
					yield* events;
				},
			})),
		},
	} as unknown as Stripe;
	const recovered = {...session, payment_status: 'paid', payment_intent: intent} as unknown as Stripe.Checkout.Session;
	expect(
		await resolveOperatorPassSettledAt(
			lookup,
			{...recovered, invoice: {id: 'in_test', status_transitions: {paid_at: settledAt.getTime() / 1000}}} as never,
			new Date(),
		),
	).toEqual(settledAt);
	expect(await resolveOperatorPassSettledAt(lookup, recovered, new Date())).toEqual(settledAt);

	const kvClient = getKVClient();
	await runOperatorPassFulfilmentSweep({
		kvClient,
		now: () => Date.now() + 26 * 60 * 60 * 1000,
		repository: {
			scanPurchases: async () => ({rows: [(await repository.findPurchase(session.id))!], pageState: null}),
		},
		listCompletedSessionIds: async () => new Set([session.id]),
		retrieveSession: async () => recovered,
		resolvePaidAt: (sweptSession, fallback) => resolveOperatorPassSettledAt(lookup, sweptSession, fallback),
		checkout: fulfilment,
		recordDelivery: async () => {},
		requeueWithdrawalReceipts: async () => {},
	});
	const purchase = (await repository.findPurchase(session.id))!;
	expect(purchase.purchased_at).toEqual(settledAt);
	const [certificate] = await repository.listCertificatesByPurchase(session.id);
	expect(certificate?.paid_at).toEqual(settledAt);
});

test('a refund that takes the purchase lock after it expired mid-fulfilment is retried, and fulfilment never completes the refunded purchase', async () => {
	const {repository, fulfilment, session, stripe, deliverPurchase, emails} = await startPurchase(['Ada Lovelace'], 1);
	const paid = {...session, payment_status: 'paid'} as Stripe.Checkout.Session;
	let releaseMetadata: () => void = () => {};
	const metadataStarted = new Promise<void>((started) => {
		vi.mocked(stripe.paymentIntents.update).mockImplementationOnce(async () => {
			started();
			await new Promise<void>((release) => {
				releaseMetadata = release;
			});
			return {} as Stripe.Response<Stripe.PaymentIntent>;
		});
	});
	const fulfilling = fulfilment.handleCheckoutSessionCompleted(paid);
	await metadataStarted;
	await getKVClient().del(formatLockKey(operatorPassPurchaseLockKey(session.id)));
	const amount = (await repository.findPurchase(session.id))!.amount_minor;
	vi.mocked(stripe.paymentIntents.retrieve).mockResolvedValue({
		latest_charge: {
			id: `ch_test_${session.id}`,
			created: Math.floor(Date.now() / 1000),
			refunded: true,
			amount_refunded: amount,
			payment_method_details: {type: 'sepa_debit'},
		},
	} as never);
	const refundCharge = {id: `ch_test_${session.id}`, refunded: true, amount_refunded: amount} as Stripe.Charge;

	await expect(
		fulfilment.handleChargeRefunded((await repository.findPurchase(session.id))!, refundCharge),
	).rejects.toThrow();
	expect((await repository.findPurchase(session.id))?.status).toBe('pending');

	releaseMetadata();
	await expect(fulfilling).rejects.toThrow();
	expect((await repository.findPurchase(session.id))?.status).toBe('pending');
	expect(deliverPurchase).not.toHaveBeenCalled();

	await fulfilment.handleCheckoutSessionCompleted(paid);
	await fulfilment.handleChargeRefunded((await repository.findPurchase(session.id))!, refundCharge);
	expect((await repository.findPurchase(session.id))?.status).toBe('refunded');
	expect(deliverPurchase).not.toHaveBeenCalled();
	expect(emails.listSentEmails().map((email) => email.type)).not.toContain('operator_pass_welcome');
});

test('successive partial refunds after the recipient of a passed-on pass deleted their account cancel the page once the whole price is back', async () => {
	const {repository, fulfilment, session} = await startPurchase(['Ana Souza'], 1);
	await fulfilment.handleCheckoutSessionCompleted({...session, payment_status: 'paid'} as Stripe.Checkout.Session);
	const purchase = (await repository.findPurchase(session.id))!;
	await deliverHeldUnits(repository, purchase, 1);
	const friend = createUserID(BigInt((await createTestAccount(harness)).userId));
	const passId = `${session.id}:1`;
	await repository.updateUnit((await repository.findUnit(passId))!, {
		holder_id: Db.set(friend),
		source: Db.set('transfer'),
	});
	await repository.moveUnitHolder(passId, purchase.purchaser_id, friend);
	await endOperatorPassesForDeletion(repository, getCacheService(), friend);
	const tenth = Math.round(purchase.amount_minor / 10);

	await fulfilment.handleChargeRefunded(purchase, {
		id: `ch_test_${session.id}`,
		refunded: false,
		amount_refunded: tenth,
	} as Stripe.Charge);
	expect((await repository.findPurchase(session.id))?.status).toBe('completed');
	expect((await repository.findLatestCertificate(passId))?.cancelled_at ?? null).toBeNull();

	const full = {id: `ch_test_${session.id}`, refunded: true, amount_refunded: purchase.amount_minor} as Stripe.Charge;
	await fulfilment.handleChargeRefunded((await repository.findPurchase(session.id))!, full);
	expect((await repository.findPurchase(session.id))?.status).toBe('refunded');
	expect(await repository.findUnit(passId)).toMatchObject({end_reason: 'refund'});
	expect((await repository.findLatestCertificate(passId))?.cancelled_at).not.toBeNull();
	const token = (await repository.findLatestCertificate(passId))!.link_token!;
	expect(await fulfilment.certificates.loadLinkedPage(token)).toMatchObject({cancelled: true});
});

test('a full refund replayed on a purchase already marked refunded still cancels its page', async () => {
	const {repository, fulfilment, session} = await startPurchase(['Ana Souza'], 1);
	await fulfilment.handleCheckoutSessionCompleted({...session, payment_status: 'paid'} as Stripe.Checkout.Session);
	const delivered = (await repository.findPurchase(session.id))!;
	await deliverHeldUnits(repository, delivered, 1);
	const passId = `${session.id}:1`;
	await repository.updateUnit((await repository.findUnit(passId))!, {
		status: Db.set('revoked'),
		end_reason: Db.set('deletion'),
		ended_at: Db.set(new Date()),
	});
	const refunded = await repository.updatePurchase(delivered, {
		status: Db.set('refunded'),
		refunded_at: Db.set(new Date()),
	});
	expect((await repository.findLatestCertificate(passId))?.cancelled_at ?? null).toBeNull();

	await fulfilment.handleChargeRefunded(refunded, {
		id: `ch_test_${session.id}`,
		refunded: true,
		amount_refunded: refunded.amount_minor,
	} as Stripe.Charge);
	expect(await repository.findUnit(passId)).toMatchObject({end_reason: 'refund'});
	expect((await repository.findLatestCertificate(passId))?.cancelled_at).not.toBeNull();
	expect((await repository.findPurchase(session.id))?.refunded_at).toEqual(refunded.refunded_at);
});

test('account deletion erases a certificate page whose purchase index was never written, after the purchase was withdrawn', async () => {
	const {repository, fulfilment, session} = await startPurchase(['Ana Souza', 'Bruno Lima'], 2);
	const indexCertificate = vi
		.spyOn(repository, 'indexCertificate')
		.mockRejectedValueOnce(new Error('index write failed'));
	await expect(
		fulfilment.handleCheckoutSessionCompleted({...session, payment_status: 'paid'} as Stripe.Checkout.Session),
	).rejects.toThrow('index write failed');
	indexCertificate.mockRestore();
	const passId = `${session.id}:1`;
	expect((await repository.listCertificateVersions(passId))[0]?.holder_name).toBe('Ana Souza');
	expect((await repository.listCertificatesByPurchase(session.id)).map((row) => row.pass_id)).not.toContain(passId);
	const purchase = (await repository.findPurchase(session.id))!;
	expect(purchase.delivered_at ?? null).toBeNull();
	const purchaser = await getUserRepository().findUniqueAssert(purchase.purchaser_id);
	try {
		await fulfilment.withdraw({
			purchaser,
			purchaseId: session.id,
			name: 'Ana Souza',
			channel: 'button',
			receivedAt: new Date(),
		});
	} finally {
		setInjectedWorkerService(new NoopWorkerService());
	}
	expect((await repository.findPurchase(session.id))?.status).toBe('withdrawn');

	await endOperatorPassesForDeletion(repository, getCacheService(), purchase.purchaser_id);

	for (const id of [passId, `${session.id}:2`]) {
		for (const version of await repository.listCertificateVersions(id)) {
			expect([version.holder_name, version.pdf.length]).toEqual(['', 0]);
		}
	}
});

test.each([
	['withdrawal', 'withdrawn'],
	['refund', 'revoked'],
	['dispute', 'revoked'],
] as const)('an upheld objection erases the name and PDF of a page cancelled after a %s', async (cause, status) => {
	const {repository, fulfilment, session} = await startPurchase(['Ana Souza', 'Bruno Lima'], 2);
	await fulfilment.handleCheckoutSessionCompleted({...session, payment_status: 'paid'} as Stripe.Checkout.Session);
	await deliverHeldUnits(repository, (await repository.findPurchase(session.id))!, 2);
	const passId = `${session.id}:2`;
	await fulfilment.reissueCertificatePage({passId, holderName: 'Bruno Lima Neto', reason: 'correction'});
	const cancelledAt = new Date();
	await repository.updateUnit((await repository.findUnit(passId))!, {
		status: Db.set(status),
		end_reason: Db.set(cause),
		ended_at: Db.set(cancelledAt),
	});
	await repository.markCertificateCancelled(passId, cancelledAt);
	const before = await repository.listCertificateVersions(passId);

	await expect(
		fulfilment.reissueCertificatePage({passId, holderName: 'Bea Lima', reason: 'correction'}),
	).rejects.toBeInstanceOf(InputValidationError);
	const erased = await fulfilment.reissueCertificatePage({passId, holderName: 'Bea Lima', reason: 'objection'});

	expect(erased.version).toBe(2);
	const versions = await repository.listCertificateVersions(passId);
	expect(versions.map((row) => [row.version, row.holder_name, row.pdf.length])).toEqual([
		[1, '', 0],
		[2, '', 0],
	]);
	expect(versions.map((row) => [row.number, row.sha256, row.issued_at.getTime(), row.cancelled_at?.getTime()])).toEqual(
		before.map((row) => [row.number, row.sha256, row.issued_at.getTime(), cancelledAt.getTime()]),
	);
	expect((await repository.listCertificateVersions(`${session.id}:1`))[0]!.holder_name).toBe('Ana Souza');
	expect((await repository.findPurchase(session.id))!.certificate_names).toEqual(['Ana Souza', '']);
});

test('a delivery time queued while Stripe is off stays unrecorded until Stripe is back', async () => {
	const {repository, fulfilment, session, stripe, intentMetadata} = await startPurchase(['Ada Lovelace'], 1);
	vi.mocked(stripe.paymentIntents.update).mockRejectedValue(new Error('Stripe is down'));
	await fulfilment.handleCheckoutSessionCompleted({...session, payment_status: 'paid'} as Stripe.Checkout.Session);
	const delivered = (await repository.findPurchase(session.id))!;
	expect(delivered.delivery_recorded_at ?? null).toBeNull();

	await expect(syncOperatorPassDeliveryRecord(null, repository, delivered)).rejects.toThrow();
	expect((await repository.findPurchase(session.id))?.delivery_recorded_at ?? null).toBeNull();

	vi.mocked(stripe.paymentIntents.update).mockImplementation(async (_id, update) => {
		Object.assign(intentMetadata, update?.metadata);
		return {} as Stripe.Response<Stripe.PaymentIntent>;
	});
	await syncOperatorPassDeliveryRecord(stripe, repository, (await repository.findPurchase(session.id))!);
	expect(intentMetadata.operator_pass_delivered_at).toBe(delivered.delivered_at!.toISOString());
	expect((await repository.findPurchase(session.id))?.delivery_recorded_at).not.toBeNull();
});

test('a failed card refund that Stripe will not retry is reported once and the webhook finishes', async () => {
	const {repository, fulfilment, session, stripe} = await startPurchase(['Ana Souza'], 1);
	await fulfilment.handleCheckoutSessionCompleted({...session, payment_status: 'paid'} as Stripe.Checkout.Session);
	expect((await repository.findPurchase(session.id))?.payment_intent_id).toBe(session.payment_intent);
	const {default: StripeClient} = await import('stripe');
	vi.mocked(stripe.refunds.create).mockRejectedValueOnce(
		new StripeClient.errors.StripeInvalidRequestError({
			type: 'invalid_request_error',
			message: 'A previous attempt to refund charge ch_test to card card_test failed',
		}),
	);
	loggerError.mockClear();
	await expect(
		fulfilment.handleRefundFailed(
			{
				id: 're_failed_card',
				object: 'refund',
				status: 'failed',
				amount: 19900,
				currency: 'usd',
				failure_reason: 'expired_or_canceled_card',
				metadata: {checkout_session_id: session.id},
			} as unknown as Stripe.Refund,
			'refund.failed',
		),
	).resolves.toBeUndefined();
	expect(loggerError.mock.calls.map((call) => call[1])).toContain(
		'Stripe refused to retry the failed Operator Pass refund; support has to send the money another way',
	);
});

test('a failed pass refund announced by refund.updated and refund.failed alerts and retries once', async () => {
	const {repository, fulfilment, session, stripe} = await startPurchase(['Ana Souza'], 1);
	await fulfilment.handleCheckoutSessionCompleted({...session, payment_status: 'paid'} as Stripe.Checkout.Session);
	expect((await repository.findPurchase(session.id))?.payment_intent_id).toBe(session.payment_intent);
	vi.mocked(stripe.refunds.create).mockClear();
	const failed = {
		id: 're_failed_twice',
		object: 'refund',
		status: 'failed',
		amount: 19900,
		currency: 'usd',
		failure_reason: 'expired_or_canceled_card',
		metadata: {checkout_session_id: session.id},
	} as unknown as Stripe.Refund;
	loggerError.mockClear();
	await fulfilment.handleRefundFailed(failed, 'refund.updated');
	await fulfilment.handleRefundFailed(failed, 'refund.failed');
	expect(
		loggerError.mock.calls.filter((call) => call[1] === 'An Operator Pass refund failed; retrying once'),
	).toHaveLength(1);
	expect(vi.mocked(stripe.refunds.create)).toHaveBeenCalledTimes(1);
});

test('an early fraud warning that arrives before the checkout webhook is recorded on the pending purchase', async () => {
	const {repository, fulfilment, session, stripe} = await startPurchase(['Ana Souza'], 1);
	Object.assign(stripe, {checkout: {sessions: {list: vi.fn(async () => ({data: [session]}))}}});
	expect((await repository.findPurchase(session.id))?.payment_intent_id ?? null).toBeNull();
	await fulfilment.markFraudWarning(session.payment_intent as string);
	expect((await repository.findPurchase(session.id))?.fraud_warning_at).toBeInstanceOf(Date);
});
