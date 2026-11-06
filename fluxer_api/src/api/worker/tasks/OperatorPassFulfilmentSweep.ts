// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {alertSupport} from '@app/api/billing/BillingSupportAlert';
import {Config} from '@app/api/Config';
import type {OperatorPassPurchaseRow} from '@app/api/database/types/OperatorPassTypes';
import {Logger} from '@app/api/Logger';
import {
	createOperatorPassFulfilmentService,
	isOperatorPassCheckoutSession,
	resolveOperatorPassSettledAt,
	syncOperatorPassDeliveryRecord,
} from '@app/api/operator_pass/OperatorPassFulfilmentService';
import {isOperatorPassWelcomeSettled} from '@app/api/operator_pass/OperatorPassPurchaseRules';
import type {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import type {StripeCheckoutWebhookHandler} from '@app/api/stripe/services/StripeCheckoutWebhookHandler';
import {createWorkerStripeWebhookService} from '@app/api/worker/tasks/WorkerStripeWebhookService';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {ms} from 'itty-time';
import type Stripe from 'stripe';

const LOCK_KEY = 'operator_pass:fulfilment_sweep:lock';
const LOCK_TTL_SECONDS = 120;
const PAGE_SIZE = 100;
const TIME_BUDGET_MS = ms('100 seconds');
const PENDING_GRACE_MS = ms('30 minutes');
const SESSION_LOOKBACK_MS = ms('25 hours');
const SESSION_CREATED_SLACK_MS = ms('10 minutes');
const WELCOME_RETRY_MS = ms('7 days');
const UNDELIVERED_ALERT_MS = ms('24 hours');
const WELCOME_LAPSED_KEY_PREFIX = 'operator_pass:welcome_lapsed:';
const WELCOME_LAPSED_MARKER_TTL_SECONDS = 400 * 24 * 60 * 60;
const FAILED_PAYMENT_INTENT_STATUSES: ReadonlySet<string> = new Set(['requires_payment_method', 'canceled']);

type CheckoutEvents = Pick<
	StripeCheckoutWebhookHandler,
	'handleAsyncPaymentSucceeded' | 'handleCheckoutSessionExpired' | 'handleAsyncPaymentFailed'
>;

interface FulfilmentSweepDependencies {
	kvClient: IKVProvider;
	repository: Pick<OperatorPassRepository, 'scanPurchases'>;
	listCompletedSessionIds: (createdSince: Date) => Promise<ReadonlySet<string>>;
	retrieveSession: (sessionId: string) => Promise<Stripe.Checkout.Session>;
	resolvePaidAt: (session: Stripe.Checkout.Session, fallback: Date) => Promise<Date>;
	checkout: CheckoutEvents;
	recordDelivery: (purchase: OperatorPassPurchaseRow) => Promise<void>;
	requeueWithdrawalReceipts: (purchase: OperatorPassPurchaseRow, now: Date) => Promise<void>;
	now?: () => number;
}

interface SweepCandidate {
	purchase: OperatorPassPurchaseRow;
	need: SweepNeed;
	record: boolean;
	receipts: boolean;
}

interface FulfilmentSweepResult {
	ran: boolean;
	checked: number;
	failed: number;
}

type SweepNeed = 'sweep' | 'if_completed' | 'welcome_lapsed' | 'skip';

function sweepNeed(purchase: OperatorPassPurchaseRow, nowMs: number): SweepNeed {
	if (purchase.status === 'pending') {
		const createdMs = purchase.created_at.getTime();
		if (createdMs + SESSION_LOOKBACK_MS <= nowMs) return 'sweep';
		return createdMs + PENDING_GRACE_MS <= nowMs ? 'if_completed' : 'skip';
	}
	if (purchase.status === 'completed' && !isOperatorPassWelcomeSettled(purchase)) {
		return welcomeWindowStartMs(purchase) + WELCOME_RETRY_MS > nowMs ? 'sweep' : 'welcome_lapsed';
	}
	return 'skip';
}

function welcomeWindowStartMs(purchase: OperatorPassPurchaseRow): number {
	const completedMs = (purchase.completed_at ?? purchase.created_at).getTime();
	return Math.max(completedMs, purchase.dispute_closed_at?.getTime() ?? 0);
}

function needsDeliveryRecord(purchase: OperatorPassPurchaseRow): boolean {
	return purchase.delivered_at != null && purchase.delivery_recorded_at == null;
}

function expandedObject<T extends {id: string}>(value: string | T | null | undefined): T | null {
	return value && typeof value === 'object' ? value : null;
}

function isSessionPaid(session: Stripe.Checkout.Session): boolean {
	return session.payment_status === 'paid' || session.payment_status === 'no_payment_required';
}

async function alertIfUndelivered(
	purchase: OperatorPassPurchaseRow,
	paidAt: Date,
	nowMs: number,
	context: object,
): Promise<void> {
	if (paidAt.getTime() + UNDELIVERED_ALERT_MS > nowMs) return;
	if (purchase.delivered_at != null) {
		Logger.warn(context, 'Operator Pass certificate email not sent 24 hours after payment');
		return;
	}
	Logger.error(context, 'Operator Pass purchase is still not delivered 24 hours after payment');
	await alertSupport({
		dedupeKey: `operator-pass-undelivered:${purchase.checkout_session_id}`,
		subject: 'Operator Pass not delivered 24 hours after payment',
		summary:
			'An Operator Pass purchase was paid more than 24 hours ago and is still not delivered. Check the purchase in admin and deliver it or refund the buyer.',
		facts: {
			checkoutSessionId: purchase.checkout_session_id,
			paymentIntentId: purchase.payment_intent_id,
			userId: purchase.purchaser_id,
			documentNumber: purchase.invoice_number,
			amountMinor: purchase.amount_minor,
			currency: purchase.currency,
		},
	});
}

async function sweepSession(
	deps: FulfilmentSweepDependencies,
	session: Stripe.Checkout.Session,
	nowMs: number,
): Promise<void> {
	const paymentIntent = expandedObject<Stripe.PaymentIntent>(session.payment_intent);
	if (isSessionPaid(session)) {
		await deps.checkout.handleAsyncPaymentSucceeded(session, await deps.resolvePaidAt(session, new Date(nowMs)));
		return;
	}
	if (session.status === 'expired') {
		await deps.checkout.handleCheckoutSessionExpired(session);
		return;
	}
	if (session.status === 'complete' && paymentIntent && FAILED_PAYMENT_INTENT_STATUSES.has(paymentIntent.status)) {
		await deps.checkout.handleAsyncPaymentFailed(session);
	}
}

async function sweepCandidate(
	deps: FulfilmentSweepDependencies,
	{purchase, need, record, receipts}: SweepCandidate,
	nowMs: number,
	completedSessionIds: () => Promise<ReadonlySet<string>>,
	result: FulfilmentSweepResult,
): Promise<void> {
	if (record) {
		try {
			await deps.recordDelivery(purchase);
		} catch (error) {
			Logger.warn(
				{sessionId: purchase.checkout_session_id, error},
				'Operator Pass fulfilment sweep could not record the certificate delivery time on the payment',
			);
		}
	}
	if (receipts) {
		try {
			await deps.requeueWithdrawalReceipts(purchase, new Date(nowMs));
		} catch (error) {
			Logger.warn(
				{sessionId: purchase.checkout_session_id, error},
				'Operator Pass fulfilment sweep could not queue the owed withdrawal receipts again',
			);
		}
	}
	if (need === 'skip' || (need === 'if_completed' && !(await completedSessionIds()).has(purchase.checkout_session_id)))
		return;
	const context = {
		sessionId: purchase.checkout_session_id,
		purchaserId: purchase.purchaser_id,
		status: purchase.status,
		purchasedAt: purchase.purchased_at ?? null,
	};
	if (need === 'welcome_lapsed') {
		const marker = `${WELCOME_LAPSED_KEY_PREFIX}${purchase.checkout_session_id}:${welcomeWindowStartMs(purchase)}`;
		try {
			if (await deps.kvClient.setnx(marker, '1', WELCOME_LAPSED_MARKER_TTL_SECONDS)) {
				Logger.error(context, 'Operator Pass certificate email was never sent, the sweep stopped retrying it');
				await alertSupport({
					dedupeKey: `operator-pass-certificate-email:${purchase.checkout_session_id}`,
					subject: 'Operator Pass certificate email never sent',
					summary:
						'The certificate email for an Operator Pass purchase was never sent, and the sweep stopped retrying it after 7 days. Send the buyer the certificate by hand.',
					facts: {
						checkoutSessionId: purchase.checkout_session_id,
						paymentIntentId: purchase.payment_intent_id,
						userId: purchase.purchaser_id,
						documentNumber: purchase.invoice_number,
					},
				});
			}
		} catch (error) {
			Logger.warn({...context, error}, 'Operator Pass fulfilment sweep could not record a lapsed certificate email');
		}
		return;
	}
	if (purchase.purchased_at) await alertIfUndelivered(purchase, purchase.purchased_at, nowMs, context);
	result.checked += 1;
	try {
		const session = await deps.retrieveSession(purchase.checkout_session_id);
		if (!purchase.purchased_at && isSessionPaid(session)) {
			await alertIfUndelivered(purchase, purchase.created_at, nowMs, context);
		}
		await sweepSession(deps, session, nowMs);
	} catch (error) {
		result.failed += 1;
		Logger.warn({...context, error}, 'Operator Pass fulfilment sweep could not finish a purchase');
	}
}

export async function runOperatorPassFulfilmentSweep(
	deps: FulfilmentSweepDependencies,
): Promise<FulfilmentSweepResult> {
	const result: FulfilmentSweepResult = {ran: false, checked: 0, failed: 0};
	const now = deps.now ?? Date.now;
	const deadline = now() + TIME_BUDGET_MS;
	const token = randomUUID();
	if (!(await deps.kvClient.acquireLock(LOCK_KEY, token, LOCK_TTL_SECONDS))) return result;
	result.ran = true;
	try {
		let completedIds: Promise<ReadonlySet<string>> | null = null;
		const completedSessionIds = () => {
			completedIds ??= deps.listCompletedSessionIds(new Date(now() - SESSION_LOOKBACK_MS - SESSION_CREATED_SLACK_MS));
			return completedIds;
		};
		let pageState: string | null = null;
		do {
			if (now() >= deadline || !(await deps.kvClient.extendLock(LOCK_KEY, token, LOCK_TTL_SECONDS))) return result;
			const page = await deps.repository.scanPurchases(PAGE_SIZE, pageState);
			for (const purchase of page.rows) {
				const nowMs = now();
				if (nowMs >= deadline) return result;
				const need = sweepNeed(purchase, nowMs);
				const record = needsDeliveryRecord(purchase);
				const receipts = (purchase.withdrawal_receipts_owed?.size ?? 0) > 0;
				if (need === 'skip' && !record && !receipts) continue;
				await sweepCandidate(deps, {purchase, need, record, receipts}, nowMs, completedSessionIds, result);
			}
			pageState = page.pageState;
		} while (pageState);
		return result;
	} finally {
		await deps.kvClient.releaseLock(LOCK_KEY, token);
	}
}

const operatorPassFulfilmentSweep: WorkerTaskHandler = async (_payload, helpers) => {
	if (Config.instance.selfHosted) return;
	const deps = getWorkerDependencies();
	const stripe = deps.stripe;
	if (!stripe) return;
	const webhookService = createWorkerStripeWebhookService(deps, stripe);
	const fulfilment = createOperatorPassFulfilmentService({
		stripe,
		userRepository: deps.userRepository,
		emailService: deps.emailService,
		cacheService: deps.cacheService,
		services: deps.operatorPassServices,
	});
	const result = await runOperatorPassFulfilmentSweep({
		kvClient: deps.kvClient,
		repository: deps.operatorPassRepository,
		listCompletedSessionIds: async (createdSince) => {
			const ids = new Set<string>();
			const sessions = stripe.checkout.sessions.list({
				status: 'complete',
				created: {gte: Math.floor(createdSince.getTime() / 1000)},
				limit: 100,
			});
			for await (const session of sessions) {
				if (isOperatorPassCheckoutSession(session)) ids.add(session.id);
			}
			return ids;
		},
		retrieveSession: (sessionId) =>
			stripe.checkout.sessions.retrieve(sessionId, {expand: ['payment_intent.latest_charge', 'invoice']}),
		resolvePaidAt: (session, fallback) => resolveOperatorPassSettledAt(stripe, session, fallback),
		checkout: webhookService.checkoutHandler,
		recordDelivery: async (purchase) => {
			await syncOperatorPassDeliveryRecord(stripe, deps.operatorPassRepository, purchase);
		},
		requeueWithdrawalReceipts: async (purchase, now) => {
			await fulfilment?.requeueOwedWithdrawalReceipts(purchase, now);
		},
	});
	if (result.checked > 0) {
		helpers.logger.info(result, 'Swept Operator Pass purchases that were paid but not fully delivered');
	}
};

export default operatorPassFulfilmentSweep;
