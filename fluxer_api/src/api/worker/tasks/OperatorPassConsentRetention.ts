// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import type {UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {Db} from '@app/api/database/CassandraTypes';
import type {OperatorPassPurchaseRow, OperatorPassRow} from '@app/api/database/types/OperatorPassTypes';
import {getPaymentDisputeRepository} from '@app/api/middleware/ServiceSingletons';
import {replaceOperatorPassCertificateName} from '@app/api/operator_pass/OperatorPassPurchaseRules';
import type {
	OperatorPassCertificatePageSummary,
	OperatorPassRepository,
} from '@app/api/operator_pass/OperatorPassRepository';
import type {PaymentDisputeRepository} from '@app/api/payment_disputes/PaymentDisputeRepository';
import {isPaymentDisputeOpen} from '@app/api/payment_disputes/PaymentDisputeReviews';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {ms} from 'itty-time';

const LOCK_KEY = 'operator_pass:consent_retention:lock';
const LOCK_TTL_SECONDS = 120;
const PAGE_SIZE = 100;
const TIME_BUDGET_MS = ms('50 seconds');

const OPERATOR_PASS_CONSENT_DETAILS_RETENTION_MS = ms('730 days');

type ConsentDetails = Pick<
	OperatorPassRow | OperatorPassPurchaseRow,
	'waiver_consent_at' | 'waiver_consent_ip' | 'waiver_consent_user_agent'
>;

function consentDetailsExpired(row: ConsentDetails, startedAt: Date | null | undefined, nowMs: number): boolean {
	if (row.waiver_consent_ip == null && row.waiver_consent_user_agent == null) return false;
	const from = startedAt ?? row.waiver_consent_at;
	return from != null && from.getTime() + OPERATOR_PASS_CONSENT_DETAILS_RETENTION_MS <= nowMs;
}

function cancelledPageExpired(page: OperatorPassCertificatePageSummary, nowMs: number): boolean {
	return (
		page.cancelled_at != null &&
		page.holder_name !== '' &&
		page.paid_at.getTime() + OPERATOR_PASS_CONSENT_DETAILS_RETENTION_MS <= nowMs
	);
}

interface ConsentHoldTarget {
	userId: UserID;
	checkoutSessionId: string | null;
	paymentIntentId: string | null;
}

export async function isOperatorPassPurchaseDisputed(
	repository: Pick<PaymentDisputeRepository, 'listReviewsByPurchaser'>,
	target: ConsentHoldTarget,
): Promise<boolean> {
	const reviews = await repository.listReviewsByPurchaser(target.userId);
	return reviews.some(
		(review) =>
			review.product === 'operator_pass' &&
			isPaymentDisputeOpen(review) &&
			((target.checkoutSessionId != null && review.order_id === target.checkoutSessionId) ||
				(target.paymentIntentId != null && review.payment_intent_id === target.paymentIntentId)),
	);
}

interface ConsentRetentionDependencies {
	kvClient: IKVProvider;
	repository: Pick<
		OperatorPassRepository,
		| 'scanPurchases'
		| 'scanOperatorPasses'
		| 'scanCertificatePages'
		| 'findPurchase'
		| 'updatePurchase'
		| 'clearOperatorPassConsentDetails'
		| 'clearCertificatePersonalData'
		| 'scanVisionaryActivations'
		| 'updateVisionaryActivation'
	>;
	isOnHold: (purchase: ConsentHoldTarget) => Promise<boolean>;
	now?: () => number;
}

interface ConsentRetentionResult {
	ran: boolean;
	purchases: number;
	passes: number;
	cancelledPages: number;
	visionaryActivations: number;
}

export async function runOperatorPassConsentRetention(
	deps: ConsentRetentionDependencies,
): Promise<ConsentRetentionResult> {
	const result: ConsentRetentionResult = {
		ran: false,
		purchases: 0,
		passes: 0,
		cancelledPages: 0,
		visionaryActivations: 0,
	};
	const now = deps.now ?? Date.now;
	const deadline = now() + TIME_BUDGET_MS;
	const token = randomUUID();
	if (!(await deps.kvClient.acquireLock(LOCK_KEY, token, LOCK_TTL_SECONDS))) return result;
	result.ran = true;
	const erasedPassIds = new Set<string>();
	try {
		let pageState: string | null = null;
		do {
			if (now() >= deadline || !(await deps.kvClient.extendLock(LOCK_KEY, token, LOCK_TTL_SECONDS))) return result;
			const page = await deps.repository.scanPurchases(PAGE_SIZE, pageState);
			for (const scanned of page.rows) {
				if (!consentDetailsExpired(scanned, scanned.purchased_at, now())) continue;
				const target = {
					userId: scanned.purchaser_id,
					checkoutSessionId: scanned.checkout_session_id,
					paymentIntentId: scanned.payment_intent_id ?? null,
				};
				if (await deps.isOnHold(target)) continue;
				const current = await deps.repository.findPurchase(scanned.checkout_session_id);
				if (!current || !consentDetailsExpired(current, current.purchased_at, now())) continue;
				await deps.repository.updatePurchase(current, {
					waiver_consent_ip: Db.clear(),
					waiver_consent_user_agent: Db.clear(),
				});
				result.purchases += 1;
			}
			pageState = page.pageState;
		} while (pageState);
		do {
			if (now() >= deadline || !(await deps.kvClient.extendLock(LOCK_KEY, token, LOCK_TTL_SECONDS))) return result;
			const page = await deps.repository.scanOperatorPasses(PAGE_SIZE, pageState);
			for (const pass of page.rows) {
				if (!consentDetailsExpired(pass, null, now())) continue;
				const target = {
					userId: pass.user_id,
					checkoutSessionId: pass.checkout_session_id ?? null,
					paymentIntentId: pass.payment_intent_id ?? null,
				};
				if (await deps.isOnHold(target)) continue;
				await deps.repository.clearOperatorPassConsentDetails(pass.user_id);
				result.passes += 1;
			}
			pageState = page.pageState;
		} while (pageState);
		do {
			if (now() >= deadline || !(await deps.kvClient.extendLock(LOCK_KEY, token, LOCK_TTL_SECONDS))) return result;
			const page = await deps.repository.scanCertificatePages(PAGE_SIZE, pageState);
			for (const certificate of page.rows) {
				if (
					!certificate.purchase_id ||
					erasedPassIds.has(certificate.pass_id) ||
					!cancelledPageExpired(certificate, now())
				) {
					continue;
				}
				const purchase = await deps.repository.findPurchase(certificate.purchase_id);
				if (!purchase) continue;
				const target = {
					userId: purchase.purchaser_id,
					checkoutSessionId: purchase.checkout_session_id,
					paymentIntentId: purchase.payment_intent_id ?? null,
				};
				if (await deps.isOnHold(target)) continue;
				await deps.repository.clearCertificatePersonalData(certificate.pass_id);
				if (purchase.certificate_names != null) {
					const names = replaceOperatorPassCertificateName(purchase, certificate.pass_id, '');
					if (names.some((name, position) => name !== purchase.certificate_names?.[position])) {
						await deps.repository.updatePurchase(purchase, {certificate_names: Db.set(names)});
					}
				}
				erasedPassIds.add(certificate.pass_id);
				result.cancelledPages += 1;
			}
			pageState = page.pageState;
		} while (pageState);
		do {
			if (now() >= deadline || !(await deps.kvClient.extendLock(LOCK_KEY, token, LOCK_TTL_SECONDS))) return result;
			const page = await deps.repository.scanVisionaryActivations(PAGE_SIZE, pageState);
			for (const activation of page.rows) {
				const expired =
					(activation.consent_ip != null || activation.consent_user_agent != null) &&
					activation.activated_at != null &&
					activation.activated_at.getTime() + OPERATOR_PASS_CONSENT_DETAILS_RETENTION_MS <= now();
				if (!expired) continue;
				await deps.repository.updateVisionaryActivation(activation, {
					consent_ip: Db.clear(),
					consent_user_agent: Db.clear(),
				});
				result.visionaryActivations += 1;
			}
			pageState = page.pageState;
		} while (pageState);
		return result;
	} finally {
		await deps.kvClient.releaseLock(LOCK_KEY, token);
	}
}

const operatorPassConsentRetention: WorkerTaskHandler = async (_payload, helpers) => {
	if (Config.instance.selfHosted) return;
	const {kvClient, operatorPassRepository} = getWorkerDependencies();
	const disputes = getPaymentDisputeRepository();
	const result = await runOperatorPassConsentRetention({
		kvClient,
		repository: operatorPassRepository,
		isOnHold: (target) => isOperatorPassPurchaseDisputed(disputes, target),
	});
	if (result.purchases > 0 || result.passes > 0 || result.cancelledPages > 0 || result.visionaryActivations > 0) {
		helpers.logger.info(
			result,
			'Cleared Operator Pass checkout IP addresses, browser details and cancelled certificate pages past retention',
		);
	}
};

export default operatorPassConsentRetention;
