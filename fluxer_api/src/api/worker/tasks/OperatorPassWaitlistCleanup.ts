// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {Config} from '@app/api/Config';
import type {OperatorPassWaitlistRow} from '@app/api/database/types/OperatorPassTypes';
import type {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {ms} from 'itty-time';

const OPERATOR_PASS_WAITLIST_CLEANUP_LOCK_KEY = 'operator_pass:waitlist_cleanup:lock';

const OPERATOR_PASS_WAITLIST_PENDING_RETENTION_MS = ms('7 days');
const OPERATOR_PASS_WAITLIST_RETENTION_MS = ms('30 days');

const LOCK_TTL_SECONDS = 120;
const DEFAULT_PAGE_SIZE = 100;
const DEFAULT_TIME_BUDGET_MS = ms('50 seconds');

interface OperatorPassWaitlistCleanupDependencies {
	kvClient: IKVProvider;
	operatorPassRepository: OperatorPassRepository;
}

interface OperatorPassWaitlistCleanupOptions {
	pageSize?: number;
	timeBudgetMs?: number;
	now?: () => number;
}

interface OperatorPassWaitlistCleanupResult {
	ran: boolean;
	scanned: number;
	deleted: number;
	exhausted: boolean;
}

function getStatusDeleteAt(entry: OperatorPassWaitlistRow): number | null {
	switch (entry.status) {
		case 'pending':
			return entry.updated_at.getTime() + OPERATOR_PASS_WAITLIST_PENDING_RETENTION_MS;
		case 'unsubscribed':
			return (entry.unsubscribed_at ?? entry.updated_at).getTime() + OPERATOR_PASS_WAITLIST_RETENTION_MS;
		case 'bounced':
			return entry.updated_at.getTime() + OPERATOR_PASS_WAITLIST_RETENTION_MS;
		case 'confirmed':
			return entry.sale_email_sent_at ? entry.sale_email_sent_at.getTime() + OPERATOR_PASS_WAITLIST_RETENTION_MS : null;
	}
}

function getOperatorPassWaitlistDeleteAt(
	entry: OperatorPassWaitlistRow,
	priceRisesAt: Date | null = Config.operatorPass.priceRisesAt,
): number {
	const backstop = priceRisesAt
		? priceRisesAt.getTime() + OPERATOR_PASS_WAITLIST_RETENTION_MS
		: Number.POSITIVE_INFINITY;
	return Math.min(getStatusDeleteAt(entry) ?? backstop, backstop);
}

function isDue(entry: OperatorPassWaitlistRow | null, nowMs: number): entry is OperatorPassWaitlistRow {
	return entry != null && getOperatorPassWaitlistDeleteAt(entry) <= nowMs;
}

async function runOperatorPassWaitlistCleanup(
	deps: OperatorPassWaitlistCleanupDependencies,
	options: OperatorPassWaitlistCleanupOptions = {},
): Promise<OperatorPassWaitlistCleanupResult> {
	const result: OperatorPassWaitlistCleanupResult = {ran: false, scanned: 0, deleted: 0, exhausted: false};
	const now = options.now ?? Date.now;
	const pageSize = options.pageSize ?? DEFAULT_PAGE_SIZE;
	const deadline = now() + (options.timeBudgetMs ?? DEFAULT_TIME_BUDGET_MS);
	const lockToken = randomUUID();
	if (!(await deps.kvClient.acquireLock(OPERATOR_PASS_WAITLIST_CLEANUP_LOCK_KEY, lockToken, LOCK_TTL_SECONDS))) {
		return result;
	}
	result.ran = true;
	try {
		let pageState: string | null = null;
		do {
			if (now() >= deadline) return result;
			if (!(await deps.kvClient.extendLock(OPERATOR_PASS_WAITLIST_CLEANUP_LOCK_KEY, lockToken, LOCK_TTL_SECONDS))) {
				return result;
			}
			const page = await deps.operatorPassRepository.scanWaitlistPage(pageSize, pageState);
			for (const scanned of page.entries) {
				result.scanned += 1;
				if (!isDue(scanned, Date.now())) continue;
				const entry = await deps.operatorPassRepository.findWaitlistEntryByEmail(scanned.email);
				if (!isDue(entry, Date.now())) continue;
				if (await deps.operatorPassRepository.deleteWaitlistEntry(entry)) result.deleted += 1;
			}
			pageState = page.pageState;
		} while (pageState);
		result.exhausted = true;
		return result;
	} finally {
		await deps.kvClient.releaseLock(OPERATOR_PASS_WAITLIST_CLEANUP_LOCK_KEY, lockToken);
	}
}

const operatorPassWaitlistCleanup: WorkerTaskHandler = async (_payload, helpers) => {
	const {kvClient, operatorPassRepository} = getWorkerDependencies();
	const result = await runOperatorPassWaitlistCleanup({kvClient, operatorPassRepository});
	if (result.deleted > 0) {
		helpers.logger.info({...result}, 'Operator Pass waitlist cleanup ran');
	}
};

export default operatorPassWaitlistCleanup;
