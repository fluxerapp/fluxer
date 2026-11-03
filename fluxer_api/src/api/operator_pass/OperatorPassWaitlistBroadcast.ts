// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import type {OperatorPassWaitlistRow} from '@app/api/database/types/OperatorPassTypes';
import type {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import type {LoggerInterface} from '@fluxer/logger/src/LoggerInterface';
import type {IBulkEmailSession, IEmailService} from '@pkgs/email/src/IEmailService';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {ms, seconds} from 'itty-time';

const LOCK_TTL_SECONDS = 120;
const CLAIM_MARGIN_SECONDS = seconds('7 days');
const MARK_ATTEMPTS = 3;
const MARK_RETRY_DELAY_MS = 200;
const DEFAULT_PAGE_SIZE = 100;
const DEFAULT_TIME_BUDGET_MS = ms('50 seconds');
const DEFAULT_CONCURRENCY = 8;

export interface OperatorPassWaitlistBroadcastDependencies {
	kvClient: IKVProvider;
	operatorPassRepository: OperatorPassRepository;
	emailService: IEmailService;
	logger: LoggerInterface;
}

export interface OperatorPassWaitlistBroadcastOptions {
	pageSize?: number;
	timeBudgetMs?: number;
	concurrency?: number;
	now?: () => number;
}

export interface OperatorPassWaitlistBroadcastResult {
	ran: boolean;
	scanned: number;
	sent: number;
	failed: number;
	exhausted: boolean;
}

export interface OperatorPassWaitlistBroadcast {
	name: string;
	lockKey: string;
	claimPrefix: string;
	closesAt(): Date | null;
	isOpen(): boolean;
	isPending(entry: OperatorPassWaitlistRow): boolean;
	send(sender: IBulkEmailSession, entry: OperatorPassWaitlistRow): Promise<boolean>;
	markSent(repository: OperatorPassRepository, email: string, sentAt: Date): Promise<void>;
}

function claimTtlSeconds(broadcast: OperatorPassWaitlistBroadcast): number {
	const closesAt = broadcast.closesAt();
	const remaining = closesAt ? Math.ceil((closesAt.getTime() - Date.now()) / 1000) : 0;
	return Math.max(remaining, 0) + CLAIM_MARGIN_SECONDS;
}

async function markSentWithRetry(
	deps: OperatorPassWaitlistBroadcastDependencies,
	broadcast: OperatorPassWaitlistBroadcast,
	entry: OperatorPassWaitlistRow,
): Promise<void> {
	for (let attempt = 1; attempt <= MARK_ATTEMPTS; attempt++) {
		try {
			await broadcast.markSent(deps.operatorPassRepository, entry.email, new Date());
			return;
		} catch (error) {
			if (attempt === MARK_ATTEMPTS) {
				deps.logger.error(
					{error, entryId: entry.entry_id, attempts: attempt},
					`Operator Pass ${broadcast.name} email went out but could not be marked sent. The claim key blocks a resend until it expires`,
				);
				return;
			}
			await new Promise((resolve) => setTimeout(resolve, MARK_RETRY_DELAY_MS * attempt));
		}
	}
}

function shouldSend(
	broadcast: OperatorPassWaitlistBroadcast,
	entry: OperatorPassWaitlistRow | null,
): entry is OperatorPassWaitlistRow {
	return entry != null && entry.status === 'confirmed' && broadcast.isPending(entry);
}

async function processEntry(
	deps: OperatorPassWaitlistBroadcastDependencies,
	broadcast: OperatorPassWaitlistBroadcast,
	sender: IBulkEmailSession,
	scanned: OperatorPassWaitlistRow,
): Promise<'skipped' | 'sent' | 'failed'> {
	if (!shouldSend(broadcast, scanned) || !broadcast.isOpen()) return 'skipped';
	const claimKey = `${broadcast.claimPrefix}${scanned.entry_id}`;
	if (!(await deps.kvClient.setnx(claimKey, '1', claimTtlSeconds(broadcast)))) return 'skipped';
	let sent = false;
	try {
		const entry = await deps.operatorPassRepository.findWaitlistEntryByEmail(scanned.email);
		if (!shouldSend(broadcast, entry) || entry.entry_id !== scanned.entry_id || !broadcast.isOpen()) {
			await deps.kvClient.del(claimKey);
			return 'skipped';
		}
		sent = await broadcast.send(sender, entry);
	} catch (error) {
		deps.logger.error({error, entryId: scanned.entry_id}, `Operator Pass ${broadcast.name} email failed`);
	}
	if (!sent) {
		await deps.kvClient.del(claimKey);
		return 'failed';
	}
	await markSentWithRetry(deps, broadcast, scanned);
	return 'sent';
}

async function processPage(
	deps: OperatorPassWaitlistBroadcastDependencies,
	broadcast: OperatorPassWaitlistBroadcast,
	sender: IBulkEmailSession,
	entries: ReadonlyArray<OperatorPassWaitlistRow>,
	result: OperatorPassWaitlistBroadcastResult,
	budget: {concurrency: number; deadline: number; now: () => number},
): Promise<boolean> {
	let next = 0;
	let outOfTime = false;
	const lane = async (): Promise<void> => {
		while (next < entries.length) {
			if (budget.now() >= budget.deadline) {
				outOfTime = true;
				return;
			}
			const entry = entries[next++]!;
			result.scanned += 1;
			const outcome = await processEntry(deps, broadcast, sender, entry);
			if (outcome === 'sent') result.sent += 1;
			if (outcome === 'failed') result.failed += 1;
		}
	};
	const lanes = Math.max(1, Math.min(budget.concurrency, entries.length));
	const settled = await Promise.allSettled(Array.from({length: lanes}, lane));
	const rejected = settled.find((outcome) => outcome.status === 'rejected');
	if (rejected) throw rejected.reason;
	return !outOfTime;
}

export async function runOperatorPassWaitlistBroadcast(
	deps: OperatorPassWaitlistBroadcastDependencies,
	broadcast: OperatorPassWaitlistBroadcast,
	options: OperatorPassWaitlistBroadcastOptions = {},
): Promise<OperatorPassWaitlistBroadcastResult> {
	const result: OperatorPassWaitlistBroadcastResult = {ran: false, scanned: 0, sent: 0, failed: 0, exhausted: false};
	if (!broadcast.isOpen()) return result;
	const now = options.now ?? Date.now;
	const pageSize = options.pageSize ?? DEFAULT_PAGE_SIZE;
	const budget = {
		concurrency: options.concurrency ?? DEFAULT_CONCURRENCY,
		deadline: now() + (options.timeBudgetMs ?? DEFAULT_TIME_BUDGET_MS),
		now,
	};
	const lockToken = randomUUID();
	if (!(await deps.kvClient.acquireLock(broadcast.lockKey, lockToken, LOCK_TTL_SECONDS))) {
		return result;
	}
	result.ran = true;
	let sender: IBulkEmailSession | null = null;
	try {
		sender = await deps.emailService.openBulkSession();
		let pageState: string | null = null;
		do {
			if (now() >= budget.deadline || !broadcast.isOpen()) return result;
			if (!(await deps.kvClient.extendLock(broadcast.lockKey, lockToken, LOCK_TTL_SECONDS))) {
				return result;
			}
			const page = await deps.operatorPassRepository.scanWaitlistPage(pageSize, pageState);
			if (!(await processPage(deps, broadcast, sender, page.entries, result, budget))) return result;
			pageState = page.pageState;
		} while (pageState);
		result.exhausted = true;
		return result;
	} finally {
		await sender?.close().catch((error) => {
			deps.logger.warn({error}, `Failed to close the Operator Pass ${broadcast.name} email session`);
		});
		await deps.kvClient.releaseLock(broadcast.lockKey, lockToken);
	}
}
