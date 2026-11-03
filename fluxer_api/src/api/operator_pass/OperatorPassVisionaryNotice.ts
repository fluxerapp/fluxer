// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import type {UserID} from '@app/api/BrandedTypes';
import {Db} from '@app/api/database/CassandraTypes';
import type {
	OperatorPassRemovalRow,
	OperatorPassVisionaryActivationRow,
	OperatorPassVisionaryNoticeSkipReason,
} from '@app/api/database/types/OperatorPassTypes';
import type {User} from '@app/api/models/User';
import {isOperatorPassPermanentlyRemoved} from '@app/api/operator_pass/OperatorPassEntitlementService';
import type {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import {
	emptyOperatorPassVisionaryActivation,
	findOperatorPassVisionaryPaidAt,
	isOperatorPassVisionary,
	isOperatorPassVisionaryAccountInactive,
	isOperatorPassVisionaryPendingDeletion,
	resolveOperatorPassVisionarySince,
} from '@app/api/operator_pass/OperatorPassVisionaryService';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import type {LoggerInterface} from '@fluxer/logger/src/LoggerInterface';
import type {IBulkEmailSession, IEmailService} from '@pkgs/email/src/IEmailService';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {ms, seconds} from 'itty-time';

export const OPERATOR_PASS_VISIONARY_NOTICE_TASK = 'operatorPassVisionaryNotice';
const OPERATOR_PASS_VISIONARY_NOTICE_LOCK_KEY = 'operator_pass:visionary_notice:lock';

const EMAIL_CLAIM_PREFIX = 'operator_pass:visionary_notice:claim:';
const DM_CLAIM_PREFIX = 'operator_pass:visionary_notice:dm_claim:';
const CLAIM_TTL_SECONDS = seconds('30 days');
const LOCK_TTL_SECONDS = 120;
const PAGE_SIZE = 100;
const CONCURRENCY = 8;
const TIME_BUDGET_MS = ms('50 seconds');
const MARK_ATTEMPTS = 3;

type NoticeOutcome = 'send' | 'emailed' | OperatorPassVisionaryNoticeSkipReason;

export interface OperatorPassVisionaryNoticeCounts {
	recipients: number;
	emailed: number;
	dm_sent: number;
	skipped: Record<OperatorPassVisionaryNoticeSkipReason, number>;
	date_unknown: number;
}

export interface OperatorPassVisionaryNoticeDependencies {
	kvClient: IKVProvider;
	repository: Pick<
		OperatorPassRepository,
		'findVisionaryActivation' | 'createVisionaryActivation' | 'updateVisionaryActivation' | 'findRemoval'
	>;
	userRepository: Pick<
		UserRepository,
		'listVisionarySlots' | 'findUnique' | 'findGiftCodesByRedeemer' | 'findPaymentsByUserId'
	>;
	emailService: IEmailService;
	logger: LoggerInterface;
}

export interface OperatorPassVisionaryNoticeRun {
	includeDm: boolean;
	afterSlot: number;
	turnOnUrl: string;
	isLabsMember: (userId: UserID) => Promise<boolean>;
	sendDm: (user: User) => Promise<boolean>;
	now?: () => number;
}

export interface OperatorPassVisionaryNoticeRunResult {
	ran: boolean;
	emailed: number;
	dm_sent: number;
	failed: number;
	next_slot: number | null;
}

function classify(
	user: User | null,
	activation: OperatorPassVisionaryActivationRow | null,
	removal: OperatorPassRemovalRow | null,
): NoticeOutcome {
	if (!user || !isOperatorPassVisionary(user) || !user.premiumSince) return 'not_visionary';
	if (isOperatorPassVisionaryAccountInactive(user)) return 'inactive';
	if (isOperatorPassVisionaryPendingDeletion(user)) return 'pending_deletion';
	if (isOperatorPassPermanentlyRemoved(removal)) return 'removed';
	if (activation?.activated_at) return 'turned_on';
	if (activation?.notice_emailed_at) return 'emailed';
	if (!user.email || !user.emailVerified || user.emailBounced) return 'no_email';
	return 'send';
}

async function listSlotUsers(
	deps: OperatorPassVisionaryNoticeDependencies,
	afterSlot: number,
): Promise<Array<{slot: number; userId: UserID}>> {
	return (await deps.userRepository.listVisionarySlots())
		.filter((slot) => slot.userId !== null && slot.userId > 0n && slot.slotIndex > afterSlot)
		.map((slot) => ({slot: slot.slotIndex, userId: slot.userId as UserID}))
		.sort((left, right) => left.slot - right.slot);
}

async function loadRecipient(deps: OperatorPassVisionaryNoticeDependencies, userId: UserID) {
	const [user, activation, removal] = await Promise.all([
		deps.userRepository.findUnique(userId),
		deps.repository.findVisionaryActivation(userId),
		deps.repository.findRemoval(userId),
	]);
	return {user, activation, removal, outcome: classify(user, activation, removal)};
}

export async function assessOperatorPassVisionaryNotice(
	deps: OperatorPassVisionaryNoticeDependencies,
): Promise<OperatorPassVisionaryNoticeCounts> {
	const counts: OperatorPassVisionaryNoticeCounts = {
		recipients: 0,
		emailed: 0,
		dm_sent: 0,
		skipped: {not_visionary: 0, inactive: 0, pending_deletion: 0, removed: 0, turned_on: 0, no_email: 0},
		date_unknown: 0,
	};
	for (const {userId} of await listSlotUsers(deps, -1)) {
		const {user, activation, outcome} = await loadRecipient(deps, userId);
		if (activation?.notice_dm_sent_at) counts.dm_sent++;
		if (outcome === 'send') counts.recipients++;
		else if (outcome === 'emailed') counts.emailed++;
		else counts.skipped[outcome]++;
		if (!user || (outcome !== 'send' && outcome !== 'emailed' && outcome !== 'no_email')) continue;
		if (!(await findOperatorPassVisionaryPaidAt(deps.userRepository, userId))) counts.date_unknown++;
	}
	return counts;
}

async function markActivation(
	deps: OperatorPassVisionaryNoticeDependencies,
	userId: UserID,
	patch: Parameters<OperatorPassRepository['updateVisionaryActivation']>[1],
): Promise<void> {
	for (let attempt = 1; attempt <= MARK_ATTEMPTS; attempt++) {
		let current = await deps.repository.findVisionaryActivation(userId);
		if (!current) {
			await deps.repository.createVisionaryActivation(emptyOperatorPassVisionaryActivation(userId));
			current = await deps.repository.findVisionaryActivation(userId);
		}
		if (current && (await deps.repository.updateVisionaryActivation(current, patch))) return;
	}
	deps.logger.error(
		{userId, fields: Object.keys(patch)},
		'Could not record the Visionary Operator Pass email state. The claim key blocks a resend until it expires',
	);
}

async function sendEmail(
	deps: OperatorPassVisionaryNoticeDependencies,
	run: OperatorPassVisionaryNoticeRun,
	sender: IBulkEmailSession,
	user: User,
): Promise<boolean | null> {
	const claimKey = `${EMAIL_CLAIM_PREFIX}${user.id}`;
	if (!(await deps.kvClient.setnx(claimKey, '1', CLAIM_TTL_SECONDS))) return null;
	let sent = false;
	try {
		const labsMember = await run.isLabsMember(user.id);
		const visionaryDate = await resolveOperatorPassVisionarySince(deps.userRepository, user);
		if (!visionaryDate) throw new Error('Visionary has no date to show');
		sent = await sender.send(
			'operator_pass_visionary_notice',
			user.email ?? '',
			{
				visionaryDate,
				labsMember,
				turnOnUrl: run.turnOnUrl,
			},
			user.locale,
		);
	} catch (error) {
		deps.logger.error({error, userId: user.id}, 'Visionary Operator Pass email failed');
	}
	if (!sent) {
		await deps.kvClient.del(claimKey);
		return false;
	}
	await markActivation(deps, user.id, {notice_emailed_at: Db.set(new Date()), notice_skip_reason: Db.clear()});
	return true;
}

async function sendDm(
	deps: OperatorPassVisionaryNoticeDependencies,
	run: OperatorPassVisionaryNoticeRun,
	user: User,
): Promise<boolean> {
	const claimKey = `${DM_CLAIM_PREFIX}${user.id}`;
	if (!(await deps.kvClient.setnx(claimKey, '1', CLAIM_TTL_SECONDS))) return false;
	if (!(await run.sendDm(user))) {
		await deps.kvClient.del(claimKey);
		return false;
	}
	await markActivation(deps, user.id, {notice_dm_sent_at: Db.set(new Date())});
	return true;
}

async function processRecipient(
	deps: OperatorPassVisionaryNoticeDependencies,
	run: OperatorPassVisionaryNoticeRun,
	sender: IBulkEmailSession,
	userId: UserID,
	result: OperatorPassVisionaryNoticeRunResult,
): Promise<void> {
	const {user, activation, outcome} = await loadRecipient(deps, userId);
	if (!user) return;
	let reached = outcome === 'no_email' || outcome === 'emailed';
	if (outcome === 'send') {
		const sent = await sendEmail(deps, run, sender, user);
		if (sent === false) result.failed++;
		if (sent) result.emailed++;
		reached = sent === true;
	} else if (outcome !== 'emailed' && activation?.notice_skip_reason !== outcome) {
		await markActivation(deps, userId, {notice_skip_reason: Db.set(outcome)});
	}
	if (run.includeDm && reached && !activation?.notice_dm_sent_at && (await sendDm(deps, run, user))) {
		result.dm_sent++;
	}
}

export async function runOperatorPassVisionaryNotice(
	deps: OperatorPassVisionaryNoticeDependencies,
	run: OperatorPassVisionaryNoticeRun,
): Promise<OperatorPassVisionaryNoticeRunResult> {
	const result: OperatorPassVisionaryNoticeRunResult = {ran: false, emailed: 0, dm_sent: 0, failed: 0, next_slot: null};
	const now = run.now ?? Date.now;
	const deadline = now() + TIME_BUDGET_MS;
	const lockToken = randomUUID();
	if (!(await deps.kvClient.acquireLock(OPERATOR_PASS_VISIONARY_NOTICE_LOCK_KEY, lockToken, LOCK_TTL_SECONDS))) {
		return result;
	}
	result.ran = true;
	let sender: IBulkEmailSession | null = null;
	try {
		sender = await deps.emailService.openBulkSession();
		const slots = await listSlotUsers(deps, run.afterSlot);
		for (let start = 0; start < slots.length; start += PAGE_SIZE) {
			if (now() >= deadline) {
				result.next_slot = slots[start - 1]?.slot ?? run.afterSlot;
				return result;
			}
			if (!(await deps.kvClient.extendLock(OPERATOR_PASS_VISIONARY_NOTICE_LOCK_KEY, lockToken, LOCK_TTL_SECONDS))) {
				result.next_slot = slots[start - 1]?.slot ?? run.afterSlot;
				return result;
			}
			const page = slots.slice(start, start + PAGE_SIZE);
			let next = 0;
			const activeSender = sender;
			const lane = async (): Promise<void> => {
				while (next < page.length) {
					const entry = page[next++]!;
					await processRecipient(deps, run, activeSender, entry.userId, result);
				}
			};
			await Promise.all(Array.from({length: Math.min(CONCURRENCY, page.length)}, lane));
		}
		return result;
	} finally {
		await sender?.close().catch((error) => {
			deps.logger.warn({error}, 'Failed to close the Visionary Operator Pass email session');
		});
		await deps.kvClient.releaseLock(OPERATOR_PASS_VISIONARY_NOTICE_LOCK_KEY, lockToken);
	}
}

export async function isOperatorPassVisionaryNoticeRunning(kvClient: Pick<IKVProvider, 'get'>): Promise<boolean> {
	return (await kvClient.get(OPERATOR_PASS_VISIONARY_NOTICE_LOCK_KEY)) !== null;
}
