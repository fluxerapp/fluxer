// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import type {UserID} from '@app/api/BrandedTypes';
import {Db} from '@app/api/database/CassandraTypes';
import type {
	OperatorPassWallDecisionRow,
	OperatorPassWallDeclineReason,
	OperatorPassWallEntryRow,
	OperatorPassWallRevisionRow,
} from '@app/api/database/types/OperatorPassTypes';
import type {User} from '@app/api/models/User';
import type {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import {operatorPassNow} from '@app/api/operator_pass/OperatorPassSchedule';
import {liveRevisionPatch, type OperatorPassWallEntryStore} from '@app/api/operator_pass/OperatorPassWallEntryStore';
import {revisionStatuses} from '@app/api/operator_pass/OperatorPassWallHistory';
import {isOperatorPassWallListed} from '@app/api/operator_pass/OperatorPassWallPresentation';
import type {OperatorPassWallPublicationService} from '@app/api/operator_pass/OperatorPassWallPublicationService';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {mapWithConcurrency} from '@app/api/utils/ConcurrencyUtils';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {UnknownOperatorPassWallEntryError} from '@fluxer/errors/src/domains/operator_pass/UnknownOperatorPassWallEntryError';
import {compareSnowflakes} from '@fluxer/snowflake/src/SnowflakeComparison';

const PENDING_LOOKUP_CONCURRENCY = 16;

interface OperatorPassWallDecisionInput {
	reason: OperatorPassWallDeclineReason;
	note: string;
}

export interface OperatorPassWallDecisionResult {
	entry: OperatorPassWallEntryRow;
	revision: OperatorPassWallRevisionRow | null;
	decision: OperatorPassWallDecisionRow;
}

export class OperatorPassWallModerationService {
	constructor(
		private readonly repository: OperatorPassRepository,
		private readonly userRepository: UserRepository,
		private readonly entries: OperatorPassWallEntryStore,
		private readonly publication: OperatorPassWallPublicationService,
	) {}

	async approve(userId: UserID, adminId: UserID, revisionId?: bigint): Promise<OperatorPassWallDecisionResult> {
		const entry = await this.requireListed(userId);
		const targetId = revisionId ?? entry.pending_revision_id;
		if (targetId == null || targetId !== entry.pending_revision_id) {
			throw new UnknownOperatorPassWallEntryError();
		}
		const revision = await this.findRevision(userId, targetId);
		const now = operatorPassNow();
		let previousAvatar: string | null = null;
		const next = await this.entries.mutate(userId, now, adminId, (current) => {
			if (!isOperatorPassWallListed(current) || current.pending_revision_id !== targetId) {
				throw new UnknownOperatorPassWallEntryError();
			}
			previousAvatar = current.live_avatar ?? null;
			return {
				patch: {
					...liveRevisionPatch(revision),
					pending_revision_id: Db.set(null),
					pending_name: Db.set(null),
					pending_avatar: Db.set(null),
					pending_source: Db.set(null),
					pending_since: Db.set(null),
					declined_revision_id: Db.set(null),
					declined_name: Db.set(null),
					declined_avatar: Db.set(null),
					decline_reason: Db.set(null),
					decline_note: Db.set(null),
					decline_statement_id: Db.set(null),
					reviewed_at: Db.set(now),
					reviewed_by: Db.set(adminId),
					updated_at: Db.set(now),
				},
				events: [{kind: 'approved', name: revision.name}],
			};
		});
		const decision = await this.entries.appendDecision(userId, {
			kind: 'approve',
			revisionId: targetId,
			decidedBy: adminId,
			decidedAt: now,
		});
		await this.publication.publishAvatarIfServed(next);
		await this.publication.publishChanges([userId], 'preserve');
		if (previousAvatar && previousAvatar !== revision.avatar) {
			await this.publication.releaseAvatar(previousAvatar);
		}
		return {entry: next, revision, decision};
	}

	async decline(
		userId: UserID,
		adminId: UserID,
		input: OperatorPassWallDecisionInput,
	): Promise<OperatorPassWallDecisionResult> {
		const entry = await this.requireListed(userId);
		const targetId = entry.pending_revision_id;
		if (targetId == null) {
			throw new UnknownOperatorPassWallEntryError();
		}
		const revision = await this.findRevision(userId, targetId);
		const now = operatorPassNow();
		const statementId = randomUUID();
		const next = await this.entries.mutate(userId, now, adminId, (current) => {
			if (!isOperatorPassWallListed(current) || current.pending_revision_id !== targetId) {
				throw new UnknownOperatorPassWallEntryError();
			}
			return {
				patch: {
					pending_revision_id: Db.set(null),
					pending_name: Db.set(null),
					pending_avatar: Db.set(null),
					pending_source: Db.set(null),
					pending_since: Db.set(null),
					declined_revision_id: Db.set(targetId),
					declined_name: Db.set(revision.name),
					declined_avatar: Db.set(revision.avatar ?? null),
					decline_reason: Db.set(input.reason),
					decline_note: Db.set(input.note),
					decline_statement_id: Db.set(statementId),
					reviewed_at: Db.set(now),
					reviewed_by: Db.set(adminId),
					updated_at: Db.set(now),
				},
				events: [{kind: 'declined', name: revision.name, reason: input.reason}],
			};
		});
		const decision = await this.entries.appendDecision(userId, {
			kind: 'decline',
			revisionId: targetId,
			decidedBy: adminId,
			decidedAt: now,
			reason: input.reason,
			note: input.note,
			statementId,
		});
		return {entry: next, revision, decision};
	}

	async revert(userId: UserID, adminId: UserID, revisionId: bigint): Promise<OperatorPassWallDecisionResult> {
		const entry = await this.requireListed(userId);
		const [revisions, decisions] = await Promise.all([
			this.repository.listWallRevisions(userId),
			this.repository.listWallDecisions(userId),
		]);
		const statuses = revisionStatuses(revisions, decisions);
		const target = revisions.find((revision) => revision.revision_id === revisionId);
		const status = statuses.get(revisionId)?.status;
		const everApproved = decisions.some(
			(decision) => decision.kind === 'approve' && decision.revision_id === revisionId,
		);
		if (!target || !everApproved || status === 'declined') {
			throw new UnknownOperatorPassWallEntryError();
		}
		if (target.business) {
			throw InputValidationError.create(
				'revision_id',
				'A business listing cannot come back through a revert. Set it as a business listing again so the purchase is checked.',
			);
		}
		const now = operatorPassNow();
		let previousAvatar: string | null = null;
		const next = await this.entries.mutate(userId, now, adminId, (current) => {
			if (!isOperatorPassWallListed(current) || current.live_revision_id !== entry.live_revision_id) {
				throw new UnknownOperatorPassWallEntryError();
			}
			previousAvatar = current.live_avatar ?? null;
			return {
				patch: {...liveRevisionPatch(target), updated_at: Db.set(now)},
				events: [{kind: 'approved', name: target.name, reason: 'revert'}],
			};
		});
		const decision = await this.entries.appendDecision(userId, {
			kind: 'revert',
			revisionId,
			decidedBy: adminId,
			decidedAt: now,
		});
		await this.publication.publishAvatarIfServed(next);
		await this.publication.publishChanges([userId], 'preserve');
		if (previousAvatar && previousAvatar !== target.avatar) {
			await this.publication.releaseAvatar(previousAvatar);
		}
		return {entry: next, revision: target, decision};
	}

	async forceFallback(
		userId: UserID,
		adminId: UserID,
		input: OperatorPassWallDecisionInput,
	): Promise<OperatorPassWallDecisionResult> {
		const entry = await this.requireListed(userId);
		const personal = entry.live_business ? await this.lastPersonalRevision(userId) : null;
		const now = operatorPassNow();
		const statementId = randomUUID();
		let previousAvatar: string | null = null;
		const next = await this.entries.mutate(userId, now, adminId, (current) => {
			if (!isOperatorPassWallListed(current) || current.live_revision_id !== entry.live_revision_id) {
				throw new UnknownOperatorPassWallEntryError();
			}
			previousAvatar = current.live_avatar ?? null;
			return {
				patch: {
					...liveRevisionPatch(personal),
					decline_reason: Db.set(input.reason),
					decline_note: Db.set(input.note),
					decline_statement_id: Db.set(statementId),
					updated_at: Db.set(now),
				},
				events: [{kind: 'fallback', name: personal?.name ?? null, reason: input.reason}],
			};
		});
		const decision = await this.entries.appendDecision(userId, {
			kind: 'force_fallback',
			revisionId: entry.live_revision_id ?? null,
			decidedBy: adminId,
			decidedAt: now,
			reason: input.reason,
			note: input.note,
			statementId,
		});
		if (personal) {
			await this.recordBusinessEnded(userId, null, personal, adminId, now);
			await this.publication.publishAvatarIfServed(next);
		}
		await this.publication.publishChanges([userId], 'preserve');
		if (previousAvatar && previousAvatar !== personal?.avatar) {
			await this.publication.releaseAvatar(previousAvatar);
		}
		return {entry: next, revision: personal, decision};
	}

	async setHidden(
		userId: UserID,
		adminId: UserID,
		hidden: boolean,
		input: {reason: OperatorPassWallDeclineReason | null; note: string | null},
	): Promise<OperatorPassWallDecisionResult> {
		await this.requireListed(userId);
		const now = operatorPassNow();
		const next = await this.entries.mutate(userId, now, adminId, (current) => {
			if (!isOperatorPassWallListed(current)) {
				throw new UnknownOperatorPassWallEntryError();
			}
			const liveName = current.live_name ?? null;
			return {
				patch: {hidden: Db.set(hidden), updated_at: Db.set(now)},
				events: [
					hidden
						? {kind: 'taken_down', name: liveName, cause: 'hidden', reason: input.reason}
						: {kind: 'shown_again', name: liveName},
				],
			};
		});
		const decision = await this.entries.appendDecision(userId, {
			kind: hidden ? 'hide' : 'unhide',
			revisionId: null,
			decidedBy: adminId,
			decidedAt: now,
			reason: input.reason,
			note: input.note,
			statementId: hidden ? randomUUID() : null,
		});
		await this.publication.publishChanges([userId]);
		if (hidden && next.live_avatar) {
			await this.publication.releaseAvatar(next.live_avatar);
		}
		if (!hidden) {
			await this.publication.publishAvatarIfServed(next);
		}
		return {entry: next, revision: null, decision};
	}

	async listPending(): Promise<Array<{entry: OperatorPassWallEntryRow; user: User | null}>> {
		const rows = await mapWithConcurrency(
			await this.repository.listWallPending(),
			PENDING_LOOKUP_CONCURRENCY,
			(userId) => this.repository.findWallEntry(userId),
		);
		const entries = rows.filter(
			(entry): entry is OperatorPassWallEntryRow =>
				entry != null && entry.pending_revision_id != null && isOperatorPassWallListed(entry),
		);
		entries.sort((left, right) => (left.pending_since?.getTime() ?? 0) - (right.pending_since?.getTime() ?? 0));
		return Promise.all(
			entries.map(async (entry) => ({entry, user: await this.userRepository.findUnique(entry.user_id)})),
		);
	}

	async lastPersonalRevision(userId: UserID): Promise<OperatorPassWallRevisionRow | null> {
		const [revisions, decisions] = await Promise.all([
			this.repository.listWallRevisions(userId),
			this.repository.listWallDecisions(userId),
		]);
		const personal = new Map(
			revisions.filter((revision) => !revision.business).map((revision) => [revision.revision_id, revision]),
		);
		const removed = new Set<bigint>();
		let latest: OperatorPassWallRevisionRow | null = null;
		for (const decision of [...decisions].sort((left, right) =>
			compareSnowflakes(left.decision_id, right.decision_id),
		)) {
			const revision = decision.revision_id != null ? personal.get(decision.revision_id) : undefined;
			if (!revision) {
				continue;
			}
			if (decision.kind === 'decline' || decision.kind === 'force_fallback') {
				removed.add(revision.revision_id);
			} else if (decision.kind === 'approve' || decision.kind === 'revert') {
				latest = revision;
			}
		}
		return latest && !removed.has(latest.revision_id) ? latest : null;
	}

	async recordBusinessEnded(
		userId: UserID,
		businessRevisionId: bigint | null,
		personal: OperatorPassWallRevisionRow | null,
		decidedBy: UserID | null,
		decidedAt: Date,
	): Promise<void> {
		await this.entries.appendDecision(userId, {
			kind: personal ? 'revert' : 'force_fallback',
			revisionId: personal?.revision_id ?? businessRevisionId,
			decidedBy,
			decidedAt,
		});
	}

	private async requireListed(userId: UserID): Promise<OperatorPassWallEntryRow> {
		const entry = await this.repository.findWallEntry(userId);
		if (!entry || !isOperatorPassWallListed(entry)) {
			throw new UnknownOperatorPassWallEntryError();
		}
		return entry;
	}

	private async findRevision(userId: UserID, revisionId: bigint): Promise<OperatorPassWallRevisionRow> {
		const revision = (await this.repository.listWallRevisions(userId)).find((row) => row.revision_id === revisionId);
		if (!revision) {
			throw new UnknownOperatorPassWallEntryError();
		}
		return revision;
	}
}
