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
import type {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import {operatorPassNow} from '@app/api/operator_pass/OperatorPassSchedule';
import {liveRevisionPatch, type OperatorPassWallEntryStore} from '@app/api/operator_pass/OperatorPassWallEntryStore';
import {revisionStatuses} from '@app/api/operator_pass/OperatorPassWallHistory';
import {isOperatorPassWallListed} from '@app/api/operator_pass/OperatorPassWallPresentation';
import type {OperatorPassWallPublicationService} from '@app/api/operator_pass/OperatorPassWallPublicationService';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {UnknownOperatorPassWallEntryError} from '@fluxer/errors/src/domains/operator_pass/UnknownOperatorPassWallEntryError';
import {compareSnowflakes} from '@fluxer/snowflake/src/SnowflakeComparison';

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
		private readonly entries: OperatorPassWallEntryStore,
		private readonly publication: OperatorPassWallPublicationService,
	) {}

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

	async clearQuote(
		userId: UserID,
		adminId: UserID,
		reviewed: string,
		input: OperatorPassWallDecisionInput,
	): Promise<OperatorPassWallDecisionResult> {
		const now = operatorPassNow();
		const next = await this.entries.mutateExisting(userId, now, adminId, (current) => {
			if (current.quote == null) {
				throw new UnknownOperatorPassWallEntryError();
			}
			if (current.quote !== reviewed) {
				throw InputValidationError.create(
					'quote',
					'The holder changed the quote after this page loaded. Reload the page and review the new quote.',
				);
			}
			return {
				patch: {quote: Db.set(null), updated_at: Db.set(now)},
				events: [{kind: 'quote_cleared', reason: input.reason}],
			};
		});
		const decision = await this.entries.appendDecision(userId, {
			kind: 'clear_quote',
			revisionId: null,
			decidedBy: adminId,
			decidedAt: now,
			reason: input.reason,
			note: input.note,
			statementId: randomUUID(),
		});
		await this.publication.publishChanges([userId], 'preserve');
		return {entry: next, revision: null, decision};
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
}
