// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	BatchBuilder,
	executeConditional,
	fetchMany,
	fetchManyInChunks,
	fetchOne,
	upsertOne,
} from '@app/api/database/CassandraQueryExecution';
import {Db} from '@app/api/database/CassandraTypes';
import type {
	EnforcementDecisionBySubjectRow,
	EnforcementDecisionEvidenceRow,
	EnforcementDecisionNoteRow,
	EnforcementDecisionRow,
	EnforcementEndOutcome,
	EnforcementNoticeChannel,
	EnforcementNoticeStatus,
	EnforcementReviewQueueRow,
	EnforcementSubjectType,
} from '@app/api/database/types/EnforcementTypes';
import {
	EnforcementDecisionEvidence,
	EnforcementDecisionNotes,
	EnforcementDecisions,
	EnforcementDecisionsBySubject,
	EnforcementReviewQueue,
} from '@app/api/Tables';

const GET_DECISION_QUERY = EnforcementDecisions.select({where: EnforcementDecisions.where.eq('decision_id')});
const GET_DECISIONS_QUERY = EnforcementDecisions.select({
	where: EnforcementDecisions.where.in('decision_id', 'decision_ids'),
});
const GET_EVIDENCE_QUERY = EnforcementDecisionEvidence.select({
	where: EnforcementDecisionEvidence.where.eq('decision_id'),
});
const LIST_NOTES_QUERY = EnforcementDecisionNotes.select({
	where: EnforcementDecisionNotes.where.eq('decision_id'),
	orderBy: {col: 'note_id', direction: 'ASC'},
});

type DecisionReportLink = Pick<EnforcementDecisionRow, 'decision_id' | 'report_ids' | 'created_at'>;

function listReportLinksQuery(limit: number, paged: boolean) {
	return EnforcementDecisions.select({
		columns: ['decision_id', 'report_ids', 'created_at'],
		...(paged ? {where: EnforcementDecisions.where.tokenGt('decision_id', 'after')} : {}),
		limit,
	});
}

const REVIEW_QUEUE_BUCKET = 0;
const LIST_REVIEW_QUEUE_QUERY = EnforcementReviewQueue.select({
	columns: ['decision_id'],
	where: EnforcementReviewQueue.where.eq('bucket'),
	orderBy: {col: 'decision_id', direction: 'DESC'},
});

function listBySubjectQuery(limit: number, paged: boolean) {
	const {where} = EnforcementDecisionsBySubject;
	return EnforcementDecisionsBySubject.select({
		columns: ['decision_id'],
		where: paged
			? [where.eq('subject_type'), where.eq('subject_id'), where.lt('decision_id', 'before')]
			: [where.eq('subject_type'), where.eq('subject_id')],
		orderBy: {col: 'decision_id', direction: 'DESC'},
		limit,
	});
}

interface EnforcementDecisionCreate {
	decision: EnforcementDecisionRow;
	evidence: EnforcementDecisionEvidenceRow | null;
	firstNote: EnforcementDecisionNoteRow;
}

interface EnforcementDecisionEnd {
	outcome: EnforcementEndOutcome;
	endedAt: Date;
	endedBy: bigint;
	statement: string | null;
}

export interface EnforcementNoticeRecord {
	status: EnforcementNoticeStatus;
	channels: ReadonlySet<EnforcementNoticeChannel>;
	locale: string | null;
	text: string | null;
	sentAt: Date | null;
}

function subjectKey(decision: EnforcementDecisionRow): EnforcementDecisionBySubjectRow {
	return {subject_type: decision.subject_type, subject_id: decision.subject_id, decision_id: decision.decision_id};
}

function deleteReviewQueueRow(decisionId: bigint) {
	return EnforcementReviewQueue.deleteByPk({bucket: REVIEW_QUEUE_BUCKET, decision_id: decisionId});
}

function endPatch(end: EnforcementDecisionEnd) {
	return {
		status: Db.set<EnforcementDecisionRow['status']>('ended'),
		end_outcome: Db.set(end.outcome),
		ended_at: Db.set(end.endedAt),
		ended_by: Db.set(end.endedBy),
		end_statement: end.statement === null ? Db.clear() : Db.set(end.statement),
	};
}

const SUBJECT_DECISION_SCAN_LIMIT = 100;

export class EnforcementDecisionRepository {
	async create({decision, evidence, firstNote}: EnforcementDecisionCreate): Promise<void> {
		const batch = new BatchBuilder()
			.addPrepared(EnforcementDecisions.insert(decision))
			.addPrepared(EnforcementDecisionsBySubject.insert(subjectKey(decision)))
			.addPrepared(EnforcementDecisionNotes.insert(firstNote));
		if (evidence) {
			batch.addPrepared(EnforcementDecisionEvidence.insert(evidence));
		}
		if (decision.automation === 'automated') {
			batch.addPrepared(
				EnforcementReviewQueue.insert({bucket: REVIEW_QUEUE_BUCKET, decision_id: decision.decision_id}),
			);
		}
		await batch.execute();
	}

	async delete(decision: EnforcementDecisionRow): Promise<void> {
		const batch = new BatchBuilder()
			.addPrepared(EnforcementDecisions.deleteByPk({decision_id: decision.decision_id}))
			.addPrepared(EnforcementDecisionsBySubject.deleteByPk(subjectKey(decision)))
			.addPrepared(EnforcementDecisionEvidence.deleteByPk({decision_id: decision.decision_id}))
			.addPrepared(EnforcementDecisionNotes.deletePartition({decision_id: decision.decision_id}));
		if (decision.automation === 'automated') {
			batch.addPrepared(deleteReviewQueueRow(decision.decision_id));
		}
		await batch.execute();
	}

	async get(decisionId: bigint): Promise<EnforcementDecisionRow | null> {
		return fetchOne<EnforcementDecisionRow>(GET_DECISION_QUERY.bind({decision_id: decisionId}));
	}

	async getMany(decisionIds: ReadonlyArray<bigint>): Promise<Array<EnforcementDecisionRow>> {
		const rows = await fetchManyInChunks<EnforcementDecisionRow, bigint>(
			GET_DECISIONS_QUERY,
			[...decisionIds],
			(chunk) => ({decision_ids: chunk}),
		);
		const byId = new Map(rows.map((row) => [row.decision_id, row]));
		return decisionIds.flatMap((decisionId) => {
			const row = byId.get(decisionId);
			return row ? [row] : [];
		});
	}

	async listBySubject(
		subjectType: EnforcementSubjectType,
		subjectId: bigint,
		limit: number = SUBJECT_DECISION_SCAN_LIMIT,
		before?: bigint,
	): Promise<Array<EnforcementDecisionRow>> {
		const keys = await fetchMany<Pick<EnforcementDecisionBySubjectRow, 'decision_id'>>(
			listBySubjectQuery(limit, before !== undefined).bind({
				subject_type: subjectType,
				subject_id: subjectId,
				...(before !== undefined ? {before} : {}),
			}),
		);
		return this.getMany(keys.map((key) => key.decision_id));
	}

	async listReportLinks(limit: number, after?: bigint): Promise<Array<DecisionReportLink>> {
		return fetchMany<DecisionReportLink>(
			listReportLinksQuery(limit, after !== undefined).bind(after !== undefined ? {after} : {}),
		);
	}

	async end(decisionId: bigint, end: EnforcementDecisionEnd): Promise<void> {
		await upsertOne(EnforcementDecisions.patchByPk({decision_id: decisionId}, endPatch(end)));
	}

	async endIfActive(decisionId: bigint, end: EnforcementDecisionEnd): Promise<boolean> {
		return executeConditional(
			EnforcementDecisions.conditionalPatchByPk({decision_id: decisionId}, endPatch(end), {status: 'active'}),
		);
	}

	async setNotice(decisionId: bigint, notice: EnforcementNoticeRecord): Promise<void> {
		await upsertOne(
			EnforcementDecisions.patchByPk(
				{decision_id: decisionId},
				{
					notice_status: Db.set(notice.status),
					notice_channels: notice.channels.size > 0 ? Db.set(new Set(notice.channels)) : Db.clear(),
					notice_locale: notice.locale === null ? Db.clear() : Db.set(notice.locale),
					notice_text: notice.text === null ? Db.clear() : Db.set(notice.text),
					notice_sent_at: notice.sentAt === null ? Db.clear() : Db.set(notice.sentAt),
				},
			),
		);
	}

	async listReviewQueue(): Promise<Array<EnforcementDecisionRow>> {
		const keys = await fetchMany<Pick<EnforcementReviewQueueRow, 'decision_id'>>(
			LIST_REVIEW_QUEUE_QUERY.bind({bucket: REVIEW_QUEUE_BUCKET}),
		);
		return this.getMany(keys.map((key) => key.decision_id));
	}

	async removeFromReviewQueue(decisionIds: ReadonlyArray<bigint>): Promise<void> {
		if (decisionIds.length === 0) return;
		const batch = new BatchBuilder();
		for (const decisionId of decisionIds) {
			batch.addPrepared(deleteReviewQueueRow(decisionId));
		}
		await batch.execute();
	}

	async addNote(note: EnforcementDecisionNoteRow): Promise<void> {
		await upsertOne(EnforcementDecisionNotes.insert(note));
	}

	async listNotes(decisionId: bigint): Promise<Array<EnforcementDecisionNoteRow>> {
		return fetchMany<EnforcementDecisionNoteRow>(LIST_NOTES_QUERY.bind({decision_id: decisionId}));
	}

	async getEvidence(decisionId: bigint): Promise<EnforcementDecisionEvidenceRow | null> {
		return fetchOne<EnforcementDecisionEvidenceRow>(GET_EVIDENCE_QUERY.bind({decision_id: decisionId}));
	}
}
