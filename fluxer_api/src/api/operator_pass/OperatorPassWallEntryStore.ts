// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import type {UserID} from '@app/api/BrandedTypes';
import {Db, type DbOp} from '@app/api/database/CassandraTypes';
import type {
	OperatorPassWallDecisionKind,
	OperatorPassWallDecisionRow,
	OperatorPassWallDeclineReason,
	OperatorPassWallEntryRow,
	OperatorPassWallEventKind,
	OperatorPassWallRevisionRow,
	OperatorPassWallTakeDownCause,
} from '@app/api/database/types/OperatorPassTypes';
import {getOperatorPassWallInfrastructure} from '@app/api/middleware/ServiceSingletons';
import type {OperatorPassRepository, OperatorPassWallEntryPatch} from '@app/api/operator_pass/OperatorPassRepository';
import {OPERATOR_PASS_WALL_UPDATE_ATTEMPTS} from '@app/api/operator_pass/OperatorPassWallConstants';
import {UnknownOperatorPassWallEntryError} from '@fluxer/errors/src/domains/operator_pass/UnknownOperatorPassWallEntryError';

const SCAN_PAGE_SIZE = 500;

export interface WallEventDraft {
	kind: OperatorPassWallEventKind;
	name?: string | null;
	reason?: string | null;
	cause?: OperatorPassWallTakeDownCause | null;
	purchaseId?: string | null;
	releaseYear?: number | null;
	releaseTag?: string | null;
	passCount?: number | null;
	firstPassAt?: Date | null;
}

interface WallMutation {
	patch: OperatorPassWallEntryPatch;
	events: Array<WallEventDraft>;
}

export function liveRevisionPatch(revision: OperatorPassWallRevisionRow | null): OperatorPassWallEntryPatch {
	return {
		live_revision_id: Db.set(revision?.revision_id ?? null),
		live_name: Db.set(revision?.name ?? null),
		live_avatar: Db.set(revision?.avatar ?? null),
		live_business: Db.set(revision?.business ?? false),
	};
}

function emptyEntry(userId: UserID, now: Date): OperatorPassWallEntryRow {
	return {
		user_id: userId,
		name: null,
		opted_out: true,
		keep_as_is: false,
		release_list_opt_in: false,
		hidden: false,
		legal_hold: false,
		left_wall_at: null,
		username_declined: false,
		live_revision_id: null,
		live_name: null,
		live_avatar: null,
		live_business: null,
		pending_revision_id: null,
		pending_name: null,
		pending_avatar: null,
		pending_source: null,
		pending_since: null,
		declined_revision_id: null,
		declined_name: null,
		declined_avatar: null,
		decline_reason: null,
		decline_note: null,
		decline_statement_id: null,
		reviewed_at: null,
		reviewed_by: null,
		listed_at: null,
		delivered_at: null,
		release_published_year: null,
		release_published_at: null,
		release_erased_at: null,
		updated_at: now,
		version: 0,
	};
}

function applyWallPatch<K extends keyof OperatorPassWallEntryPatch>(
	row: OperatorPassWallEntryRow,
	patch: Partial<{[P in K]: DbOp<OperatorPassWallEntryRow[P]>}>,
): OperatorPassWallEntryRow {
	const next = {...row};
	for (const key in patch) {
		if (!Object.hasOwn(patch, key)) {
			continue;
		}
		const op = patch[key];
		if (op && typeof op === 'object' && 'value' in op) {
			next[key] = op.value;
		}
	}
	return next;
}

export class OperatorPassWallEntryStore {
	constructor(
		private readonly repository: OperatorPassRepository,
		private readonly invalidate: () => void,
	) {}

	async appendDecision(
		userId: UserID,
		params: {
			kind: OperatorPassWallDecisionKind;
			revisionId: bigint | null;
			decidedBy: UserID | null;
			decidedAt: Date;
			reason?: OperatorPassWallDeclineReason | null;
			note?: string | null;
			statementId?: string | null;
			usernameDeclined?: boolean;
		},
	): Promise<OperatorPassWallDecisionRow> {
		const decision: OperatorPassWallDecisionRow = {
			user_id: userId,
			decision_id: await getOperatorPassWallInfrastructure().nextId(),
			revision_id: params.revisionId,
			kind: params.kind,
			decided_by: params.decidedBy,
			decided_at: params.decidedAt,
			reason: params.reason ?? null,
			note: params.note ?? null,
			statement_id: params.statementId ?? null,
			username_declined: params.usernameDeclined ?? null,
		};
		await this.repository.appendWallDecision(decision);
		return decision;
	}

	async mutateExisting(
		userId: UserID,
		now: Date,
		actorId: UserID | null,
		decide: (current: OperatorPassWallEntryRow) => WallMutation | null,
	): Promise<OperatorPassWallEntryRow> {
		return this.mutate(userId, now, actorId, decide, 'reject');
	}

	async mutate(
		userId: UserID,
		now: Date,
		actorId: UserID | null,
		decide: (current: OperatorPassWallEntryRow) => WallMutation | null,
		missingEntry: 'create' | 'reject' = 'create',
	): Promise<OperatorPassWallEntryRow> {
		for (let attempt = 0; attempt < OPERATOR_PASS_WALL_UPDATE_ATTEMPTS; attempt++) {
			const existing = await this.repository.findWallEntry(userId);
			if (!existing && missingEntry === 'reject') {
				throw new UnknownOperatorPassWallEntryError();
			}
			const current = existing ?? emptyEntry(userId, now);
			const mutation = decide(current);
			if (!mutation) {
				return current;
			}
			let next: OperatorPassWallEntryRow | null;
			if (existing) {
				next = await this.repository.updateWallEntry(existing, mutation.patch);
			} else {
				const created = {...applyWallPatch(current, mutation.patch), version: 1};
				next = (await this.repository.createWallEntry(created)) ? created : null;
			}
			if (!next) {
				continue;
			}
			this.invalidate();
			if ((existing?.pending_revision_id != null) !== (next.pending_revision_id != null)) {
				await this.repository.setWallPending(userId, next.pending_revision_id != null);
			}
			for (const event of mutation.events) {
				await this.appendEvent(userId, now, event, actorId);
			}
			return next;
		}
		throw new Error("Operators' wall entry changed too often to update");
	}

	async appendEvent(userId: UserID, now: Date, event: WallEventDraft, actorId: UserID | null): Promise<void> {
		this.invalidate();
		await this.repository.appendWallEvent({
			user_id: userId,
			occurred_at: now,
			event_id: `${process.hrtime.bigint().toString().padStart(24, '0')}-${randomUUID()}`,
			kind: event.kind,
			name: event.name ?? null,
			reason: event.reason ?? null,
			cause: event.cause ?? null,
			purchase_id: event.purchaseId ?? null,
			release_year: event.releaseYear ?? null,
			release_tag: event.releaseTag ?? null,
			actor_id: actorId,
			pass_count: event.passCount ?? null,
			first_pass_at: event.firstPassAt ?? null,
		});
	}

	async scanAll(): Promise<Array<OperatorPassWallEntryRow>> {
		const rows: Array<OperatorPassWallEntryRow> = [];
		let pageState: string | null = null;
		do {
			const page = await this.repository.scanWallEntries(SCAN_PAGE_SIZE, pageState);
			rows.push(...page.rows);
			pageState = page.pageState;
		} while (pageState);
		return rows;
	}
}
