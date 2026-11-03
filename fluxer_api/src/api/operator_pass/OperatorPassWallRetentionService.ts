// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHmac} from 'node:crypto';
import type {AdminRepository} from '@app/api/admin/AdminRepository';
import type {UserID} from '@app/api/BrandedTypes';
import {Db} from '@app/api/database/CassandraTypes';
import type {
	OperatorPassWallDecisionRow,
	OperatorPassWallEntryRow,
	OperatorPassWallRevisionRow,
} from '@app/api/database/types/OperatorPassTypes';
import {Logger} from '@app/api/Logger';
import {getOperatorPassWallInfrastructure} from '@app/api/middleware/ServiceSingletons';
import type {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import {operatorPassNow} from '@app/api/operator_pass/OperatorPassSchedule';
import {deriveOperatorPassSecret} from '@app/api/operator_pass/OperatorPassSecrets';
import {liveRevisionPatch, type OperatorPassWallEntryStore} from '@app/api/operator_pass/OperatorPassWallEntryStore';
import {isOperatorPassWallListed} from '@app/api/operator_pass/OperatorPassWallPresentation';
import type {OperatorPassWallPublicationService} from '@app/api/operator_pass/OperatorPassWallPublicationService';
import {getAuditLogSearchService} from '@app/api/SearchFactory';
import {MS_PER_DAY} from '@fluxer/date_utils/src/DateConstants';

const WALL_SUBMISSION_EVENT_KINDS: ReadonlySet<string> = new Set(['declined', 'changed', 'listed']);

const DECLINED_RETENTION_MS = 365 * MS_PER_DAY;

const LEFT_WALL_RETENTION_MS = 60 * MS_PER_DAY;

export class OperatorPassWallRetentionService {
	constructor(
		private readonly repository: OperatorPassRepository,
		private readonly entries: OperatorPassWallEntryStore,
		private readonly publication: OperatorPassWallPublicationService,
		private readonly auditLogs: AdminRepository,
	) {}

	async setLegalHold(userId: UserID, adminId: UserID, held: boolean): Promise<OperatorPassWallEntryRow> {
		const now = operatorPassNow();
		return this.entries.mutateExisting(userId, now, adminId, (current) => {
			if (current.legal_hold === held) {
				return null;
			}
			return {
				patch: {legal_hold: Db.set(held), updated_at: Db.set(now)},
				events: [{kind: held ? 'legal_hold_set' : 'legal_hold_cleared'}],
			};
		});
	}

	async purgeExpired(
		params: {now?: Date; isOnHold?: (userId: UserID) => Promise<boolean>} = {},
	): Promise<{redacted: number; purged: number}> {
		const now = params.now ?? operatorPassNow();
		let redacted = 0;
		let purged = 0;
		for (const entry of await this.entries.scanAll()) {
			if (entry.legal_hold || (params.isOnHold && (await params.isOnHold(entry.user_id)))) {
				continue;
			}
			const [revisions, decisions] = await Promise.all([
				this.repository.listWallRevisions(entry.user_id),
				this.repository.listWallDecisions(entry.user_id),
			]);
			const left =
				!isOperatorPassWallListed(entry) &&
				entry.left_wall_at != null &&
				now.getTime() - entry.left_wall_at.getTime() >= LEFT_WALL_RETENTION_MS;
			if (left && entry.delivered_at != null) {
				if (await this.redactLeftHolder(entry, revisions, decisions)) {
					redacted += 1;
				}
				continue;
			}
			if (left) {
				for (const avatar of new Set(revisions.flatMap((revision) => (revision.avatar ? [revision.avatar] : [])))) {
					await this.forgetAvatarCopy(avatar, entry.user_id);
				}
				const userId = entry.user_id;
				const digest = createHmac('sha256', deriveOperatorPassSecret('fluxer-operator-pass-wall-token-v1'))
					.update(userId.toString())
					.digest('hex');
				const token = -BigInt(`0x${digest.slice(0, 15)}`);
				for (const decision of decisions) {
					await this.repository.saveWallDecision({
						...decision,
						user_id: token as UserID,
						decided_by: decision.decided_by === entry.user_id ? null : decision.decided_by,
						note: null,
						statement_id: decision.statement_id,
					});
				}
				for (const event of await this.repository.listWallEvents(entry.user_id)) {
					await this.repository.appendWallEvent({
						...event,
						user_id: token as UserID,
						name: null,
						reason: null,
						actor_id: event.actor_id === entry.user_id ? null : event.actor_id,
						purchase_id: null,
					});
				}
				for (const logId of await this.repository.listWallAuditLogIds(entry.user_id)) {
					await this.redactAuditLog(logId, token, ['sha256', 'note']);
				}
				await this.repository.saveWallTombstone({token_: token, left_at: entry.left_wall_at!, purged_at: now});
				await this.repository.setWallPending(entry.user_id, false);
				await this.repository.deleteWallPartitions(entry.user_id);
				purged += 1;
				continue;
			}
			for (const decision of decisions) {
				if (decision.kind !== 'decline' || decision.revision_id == null) continue;
				if (now.getTime() - decision.decided_at.getTime() < DECLINED_RETENTION_MS) continue;
				const revision = revisions.find((row) => row.revision_id === decision.revision_id);
				if (!revision || (revision.name === '' && revision.avatar == null)) continue;
				await this.redactDeclinedRevision(entry, revision, revisions);
				await this.repository.saveWallDecision({...decision, note: null});
				redacted += 1;
			}
		}
		return {redacted, purged};
	}

	private async forgetAvatarCopy(avatar: string, userId: UserID): Promise<void> {
		await this.repository.removeWallAvatarRef(avatar, userId);
		if (!(await this.publication.isAvatarServed(avatar))) {
			await getOperatorPassWallInfrastructure().avatarStore.unpublish(avatar);
		}
		if ((await this.repository.listWallAvatarRefs(avatar)).length === 0) {
			await getOperatorPassWallInfrastructure().avatarStore.deletePrivate(avatar);
		}
	}

	private async redactLeftHolder(
		entry: OperatorPassWallEntryRow,
		revisions: ReadonlyArray<OperatorPassWallRevisionRow>,
		decisions: ReadonlyArray<OperatorPassWallDecisionRow>,
	): Promise<boolean> {
		const userId = entry.user_id;
		const copies = revisions.filter((revision) => revision.name !== '' || revision.avatar != null);
		if (copies.length === 0 && entry.name == null) {
			return false;
		}
		for (const avatar of new Set(copies.flatMap((revision) => (revision.avatar ? [revision.avatar] : [])))) {
			await this.forgetAvatarCopy(avatar, userId);
		}
		for (const revision of copies) {
			await this.repository.saveWallRevision({...revision, name: '', avatar: null});
		}
		for (const decision of decisions) {
			if (decision.note != null) {
				await this.repository.saveWallDecision({...decision, note: null});
			}
		}
		for (const event of await this.repository.listWallEvents(userId)) {
			if (event.name != null) {
				await this.repository.appendWallEvent({...event, name: null});
			}
		}
		for (const logId of await this.repository.listWallAuditLogIds(userId)) {
			await this.redactAuditLog(logId, userId, ['sha256', 'note']);
		}
		await this.entries.mutateExisting(userId, operatorPassNow(), null, () => ({
			patch: {
				name: Db.set(null),
				...liveRevisionPatch(null),
				pending_revision_id: Db.set(null),
				pending_name: Db.set(null),
				pending_avatar: Db.set(null),
				pending_source: Db.set(null),
				pending_since: Db.set(null),
				declined_revision_id: Db.set(null),
				declined_name: Db.set(null),
				declined_avatar: Db.set(null),
				decline_note: Db.set(null),
			},
			events: [],
		}));
		return true;
	}

	private async redactDeclinedRevision(
		entry: OperatorPassWallEntryRow,
		revision: OperatorPassWallRevisionRow,
		revisions: ReadonlyArray<OperatorPassWallRevisionRow>,
	): Promise<void> {
		const userId = entry.user_id;
		const others = revisions.filter((row) => row.revision_id !== revision.revision_id);
		if (revision.avatar && !others.some((row) => row.avatar === revision.avatar)) {
			await this.repository.removeWallAvatarRef(revision.avatar, userId);
			if (
				!(await this.publication.isAvatarServed(revision.avatar)) &&
				(await this.repository.listWallAvatarRefs(revision.avatar)).length === 0
			) {
				await getOperatorPassWallInfrastructure().avatarStore.deletePrivate(revision.avatar);
			}
		}
		await this.repository.saveWallRevision({...revision, name: '', avatar: null});
		if (entry.declined_revision_id === revision.revision_id) {
			await this.entries.mutateExisting(userId, operatorPassNow(), null, (current) =>
				current.declined_revision_id === revision.revision_id
					? {
							patch: {
								declined_name: Db.set(null),
								declined_avatar: Db.set(null),
								decline_note: Db.set(null),
							},
							events: [],
						}
					: null,
			);
		}
		const keptNames = new Set(
			[entry.live_name, entry.pending_name, ...others.map((row) => row.name)].filter((name): name is string =>
				Boolean(name),
			),
		);
		if (revision.name && !keptNames.has(revision.name)) {
			for (const event of await this.repository.listWallEvents(userId)) {
				if (event.name === revision.name && WALL_SUBMISSION_EVENT_KINDS.has(event.kind)) {
					await this.repository.appendWallEvent({...event, name: null});
				}
			}
		}
		for (const logId of await this.repository.listWallAuditLogIds(userId)) {
			const log = await this.auditLogs.getAuditLog(logId);
			if (!log) continue;
			const dropKeys: Array<string> = [];
			if (log.metadata.get('revision_id') === revision.revision_id.toString() && log.metadata.has('note')) {
				dropKeys.push('note');
			}
			if (revision.avatar && log.metadata.get('sha256') === revision.avatar) {
				dropKeys.push('sha256');
			}
			if (dropKeys.length > 0) {
				await this.redactAuditLog(logId, log.targetId, dropKeys);
			}
		}
	}

	private async redactAuditLog(logId: bigint, targetId: bigint, dropKeys: ReadonlyArray<string>): Promise<void> {
		const updated = await this.auditLogs.redactAuditLog(logId, targetId, dropKeys);
		if (updated) {
			await getAuditLogSearchService()
				?.indexAuditLog(updated)
				.catch((error) => Logger.warn({error, logId}, "Failed to reindex a redacted Operators' wall audit log"));
		}
	}
}
