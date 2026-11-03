// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {
	BatchBuilder,
	deleteOneOrMany,
	executeConditional,
	fetchMany,
	fetchOne,
	fetchPage,
	upsertOne,
} from '@app/api/database/CassandraQueryExecution';
import {Db, type DbOp, nextVersion, type PatchObject} from '@app/api/database/CassandraTypes';
import {applyPatchToRow} from '@app/api/database/CassandraVersionedUpdate';
import type {
	OperatorPassCertificateLinkRow,
	OperatorPassCertificateNumberRow,
	OperatorPassCertificateRow,
	OperatorPassLabsGreetingRow,
	OperatorPassLabsLeftReason,
	OperatorPassLabsMemberRow,
	OperatorPassOfferRow,
	OperatorPassPurchaseByPaymentIntentRow,
	OperatorPassPurchaseByPurchaserRow,
	OperatorPassPurchaseRow,
	OperatorPassReleaseRow,
	OperatorPassRemovalEndReason,
	OperatorPassRemovalHistoryRow,
	OperatorPassRemovalRow,
	OperatorPassRow,
	OperatorPassUnitEventRow,
	OperatorPassUnitRow,
	OperatorPassVisionaryActivationRow,
	OperatorPassWaitlistByEntryIdRow,
	OperatorPassWaitlistRow,
	OperatorPassWallDecisionRow,
	OperatorPassWallEntryRow,
	OperatorPassWallEventRow,
	OperatorPassWallRevisionRow,
	OperatorPassWallStateRow,
	OperatorPassWallTombstoneRow,
} from '@app/api/database/types/OperatorPassTypes';
import {OPERATOR_PASS_WAITLIST_COLUMNS} from '@app/api/database/types/OperatorPassTypes';
import {
	OperatorPassCertificateLinks,
	OperatorPassCertificateNumbers,
	OperatorPassCertificates,
	OperatorPassCertificatesByPurchase,
	OperatorPasses,
	OperatorPassLabsGreetings,
	OperatorPassLabsMembers,
	OperatorPassOffers,
	OperatorPassOffersByUser,
	OperatorPassPurchases,
	OperatorPassPurchasesByPaymentIntent,
	OperatorPassPurchasesByPurchaser,
	OperatorPassReleases,
	OperatorPassRemovalHistory,
	OperatorPassRemovals,
	OperatorPassUnitEvents,
	OperatorPassUnits,
	OperatorPassUnitsByHolder,
	OperatorPassUnitsByPurchase,
	OperatorPassVisionaryActivations,
	OperatorPassWaitlist,
	OperatorPassWaitlistByEntryId,
	OperatorPassWallAuditLogs,
	OperatorPassWallAvatarRefs,
	OperatorPassWallDecisions,
	OperatorPassWallEntries,
	OperatorPassWallEvents,
	OperatorPassWallPending,
	OperatorPassWallRevisions,
	OperatorPassWallState,
	OperatorPassWallTombstones,
} from '@app/api/operator_pass/OperatorPassTables';
import {compareSnowflakes} from '@fluxer/snowflake/src/SnowflakeComparison';
import {BACKGROUND_READ_TIMEOUT_MS} from '@pkgs/cassandra/src/Client';

type RowPatch<Row, PK extends keyof Row> = Partial<{
	[K in Exclude<keyof Row, PK | 'version'>]: DbOp<Row[K]>;
}>;

export type OperatorPassWaitlistPatch = RowPatch<OperatorPassWaitlistRow, 'email'>;
export type OperatorPassPurchasePatch = RowPatch<OperatorPassPurchaseRow, 'checkout_session_id'>;
export type OperatorPassUnitPatch = RowPatch<OperatorPassUnitRow, 'pass_id'>;
export type OperatorPassOfferPatch = RowPatch<OperatorPassOfferRow, 'offer_id'>;
export type OperatorPassWallEntryPatch = RowPatch<OperatorPassWallEntryRow, 'user_id'>;
export type OperatorPassVisionaryActivationPatch = RowPatch<OperatorPassVisionaryActivationRow, 'user_id'>;

export type OperatorPassCertificatePageSummary = Pick<
	OperatorPassCertificateRow,
	'pass_id' | 'version' | 'purchase_id' | 'holder_name' | 'paid_at' | 'cancelled_at'
>;

export type OperatorPassCertificateLinkSummary = Pick<
	OperatorPassCertificateRow,
	'pass_id' | 'link_token' | 'cancelled_at'
>;

export interface OperatorPassScanPage<Row> {
	rows: Array<Row>;
	pageState: string | null;
}

export interface OperatorPassWaitlistPage {
	entries: Array<OperatorPassWaitlistRow>;
	pageState: string | null;
}

const FETCH_WAITLIST_BY_EMAIL = OperatorPassWaitlist.selectCql({
	where: OperatorPassWaitlist.where.eq('email'),
	limit: 1,
});
const FETCH_WAITLIST_EMAIL_BY_ENTRY_ID = OperatorPassWaitlistByEntryId.selectCql({
	where: OperatorPassWaitlistByEntryId.where.eq('entry_id'),
	limit: 1,
});
const SCAN_WAITLIST = OperatorPassWaitlist.selectCql();
const FETCH_PASS_BY_USER = OperatorPasses.selectCql({
	where: OperatorPasses.where.eq('user_id'),
	limit: 1,
});
const FETCH_PURCHASE = OperatorPassPurchases.selectCql({
	where: OperatorPassPurchases.where.eq('checkout_session_id'),
	limit: 1,
});
const SCAN_PASSES = OperatorPasses.selectCql();
const SCAN_PURCHASES = OperatorPassPurchases.selectCql();
const FETCH_PURCHASE_BY_PAYMENT_INTENT = OperatorPassPurchasesByPaymentIntent.selectCql({
	where: OperatorPassPurchasesByPaymentIntent.where.eq('payment_intent_id'),
	limit: 1,
});
const FETCH_PURCHASE_IDS_BY_PURCHASER = OperatorPassPurchasesByPurchaser.selectCql({
	columns: ['checkout_session_id'],
	where: OperatorPassPurchasesByPurchaser.where.eq('purchaser_id'),
});
const FETCH_REMOVAL = OperatorPassRemovals.selectCql({
	where: OperatorPassRemovals.where.eq('user_id'),
	limit: 1,
});
const SCAN_REMOVALS = OperatorPassRemovals.selectCql();
const FETCH_REMOVAL_HISTORY = OperatorPassRemovalHistory.selectCql({
	where: OperatorPassRemovalHistory.where.eq('user_id'),
});

const FETCH_WALL_ENTRY = OperatorPassWallEntries.selectCql({
	where: OperatorPassWallEntries.where.eq('user_id'),
	limit: 1,
});
const SCAN_WALL_ENTRIES = OperatorPassWallEntries.selectCql();
const FETCH_WALL_EVENTS = OperatorPassWallEvents.selectCql({
	where: OperatorPassWallEvents.where.eq('user_id'),
});
const FETCH_WALL_REVISIONS = OperatorPassWallRevisions.selectCql({
	where: OperatorPassWallRevisions.where.eq('user_id'),
});
const FETCH_WALL_DECISIONS = OperatorPassWallDecisions.selectCql({
	where: OperatorPassWallDecisions.where.eq('user_id'),
});
const FETCH_WALL_AUDIT_LOGS = OperatorPassWallAuditLogs.selectCql({
	where: OperatorPassWallAuditLogs.where.eq('user_id'),
});
const FETCH_WALL_AVATAR_REFS = OperatorPassWallAvatarRefs.selectCql({
	where: OperatorPassWallAvatarRefs.where.eq('avatar'),
});
const FETCH_WALL_PENDING = OperatorPassWallPending.selectCql({
	where: OperatorPassWallPending.where.eq('bucket'),
});
const WALL_PENDING_BUCKET = 'pending';
const SCAN_WALL_TOMBSTONES = OperatorPassWallTombstones.selectCql();
const FETCH_WALL_STATE = OperatorPassWallState.selectCql({
	where: OperatorPassWallState.where.eq('id'),
});
const FETCH_RELEASE = OperatorPassReleases.selectCql({
	where: OperatorPassReleases.where.eq('year'),
	limit: 1,
});
const SCAN_RELEASES = OperatorPassReleases.selectCql();

const FETCH_CERTIFICATE_VERSIONS = OperatorPassCertificates.selectCql({
	where: OperatorPassCertificates.where.eq('pass_id'),
});
const SCAN_CERTIFICATE_PAGES = OperatorPassCertificates.selectCql({
	columns: ['pass_id', 'version', 'purchase_id', 'holder_name', 'paid_at', 'cancelled_at'],
});
const FETCH_CERTIFICATE_IDS_BY_PURCHASE = OperatorPassCertificatesByPurchase.selectCql({
	columns: ['pass_id'],
	where: OperatorPassCertificatesByPurchase.where.eq('purchase_id'),
});
const FETCH_LATEST_CERTIFICATE_LINK = OperatorPassCertificates.selectCql({
	columns: ['pass_id', 'version', 'link_token', 'cancelled_at'],
	where: OperatorPassCertificates.where.eq('pass_id'),
	orderBy: {col: 'version', direction: 'DESC'},
	limit: 1,
});
const FETCH_CERTIFICATE_LINK = OperatorPassCertificateLinks.selectCql({
	where: OperatorPassCertificateLinks.where.eq('token_'),
	limit: 1,
});
const FETCH_CERTIFICATE_NUMBER = OperatorPassCertificateNumbers.selectCql({
	where: OperatorPassCertificateNumbers.where.eq('number'),
	limit: 1,
});
const SCAN_CERTIFICATE_NUMBERS = OperatorPassCertificateNumbers.selectCql({columns: ['number']});
const SCAN_CERTIFICATE_NUMBER_OWNERS = OperatorPassCertificateNumbers.selectCql({columns: ['number', 'pass_id']});

const FETCH_UNIT = OperatorPassUnits.selectCql({where: OperatorPassUnits.where.eq('pass_id'), limit: 1});
const FETCH_UNIT_IDS_BY_HOLDER = OperatorPassUnitsByHolder.selectCql({
	columns: ['pass_id'],
	where: OperatorPassUnitsByHolder.where.eq('holder_id'),
});
const FETCH_UNIT_IDS_BY_PURCHASE = OperatorPassUnitsByPurchase.selectCql({
	columns: ['pass_id'],
	where: OperatorPassUnitsByPurchase.where.eq('purchase_id'),
});
const FETCH_OFFER = OperatorPassOffers.selectCql({where: OperatorPassOffers.where.eq('offer_id'), limit: 1});
const SCAN_OFFERS = OperatorPassOffers.selectCql();
const FETCH_OFFER_IDS_BY_USER = OperatorPassOffersByUser.selectCql({
	columns: ['offer_id'],
	where: OperatorPassOffersByUser.where.eq('user_id'),
});
const FETCH_UNIT_EVENTS = OperatorPassUnitEvents.selectCql({where: OperatorPassUnitEvents.where.eq('pass_id')});
const FETCH_VISIONARY_ACTIVATION = OperatorPassVisionaryActivations.selectCql({
	where: OperatorPassVisionaryActivations.where.eq('user_id'),
	limit: 1,
});
const SCAN_VISIONARY_ACTIVATIONS = OperatorPassVisionaryActivations.selectCql();
const FETCH_LABS_MEMBER = OperatorPassLabsMembers.selectCql({
	where: OperatorPassLabsMembers.where.eq('user_id'),
	limit: 1,
});
const FETCH_LABS_GREETINGS = OperatorPassLabsGreetings.selectCql({
	where: OperatorPassLabsGreetings.where.eq('bucket'),
});
const LABS_GREETING_BUCKET = 'pending';

function withPatch<Row extends {version: number}>(current: Row, patch: object, version: number): Row {
	return {...applyPatchToRow<Row>(current, patch as PatchObject), version} as Row;
}

export class OperatorPassRepository {
	async findWaitlistEntryByEmail(email: string): Promise<OperatorPassWaitlistRow | null> {
		return fetchOne<OperatorPassWaitlistRow>(FETCH_WAITLIST_BY_EMAIL, {email});
	}

	async findWaitlistEntryByEntryId(entryId: string): Promise<OperatorPassWaitlistRow | null> {
		const mapping = await fetchOne<OperatorPassWaitlistByEntryIdRow>(FETCH_WAITLIST_EMAIL_BY_ENTRY_ID, {
			entry_id: entryId,
		});
		if (!mapping) {
			return null;
		}
		const entry = await this.findWaitlistEntryByEmail(mapping.email);
		return entry?.entry_id === entryId ? entry : null;
	}

	async saveWaitlistEntry(
		entry: OperatorPassWaitlistRow,
		previous: OperatorPassWaitlistRow | null = null,
	): Promise<boolean> {
		const applied = previous
			? await executeConditional(
					OperatorPassWaitlist.conditionalPatchByPk(
						{email: entry.email},
						waitlistRowPatch(entry),
						expectedWaitlistVersion(previous),
					),
				)
			: await executeConditional(OperatorPassWaitlist.insertIfNotExists(entry));
		if (!applied) return false;
		await upsertOne(OperatorPassWaitlistByEntryId.upsertAll({entry_id: entry.entry_id, email: entry.email}));
		if (previous && previous.entry_id !== entry.entry_id) {
			await deleteOneOrMany(OperatorPassWaitlistByEntryId.deleteByPk({entry_id: previous.entry_id}));
		}
		return true;
	}

	async probeWaitlistEntry(email: string, entryId: string): Promise<void> {
		await executeConditional(
			OperatorPassWaitlist.conditionalPatchByPk({email}, {updated_at: Db.set(new Date(0))}, {version: 0}),
		);
		await fetchOne<OperatorPassWaitlistByEntryIdRow>(FETCH_WAITLIST_EMAIL_BY_ENTRY_ID, {entry_id: entryId});
	}

	async updateWaitlistEntry(
		current: OperatorPassWaitlistRow,
		patch: OperatorPassWaitlistPatch,
	): Promise<OperatorPassWaitlistRow | null> {
		const version = nextVersion(current.version);
		const applied = await executeConditional(
			OperatorPassWaitlist.conditionalPatchByPk(
				{email: current.email},
				{...patch, version: Db.set(version)},
				expectedWaitlistVersion(current),
			),
		);
		return applied ? withPatch(current, patch, version) : null;
	}

	async deleteWaitlistEntry(entry: OperatorPassWaitlistRow): Promise<boolean> {
		const applied = await executeConditional(
			OperatorPassWaitlist.conditionalDeleteByPk({email: entry.email}, expectedWaitlistVersion(entry)),
		);
		if (applied) await deleteOneOrMany(OperatorPassWaitlistByEntryId.deleteByPk({entry_id: entry.entry_id}));
		return applied;
	}

	async markWaitlistSaleEmailSent(email: string, sentAt: Date): Promise<boolean> {
		return executeConditional(
			OperatorPassWaitlist.conditionalPatchByPk({email}, {sale_email_sent_at: Db.set(sentAt)}, {status: 'confirmed'}),
		);
	}

	async scanWaitlistPage(pageSize: number, pageState?: string | null): Promise<OperatorPassWaitlistPage> {
		const page = await fetchPage<OperatorPassWaitlistRow>(
			SCAN_WAITLIST,
			{},
			{pageSize, pageState, readTimeout: BACKGROUND_READ_TIMEOUT_MS},
		);
		return {entries: page.rows, pageState: page.pageState};
	}

	async findOperatorPass(userId: UserID): Promise<OperatorPassRow | null> {
		return fetchOne<OperatorPassRow>(FETCH_PASS_BY_USER, {user_id: userId});
	}

	async deleteOperatorPass(userId: UserID): Promise<boolean> {
		const current = await this.findOperatorPass(userId);
		if (!current) {
			return false;
		}
		return executeConditional(OperatorPasses.conditionalDeleteByPk({user_id: userId}, {version: current.version}));
	}

	async scanOperatorPasses(
		pageSize: number,
		pageState?: string | null,
	): Promise<OperatorPassScanPage<OperatorPassRow>> {
		const page = await fetchPage<OperatorPassRow>(
			SCAN_PASSES,
			{},
			{pageSize, pageState, readTimeout: BACKGROUND_READ_TIMEOUT_MS},
		);
		return {rows: page.rows, pageState: page.pageState};
	}

	async clearOperatorPassConsentDetails(userId: UserID): Promise<void> {
		await upsertOne(
			OperatorPasses.patchByPk(
				{user_id: userId},
				{waiver_consent_ip: Db.clear(), waiver_consent_user_agent: Db.clear()},
			),
		);
	}

	async scanPurchases(
		pageSize: number,
		pageState?: string | null,
	): Promise<OperatorPassScanPage<OperatorPassPurchaseRow>> {
		const page = await fetchPage<OperatorPassPurchaseRow>(
			SCAN_PURCHASES,
			{},
			{pageSize, pageState, readTimeout: BACKGROUND_READ_TIMEOUT_MS},
		);
		return {rows: page.rows, pageState: page.pageState};
	}

	async createPurchase(purchase: OperatorPassPurchaseRow): Promise<void> {
		const batch = new BatchBuilder();
		batch.addPrepared(OperatorPassPurchases.upsertAll(purchase));
		batch.addPrepared(
			OperatorPassPurchasesByPurchaser.upsertAll({
				purchaser_id: purchase.purchaser_id,
				checkout_session_id: purchase.checkout_session_id,
			}),
		);
		if (purchase.payment_intent_id) {
			batch.addPrepared(
				OperatorPassPurchasesByPaymentIntent.upsertAll({
					payment_intent_id: purchase.payment_intent_id,
					checkout_session_id: purchase.checkout_session_id,
				}),
			);
		}
		await batch.execute();
	}

	async deletePurchase(
		purchase: Pick<OperatorPassPurchaseRow, 'checkout_session_id' | 'purchaser_id' | 'payment_intent_id'>,
	): Promise<void> {
		const batch = new BatchBuilder();
		batch.addPrepared(OperatorPassPurchases.deleteByPk({checkout_session_id: purchase.checkout_session_id}));
		batch.addPrepared(
			OperatorPassPurchasesByPurchaser.deleteByPk({
				purchaser_id: purchase.purchaser_id,
				checkout_session_id: purchase.checkout_session_id,
			}),
		);
		if (purchase.payment_intent_id) {
			batch.addPrepared(
				OperatorPassPurchasesByPaymentIntent.deleteByPk({payment_intent_id: purchase.payment_intent_id}),
			);
		}
		await batch.execute();
	}

	async findPurchase(checkoutSessionId: string): Promise<OperatorPassPurchaseRow | null> {
		return fetchOne<OperatorPassPurchaseRow>(FETCH_PURCHASE, {checkout_session_id: checkoutSessionId});
	}

	async listPurchasesByPurchaser(purchaserId: UserID): Promise<Array<OperatorPassPurchaseRow>> {
		const mappings = await fetchMany<Pick<OperatorPassPurchaseByPurchaserRow, 'checkout_session_id'>>(
			FETCH_PURCHASE_IDS_BY_PURCHASER,
			{purchaser_id: purchaserId},
		);
		const purchases = await Promise.all(mappings.map((mapping) => this.findPurchase(mapping.checkout_session_id)));
		return purchases.filter((purchase): purchase is OperatorPassPurchaseRow => purchase !== null);
	}

	async findPurchaseByPaymentIntent(paymentIntentId: string): Promise<OperatorPassPurchaseRow | null> {
		const mapping = await fetchOne<OperatorPassPurchaseByPaymentIntentRow>(FETCH_PURCHASE_BY_PAYMENT_INTENT, {
			payment_intent_id: paymentIntentId,
		});
		if (!mapping) {
			return null;
		}
		return this.findPurchase(mapping.checkout_session_id);
	}

	async updatePurchase(
		current: OperatorPassPurchaseRow,
		patch: OperatorPassPurchasePatch,
	): Promise<OperatorPassPurchaseRow> {
		const version = nextVersion(current.version);
		const batch = new BatchBuilder();
		batch.addPrepared(
			OperatorPassPurchases.patchByPk(
				{checkout_session_id: current.checkout_session_id},
				{...patch, version: Db.set(version)},
			),
		);
		const paymentIntent = patch.payment_intent_id;
		if (paymentIntent?.kind === 'set' && paymentIntent.value) {
			batch.addPrepared(
				OperatorPassPurchasesByPaymentIntent.upsertAll({
					payment_intent_id: paymentIntent.value,
					checkout_session_id: current.checkout_session_id,
				}),
			);
		}
		await batch.execute();
		return withPatch(current, patch, version);
	}

	async findRemoval(userId: UserID): Promise<OperatorPassRemovalRow | null> {
		return fetchOne<OperatorPassRemovalRow>(FETCH_REMOVAL, {user_id: userId});
	}

	async saveRemoval(row: OperatorPassRemovalRow): Promise<void> {
		await upsertOne(OperatorPassRemovals.upsertAll(row));
	}

	async deleteRemoval(userId: UserID): Promise<void> {
		await deleteOneOrMany(OperatorPassRemovals.deleteByPk({user_id: userId}));
	}

	async scanRemovals(
		pageSize: number,
		pageState?: string | null,
	): Promise<OperatorPassScanPage<OperatorPassRemovalRow>> {
		const page = await fetchPage<OperatorPassRemovalRow>(
			SCAN_REMOVALS,
			{},
			{pageSize, pageState, readTimeout: BACKGROUND_READ_TIMEOUT_MS},
		);
		return {rows: page.rows, pageState: page.pageState};
	}

	async saveRemovalHistory(row: OperatorPassRemovalHistoryRow): Promise<void> {
		await upsertOne(OperatorPassRemovalHistory.upsertAll(row));
	}

	async endRemovalHistory(
		userId: UserID,
		removedAt: Date,
		endedAt: Date,
		reason: OperatorPassRemovalEndReason,
	): Promise<void> {
		await upsertOne(
			OperatorPassRemovalHistory.patchByPk(
				{user_id: userId, removed_at: removedAt},
				{ended_at: Db.set(endedAt), end_reason: Db.set(reason)},
			),
		);
	}

	async listRemovalHistory(userId: UserID): Promise<Array<OperatorPassRemovalHistoryRow>> {
		const rows = await fetchMany<OperatorPassRemovalHistoryRow>(FETCH_REMOVAL_HISTORY, {user_id: userId});
		return rows.sort((left, right) => left.removed_at.getTime() - right.removed_at.getTime());
	}

	async findWallEntry(userId: UserID): Promise<OperatorPassWallEntryRow | null> {
		return fetchOne<OperatorPassWallEntryRow>(FETCH_WALL_ENTRY, {user_id: userId});
	}

	async createWallEntry(entry: OperatorPassWallEntryRow): Promise<boolean> {
		return executeConditional(OperatorPassWallEntries.insertIfNotExists(entry));
	}

	async updateWallEntry(
		current: OperatorPassWallEntryRow,
		patch: OperatorPassWallEntryPatch,
	): Promise<OperatorPassWallEntryRow | null> {
		const version = nextVersion(current.version);
		const applied = await executeConditional(
			OperatorPassWallEntries.conditionalPatchByPk(
				{user_id: current.user_id},
				{...patch, version: Db.set(version)},
				{version: current.version},
			),
		);
		return applied ? withPatch(current, patch, version) : null;
	}

	async scanWallEntries(
		pageSize: number,
		pageState?: string | null,
	): Promise<OperatorPassScanPage<OperatorPassWallEntryRow>> {
		const page = await fetchPage<OperatorPassWallEntryRow>(
			SCAN_WALL_ENTRIES,
			{},
			{pageSize, pageState, readTimeout: BACKGROUND_READ_TIMEOUT_MS},
		);
		return {rows: page.rows, pageState: page.pageState};
	}

	async appendWallEvent(row: OperatorPassWallEventRow): Promise<void> {
		await upsertOne(OperatorPassWallEvents.upsertAll(row));
	}

	async listWallEvents(userId: UserID): Promise<Array<OperatorPassWallEventRow>> {
		const rows = await fetchMany<OperatorPassWallEventRow>(FETCH_WALL_EVENTS, {user_id: userId});
		return rows.sort(
			(left, right) =>
				left.occurred_at.getTime() - right.occurred_at.getTime() || left.event_id.localeCompare(right.event_id),
		);
	}

	async appendWallRevision(row: OperatorPassWallRevisionRow): Promise<boolean> {
		return executeConditional(OperatorPassWallRevisions.insertIfNotExists(row));
	}

	async listWallRevisions(userId: UserID): Promise<Array<OperatorPassWallRevisionRow>> {
		const rows = await fetchMany<OperatorPassWallRevisionRow>(FETCH_WALL_REVISIONS, {user_id: userId});
		return rows.sort((left, right) => compareSnowflakes(left.revision_id, right.revision_id));
	}

	async appendWallDecision(row: OperatorPassWallDecisionRow): Promise<boolean> {
		return executeConditional(OperatorPassWallDecisions.insertIfNotExists(row));
	}

	async listWallDecisions(userId: UserID): Promise<Array<OperatorPassWallDecisionRow>> {
		const rows = await fetchMany<OperatorPassWallDecisionRow>(FETCH_WALL_DECISIONS, {user_id: userId});
		return rows.sort((left, right) => compareSnowflakes(left.decision_id, right.decision_id));
	}

	async saveWallRevision(row: OperatorPassWallRevisionRow): Promise<void> {
		await upsertOne(OperatorPassWallRevisions.upsertAll(row));
	}

	async saveWallDecision(row: OperatorPassWallDecisionRow): Promise<void> {
		await upsertOne(OperatorPassWallDecisions.upsertAll(row));
	}

	async addWallAvatarRef(avatar: string, userId: UserID): Promise<void> {
		await upsertOne(OperatorPassWallAvatarRefs.upsertAll({avatar, user_id: userId}));
	}

	async removeWallAvatarRef(avatar: string, userId: UserID): Promise<void> {
		await deleteOneOrMany(OperatorPassWallAvatarRefs.deleteByPk({avatar, user_id: userId}));
	}

	async listWallAvatarRefs(avatar: string): Promise<Array<UserID>> {
		const rows = await fetchMany<{user_id: UserID}>(FETCH_WALL_AVATAR_REFS, {avatar});
		return rows.map((row) => row.user_id);
	}

	async setWallPending(userId: UserID, pending: boolean): Promise<void> {
		if (pending) {
			await upsertOne(OperatorPassWallPending.upsertAll({bucket: WALL_PENDING_BUCKET, user_id: userId}));
		} else {
			await deleteOneOrMany(OperatorPassWallPending.deleteByPk({bucket: WALL_PENDING_BUCKET, user_id: userId}));
		}
	}

	async listWallPending(): Promise<Array<UserID>> {
		const rows = await fetchMany<{user_id: UserID}>(FETCH_WALL_PENDING, {bucket: WALL_PENDING_BUCKET});
		return rows.map((row) => row.user_id);
	}

	async addWallAuditLog(userId: UserID, logId: bigint): Promise<void> {
		await upsertOne(OperatorPassWallAuditLogs.upsertAll({user_id: userId, log_id: logId}));
	}

	async listWallAuditLogIds(userId: UserID): Promise<Array<bigint>> {
		const rows = await fetchMany<{log_id: bigint}>(FETCH_WALL_AUDIT_LOGS, {user_id: userId});
		return rows.map((row) => row.log_id);
	}

	async deleteWallPartitions(userId: UserID): Promise<void> {
		await deleteOneOrMany(OperatorPassWallAuditLogs.deletePartition({user_id: userId}));
		await deleteOneOrMany(OperatorPassWallRevisions.deletePartition({user_id: userId}));
		await deleteOneOrMany(OperatorPassWallDecisions.deletePartition({user_id: userId}));
		await deleteOneOrMany(OperatorPassWallEvents.deletePartition({user_id: userId}));
		const entry = await this.findWallEntry(userId);
		if (entry) {
			await executeConditional(
				OperatorPassWallEntries.conditionalDeleteByPk({user_id: userId}, {version: entry.version}),
			);
		}
	}

	async saveWallTombstone(row: OperatorPassWallTombstoneRow): Promise<void> {
		await upsertOne(OperatorPassWallTombstones.upsertAll(row));
	}

	async listWallTombstones(): Promise<Array<OperatorPassWallTombstoneRow>> {
		return fetchMany<OperatorPassWallTombstoneRow>(SCAN_WALL_TOMBSTONES, {});
	}

	async findWallState(id: string): Promise<OperatorPassWallStateRow | null> {
		return fetchOne<OperatorPassWallStateRow>(FETCH_WALL_STATE, {id});
	}

	async createWallState(row: OperatorPassWallStateRow): Promise<boolean> {
		return executeConditional(OperatorPassWallState.insertIfNotExists(row));
	}

	async updateWallState(
		current: OperatorPassWallStateRow,
		patch: RowPatch<OperatorPassWallStateRow, 'id'>,
	): Promise<OperatorPassWallStateRow | null> {
		const version = nextVersion(current.version);
		const applied = await executeConditional(
			OperatorPassWallState.conditionalPatchByPk(
				{id: current.id},
				{...patch, version: Db.set(version)},
				{version: current.version},
			),
		);
		return applied ? withPatch(current, patch, version) : null;
	}

	async findRelease(year: number): Promise<OperatorPassReleaseRow | null> {
		return fetchOne<OperatorPassReleaseRow>(FETCH_RELEASE, {year});
	}

	async createRelease(row: OperatorPassReleaseRow): Promise<boolean> {
		return executeConditional(OperatorPassReleases.insertIfNotExists(row));
	}

	async saveRelease(row: OperatorPassReleaseRow): Promise<void> {
		await upsertOne(OperatorPassReleases.upsertAll(row));
	}

	async listReleases(): Promise<Array<OperatorPassReleaseRow>> {
		const rows = await fetchMany<OperatorPassReleaseRow>(SCAN_RELEASES, {});
		return rows.sort((left, right) => left.year - right.year);
	}

	async createUnit(unit: OperatorPassUnitRow): Promise<boolean> {
		const created = await executeConditional(OperatorPassUnits.insertIfNotExists(unit));
		if (created) {
			const batch = new BatchBuilder();
			batch.addPrepared(OperatorPassUnitsByHolder.upsertAll({holder_id: unit.holder_id, pass_id: unit.pass_id}));
			if (unit.purchase_id) {
				batch.addPrepared(
					OperatorPassUnitsByPurchase.upsertAll({purchase_id: unit.purchase_id, pass_id: unit.pass_id}),
				);
			}
			await batch.execute();
		}
		return created;
	}

	async findUnit(passId: string): Promise<OperatorPassUnitRow | null> {
		return fetchOne<OperatorPassUnitRow>(FETCH_UNIT, {pass_id: passId});
	}

	async updateUnit(current: OperatorPassUnitRow, patch: OperatorPassUnitPatch): Promise<OperatorPassUnitRow | null> {
		const version = nextVersion(current.version);
		const applied = await executeConditional(
			OperatorPassUnits.conditionalPatchByPk(
				{pass_id: current.pass_id},
				{...patch, version: Db.set(version)},
				{version: current.version},
			),
		);
		return applied ? withPatch(current, patch, version) : null;
	}

	async moveUnitHolder(passId: string, from: UserID, to: UserID): Promise<void> {
		const batch = new BatchBuilder();
		batch.addPrepared(OperatorPassUnitsByHolder.deleteByPk({holder_id: from, pass_id: passId}));
		batch.addPrepared(OperatorPassUnitsByHolder.upsertAll({holder_id: to, pass_id: passId}));
		await batch.execute();
	}

	async indexUnitHolder(passId: string, holderId: UserID): Promise<void> {
		await upsertOne(OperatorPassUnitsByHolder.upsertAll({holder_id: holderId, pass_id: passId}));
	}

	async removeUnitFromHolder(passId: string, holderId: UserID): Promise<void> {
		await deleteOneOrMany(OperatorPassUnitsByHolder.deleteByPk({holder_id: holderId, pass_id: passId}));
	}

	async listUnitsByHolder(holderId: UserID): Promise<Array<OperatorPassUnitRow>> {
		const ids = await fetchMany<{pass_id: string}>(FETCH_UNIT_IDS_BY_HOLDER, {holder_id: holderId});
		const units = await Promise.all(ids.map(({pass_id}) => this.findUnit(pass_id)));
		return units
			.filter((unit): unit is OperatorPassUnitRow => unit !== null && unit.holder_id === holderId)
			.sort((left, right) => left.first_delivered_at.getTime() - right.first_delivered_at.getTime());
	}

	async listCertificateVersions(passId: string): Promise<Array<OperatorPassCertificateRow>> {
		const rows = await fetchMany<OperatorPassCertificateRow>(FETCH_CERTIFICATE_VERSIONS, {pass_id: passId});
		return rows.sort((left, right) => left.version - right.version);
	}

	async findLatestCertificate(passId: string): Promise<OperatorPassCertificateRow | null> {
		return (await this.listCertificateVersions(passId)).at(-1) ?? null;
	}

	async scanCertificatePages(
		pageSize: number,
		pageState?: string | null,
	): Promise<OperatorPassScanPage<OperatorPassCertificatePageSummary>> {
		const page = await fetchPage<OperatorPassCertificatePageSummary>(
			SCAN_CERTIFICATE_PAGES,
			{},
			{pageSize, pageState, readTimeout: BACKGROUND_READ_TIMEOUT_MS},
		);
		return {rows: page.rows, pageState: page.pageState};
	}

	async listCertificatesByPurchase(purchaseId: string): Promise<Array<OperatorPassCertificateRow>> {
		const ids = await fetchMany<{pass_id: string}>(FETCH_CERTIFICATE_IDS_BY_PURCHASE, {purchase_id: purchaseId});
		const rows = await Promise.all(ids.map(({pass_id}) => this.findLatestCertificate(pass_id)));
		return rows
			.filter((row): row is OperatorPassCertificateRow => row !== null)
			.sort((left, right) => left.number - right.number);
	}

	async createCertificate(row: OperatorPassCertificateRow): Promise<boolean> {
		const created = await executeConditional(OperatorPassCertificates.insertIfNotExists(row));
		if (created) {
			await this.indexCertificate(row);
		}
		return created;
	}

	async indexCertificate(
		row: Pick<OperatorPassCertificateRow, 'purchase_id' | 'pass_id' | 'link_token'>,
	): Promise<void> {
		if (row.purchase_id) {
			await upsertOne(
				OperatorPassCertificatesByPurchase.upsertAll({purchase_id: row.purchase_id, pass_id: row.pass_id}),
			);
		}
		if (row.link_token) {
			await upsertOne(OperatorPassCertificateLinks.upsertAll({token_: row.link_token, pass_id: row.pass_id}));
		}
	}

	async findCertificateLink(token: string): Promise<OperatorPassCertificateLinkRow | null> {
		return fetchOne<OperatorPassCertificateLinkRow>(FETCH_CERTIFICATE_LINK, {token_: token});
	}

	async deleteCertificateLink(token: string): Promise<void> {
		await deleteOneOrMany(OperatorPassCertificateLinks.deleteByPk({token_: token}));
	}

	async findLatestCertificateLink(passId: string): Promise<OperatorPassCertificateLinkSummary | null> {
		return fetchOne<OperatorPassCertificateLinkSummary>(FETCH_LATEST_CERTIFICATE_LINK, {pass_id: passId});
	}

	async listCertificateLinksByPurchase(purchaseId: string): Promise<Array<OperatorPassCertificateLinkSummary>> {
		const ids = await fetchMany<{pass_id: string}>(FETCH_CERTIFICATE_IDS_BY_PURCHASE, {purchase_id: purchaseId});
		const rows = await Promise.all(ids.map(({pass_id}) => this.findLatestCertificateLink(pass_id)));
		return rows.filter((row): row is OperatorPassCertificateLinkSummary => row !== null);
	}

	async claimCertificateNumber(number: number, passId: string): Promise<boolean> {
		return executeConditional(OperatorPassCertificateNumbers.insertIfNotExists({number, pass_id: passId}));
	}

	async findCertificateNumber(number: number): Promise<OperatorPassCertificateNumberRow | null> {
		return fetchOne<OperatorPassCertificateNumberRow>(FETCH_CERTIFICATE_NUMBER, {number});
	}

	async findHighestCertificateNumber(): Promise<number> {
		let highest = 0;
		let pageState: string | null = null;
		do {
			const page: {rows: Array<{number: number}>; pageState: string | null} = await fetchPage<{number: number}>(
				SCAN_CERTIFICATE_NUMBERS,
				{},
				{pageSize: 1000, pageState, readTimeout: BACKGROUND_READ_TIMEOUT_MS},
			);
			for (const row of page.rows) {
				highest = Math.max(highest, row.number);
			}
			pageState = page.pageState;
		} while (pageState);
		return highest;
	}

	async scanCertificateNumberOwners(): Promise<Array<OperatorPassCertificateNumberRow>> {
		const owners: Array<OperatorPassCertificateNumberRow> = [];
		let pageState: string | null = null;
		do {
			const page: {rows: Array<OperatorPassCertificateNumberRow>; pageState: string | null} =
				await fetchPage<OperatorPassCertificateNumberRow>(
					SCAN_CERTIFICATE_NUMBER_OWNERS,
					{},
					{pageSize: 1000, pageState, readTimeout: BACKGROUND_READ_TIMEOUT_MS},
				);
			owners.push(...page.rows);
			pageState = page.pageState;
		} while (pageState);
		return owners;
	}

	async deleteCertificateRecords(passId: string): Promise<void> {
		const versions = await this.listCertificateVersions(passId);
		for (const token of new Set(versions.map((row) => row.link_token))) {
			if (token) {
				await this.deleteCertificateLink(token);
			}
		}
		for (const row of versions) {
			const owner = await this.findCertificateNumber(row.number);
			if (owner?.pass_id === passId) {
				await executeConditional(
					OperatorPassCertificateNumbers.conditionalDeleteByPk({number: row.number}, {pass_id: passId}),
				);
			}
		}
		await deleteOneOrMany(OperatorPassCertificates.deletePartition({pass_id: passId}));
	}

	async deleteCertificateNumberClaim(number: number, passId: string): Promise<boolean> {
		return executeConditional(OperatorPassCertificateNumbers.conditionalDeleteByPk({number}, {pass_id: passId}));
	}

	async deleteCertificateIndexForPurchase(purchaseId: string): Promise<void> {
		await deleteOneOrMany(OperatorPassCertificatesByPurchase.deletePartition({purchase_id: purchaseId}));
	}

	async deleteUnitRecords(unit: OperatorPassUnitRow): Promise<boolean> {
		const deleted = await executeConditional(
			OperatorPassUnits.conditionalDeleteByPk({pass_id: unit.pass_id}, {version: unit.version}),
		);
		if (!deleted) {
			return false;
		}
		const batch = new BatchBuilder();
		batch.addPrepared(OperatorPassUnitsByHolder.deleteByPk({holder_id: unit.holder_id, pass_id: unit.pass_id}));
		if (unit.purchase_id) {
			batch.addPrepared(OperatorPassUnitsByPurchase.deleteByPk({purchase_id: unit.purchase_id, pass_id: unit.pass_id}));
		}
		await batch.execute();
		await deleteOneOrMany(OperatorPassUnitEvents.deletePartition({pass_id: unit.pass_id}));
		return true;
	}

	async deleteOffer(offer: OperatorPassOfferRow): Promise<boolean> {
		const deleted = await executeConditional(
			OperatorPassOffers.conditionalDeleteByPk({offer_id: offer.offer_id}, {version: offer.version}),
		);
		if (deleted) {
			const batch = new BatchBuilder();
			batch.addPrepared(OperatorPassOffersByUser.deleteByPk({user_id: offer.sender_id, offer_id: offer.offer_id}));
			batch.addPrepared(OperatorPassOffersByUser.deleteByPk({user_id: offer.recipient_id, offer_id: offer.offer_id}));
			await batch.execute();
		}
		return deleted;
	}

	async markCertificateCancelled(passId: string, cancelledAt: Date): Promise<void> {
		for (const row of await this.listCertificateVersions(passId)) {
			if (row.cancelled_at) continue;
			await upsertOne(
				OperatorPassCertificates.patchByPk(
					{pass_id: row.pass_id, version: row.version},
					{cancelled_at: Db.set(cancelledAt)},
				),
			);
		}
	}

	async clearCertificatePersonalData(passId: string, beforeVersion = Number.POSITIVE_INFINITY): Promise<void> {
		for (const row of await this.listCertificateVersions(passId)) {
			if (row.version >= beforeVersion || (row.pdf.length === 0 && row.holder_name === '')) continue;
			await upsertOne(
				OperatorPassCertificates.patchByPk(
					{pass_id: row.pass_id, version: row.version},
					{pdf: Db.set(Buffer.alloc(0)), holder_name: Db.set('')},
				),
			);
		}
	}

	async listUnitsByPurchase(purchaseId: string): Promise<Array<OperatorPassUnitRow>> {
		const ids = await fetchMany<{pass_id: string}>(FETCH_UNIT_IDS_BY_PURCHASE, {purchase_id: purchaseId});
		const units = await Promise.all(ids.map(({pass_id}) => this.findUnit(pass_id)));
		return units
			.filter((unit): unit is OperatorPassUnitRow => unit !== null)
			.sort((left, right) => left.pass_id.localeCompare(right.pass_id));
	}

	async appendUnitEvent(row: OperatorPassUnitEventRow): Promise<void> {
		await upsertOne(OperatorPassUnitEvents.upsertAll(row));
	}

	async listUnitEvents(passId: string): Promise<Array<OperatorPassUnitEventRow>> {
		const rows = await fetchMany<OperatorPassUnitEventRow>(FETCH_UNIT_EVENTS, {pass_id: passId});
		return rows.sort(
			(left, right) =>
				left.occurred_at.getTime() - right.occurred_at.getTime() || left.event_id.localeCompare(right.event_id),
		);
	}

	async findVisionaryActivation(userId: UserID): Promise<OperatorPassVisionaryActivationRow | null> {
		return fetchOne<OperatorPassVisionaryActivationRow>(FETCH_VISIONARY_ACTIVATION, {user_id: userId});
	}

	async createVisionaryActivation(row: OperatorPassVisionaryActivationRow): Promise<boolean> {
		return executeConditional(OperatorPassVisionaryActivations.insertIfNotExists(row));
	}

	async updateVisionaryActivation(
		current: OperatorPassVisionaryActivationRow,
		patch: OperatorPassVisionaryActivationPatch,
	): Promise<OperatorPassVisionaryActivationRow | null> {
		const version = nextVersion(current.version);
		const applied = await executeConditional(
			OperatorPassVisionaryActivations.conditionalPatchByPk(
				{user_id: current.user_id},
				{...patch, version: Db.set(version)},
				{version: current.version},
			),
		);
		return applied ? withPatch(current, patch, version) : null;
	}

	async scanVisionaryActivations(
		pageSize: number,
		pageState?: string | null,
	): Promise<OperatorPassScanPage<OperatorPassVisionaryActivationRow>> {
		const page = await fetchPage<OperatorPassVisionaryActivationRow>(
			SCAN_VISIONARY_ACTIVATIONS,
			{},
			{pageSize, pageState, readTimeout: BACKGROUND_READ_TIMEOUT_MS},
		);
		return {rows: page.rows, pageState: page.pageState};
	}

	async createOffer(offer: OperatorPassOfferRow): Promise<boolean> {
		const created = await executeConditional(OperatorPassOffers.insertIfNotExists(offer));
		if (created) {
			const batch = new BatchBuilder();
			batch.addPrepared(OperatorPassOffersByUser.upsertAll({user_id: offer.sender_id, offer_id: offer.offer_id}));
			batch.addPrepared(OperatorPassOffersByUser.upsertAll({user_id: offer.recipient_id, offer_id: offer.offer_id}));
			await batch.execute();
		}
		return created;
	}

	async findOffer(offerId: string): Promise<OperatorPassOfferRow | null> {
		return fetchOne<OperatorPassOfferRow>(FETCH_OFFER, {offer_id: offerId});
	}

	async updateOffer(
		current: OperatorPassOfferRow,
		patch: OperatorPassOfferPatch,
	): Promise<OperatorPassOfferRow | null> {
		const version = nextVersion(current.version);
		const applied = await executeConditional(
			OperatorPassOffers.conditionalPatchByPk(
				{offer_id: current.offer_id},
				{...patch, version: Db.set(version)},
				{version: current.version},
			),
		);
		return applied ? withPatch(current, patch, version) : null;
	}

	async listOffersByUser(userId: UserID): Promise<Array<OperatorPassOfferRow>> {
		const ids = await fetchMany<{offer_id: string}>(FETCH_OFFER_IDS_BY_USER, {user_id: userId});
		const offers = await Promise.all(ids.map(({offer_id}) => this.findOffer(offer_id)));
		return offers
			.filter((offer): offer is OperatorPassOfferRow => offer !== null)
			.sort((left, right) => right.created_at.getTime() - left.created_at.getTime());
	}

	async scanOffers(pageSize: number, pageState?: string | null): Promise<OperatorPassScanPage<OperatorPassOfferRow>> {
		const page = await fetchPage<OperatorPassOfferRow>(
			SCAN_OFFERS,
			{},
			{pageSize, pageState, readTimeout: BACKGROUND_READ_TIMEOUT_MS},
		);
		return {rows: page.rows, pageState: page.pageState};
	}

	async saveOperatorPass(pass: OperatorPassRow): Promise<void> {
		await upsertOne(OperatorPasses.upsertAll(pass));
	}

	async findLabsMember(userId: UserID): Promise<OperatorPassLabsMemberRow | null> {
		return fetchOne<OperatorPassLabsMemberRow>(FETCH_LABS_MEMBER, {user_id: userId});
	}

	async setLabsGreeting(userId: UserID, greeting: boolean): Promise<void> {
		await upsertOne(OperatorPassLabsMembers.patchByPk({user_id: userId}, {greeting: Db.set(greeting)}));
	}

	async recordLabsAdded(userId: UserID, at: Date): Promise<void> {
		await upsertOne(OperatorPassLabsMembers.patchByPk({user_id: userId}, {added_at: Db.set(at)}));
	}

	async recordLabsLeft(userId: UserID, at: Date, reason: OperatorPassLabsLeftReason): Promise<void> {
		await upsertOne(
			OperatorPassLabsMembers.patchByPk({user_id: userId}, {left_at: Db.set(at), left_reason: Db.set(reason)}),
		);
	}

	async recordLabsGreeted(userId: UserID, at: Date, messageId: bigint): Promise<void> {
		await upsertOne(
			OperatorPassLabsMembers.patchByPk(
				{user_id: userId},
				{greeted_at: Db.set(at), greeting_message_id: Db.set(messageId)},
			),
		);
	}

	async deleteLabsMember(userId: UserID): Promise<void> {
		await deleteOneOrMany(OperatorPassLabsMembers.deleteByPk({user_id: userId}));
		await this.removeLabsGreeting(userId);
	}

	async queueLabsGreeting(userId: UserID, at: Date): Promise<void> {
		await upsertOne(
			OperatorPassLabsGreetings.patchByPk({bucket: LABS_GREETING_BUCKET, user_id: userId}, {queued_at: Db.set(at)}),
		);
	}

	async listLabsGreetings(): Promise<Array<OperatorPassLabsGreetingRow>> {
		return fetchMany<OperatorPassLabsGreetingRow>(FETCH_LABS_GREETINGS, {bucket: LABS_GREETING_BUCKET});
	}

	async claimLabsGreeting(userId: UserID, messageId: bigint): Promise<void> {
		await upsertOne(
			OperatorPassLabsGreetings.patchByPk(
				{bucket: LABS_GREETING_BUCKET, user_id: userId},
				{message_id: Db.set(messageId)},
			),
		);
	}

	async removeLabsGreeting(userId: UserID): Promise<void> {
		await deleteOneOrMany(OperatorPassLabsGreetings.deleteByPk({bucket: LABS_GREETING_BUCKET, user_id: userId}));
	}
}

function expectedWaitlistVersion(row: OperatorPassWaitlistRow): Partial<OperatorPassWaitlistRow> {
	return row.version == null ? {entry_id: row.entry_id} : {version: row.version};
}

function waitlistRowPatch(entry: OperatorPassWaitlistRow): OperatorPassWaitlistPatch {
	const patch: Record<string, DbOp<unknown>> = {};
	for (const column of OPERATOR_PASS_WAITLIST_COLUMNS) {
		if (column === 'email') continue;
		const value = entry[column];
		patch[column] = value == null ? Db.clear() : Db.set(value);
	}
	return patch as OperatorPassWaitlistPatch;
}
