// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	BillingDocumentNumbers,
	BillingDocuments,
	BillingDocumentsByMonth,
	BillingDocumentsBySource,
	BillingDocumentsByUser,
} from '@app/api/billing_documents/BillingDocumentTables';
import {
	deleteOneOrMany,
	executeConditional,
	fetchMany,
	fetchOne,
	upsertOne,
} from '@app/api/database/CassandraQueryExecution';
import {Db} from '@app/api/database/CassandraTypes';
import type {
	BillingDocumentBySourceRow,
	BillingDocumentNumberRow,
	BillingDocumentRow,
} from '@app/api/database/types/BillingDocumentTypes';
import {stockholmDate} from '@pkgs/email/src/billing_documents/BillingDocumentContent';

const HEAD_NUMBER = 0n;
const RECENT_CLAIM_SCAN = 200;
const MAX_CLAIM_ATTEMPTS = 100;

const FETCH_SOURCE = BillingDocumentsBySource.selectCql({
	where: BillingDocumentsBySource.where.eq('source_key'),
	limit: 1,
});
const FETCH_DOCUMENT = BillingDocuments.selectCql({where: BillingDocuments.where.eq('document_id'), limit: 1});
const FETCH_NUMBER = BillingDocumentNumbers.selectCql({
	where: [BillingDocumentNumbers.where.eq('series'), BillingDocumentNumbers.where.eq('number')],
	limit: 1,
});
const FETCH_CLAIMS_AFTER = BillingDocumentNumbers.selectCql({
	where: [BillingDocumentNumbers.where.eq('series'), BillingDocumentNumbers.where.gt('number', 'after')],
});
const FETCH_RECENT_CLAIMS = BillingDocumentNumbers.selectCql({
	where: [BillingDocumentNumbers.where.eq('series'), BillingDocumentNumbers.where.gt('number', 'after')],
	orderBy: {col: 'number', direction: 'DESC'},
	limit: RECENT_CLAIM_SCAN,
});
const FETCH_USER_DOCUMENTS = BillingDocumentsByUser.selectCql({
	columns: ['document_id', 'issued_at'],
	where: BillingDocumentsByUser.where.eq('user_id'),
	orderBy: {col: 'issued_at', direction: 'DESC'},
});
const FETCH_USER_DOCUMENTS_BEFORE = BillingDocumentsByUser.selectCql({
	columns: ['document_id', 'issued_at'],
	where: [BillingDocumentsByUser.where.eq('user_id'), BillingDocumentsByUser.where.lt('issued_at', 'before')],
	orderBy: {col: 'issued_at', direction: 'DESC'},
});
const FETCH_MONTH_DOCUMENTS = BillingDocumentsByMonth.selectCql({
	columns: ['document_id'],
	where: BillingDocumentsByMonth.where.eq('month'),
});
const FETCH_SERIES_CLAIMS = BillingDocumentNumbers.selectCql({
	where: [BillingDocumentNumbers.where.eq('series'), BillingDocumentNumbers.where.gt('number', 'after')],
});

export interface ClaimedNumber {
	series: string;
	number: bigint;
}

export class BillingDocumentRepository {
	async findSource(sourceKey: string): Promise<BillingDocumentBySourceRow | null> {
		return fetchOne<BillingDocumentBySourceRow>(FETCH_SOURCE, {source_key: sourceKey});
	}

	async claimSource(sourceKey: string, documentId: bigint): Promise<BillingDocumentBySourceRow> {
		const claimed = await executeConditional(
			BillingDocumentsBySource.insertIfNotExists({
				source_key: sourceKey,
				document_id: documentId,
				series: null,
				number: null,
			}),
		);
		if (claimed) {
			return {source_key: sourceKey, document_id: documentId, series: null, number: null};
		}
		const existing = await this.findSource(sourceKey);
		if (!existing) {
			throw new Error(`Billing document source ${sourceKey} was claimed but cannot be read`);
		}
		return existing;
	}

	async aliasSource(sourceKey: string, documentId: bigint): Promise<void> {
		await executeConditional(
			BillingDocumentsBySource.insertIfNotExists({
				source_key: sourceKey,
				document_id: documentId,
				series: null,
				number: null,
			}),
		);
	}

	async findDocument(documentId: bigint): Promise<BillingDocumentRow | null> {
		return fetchOne<BillingDocumentRow>(FETCH_DOCUMENT, {document_id: documentId});
	}

	async findDocumentBySource(sourceKey: string): Promise<BillingDocumentRow | null> {
		const source = await this.findSource(sourceKey);
		return source ? this.findDocument(source.document_id) : null;
	}

	async findNumber(series: string, number: bigint): Promise<BillingDocumentNumberRow | null> {
		return fetchOne<BillingDocumentNumberRow>(FETCH_NUMBER, {series, number});
	}

	async claimNumber(series: string, sourceKey: string, documentId: bigint): Promise<ClaimedNumber> {
		const remembered = await this.findSource(sourceKey);
		if (remembered?.series === series && remembered.number != null) {
			return {series, number: remembered.number};
		}
		let head = await this.readHead(series);
		const recent = await fetchMany<BillingDocumentNumberRow>(FETCH_RECENT_CLAIMS, {series, after: HEAD_NUMBER});
		let checkedThrough = head;
		const found = recent.find((row) => row.source_key === sourceKey);
		if (found) {
			return this.rememberClaim(sourceKey, documentId, series, found.number);
		}
		for (let attempt = 0; attempt < MAX_CLAIM_ATTEMPTS; attempt++) {
			if (head > checkedThrough) {
				const newer = await fetchMany<BillingDocumentNumberRow>(FETCH_CLAIMS_AFTER, {series, after: checkedThrough});
				const own = newer.find((row) => row.source_key === sourceKey);
				if (own) {
					return this.rememberClaim(sourceKey, documentId, series, own.number);
				}
				checkedThrough = head;
			}
			const next = head + 1n;
			const applied = await executeConditional(
				BillingDocumentNumbers.conditionalBatch([
					{
						action: 'patch',
						pk: {series, number: HEAD_NUMBER},
						patch: {last_number: Db.set(next)},
						expected: {last_number: head},
					},
					{
						action: 'insert',
						row: {series, number: next, last_number: null, source_key: sourceKey, document_id: documentId},
					},
				]),
			);
			if (applied) {
				return this.rememberClaim(sourceKey, documentId, series, next);
			}
			head = await this.readHead(series);
		}
		throw new Error(`Could not claim a ${series} document number`);
	}

	private async readHead(series: string): Promise<bigint> {
		const head = await this.findNumber(series, HEAD_NUMBER);
		if (head?.last_number != null) {
			return head.last_number;
		}
		await executeConditional(
			BillingDocumentNumbers.insertIfNotExists({
				series,
				number: HEAD_NUMBER,
				last_number: 0n,
				source_key: null,
				document_id: null,
			}),
		);
		const seeded = await this.findNumber(series, HEAD_NUMBER);
		if (seeded?.last_number == null) {
			throw new Error(`The ${series} document series has no counter`);
		}
		return seeded.last_number;
	}

	private async rememberClaim(
		sourceKey: string,
		documentId: bigint,
		series: string,
		number: bigint,
	): Promise<ClaimedNumber> {
		await upsertOne(
			BillingDocumentsBySource.upsertAll({source_key: sourceKey, document_id: documentId, series, number}),
		);
		return {series, number};
	}

	async createDocument(row: BillingDocumentRow, {indexMonth = true} = {}): Promise<BillingDocumentRow> {
		const created = await executeConditional(BillingDocuments.insertIfNotExists(row));
		const stored = created ? row : await this.findDocument(row.document_id);
		if (!stored) {
			throw new Error(`Billing document ${row.document_id} could not be stored`);
		}
		if (stored.user_id != null) {
			await upsertOne(
				BillingDocumentsByUser.upsertAll({
					user_id: stored.user_id,
					issued_at: stored.issued_at,
					document_id: stored.document_id,
				}),
			);
		}
		if (indexMonth) {
			await upsertOne(
				BillingDocumentsByMonth.upsertAll({
					month: stockholmDate(stored.issued_at.toISOString()).slice(0, 7),
					document_id: stored.document_id,
				}),
			);
		}
		return stored;
	}

	async markEmailed(documentId: bigint, at: Date): Promise<void> {
		await upsertOne(BillingDocuments.patchByPk({document_id: documentId}, {emailed_at: Db.set(at)}));
	}

	async markArchived(documentId: bigint, at: Date): Promise<void> {
		await upsertOne(BillingDocuments.patchByPk({document_id: documentId}, {archived_at: Db.set(at)}));
	}

	async listUserDocuments(userId: bigint, before: Date | null, limit: number): Promise<Array<BillingDocumentRow>> {
		const index = before
			? await fetchMany<{document_id: bigint}>(FETCH_USER_DOCUMENTS_BEFORE, {user_id: userId, before})
			: await fetchMany<{document_id: bigint}>(FETCH_USER_DOCUMENTS, {user_id: userId});
		const rows = await Promise.all(index.slice(0, limit).map(({document_id}) => this.findDocument(document_id)));
		return rows.filter((row): row is BillingDocumentRow => row !== null);
	}

	async removeUserIndex(userId: bigint): Promise<void> {
		const index = await fetchMany<{document_id: bigint; issued_at: Date}>(FETCH_USER_DOCUMENTS, {user_id: userId});
		for (const row of index) {
			await deleteOneOrMany(
				BillingDocumentsByUser.deleteByPk({user_id: userId, issued_at: row.issued_at, document_id: row.document_id}),
			);
		}
	}

	async listMonthDocuments(month: string): Promise<Array<BillingDocumentRow>> {
		const index = await fetchMany<{document_id: bigint}>(FETCH_MONTH_DOCUMENTS, {month});
		const rows = await Promise.all(index.map(({document_id}) => this.findDocument(document_id)));
		return rows
			.filter((row): row is BillingDocumentRow => row !== null)
			.sort((left, right) => left.issued_at.getTime() - right.issued_at.getTime());
	}

	async listSeriesClaims(series: string): Promise<Array<BillingDocumentNumberRow>> {
		return fetchMany<BillingDocumentNumberRow>(FETCH_SERIES_CLAIMS, {series, after: HEAD_NUMBER});
	}
}
