// SPDX-License-Identifier: AGPL-3.0-or-later

import type Stripe from 'stripe';
import {fetchMany, fetchOne, fetchPage, type PagedQueryResult, upsertOne} from '../../database/CassandraQueryExecution';
import type {BillingInvoiceRow} from '../../database/types/BillingTypes';
import {BILLING_INVOICE_COLUMNS} from '../../database/types/BillingTypes';
import {
	BillingCustomersByUserId,
	BillingInvoices,
	BillingInvoicesByCustomer,
	BillingInvoicesBySubscription,
} from '../../Tables';
import {mapStripeInvoiceToRow} from '../mappers/StripeToBillingMapper';
import type {BillingPaymentRepository} from './BillingPaymentRepository';
import {
	BILLING_REVERSE_CHRONOLOGICAL_ORDER,
	buildPatchFromRow,
	executeBillingVersionedUpdate,
	isExistingNewer,
	restoreReferenceOrder,
	rowsEquivalent,
} from './BillingRepoHelpers';

const FETCH_BY_ID = BillingInvoices.selectCql({
	where: BillingInvoices.where.eq('provider_id'),
	limit: 1,
});
const FETCH_BY_CUSTOMER_PARTITION = BillingInvoicesByCustomer.selectCql({
	where: BillingInvoicesByCustomer.where.eq('customer_id'),
	orderBy: BILLING_REVERSE_CHRONOLOGICAL_ORDER,
});
const FETCH_BY_SUBSCRIPTION_PARTITION = BillingInvoicesBySubscription.selectCql({
	where: BillingInvoicesBySubscription.where.eq('subscription_id'),
	orderBy: BILLING_REVERSE_CHRONOLOGICAL_ORDER,
});
const FETCH_BY_PROVIDER_IDS = BillingInvoices.selectCql({
	where: BillingInvoices.where.in('provider_id', 'provider_ids'),
});
const FETCH_CUSTOMERS_BY_USER = BillingCustomersByUserId.selectCql({
	where: BillingCustomersByUserId.where.eq('user_id'),
});

interface InvoiceRef {
	provider_id: string;
	stripe_created_at: Date;
}

interface UserInvoicePartitionState {
	customerId: string;
	nextPageState: string | null;
	exhausted: boolean;
	buffer: Array<{providerId: string; stripeCreatedAt: string}>;
}

interface UserInvoicePageState {
	version: 1;
	partitions: Array<UserInvoicePartitionState>;
}

function decodeUserInvoicePageState(value: string): UserInvoicePageState {
	const decoded = JSON.parse(Buffer.from(value, 'base64url').toString('utf8')) as UserInvoicePageState;
	const validPartitions =
		Array.isArray(decoded.partitions) &&
		decoded.partitions.every((partition) => {
			if (typeof partition.customerId !== 'string') return false;
			if (partition.nextPageState !== null && typeof partition.nextPageState !== 'string') return false;
			if (typeof partition.exhausted !== 'boolean' || !Array.isArray(partition.buffer)) return false;
			return partition.buffer.every(
				(ref) =>
					typeof ref.providerId === 'string' &&
					typeof ref.stripeCreatedAt === 'string' &&
					Number.isFinite(new Date(ref.stripeCreatedAt).getTime()),
			);
		});
	if (decoded.version !== 1 || !validPartitions) {
		throw new Error('Invalid billing invoice user page state');
	}
	return decoded;
}

function encodeUserInvoicePageState(state: UserInvoicePageState): string {
	return Buffer.from(JSON.stringify(state)).toString('base64url');
}

export class BillingInvoiceRepository {
	constructor(private paymentsRepo: BillingPaymentRepository) {}

	async findById(providerId: string): Promise<BillingInvoiceRow | null> {
		return fetchOne<BillingInvoiceRow>(FETCH_BY_ID, {provider_id: providerId});
	}

	async listByCustomer(
		customerId: string,
		page?: {
			pageSize: number;
			pageState?: string | null;
		},
	): Promise<PagedQueryResult<BillingInvoiceRow>> {
		const refsPage = await fetchPage<{
			provider_id: string;
		}>(
			FETCH_BY_CUSTOMER_PARTITION,
			{customer_id: customerId},
			{pageSize: page?.pageSize ?? 50, pageState: page?.pageState ?? null},
		);
		if (refsPage.rows.length === 0) {
			return {rows: [], pageState: refsPage.pageState};
		}
		const ids = refsPage.rows.map((r) => r.provider_id);
		const rows = await fetchMany<BillingInvoiceRow>(FETCH_BY_PROVIDER_IDS, {provider_ids: ids});
		return {rows: restoreReferenceOrder(refsPage.rows, rows), pageState: refsPage.pageState};
	}

	async listBySubscription(
		subscriptionId: string,
		page?: {
			pageSize: number;
			pageState?: string | null;
		},
	): Promise<PagedQueryResult<BillingInvoiceRow>> {
		const refsPage = await fetchPage<{
			provider_id: string;
		}>(
			FETCH_BY_SUBSCRIPTION_PARTITION,
			{subscription_id: subscriptionId},
			{pageSize: page?.pageSize ?? 50, pageState: page?.pageState ?? null},
		);
		if (refsPage.rows.length === 0) {
			return {rows: [], pageState: refsPage.pageState};
		}
		const ids = refsPage.rows.map((r) => r.provider_id);
		const rows = await fetchMany<BillingInvoiceRow>(FETCH_BY_PROVIDER_IDS, {provider_ids: ids});
		return {rows: restoreReferenceOrder(refsPage.rows, rows), pageState: refsPage.pageState};
	}

	async listByUser(
		userId: bigint,
		page?: {
			pageSize: number;
			pageState?: string | null;
		},
	): Promise<PagedQueryResult<BillingInvoiceRow>> {
		const pageSize = page?.pageSize ?? 50;
		const customerRefs = await fetchMany<{
			provider_id: string;
		}>(FETCH_CUSTOMERS_BY_USER, {user_id: userId});
		if (customerRefs.length === 0) {
			return {rows: [], pageState: null};
		}
		const customerIds = customerRefs.map((ref) => ref.provider_id).sort();
		const decoded = page?.pageState ? decodeUserInvoicePageState(page.pageState) : null;
		if (
			decoded &&
			decoded.partitions
				.map((partition) => partition.customerId)
				.sort()
				.join('\0') !== customerIds.join('\0')
		) {
			throw new Error('Billing invoice user page state does not match the current customer partitions');
		}
		const partitions: Array<UserInvoicePartitionState> =
			decoded?.partitions ??
			customerIds.map((customerId) => ({customerId, nextPageState: null, exhausted: false, buffer: []}));
		const fill = async (partition: UserInvoicePartitionState): Promise<void> => {
			if (partition.buffer.length > 0 || partition.exhausted) return;
			const refsPage = await fetchPage<InvoiceRef>(
				FETCH_BY_CUSTOMER_PARTITION,
				{customer_id: partition.customerId},
				{pageSize, pageState: partition.nextPageState},
			);
			partition.buffer = refsPage.rows.map((ref) => ({
				providerId: ref.provider_id,
				stripeCreatedAt: ref.stripe_created_at.toISOString(),
			}));
			partition.nextPageState = refsPage.pageState;
			partition.exhausted = refsPage.pageState === null;
		};
		await Promise.all(partitions.map(fill));
		const selected: Array<{provider_id: string}> = [];
		while (selected.length < pageSize) {
			let selectedPartition: UserInvoicePartitionState | null = null;
			for (const partition of partitions) {
				const candidate = partition.buffer[0];
				const current = selectedPartition?.buffer[0];
				if (
					candidate &&
					(!current ||
						candidate.stripeCreatedAt > current.stripeCreatedAt ||
						(candidate.stripeCreatedAt === current.stripeCreatedAt && candidate.providerId < current.providerId))
				) {
					selectedPartition = partition;
				}
			}
			if (!selectedPartition) break;
			const next = selectedPartition.buffer.shift()!;
			selected.push({provider_id: next.providerId});
			await fill(selectedPartition);
		}
		const ids = selected.map((ref) => ref.provider_id);
		const rows = ids.length === 0 ? [] : await fetchMany<BillingInvoiceRow>(FETCH_BY_PROVIDER_IDS, {provider_ids: ids});
		const hasMore = partitions.some((partition) => partition.buffer.length > 0 || !partition.exhausted);
		return {
			rows: restoreReferenceOrder(selected, rows),
			pageState: hasMore ? encodeUserInvoicePageState({version: 1, partitions}) : null,
		};
	}

	async upsertFromStripe(
		inv: Stripe.Invoice,
		hints?: {
			knownUserId?: bigint;
		},
	): Promise<{
		changed: boolean;
		row: BillingInvoiceRow;
	}> {
		const mapped = mapStripeInvoiceToRow(inv, hints);
		const existing = await this.findById(mapped.primary.provider_id);
		if (isExistingNewer(existing, mapped.primary)) {
			for (const p of mapped.payments) {
				await this.paymentsRepo.upsertFromStripeMapped(p);
			}
			return {changed: false, row: existing!};
		}
		if (existing && rowsEquivalent(existing, mapped.primary, ['mirrored_at', 'version'])) {
			for (const p of mapped.payments) {
				await this.paymentsRepo.upsertFromStripeMapped(p);
			}
			return {changed: false, row: existing};
		}
		const result = await executeBillingVersionedUpdate<BillingInvoiceRow, 'provider_id'>(
			async () => existing,
			(current) => ({
				pk: {provider_id: mapped.primary.provider_id},
				patch: buildPatchFromRow(mapped.primary, current, BILLING_INVOICE_COLUMNS, ['provider_id']),
			}),
			BillingInvoices,
			{initialData: existing},
		);
		await upsertOne(BillingInvoicesByCustomer.upsertAll(mapped.byCustomer));
		if (mapped.bySubscription) {
			await upsertOne(BillingInvoicesBySubscription.upsertAll(mapped.bySubscription));
		}
		for (const p of mapped.payments) {
			await this.paymentsRepo.upsertFromStripeMapped(p);
		}
		return {changed: true, row: {...mapped.primary, version: result.finalVersion}};
	}
}
