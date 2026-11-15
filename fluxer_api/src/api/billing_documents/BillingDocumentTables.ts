// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineTable} from '@app/api/database/CassandraTableDsl';
import {
	BILLING_DOCUMENT_BY_MONTH_COLUMNS,
	BILLING_DOCUMENT_BY_SOURCE_COLUMNS,
	BILLING_DOCUMENT_BY_USER_COLUMNS,
	BILLING_DOCUMENT_COLUMNS,
	BILLING_DOCUMENT_NUMBER_COLUMNS,
	type BillingDocumentByMonthRow,
	type BillingDocumentBySourceRow,
	type BillingDocumentByUserRow,
	type BillingDocumentNumberRow,
	type BillingDocumentRow,
} from '@app/api/database/types/BillingDocumentTypes';

export const BillingDocumentNumbers = defineTable<BillingDocumentNumberRow, 'series' | 'number', 'series'>({
	name: 'billing_document_numbers',
	columns: BILLING_DOCUMENT_NUMBER_COLUMNS,
	primaryKey: ['series', 'number'],
	partitionKey: ['series'],
});

export const BillingDocuments = defineTable<BillingDocumentRow, 'document_id'>({
	name: 'billing_documents',
	columns: BILLING_DOCUMENT_COLUMNS,
	primaryKey: ['document_id'],
});

export const BillingDocumentsBySource = defineTable<BillingDocumentBySourceRow, 'source_key'>({
	name: 'billing_documents_by_source',
	columns: BILLING_DOCUMENT_BY_SOURCE_COLUMNS,
	primaryKey: ['source_key'],
});

export const BillingDocumentsByUser = defineTable<
	BillingDocumentByUserRow,
	'user_id' | 'issued_at' | 'document_id',
	'user_id'
>({
	name: 'billing_documents_by_user',
	columns: BILLING_DOCUMENT_BY_USER_COLUMNS,
	primaryKey: ['user_id', 'issued_at', 'document_id'],
	partitionKey: ['user_id'],
});

export const BillingDocumentsByMonth = defineTable<BillingDocumentByMonthRow, 'month' | 'document_id', 'month'>({
	name: 'billing_documents_by_month',
	columns: BILLING_DOCUMENT_BY_MONTH_COLUMNS,
	primaryKey: ['month', 'document_id'],
	partitionKey: ['month'],
});
