// SPDX-License-Identifier: AGPL-3.0-or-later

import type {SearchableSettingDescriptor} from '@app/features/user/components/settings_utils/search_index/SearchIndexTypes';
import {msg} from '@lingui/core/macro';

const PAYMENTS_DESCRIPTOR = msg({
	message: 'Payments',
	comment:
		'Settings search entry label for the Payments settings tab, which holds the payment methods on file, refunds and receipts.',
});
const PAYMENT_HISTORY_DESCRIPTOR = msg({
	message: 'Payment history',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PURCHASE_HISTORY_DESCRIPTOR = msg({
	message: 'Purchase history',
	comment: 'Settings search entry label. Also used as a search synonym in the settings search bar.',
});
const BILLING_HISTORY_DESCRIPTOR = msg({
	message: 'Billing history',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const INVOICE_DESCRIPTOR = msg({
	message: 'Invoice',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const RECEIPT_DESCRIPTOR = msg({
	message: 'Receipt',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const REFUND_DESCRIPTOR = msg({
	message: 'Refund',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const REVIEW_PAST_PURCHASES_AND_REFUND_OPTIONS_DESCRIPTOR = msg({
	message: 'Review past purchases and refund options',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});

export const paymentsIndex: Array<SearchableSettingDescriptor> = [
	{
		id: 'payments',
		tabType: 'payments',
		label: PAYMENTS_DESCRIPTOR,
		keywords: [
			PAYMENT_HISTORY_DESCRIPTOR,
			PURCHASE_HISTORY_DESCRIPTOR,
			BILLING_HISTORY_DESCRIPTOR,
			INVOICE_DESCRIPTOR,
			RECEIPT_DESCRIPTOR,
			REFUND_DESCRIPTOR,
		],
		description: REVIEW_PAST_PURCHASES_AND_REFUND_OPTIONS_DESCRIPTOR,
	},
];
