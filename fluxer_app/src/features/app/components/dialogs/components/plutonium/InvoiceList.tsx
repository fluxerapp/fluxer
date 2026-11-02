// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/dialogs/components/plutonium/InvoiceList.module.css';
import {downloadBillingDocument} from '@app/features/premium/commands/BillingDocumentCommands';
import PremiumState from '@app/features/premium/state/PremiumState';
import {Button} from '@app/features/ui/button/Button';
import {getCurrentLocale} from '@app/features/user/utils/LocaleUtils';
import {getFormattedShortDate} from '@fluxer/date_utils/src/DateFormatting';
import {formatMinorUnitPrice} from '@fluxer/number_utils/src/PriceFormatting';
import type {BillingDocumentResponse} from '@fluxer/schema/src/domains/billing_documents/BillingDocumentSchemas';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const OPEN_RECEIPT_LABEL_DESCRIPTOR = msg({
	message: 'Open receipt {invoiceLabel}',
	comment:
		'Accessible label of the button that opens one receipt in the Payments settings. {invoiceLabel} is the receipt number, keep it as is.',
});

const AMOUNT_WITH_CURRENCY_DESCRIPTOR = msg({
	message: '{amount} ({currency})',
	comment:
		'Amount of an invoice in the billing history, followed by its three-letter currency code so the receipt always names the currency, for example "9,99 kr (SEK)". {amount} is a price already formatted by code and {currency} is an ISO currency code such as SEK or USD. Keep both placeholders as they are and use the brackets your language normally uses.',
});

const SHARED_CURRENCY_SYMBOLS = new Set(['$', 'kr', 'kr.', 'Kr', '¥', 'R', 'Rs', 'fr', 'Fr', 'zł']);

function formatDocumentAmount(i18n: I18n, document: BillingDocumentResponse): string {
	const amount = formatMinorUnitPrice(
		document.total,
		document.currency,
		getCurrentLocale(),
		PremiumState.state?.pricing.country_code,
	);
	if (!amount) return '';
	const currency = document.currency.toUpperCase();
	const symbol = amount.replace(/[\d\s.,'  −-]/g, '');
	if (!SHARED_CURRENCY_SYMBOLS.has(symbol)) return amount;
	return i18n._(AMOUNT_WITH_CURRENCY_DESCRIPTOR, {amount, currency});
}

interface InvoiceListProps {
	documents: ReadonlyArray<BillingDocumentResponse>;
}

export const InvoiceList: React.FC<InvoiceListProps> = observer(({documents}) => {
	const {i18n} = useLingui();
	return (
		<div className={styles.list} data-flx="app.plutonium.invoice-list.list">
			<div className={styles.header} data-flx="app.plutonium.invoice-list.header">
				<span data-flx="app.plutonium.invoice-list.span">
					<Trans comment="Column header for invoice payment or creation date.">Date</Trans>
				</span>
				<span data-flx="app.plutonium.invoice-list.span--2">
					<Trans comment="Column header for invoice amount.">Amount</Trans>
				</span>
				<span aria-hidden="true" data-flx="app.plutonium.invoice-list.span--3" />
				<span data-flx="app.plutonium.invoice-list.span--4">
					<Trans comment="Column header in the Payments settings above the buttons that open each receipt.">
						Receipt
					</Trans>
				</span>
			</div>
			{documents.map((document) => (
				<div className={styles.row} key={document.id} data-flx="app.plutonium.invoice-list.row">
					<span className={styles.date} data-flx="app.plutonium.invoice-list.date">
						{getFormattedShortDate(new Date(document.issued_at), getCurrentLocale())}
					</span>
					<span className={styles.amount} data-flx="app.plutonium.invoice-list.amount">
						{formatDocumentAmount(i18n, document)}
					</span>
					<span data-flx="app.plutonium.invoice-list.span--5">{document.number}</span>
					<Button
						variant="secondary"
						small
						aria-label={i18n._(OPEN_RECEIPT_LABEL_DESCRIPTOR, {invoiceLabel: document.number})}
						onClick={() => void downloadBillingDocument(document)}
						data-flx="app.plutonium.invoice-list.button"
					>
						<Trans comment="Button in the Payments settings that opens one receipt as a PDF.">Open receipt</Trans>
					</Button>
				</div>
			))}
		</div>
	);
});
