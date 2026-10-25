// SPDX-License-Identifier: AGPL-3.0-or-later

import {OPERATOR_PASS_NAME} from '@app/features/app/config/I18nDisplayConstants';
import styles from '@app/features/premium/components/modals/OperatorPassCheckoutModal.module.css';
import {
	formatOperatorPassDateTime,
	formatOperatorPassEarlyBirdEnd,
	formatOperatorPassLongDate,
	formatOperatorPassSinceDate,
} from '@app/features/premium/utils/OperatorPassUtils';
import type {OperatorPassStateResponse} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {TicketIcon} from '@phosphor-icons/react';

interface CheckoutPriceOptions {
	current: OperatorPassStateResponse;
	count: number;
	i18n: I18n;
	formatPrice: (amountMinor: number, currency: string) => string;
}

interface OperatorCardOptions {
	current: OperatorPassStateResponse;
	i18n: I18n;
}

const EARLY_BIRD_PRICE_DESCRIPTOR = msg({
	message: 'Early bird price',
	comment: 'Label above the Operator Pass price while the early bird price is active.',
});
const PRICE_DESCRIPTOR = msg({
	message: 'Price',
	comment: 'Label above the Operator Pass price once the early bird price has ended.',
});
const PER_PASS_DESCRIPTOR = msg({
	message: '{count} × {price}, including tax, paid once.',
	comment:
		'Note under the Operator Pass total when buying more than one pass. {count} is the number of passes, 2 or more, and {price} the price of one pass with currency, including tax. The passes are paid once and never renew.',
});
const SINGLE_PASS_PRICE_DESCRIPTOR = msg({
	message: 'Including tax, paid once.',
	comment:
		'Note under the Operator Pass price when buying one pass. The price includes tax and is paid once, it never renews.',
});
const EARLY_BIRD_UNTIL_DESCRIPTOR = msg({
	message: 'Early bird price until {date}.',
	comment:
		'Note under the early bird Operator Pass price, saying the last day of the early bird price. {date} is a date such as January 1, 2027.',
});
const ON_SALE_FROM_DESCRIPTOR = msg({
	message: 'On sale from {saleOpensAt}.',
	comment:
		'Note in the Operator Pass dialog before the pass can be bought. {saleOpensAt} is a date without a time, such as November 1, 2026.',
});
const OPERATOR_TITLE_DESCRIPTOR = msg({
	message: "You're an Operator",
	comment:
		'Heading in the Operator Pass dialog when the account already has an Operator Pass. Operator is the badge and role name.',
});
const OPERATOR_SINCE_DESCRIPTOR = msg({
	message: '{operatorPassName} since {sinceDate}',
	comment:
		'Line in the Operator Pass dialog for someone who already has an Operator Pass. {sinceDate} is a date such as November 14, 2026. Operator Pass is a product name and stays untranslated.',
});
const PASS_COUNT_DESCRIPTOR = msg({
	message: '{count, plural, one {# Operator Pass} other {# Operator Passes}}',
	comment:
		'How many Operator Passes the account has, in the Operator Pass dialog, after the line that says since when. Operator Pass is a product name and stays untranslated.',
});

export function renderOperatorPassPriceBlock({current, count, i18n, formatPrice}: CheckoutPriceOptions) {
	const earlyBird = current.phase !== 'standard';
	const priceRisesAt = current.price_rises_at;
	return (
		<div className={styles.priceBlock} data-flx="premium.operator-pass-checkout-modal.price-block">
			<span className={styles.priceLabel} data-flx="premium.operator-pass-checkout-modal.price-label">
				{earlyBird ? i18n._(EARLY_BIRD_PRICE_DESCRIPTOR) : i18n._(PRICE_DESCRIPTOR)}
			</span>
			<span className={styles.price} data-flx="premium.operator-pass-checkout-modal.price">
				{formatPrice(current.price_minor * count, current.currency)}
			</span>
			<span className={styles.priceNote} data-flx="premium.operator-pass-checkout-modal.price-note">
				{count > 1
					? i18n._(PER_PASS_DESCRIPTOR, {count, price: formatPrice(current.price_minor, current.currency)})
					: i18n._(SINGLE_PASS_PRICE_DESCRIPTOR)}
			</span>
			{current.phase === 'waitlist' && current.sale_opens_at && (
				<span
					className={styles.priceNote}
					title={formatOperatorPassDateTime(current.sale_opens_at)}
					data-flx="premium.operator-pass-checkout-modal.on-sale-from"
				>
					{i18n._(ON_SALE_FROM_DESCRIPTOR, {saleOpensAt: formatOperatorPassLongDate(current.sale_opens_at)})}
				</span>
			)}
			{earlyBird && priceRisesAt && (
				<span
					className={styles.priceNote}
					title={formatOperatorPassDateTime(priceRisesAt)}
					data-flx="premium.operator-pass-checkout-modal.early-bird-ends"
				>
					{i18n._(EARLY_BIRD_UNTIL_DESCRIPTOR, {date: formatOperatorPassEarlyBirdEnd(priceRisesAt)})}
				</span>
			)}
		</div>
	);
}

export function renderOperatorPassHolderCard({current, i18n}: OperatorCardOptions) {
	if (!current.holder) return null;
	const since = i18n._(OPERATOR_SINCE_DESCRIPTOR, {
		operatorPassName: OPERATOR_PASS_NAME,
		sinceDate: formatOperatorPassSinceDate(current.holder.since),
	});
	const count = current.holder.count > 1 ? i18n._(PASS_COUNT_DESCRIPTOR, {count: current.holder.count}) : null;
	return (
		<div className={styles.holder} data-flx="premium.operator-pass-checkout-modal.operator">
			<TicketIcon
				className={styles.holderIcon}
				weight="fill"
				aria-hidden="true"
				data-flx="premium.operator-pass-checkout-modal.operator-icon"
			/>
			<div className={styles.holderText} data-flx="premium.operator-pass-checkout-modal.operator-text">
				<h3 className={styles.holderTitle} data-flx="premium.operator-pass-checkout-modal.operator-title">
					{i18n._(OPERATOR_TITLE_DESCRIPTOR)}
				</h3>
				<span className={styles.priceNote} data-flx="premium.operator-pass-checkout-modal.operator-since">
					{since}
				</span>
				{count && (
					<span className={styles.priceNote} data-flx="premium.operator-pass-checkout-modal.operator-count">
						{count}
					</span>
				)}
			</div>
		</div>
	);
}
