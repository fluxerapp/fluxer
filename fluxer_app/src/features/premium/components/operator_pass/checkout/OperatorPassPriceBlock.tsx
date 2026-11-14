// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/premium/components/operator_pass/checkout/CheckoutStage.module.css';
import {
	formatOperatorPassDateTime,
	formatOperatorPassEarlyBirdEnd,
} from '@app/features/premium/utils/OperatorPassUtils';
import type {OperatorPassStateResponse} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';

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
	message: 'This early bird price is for purchases until {date}.',
	comment:
		'Note under the early bird Operator Pass price, saying that this lower price applies to purchases made until the given day. It must never read as if a price already paid will change later. {date} is a date such as January 1, 2027.',
});
const EARLY_BIRD_UNTIL_THEN_DESCRIPTOR = msg({
	message: 'This early bird price is for purchases until {date}. After that, a pass costs {standardPrice}.',
	comment:
		'Note under the early bird Operator Pass price. The first sentence says this lower price applies to purchases made until the given day. The second says what one pass costs when bought after that day. It must never read as if a price already paid will change later or as a future charge. {date} is a date such as January 1, 2027, and {standardPrice} is the price of one pass with currency.',
});

interface OperatorPassPriceBlockProps {
	state: OperatorPassStateResponse;
	count: number;
	formatPrice: (amountMinor: number, currency: string) => string;
}

export function OperatorPassPriceBlock({state, count, formatPrice}: OperatorPassPriceBlockProps) {
	const {i18n} = useLingui();
	const priceRisesAt = state.phase !== 'standard' ? state.price_rises_at : null;
	return (
		<div className={styles.price} data-flx="premium.operator-pass-checkout.price">
			<span className={styles.total} data-flx="premium.operator-pass-checkout.total">
				{formatPrice(state.price_minor * count, state.currency)}
			</span>
			<span className={styles.note} data-flx="premium.operator-pass-checkout.price-note">
				{count > 1
					? i18n._(PER_PASS_DESCRIPTOR, {count, price: formatPrice(state.price_minor, state.currency)})
					: i18n._(SINGLE_PASS_PRICE_DESCRIPTOR)}
			</span>
			{priceRisesAt && (
				<span
					className={styles.note}
					title={formatOperatorPassDateTime(priceRisesAt)}
					data-flx="premium.operator-pass-checkout.early-bird-ends"
				>
					{state.standard_price_minor != null
						? i18n._(EARLY_BIRD_UNTIL_THEN_DESCRIPTOR, {
								date: formatOperatorPassEarlyBirdEnd(priceRisesAt),
								standardPrice: formatPrice(state.standard_price_minor, state.currency),
							})
						: i18n._(EARLY_BIRD_UNTIL_DESCRIPTOR, {date: formatOperatorPassEarlyBirdEnd(priceRisesAt)})}
				</span>
			)}
		</div>
	);
}
