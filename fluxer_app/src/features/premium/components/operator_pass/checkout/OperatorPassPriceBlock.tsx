// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/premium/components/operator_pass/checkout/CheckoutStage.module.css';
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

interface OperatorPassPriceBlockProps {
	state: OperatorPassStateResponse;
	count: number;
	formatPrice: (amountMinor: number, currency: string) => string;
}

export function OperatorPassPriceBlock({state, count, formatPrice}: OperatorPassPriceBlockProps) {
	const {i18n} = useLingui();
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
		</div>
	);
}
