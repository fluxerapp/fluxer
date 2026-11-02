// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/premium/components/operator_pass/checkout/CheckoutStage.module.css';
import {Button} from '@app/features/ui/button/Button';
import {Spinner} from '@app/features/ui/components/Spinner';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';

const WAITING_BODY_DESCRIPTOR = msg({
	message: 'We opened the payment page in your browser. This window updates once your payment goes through.',
	comment:
		'Text in the Operator Pass checkout dialog while the app waits for the payment. The payment page opened in the web browser, and the dialog changes by itself when the payment is done. This window means the checkout dialog in the app.',
});
const WAITING_STATUS_DESCRIPTOR = msg({
	message: 'Waiting for your payment',
	comment:
		'Short status next to a loading indicator in the Operator Pass checkout dialog while the app waits for the payment to go through.',
});
const OPEN_PAYMENT_PAGE_DESCRIPTOR = msg({
	message: 'Open payment page again',
	comment:
		'Button in the Operator Pass checkout dialog while the app waits for the payment. It opens the same payment page again in the web browser.',
});
const START_OVER_DESCRIPTOR = msg({
	message: 'Start over',
	comment:
		'Button in the Operator Pass checkout dialog while the app waits for the payment. It goes back to the step with the checkboxes so the person can start a new payment page. Nothing was charged on the old page unless they paid there.',
});

interface CheckoutWaitingStageProps {
	canReopen: boolean;
	onReopen: () => void;
	onStartOver: () => void;
}

export function CheckoutWaitingStage({canReopen, onReopen, onStartOver}: CheckoutWaitingStageProps) {
	const {i18n} = useLingui();
	return (
		<div className={styles.stage} data-flx="premium.operator-pass-checkout.waiting">
			<p className={styles.lead} data-flx="premium.operator-pass-checkout.waiting-body">
				{i18n._(WAITING_BODY_DESCRIPTOR)}
			</p>
			<div className={styles.waitingRow} role="status" data-flx="premium.operator-pass-checkout.waiting-status">
				<span aria-hidden="true" data-flx="premium.operator-pass-checkout.spinner-wrapper">
					<Spinner size="small" data-flx="premium.operator-pass-checkout.spinner" />
				</span>
				<span className={styles.note} data-flx="premium.operator-pass-checkout.waiting-label">
					{i18n._(WAITING_STATUS_DESCRIPTOR)}
				</span>
			</div>
			<div className={styles.actions} data-flx="premium.operator-pass-checkout.waiting-actions">
				{canReopen && (
					<Button variant="secondary" onClick={onReopen} data-flx="premium.operator-pass-checkout.button.reopen">
						{i18n._(OPEN_PAYMENT_PAGE_DESCRIPTOR)}
					</Button>
				)}
				<Button variant="ghost" onClick={onStartOver} data-flx="premium.operator-pass-checkout.button.start-over">
					{i18n._(START_OVER_DESCRIPTOR)}
				</Button>
			</div>
		</div>
	);
}
