// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/premium/components/operator_pass/checkout/CheckoutStage.module.css';
import type {OperatorPassPurchaseResponse} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';

const EXPIRED_DESCRIPTOR = msg({
	message: 'This payment page has expired, so nothing was charged.',
	comment:
		'Text in the Operator Pass checkout dialog when the payment page timed out before anyone paid. A Start again button follows it.',
});

const EMAILED_REFUSALS: ReadonlySet<OperatorPassPurchaseResponse['refusal_reason']> = new Set(['names_missing']);

interface CheckoutOutcomeStageProps {
	result: 'refunded' | 'unavailable';
	purchase: OperatorPassPurchaseResponse | null;
}

function RefundedLine({purchase}: {purchase: OperatorPassPurchaseResponse}) {
	if (purchase.delivered_at != null) {
		return (
			<Trans comment="Line after an Operator Pass payment that we refunded in full after the certificate was already delivered, for example after a withdrawal. The buyer also gets an email that explains why, which can go out some time after this page shows, so keep it in the future tense.">
				We refunded this payment in full. We'll email you the details.
			</Trans>
		);
	}
	if (purchase.refusal_reason === 'email_undeliverable') {
		return (
			<Trans comment="Line after an Operator Pass payment that we refunded right away because emails to the address on the buyer's account bounce, so we could not send the purchase confirmation. The certificate is the numbered Operator certificate PDF that the price pays for. Settings means the user settings, where the email address is changed. No email follows, so never say we'll email.">
				We couldn't email the address on your account, so we refunded this payment in full and didn't create a
				certificate. Update your email in settings and you can buy again.
			</Trans>
		);
	}
	if (purchase.status === 'duplicate_refunded' && !EMAILED_REFUSALS.has(purchase.refusal_reason)) {
		return (
			<Trans comment="Line after an Operator Pass payment that we refunded right away without delivering it, in a case where no email about it follows. The certificate is the numbered Operator certificate PDF that the price pays for. Never say we'll email.">
				We refunded this payment in full and didn't create a certificate.
			</Trans>
		);
	}
	return (
		<Trans comment="Line after an Operator Pass payment that we refunded right away, for example because sales to consumers in the buyer's country are paused, the certificate names were missing, or the buyer withdrew before delivery. The certificate is the numbered Operator certificate PDF that the price pays for. The buyer also gets an email that explains why, which can go out some time after this page shows, so keep it in the future tense.">
			We refunded this payment in full and didn't create a certificate. We'll email you the details.
		</Trans>
	);
}

export function CheckoutOutcomeStage({result, purchase}: CheckoutOutcomeStageProps) {
	const {i18n} = useLingui();
	return (
		<div className={styles.stage} data-flx="premium.operator-pass-checkout.outcome">
			<p className={styles.lead} aria-live="polite" data-flx="premium.operator-pass-checkout.outcome-body">
				{result === 'refunded' && purchase ? (
					<RefundedLine
						purchase={purchase}
						data-flx="premium.operator-pass.checkout.checkout-outcome-stage.refunded-line"
					/>
				) : (
					i18n._(EXPIRED_DESCRIPTOR)
				)}
			</p>
		</div>
	);
}
