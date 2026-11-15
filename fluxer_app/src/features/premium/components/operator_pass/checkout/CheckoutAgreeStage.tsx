// SPDX-License-Identifier: AGPL-3.0-or-later

import {PurchaseDisclaimer} from '@app/features/app/components/dialogs/components/PurchaseDisclaimer';
import styles from '@app/features/premium/components/operator_pass/checkout/CheckoutStage.module.css';
import {
	AGE_BOX_ID,
	DELIVERY_BOX_ID,
	type OperatorPassCheckoutDraft,
} from '@app/features/premium/components/operator_pass/checkout/OperatorPassCheckoutDraft';
import {OperatorPassPriceBlock} from '@app/features/premium/components/operator_pass/checkout/OperatorPassPriceBlock';
import {OperatorPassConsentBox} from '@app/features/premium/components/operator_pass/OperatorPassConsentBox';
import {OPERATOR_PASS_AGE_CONFIRMATION_DESCRIPTOR} from '@app/features/premium/utils/PremiumMessageDescriptors';
import type {OperatorPassStateResponse} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type {ReactNode} from 'react';

const DELIVERY_UNTICKED_DESCRIPTOR = msg({
	message: 'The number of passes changed, so check the delivery box again.',
	comment:
		'Notice shown under the delivery checkbox in the Operator Pass checkout dialog. The buyer had ticked the required box that asks for the certificate to be delivered right away, then changed how many passes to buy, which unchecks that box because its wording changes. Check means tick the checkbox. The delivery box is the checkbox that starts with Deliver my certificate now.',
});
const DELIVERY_WAIVER_DESCRIPTOR = msg({
	message:
		"Deliver my certificate now, before the 14-day withdrawal period ends. I understand that I lose my right to withdraw once it's delivered.",
	comment:
		'Required checkbox in the Operator Pass checkout dialog when buying one pass. The buyer asks for the certificate PDF to be delivered right after payment, before the statutory withdrawal period ends, and confirms that the right of withdrawal is lost on delivery. Consumer law text, translate faithfully and keep every condition. Withdraw means the consumer right of withdrawal, never taking money out.',
});
const DELIVERY_WAIVER_MULTIPLE_DESCRIPTOR = msg({
	message:
		"Deliver my certificates now, before the 14-day withdrawal period ends. I understand that I lose my right to withdraw once they're delivered.",
	comment:
		'Required checkbox in the Operator Pass checkout dialog when buying more than one pass. The buyer asks for the certificate PDF pages to be delivered right after payment, before the statutory withdrawal period ends, and confirms that the right of withdrawal is lost on delivery. Consumer law text, translate faithfully and keep every condition. Withdraw means the consumer right of withdrawal, never taking money out.',
});
const DELIVERY_COUNTRY_DESCRIPTOR = msg({
	message:
		'Deliver my certificate now. If I have a billing address in Brazil or pay in reais, I can still withdraw within {days, plural, one {# day} other {# days}} of delivery and get a full refund.',
	comment:
		'Required checkbox in the Operator Pass checkout dialog when buying one pass from Brazil or in Brazilian reais. The buyer asks for the certificate PDF to be delivered right after payment. A buyer who has a billing address in Brazil or pays in reais (the Brazilian currency) keeps the right to withdraw for {days} days after delivery. A buyer who enters the CNPJ of a company sees a different text, so this one never needs a business exception. The billing address is not where the buyer happens to be. Nothing in this text takes the right away from anyone, so never add a sentence about losing it. Consumer law text, translate faithfully and keep every condition. Withdraw means the consumer right of withdrawal (pt-BR: desistir), never taking money out.',
});
const DELIVERY_COUNTRY_MULTIPLE_DESCRIPTOR = msg({
	message:
		'Deliver my certificates now. If I have a billing address in Brazil or pay in reais, I can still withdraw from each pass within {days, plural, one {# day} other {# days}} of delivery and get a full refund.',
	comment:
		'Required checkbox in the Operator Pass checkout dialog when buying more than one pass from Brazil or in Brazilian reais. The buyer asks for the certificate PDF pages to be delivered right after payment. A buyer who has a billing address in Brazil or pays in reais (the Brazilian currency) keeps the right to withdraw from each pass for {days} days after delivery. A buyer who enters the CNPJ of a company sees a different text, so this one never needs a business exception. The billing address is not where the buyer happens to be. Nothing in this text takes the right away from anyone, so never add a sentence about losing it. Consumer law text, translate faithfully and keep every condition. Withdraw means the consumer right of withdrawal (pt-BR: desistir), never taking money out.',
});
const DELIVERY_BUSINESS_DESCRIPTOR = msg({
	message:
		"Deliver my certificate now. I'm buying for the company with the CNPJ I entered, and I understand that this purchase has no right of withdrawal, unless Brazilian law treats the company as a consumer.",
	comment:
		'Required checkbox in the Operator Pass checkout dialog when buying one pass from Brazil or in Brazilian reais for a company whose CNPJ the buyer entered. The buyer asks for the certificate PDF to be delivered right after payment and confirms that a purchase for a company with a CNPJ has no right of withdrawal under the Brazilian Consumer Defense Code, unless Brazilian law treats the company as a consumer. CNPJ is the Brazilian company registration number, keep it as is. Consumer law text, translate faithfully and keep every condition. Withdrawal means the consumer right of withdrawal (pt-BR: direito de arrependimento), never taking money out.',
});
const DELIVERY_BUSINESS_MULTIPLE_DESCRIPTOR = msg({
	message:
		"Deliver my certificates now. I'm buying for the company with the CNPJ I entered, and I understand that this purchase has no right of withdrawal, unless Brazilian law treats the company as a consumer.",
	comment:
		'Required checkbox in the Operator Pass checkout dialog when buying more than one pass from Brazil or in Brazilian reais for a company whose CNPJ the buyer entered. The buyer asks for the certificate PDF pages to be delivered right after payment and confirms that a purchase for a company with a CNPJ has no right of withdrawal under the Brazilian Consumer Defense Code, unless Brazilian law treats the company as a consumer. CNPJ is the Brazilian company registration number, keep it as is. Consumer law text, translate faithfully and keep every condition. Withdrawal means the consumer right of withdrawal (pt-BR: direito de arrependimento), never taking money out.',
});
const WITHDRAWAL_NOTICE_DESCRIPTOR = msg({
	message: "Once we deliver your certificate, which happens right after you pay, you can't withdraw from the purchase.",
	comment:
		'Line in the Operator Pass checkout dialog right above the button that opens the payment page. It tells the buyer before paying that the consumer right of withdrawal ends when the certificate is delivered. Withdraw means the consumer right of withdrawal, never taking money out.',
});
const WITHDRAWAL_COUNTRY_NOTICE_DESCRIPTOR = msg({
	message:
		'If you have a billing address in Brazil or pay in reais, you can withdraw within {days, plural, one {# day} other {# days}} of delivery and get a full refund.',
	comment:
		'Line in the Operator Pass checkout dialog right above the button that opens the payment page, shown to buyers who appear to be in Brazil or who pay in Brazilian reais. A buyer who has a billing address in Brazil or pays in reais (the Brazilian currency) keeps the right to withdraw for {days} days after delivery. A buyer who enters the CNPJ of a company sees a different text, so this one never needs a business exception. The billing address is not where the buyer happens to be. Nothing in this text takes the right away from anyone, so never add a sentence about losing it. Consumer law text, translate faithfully and keep every condition. Withdraw means the consumer right of withdrawal (pt-BR: desistir), never taking money out.',
});
const WITHDRAWAL_NOTICE_MULTIPLE_DESCRIPTOR = msg({
	message:
		"Once we deliver your certificates, which happens right after you pay, you can't withdraw from the purchase.",
	comment:
		'Line in the Operator Pass checkout dialog right above the button that opens the payment page, when buying more than one pass. It tells the buyer before paying that the consumer right of withdrawal ends when the certificate PDF pages are delivered. Use the same word for certificates as in the checkbox that starts with "Deliver my certificates now". Withdraw means the consumer right of withdrawal, never taking money out.',
});
const WITHDRAWAL_COUNTRY_NOTICE_MULTIPLE_DESCRIPTOR = msg({
	message:
		'If you have a billing address in Brazil or pay in reais, you can withdraw from each pass within {days, plural, one {# day} other {# days}} of delivery and get a full refund.',
	comment:
		'Line in the Operator Pass checkout dialog right above the button that opens the payment page, shown to buyers who appear to be in Brazil or who pay in Brazilian reais when buying more than one pass. A buyer who has a billing address in Brazil or pays in reais (the Brazilian currency) keeps the right to withdraw from each pass for {days} days after delivery. A buyer who enters the CNPJ of a company sees a different text, so this one never needs a business exception. The billing address is not where the buyer happens to be. Nothing in this text takes the right away from anyone, so never add a sentence about losing it. Consumer law text, translate faithfully and keep every condition. Withdraw means the consumer right of withdrawal (pt-BR: desistir), never taking money out.',
});

interface CheckoutAgreeStageProps {
	draft: OperatorPassCheckoutDraft;
	state: OperatorPassStateResponse;
	businessBox: boolean;
	submitting: boolean;
	notice: ReactNode;
	formatPrice: (amountMinor: number, currency: string) => string;
}

export const CheckoutAgreeStage = observer(function CheckoutAgreeStage({
	draft,
	state,
	businessBox,
	submitting,
	notice,
	formatPrice,
}: CheckoutAgreeStageProps) {
	const {i18n} = useLingui();
	const multiple = draft.quantity > 1;
	const rule = state.withdrawal_rule;
	const deliveryText = (() => {
		if (businessBox) return i18n._(multiple ? DELIVERY_BUSINESS_MULTIPLE_DESCRIPTOR : DELIVERY_BUSINESS_DESCRIPTOR);
		if (rule) {
			return i18n._(multiple ? DELIVERY_COUNTRY_MULTIPLE_DESCRIPTOR : DELIVERY_COUNTRY_DESCRIPTOR, {days: rule.days});
		}
		return i18n._(multiple ? DELIVERY_WAIVER_MULTIPLE_DESCRIPTOR : DELIVERY_WAIVER_DESCRIPTOR);
	})();
	const withdrawalText =
		rule && !businessBox
			? i18n._(multiple ? WITHDRAWAL_COUNTRY_NOTICE_MULTIPLE_DESCRIPTOR : WITHDRAWAL_COUNTRY_NOTICE_DESCRIPTOR, {
					days: rule.days,
				})
			: i18n._(multiple ? WITHDRAWAL_NOTICE_MULTIPLE_DESCRIPTOR : WITHDRAWAL_NOTICE_DESCRIPTOR);
	return (
		<div className={styles.stage} data-flx="premium.operator-pass-checkout.agree">
			{notice}
			<OperatorPassPriceBlock
				state={state}
				count={draft.quantity}
				formatPrice={formatPrice}
				data-flx="premium.operator-pass.checkout.checkout-agree-stage.operator-pass-price-block"
			/>
			<OperatorPassConsentBox
				id={DELIVERY_BOX_ID}
				checked={draft.delivery}
				missing={draft.deliveryMissing}
				disabled={submitting}
				status={draft.deliveryUnticked ? i18n._(DELIVERY_UNTICKED_DESCRIPTOR) : null}
				onChange={draft.setDelivery}
				data-flx="premium.operator-pass-checkout.delivery"
			>
				{deliveryText}
			</OperatorPassConsentBox>
			<OperatorPassConsentBox
				id={AGE_BOX_ID}
				checked={draft.ageConfirmed}
				missing={draft.ageMissing}
				disabled={submitting}
				onChange={draft.setAgeConfirmed}
				data-flx="premium.operator-pass-checkout.age"
			>
				{i18n._(OPERATOR_PASS_AGE_CONFIRMATION_DESCRIPTOR)}
			</OperatorPassConsentBox>
			<p className={styles.note} data-flx="premium.operator-pass-checkout.withdrawal-notice">
				{withdrawalText}
			</p>
			<PurchaseDisclaimer align="left" operatorPass data-flx="premium.operator-pass-checkout.terms-line" />
		</div>
	);
});
