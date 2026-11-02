// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/premium/components/operator_pass/checkout/CheckoutStage.module.css';
import {
	CERTIFICATE_NAME_FIELD_ID,
	type OperatorPassCheckoutDraft,
} from '@app/features/premium/components/operator_pass/checkout/OperatorPassCheckoutDraft';
import {OperatorPassCertificateNamesField} from '@app/features/premium/components/operator_pass/OperatorPassCertificateNamesField';
import {clampOperatorPassQuantity} from '@app/features/premium/utils/OperatorPassUtils';
import {Button} from '@app/features/ui/button/Button';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {MinusIcon, PlusIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type {ReactNode} from 'react';
import {useRef} from 'react';

const QUANTITY_LABEL_DESCRIPTOR = msg({
	message: 'Number of passes',
	comment: 'Label of the control that sets how many Operator Passes to buy. All of them go to your own account.',
});
const QUANTITY_CAP_DESCRIPTOR = msg({
	message: 'Up to {max} per purchase.',
	comment:
		'Note under the Operator Pass quantity selector. {max} is the highest number of passes one purchase allows, such as 25.',
});
const DECREASE_QUANTITY_DESCRIPTOR = msg({
	message: 'One pass fewer',
	comment: 'Accessible label of the button that lowers the number of Operator Passes to buy by one.',
});
const INCREASE_QUANTITY_DESCRIPTOR = msg({
	message: 'One pass more',
	comment: 'Accessible label of the button that raises the number of Operator Passes to buy by one.',
});

const QUANTITY_LABEL_ID = 'operator-pass-checkout-quantity-label';

interface CheckoutChooseStageProps {
	draft: OperatorPassCheckoutDraft;
	maxQuantity: number;
	notice: ReactNode;
}

export const CheckoutChooseStage = observer(function CheckoutChooseStage({
	draft,
	maxQuantity,
	notice,
}: CheckoutChooseStageProps) {
	const {i18n} = useLingui();
	const decreaseRef = useRef<HTMLButtonElement | null>(null);
	const increaseRef = useRef<HTMLButtonElement | null>(null);
	const quantity = draft.quantity;
	const changeQuantity = (next: number) => {
		const clamped = clampOperatorPassQuantity(next, maxQuantity);
		if (clamped === quantity) return;
		draft.setQuantity(clamped);
		if (clamped <= 1) increaseRef.current?.focus();
		else if (clamped >= maxQuantity) decreaseRef.current?.focus();
	};
	return (
		<div className={styles.stage} data-flx="premium.operator-pass-checkout.choose">
			{notice}
			<div className={styles.quantity} data-flx="premium.operator-pass-checkout.quantity">
				<div className={styles.quantityRow} data-flx="premium.operator-pass-checkout.quantity-row">
					<span
						id={QUANTITY_LABEL_ID}
						className={styles.quantityLabel}
						data-flx="premium.operator-pass-checkout.quantity-label"
					>
						{i18n._(QUANTITY_LABEL_DESCRIPTOR)}
					</span>
					<div
						className={styles.stepper}
						role="group"
						aria-labelledby={QUANTITY_LABEL_ID}
						data-flx="premium.operator-pass-checkout.stepper"
					>
						<Button
							ref={decreaseRef}
							square
							compact
							variant="secondary"
							icon={<MinusIcon weight="bold" data-flx="premium.operator-pass-checkout.minus-icon" />}
							aria-label={i18n._(DECREASE_QUANTITY_DESCRIPTOR)}
							disabled={quantity <= 1}
							onClick={() => changeQuantity(quantity - 1)}
							data-flx="premium.operator-pass-checkout.button.decrease"
						/>
						<output
							className={styles.quantityValue}
							aria-live="polite"
							data-flx="premium.operator-pass-checkout.quantity-value"
						>
							{quantity}
						</output>
						<Button
							ref={increaseRef}
							square
							compact
							variant="secondary"
							icon={<PlusIcon weight="bold" data-flx="premium.operator-pass-checkout.plus-icon" />}
							aria-label={i18n._(INCREASE_QUANTITY_DESCRIPTOR)}
							disabled={quantity >= maxQuantity}
							onClick={() => changeQuantity(quantity + 1)}
							data-flx="premium.operator-pass-checkout.button.increase"
						/>
					</div>
				</div>
				<p className={styles.note} data-flx="premium.operator-pass-checkout.quantity-cap">
					{i18n._(QUANTITY_CAP_DESCRIPTOR, {max: maxQuantity})}
				</p>
			</div>
			<OperatorPassCertificateNamesField
				id={CERTIFICATE_NAME_FIELD_ID}
				names={draft.certificateNames}
				quantity={quantity}
				sameName={draft.sameCertificateName}
				problem={draft.certificateNameProblem}
				problemIndex={draft.certificateNameProblemIndex}
				disabled={false}
				onNameChange={draft.setCertificateName}
				onSameNameChange={draft.setSameCertificateName}
				data-flx="premium.operator-pass-checkout.certificate-names"
			/>
		</div>
	);
});
