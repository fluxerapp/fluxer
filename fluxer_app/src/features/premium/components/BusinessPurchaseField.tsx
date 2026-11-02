// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/premium/components/BusinessPurchaseField.module.css';
import {Checkbox} from '@app/features/ui/checkbox/Checkbox';
import {Input} from '@app/features/ui/components/form/FormInput';
import {normalizeCnpj} from '@fluxer/schema/src/domains/premium/Cnpj';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useId} from 'react';

const BUSINESS_PURCHASE_LABEL_DESCRIPTOR = msg({
	message: "I'm buying for a business",
	comment:
		'Checkbox label in the Operator Pass and gift checkout dialogs, off by default. Tick it when the purchase is for a company, so the payment page asks for a billing address for the invoice. First person, plain words.',
});
const BUSINESS_PURCHASE_HINT_DESCRIPTOR = msg({
	message: 'On the payment page, add your company name, address and VAT ID.',
	comment:
		'Hint under the checkbox "I\'m buying for a business" in the checkout dialogs. The payment page is the card payment page that opens after this dialog. VAT ID is the company\'s value-added tax number, use the usual local term.',
});
const CNPJ_LABEL_DESCRIPTOR = msg({
	message: 'CNPJ (optional)',
	comment:
		'Label of a text field in the checkout dialogs, shown only to buyers in Brazil after they tick "I\'m buying for a business". CNPJ is the Brazilian company registration number, keep the abbreviation CNPJ as is in every language.',
});
const CNPJ_HINT_DESCRIPTOR = msg({
	message:
		"With a CNPJ, this is a business purchase, so the 7-day right of withdrawal in the Brazilian Consumer Defense Code doesn't apply to it, unless Brazilian law treats your company as a consumer.",
	comment:
		'Hint under the CNPJ field in the checkout dialogs, shown only to buyers in Brazil. CNPJ is the Brazilian company registration number, keep it as is. The Brazilian Consumer Defense Code is the Código de Defesa do Consumidor, and the right of withdrawal is the direito de arrependimento in its article 49. Use the usual name of that law in your language.',
});
export const CNPJ_INVALID_DESCRIPTOR = msg({
	message: 'Enter a valid CNPJ, or leave the field empty.',
	comment:
		'Error under the CNPJ field in the checkout dialogs when the number has the wrong format or check digits. CNPJ is the Brazilian company registration number, keep it as is.',
});

export function businessPurchaseCnpj(isBusiness: boolean, brazil: boolean, cnpj: string): string | null | undefined {
	if (!isBusiness || !brazil || cnpj.trim() === '') {
		return undefined;
	}
	return normalizeCnpj(cnpj);
}

export const BusinessPurchaseField = observer(function BusinessPurchaseField({
	checked,
	disabled,
	onChange,
	cnpj,
}: {
	checked: boolean;
	disabled: boolean;
	onChange: (checked: boolean) => void;
	cnpj?: {value: string; error: string | null; onChange: (value: string) => void};
}) {
	const {i18n} = useLingui();
	const hintId = useId();
	const cnpjHintId = useId();
	return (
		<div className={styles.field} data-flx="premium.business-purchase-field.field">
			<Checkbox
				checked={checked}
				onChange={onChange}
				disabled={disabled}
				aria-describedby={hintId}
				data-flx="premium.business-purchase-field.checkbox"
			>
				<span className={styles.label} data-flx="premium.business-purchase-field.label">
					{i18n._(BUSINESS_PURCHASE_LABEL_DESCRIPTOR)}
				</span>
			</Checkbox>
			<span id={hintId} className={styles.hint} data-flx="premium.business-purchase-field.hint">
				{i18n._(BUSINESS_PURCHASE_HINT_DESCRIPTOR)}
			</span>
			{checked && cnpj && (
				<Input
					label={i18n._(CNPJ_LABEL_DESCRIPTOR)}
					value={cnpj.value}
					onChange={(event) => cnpj.onChange(event.target.value)}
					error={cnpj.error}
					disabled={disabled}
					inputMode="text"
					autoComplete="off"
					maxLength={32}
					aria-describedby={cnpjHintId}
					footer={
						<span id={cnpjHintId} className={styles.hint} data-flx="premium.business-purchase-field.cnpj-hint">
							{i18n._(CNPJ_HINT_DESCRIPTOR)}
						</span>
					}
					data-flx="premium.business-purchase-field.cnpj"
				/>
			)}
		</div>
	);
});
