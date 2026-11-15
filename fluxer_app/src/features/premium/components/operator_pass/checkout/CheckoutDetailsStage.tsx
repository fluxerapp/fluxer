// SPDX-License-Identifier: AGPL-3.0-or-later

import {BusinessPurchaseField} from '@app/features/premium/components/BusinessPurchaseField';
import styles from '@app/features/premium/components/operator_pass/checkout/CheckoutStage.module.css';
import {
	LABS_GREETING_FIELD_ID,
	type OperatorPassCheckoutDraft,
	WALL_NAME_FIELD_ID,
} from '@app/features/premium/components/operator_pass/checkout/OperatorPassCheckoutDraft';
import {OperatorPassLabsGreetingField} from '@app/features/premium/components/operator_pass/OperatorPassLabsGreetingField';
import {OperatorPassWallNameField} from '@app/features/premium/components/operator_pass/OperatorPassWallNameField';
import {getOperatorPassWallNameProblem} from '@app/features/premium/utils/OperatorPassUtils';
import {OPERATOR_PASS_LISTING_UNCHANGED_DESCRIPTOR} from '@app/features/premium/utils/PremiumMessageDescriptors';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

interface CheckoutDetailsStageProps {
	draft: OperatorPassCheckoutDraft;
	asksWallChoice: boolean;
	asksLabsGreeting: boolean;
	asksCnpj: boolean;
}

export const CheckoutDetailsStage = observer(function CheckoutDetailsStage({
	draft,
	asksWallChoice,
	asksLabsGreeting,
	asksCnpj,
}: CheckoutDetailsStageProps) {
	const {i18n} = useLingui();
	return (
		<div className={styles.stage} data-flx="premium.operator-pass-checkout.details">
			{asksWallChoice ? (
				<OperatorPassWallNameField
					id={WALL_NAME_FIELD_ID}
					name={draft.wallName}
					optOut={draft.wallOptOut}
					problem={draft.wallNameProblem}
					disabled={false}
					context="checkout"
					onNameChange={(value) => {
						draft.setWallName(value);
						if (draft.wallNameProblem) {
							draft.setWallNameProblem(getOperatorPassWallNameProblem(value, draft.wallOptOut));
						}
					}}
					onOptOutChange={(value) => {
						draft.setWallOptOut(value);
						if (value) draft.setWallNameProblem(null);
					}}
					data-flx="premium.operator-pass-checkout.wall-name"
				/>
			) : (
				<p className={styles.note} data-flx="premium.operator-pass-checkout.listing-unchanged">
					{i18n._(OPERATOR_PASS_LISTING_UNCHANGED_DESCRIPTOR)}
				</p>
			)}
			{asksLabsGreeting && (
				<OperatorPassLabsGreetingField
					id={LABS_GREETING_FIELD_ID}
					checked={draft.labsGreeting}
					disabled={false}
					onChange={draft.setLabsGreeting}
					data-flx="premium.operator-pass-checkout.labs-greeting"
				/>
			)}
			<BusinessPurchaseField
				checked={draft.isBusiness}
				disabled={false}
				onChange={draft.setIsBusiness}
				cnpj={asksCnpj ? {value: draft.cnpj, error: draft.cnpjError, onChange: draft.setCnpj} : undefined}
				data-flx="premium.operator-pass-checkout.business"
			/>
		</div>
	);
});
