// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import styles from '@app/features/app/components/dialogs/components/PurchaseDisclaimer.module.css';
import {ExternalLink} from '@app/features/app/components/shared/ExternalLink';
import {
	OPERATOR_PASS_NAME,
	OPERATOR_PASS_SELLER_ADDRESS,
	OPERATOR_PASS_SELLER_PHONE,
	PAYMENT_PROVIDER_NAME,
	SUPPORT_EMAIL,
} from '@app/features/app/config/I18nDisplayConstants';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {marketingUrl} from '@app/features/messaging/utils/MessagingUrlUtils';
import {getPremiumProductName} from '@app/features/premium/utils/PremiumUtils';
import {Trans} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type {ReactNode} from 'react';

export const PurchaseDisclaimer = observer(
	({
		isPremium = false,
		operatorPass = false,
		align = 'center',
	}: {
		isPremium?: boolean;
		operatorPass?: boolean;
		align?: 'left' | 'center';
	}) => {
		const selfHosted = RuntimeConfig.isSelfHosted();
		const premiumProductName = getPremiumProductName();
		const termsUrl = Routes.terms();
		const privacyUrl = Routes.privacy();
		if (operatorPass) {
			const operatorPassTermsUrl = Routes.operatorPassTerms();
			const operatorPassPrivacyUrl = Routes.operatorPassPrivacy();
			return (
				<p
					className={clsx(styles.disclaimer, align === 'center' ? styles.center : styles.left)}
					data-flx="app.purchase-disclaimer.operator-pass"
				>
					{termsUrl ? (
						<Trans comment="Line directly above the payment button in the Operator Pass checkout dialog. Both linked texts name legal documents: the terms of service and the Operator Pass terms. {OPERATOR_PASS_NAME} is a product name and stays untranslated. Capitalize the linked texts the way your language does in the middle of a sentence.">
							By paying, you agree to the{' '}
							<ExternalLink href={termsUrl} data-flx="app.purchase-disclaimer.operator-pass.terms-link">
								terms of service
							</ExternalLink>{' '}
							and the{' '}
							<ExternalLink
								href={operatorPassTermsUrl}
								data-flx="app.purchase-disclaimer.operator-pass.operator-pass-terms-link"
							>
								{OPERATOR_PASS_NAME} terms
							</ExternalLink>
							.
						</Trans>
					) : (
						<Trans comment="Line directly above the payment button in the Operator Pass checkout dialog on an instance that links no terms of service. The linked text names the Operator Pass terms document. {OPERATOR_PASS_NAME} is a product name and stays untranslated.">
							By paying, you agree to the{' '}
							<ExternalLink
								href={operatorPassTermsUrl}
								data-flx="app.purchase-disclaimer.operator-pass.operator-pass-terms-link--only"
							>
								{OPERATOR_PASS_NAME} terms
							</ExternalLink>
							.
						</Trans>
					)}{' '}
					{privacyUrl ? (
						<Trans comment="Sentence after the line that starts with By paying in the Operator Pass checkout dialog. Both linked texts name legal documents: the privacy policy and the Operator Pass privacy notice, which covers the certificate name, the checkout record and the name and avatar people send for the Operators' wall. People never agree to either, they only inform. {OPERATOR_PASS_NAME} is a product name and stays untranslated. Capitalize the linked texts the way your language does at their position in the sentence.">
							The{' '}
							<ExternalLink href={privacyUrl} data-flx="app.purchase-disclaimer.operator-pass.privacy-link">
								privacy policy
							</ExternalLink>{' '}
							and the{' '}
							<ExternalLink
								href={operatorPassPrivacyUrl}
								data-flx="app.purchase-disclaimer.operator-pass.operator-pass-privacy-link"
							>
								{OPERATOR_PASS_NAME} privacy notice
							</ExternalLink>{' '}
							explain how we handle your data.
						</Trans>
					) : (
						<Trans comment="Sentence after the line that starts with By paying in the Operator Pass checkout dialog on an instance that links no privacy policy. The linked text names the Operator Pass privacy notice, which covers the certificate name, the checkout record and the name and avatar people send for the Operators' wall. People never agree to it, it only informs. {OPERATOR_PASS_NAME} is a product name and stays untranslated. Capitalize the linked text the way your language does at its position in the sentence.">
							The{' '}
							<ExternalLink
								href={operatorPassPrivacyUrl}
								data-flx="app.purchase-disclaimer.operator-pass.operator-pass-privacy-link--only"
							>
								{OPERATOR_PASS_NAME} privacy notice
							</ExternalLink>{' '}
							explains how we handle your data.
						</Trans>
					)}{' '}
					<Trans comment="Sentence after the privacy sentence in the Operator Pass checkout dialog that names the company selling the pass. Fluxer Platform AB is the company name and 559537-3993 its Swedish organization number, keep both as is. Haninge is the Swedish municipality where the company is registered and stays as is. Translate registered office with the legal term your language uses for the registered seat of a company, and keep the Swedish word säte in parentheses. The address, phone number and email address are filled in and stay as is. Use the same word for seller as on a receipt.">
						Seller: Fluxer Platform AB, 559537-3993, registered office (säte) Haninge, Sweden,{' '}
						{OPERATOR_PASS_SELLER_ADDRESS}, {OPERATOR_PASS_SELLER_PHONE}, {SUPPORT_EMAIL}.
					</Trans>
				</p>
			);
		}
		let termsOnly: ReactNode = null;
		if (termsUrl) {
			if (isPremium) {
				termsOnly = (
					<Trans comment="Purchase disclaimer when the instance links only its terms of service. The linked text is the terms of service document.">
						By purchasing, you agreed to our{' '}
						<ExternalLink href={termsUrl} data-flx="app.purchase-disclaimer.external-link--terms-only">
							terms of service
						</ExternalLink>
						.
					</Trans>
				);
			} else {
				termsOnly = (
					<Trans comment="Purchase disclaimer when the instance links only its terms of service. The linked text is the terms of service document.">
						By purchasing, you agree to our{' '}
						<ExternalLink href={termsUrl} data-flx="app.purchase-disclaimer.external-link--terms-only">
							terms of service
						</ExternalLink>
						.
					</Trans>
				);
			}
		}
		const privacyOnly = privacyUrl ? (
			<Trans comment="Purchase disclaimer when the instance links only its privacy policy. The linked text is the privacy policy document. People never agree to the privacy policy, it only informs.">
				Our{' '}
				<ExternalLink href={privacyUrl} data-flx="app.purchase-disclaimer.external-link--privacy-only">
					privacy policy
				</ExternalLink>{' '}
				explains how we handle your data.
			</Trans>
		) : null;
		let agreement: ReactNode;
		if (termsUrl && privacyUrl) {
			if (isPremium) {
				agreement = (
					<Trans comment="Purchase disclaimer. People agree to the terms of service and only read the privacy policy. The first linked text is the terms of service document and the second is the privacy policy document.">
						By purchasing, you agreed to our{' '}
						<ExternalLink href={termsUrl} data-flx="app.purchase-disclaimer.external-link">
							terms of service
						</ExternalLink>
						. Our{' '}
						<ExternalLink href={privacyUrl} data-flx="app.purchase-disclaimer.external-link--2">
							privacy policy
						</ExternalLink>{' '}
						explains how we handle your data.
					</Trans>
				);
			} else {
				agreement = (
					<Trans comment="Purchase disclaimer. People agree to the terms of service and only read the privacy policy. The first linked text is the terms of service document and the second is the privacy policy document.">
						By purchasing, you agree to our{' '}
						<ExternalLink href={termsUrl} data-flx="app.purchase-disclaimer.external-link">
							terms of service
						</ExternalLink>
						. Our{' '}
						<ExternalLink href={privacyUrl} data-flx="app.purchase-disclaimer.external-link--2">
							privacy policy
						</ExternalLink>{' '}
						explains how we handle your data.
					</Trans>
				);
			}
		} else {
			agreement = termsOnly ?? privacyOnly;
		}
		return (
			<p
				className={clsx(styles.disclaimer, align === 'center' ? styles.center : styles.left)}
				data-flx="app.purchase-disclaimer.disclaimer"
			>
				{agreement}
				{agreement ? ' ' : null}
				{selfHosted ? (
					<Trans comment="Purchase disclaimer on a self-hosted instance. {PAYMENT_PROVIDER_NAME} is the payment processor name.">
						{PAYMENT_PROVIDER_NAME} handles payment securely.
					</Trans>
				) : (
					<Trans comment="Purchase disclaimer in the premium subscription checkout. Consumer law text, translate faithfully. {premiumProductName} is a product name and stays untranslated. {PAYMENT_PROVIDER_NAME} is the payment processor name. Withdraw means using the consumer right of withdrawal to cancel the purchase, never taking money out. The first withdraw is a link to the section of the terms that explains how to withdraw and has the withdrawal form, so wrap the words your language uses for that action. The UK is the United Kingdom. The terms set how long each withdrawal period lasts and how much comes back, so the sentence names no number of days and no refund amount. A payment dispute is a chargeback filed with a bank or card company. The second link, our terms, goes to the section of the terms of service on payment disputes. Action against the account means enforcement such as suspension or deletion.">
						Self-serve refunds are available within 3 days of payment, once every 30 days. Refunding a subscription
						cancels it. If you are a consumer in the EU, the EEA, the UK or Brazil, you can{' '}
						<ExternalLink
							href={`${marketingUrl('terms')}#4-3-eu-and-eea-withdrawal-right`}
							data-flx="app.purchase-disclaimer.withdrawal-link"
						>
							withdraw
						</ExternalLink>{' '}
						until your withdrawal period ends, even after {premiumProductName} starts. Please ask for a refund before
						you dispute a charge. A dispute about a wrong or unauthorized charge never leads to action against your
						account, and{' '}
						<ExternalLink
							href={`${marketingUrl('terms')}#4-4-failed-payments-and-disputes`}
							data-flx="app.purchase-disclaimer.disputes-link"
						>
							our terms
						</ExternalLink>{' '}
						list the only cases where a dispute can. {PAYMENT_PROVIDER_NAME} handles payment securely. We never see your
						full card number.
					</Trans>
				)}
			</p>
		);
	},
);
