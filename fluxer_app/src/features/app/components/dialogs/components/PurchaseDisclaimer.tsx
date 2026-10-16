// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import styles from '@app/features/app/components/dialogs/components/PurchaseDisclaimer.module.css';
import {ExternalLink} from '@app/features/app/components/shared/ExternalLink';
import {PAYMENT_PROVIDER_NAME} from '@app/features/app/config/I18nDisplayConstants';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {Trans} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';

export const PurchaseDisclaimer = observer(
	({isPremium = false, align = 'center'}: {isPremium?: boolean; align?: 'left' | 'center'}) => {
		const selfHosted = RuntimeConfig.isSelfHosted();
		const termsUrl = selfHosted ? RuntimeConfig.termsUrl : Routes.terms();
		const privacyUrl = selfHosted ? RuntimeConfig.privacyUrl : Routes.privacy();
		const terms = termsUrl ? (
			<ExternalLink href={termsUrl} data-flx="app.purchase-disclaimer.external-link">
				<Trans>Terms of service</Trans>
			</ExternalLink>
		) : null;
		const privacy = privacyUrl ? (
			<ExternalLink href={privacyUrl} data-flx="app.purchase-disclaimer.external-link--2">
				<Trans>Privacy policy</Trans>
			</ExternalLink>
		) : null;
		const termsOnly = termsUrl ? (
			isPremium ? (
				<Trans comment="Purchase disclaimer when the instance links only its terms of service. The linked text is the terms of service document.">
					By purchasing, you agreed to our{' '}
					<ExternalLink href={termsUrl} data-flx="app.purchase-disclaimer.external-link--terms-only">
						Terms of service
					</ExternalLink>
					.
				</Trans>
			) : (
				<Trans comment="Purchase disclaimer when the instance links only its terms of service. The linked text is the terms of service document.">
					By purchasing, you agree to our{' '}
					<ExternalLink href={termsUrl} data-flx="app.purchase-disclaimer.external-link--terms-only">
						Terms of service
					</ExternalLink>
					.
				</Trans>
			)
		) : null;
		const privacyOnly = privacyUrl ? (
			isPremium ? (
				<Trans comment="Purchase disclaimer when the instance links only its privacy policy. The linked text is the privacy policy document.">
					By purchasing, you agreed to our{' '}
					<ExternalLink href={privacyUrl} data-flx="app.purchase-disclaimer.external-link--privacy-only">
						Privacy policy
					</ExternalLink>
					.
				</Trans>
			) : (
				<Trans comment="Purchase disclaimer when the instance links only its privacy policy. The linked text is the privacy policy document.">
					By purchasing, you agree to our{' '}
					<ExternalLink href={privacyUrl} data-flx="app.purchase-disclaimer.external-link--privacy-only">
						Privacy policy
					</ExternalLink>
					.
				</Trans>
			)
		) : null;
		const agreement =
			terms && privacy ? (
				isPremium ? (
					<Trans>
						By purchasing, you agreed to our {terms} and {privacy}.
					</Trans>
				) : (
					<Trans>
						By purchasing, you agree to our {terms} and {privacy}.
					</Trans>
				)
			) : (
				(termsOnly ?? privacyOnly)
			);
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
					<Trans>
						Self-serve refunds available within 3 days of payment, once every 30 days. Refunding a subscription cancels
						it. EU/EEA buyers waive the 14-day right of withdrawal at checkout to access content immediately. Use the
						in-app refund button instead of a chargeback. Chargebacks can permanently restrict your account. Stripe
						handles payment securely. We never see your full card number.
					</Trans>
				)}
			</p>
		);
	},
);
