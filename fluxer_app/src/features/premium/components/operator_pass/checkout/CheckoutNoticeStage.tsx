// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {EmailVerificationAlert} from '@app/features/app/components/dialogs/components/EmailVerificationAlert';
import {ExternalLink} from '@app/features/app/components/shared/ExternalLink';
import {OPERATOR_PASS_NAME, PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {openBouncedEmailChange} from '@app/features/auth/hooks/useVerifyEmailAction';
import styles from '@app/features/premium/components/operator_pass/checkout/CheckoutStage.module.css';
import {OperatorPassPriceBlock} from '@app/features/premium/components/operator_pass/checkout/OperatorPassPriceBlock';
import type {BlockingNotice} from '@app/features/premium/utils/OperatorPassCheckoutMachine';
import {Button} from '@app/features/ui/button/Button';
import {WarningAlert} from '@app/features/ui/warning_alert/WarningAlert';
import Users from '@app/features/user/state/Users';
import {AccountVerificationNeeds} from '@fluxer/constants/src/AccountTierConstants';
import type {OperatorPassStateResponse} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const WAITLIST_DESCRIPTION_DESCRIPTOR = msg({
	message: "We'll email you when it opens.",
	comment:
		'Line in the Operator Pass dialog before the pass can be bought, under the button that opens the waitlist. It means the sale of the Operator Pass opens.',
});
const EMAIL_UNVERIFIED_NOTICE_DESCRIPTOR = msg({
	message: 'Confirm your email to buy the {operatorPassName}.',
	comment:
		'Notice in the Operator Pass checkout dialog for accounts without a verified email. Operator Pass is a product name.',
});
const EMAIL_UNDELIVERABLE_NOTICE_DESCRIPTOR = msg({
	message:
		"Emails to your address can't be delivered. Change your email before you buy the {operatorPassName}, because we send your purchase confirmation there.",
	comment:
		'Notice in the Operator Pass checkout dialog when emails to the account address bounced. The purchase confirmation is the email sent before the certificate is delivered. Operator Pass is a product name.',
});
const UNCLAIMED_NOTICE_DESCRIPTOR = msg({
	message: 'Claim your account to buy the {operatorPassName}.',
	comment:
		'Notice in the Operator Pass checkout dialog for accounts that need a password before they can buy. Operator Pass is a product name.',
});
const NO_EMAIL_NOTICE_DESCRIPTOR = msg({
	message: 'Add an email to your account to buy the {operatorPassName}.',
	comment:
		'Notice in the Operator Pass checkout dialog for accounts that have no email yet. Operator Pass is a product name.',
});
const REMOVED_NOTICE_DESCRIPTOR = msg({
	message:
		"We permanently removed what comes with the {operatorPassName} from this account, so it can't buy a pass again.",
	comment:
		'Notice in the Operator Pass checkout dialog when the pass was removed from the account permanently. Operator Pass is a product name and stays untranslated.',
});
const PAYMENT_DISPUTE_NOTICE_DESCRIPTOR = msg({
	message:
		'Purchases are paused while a payment dispute on your account is open, because a disputed payment can still be reversed.',
	comment:
		'Notice in the Operator Pass checkout dialog while a payment dispute this account made is open. A payment dispute is a chargeback filed with a bank or card company. Neutral tone, never suggest the person did something wrong.',
});
const OWNED_BODY_DESCRIPTOR = msg({
	message: 'Thanks for backing {productName}. Your passes are under "{operatorPassName}" in your settings.',
	comment:
		'Text in the Operator Pass dialog when the account already has a pass and buying is not open. Backing means supporting the project. "{operatorPassName}" is the name of the settings tab that lists the passes, keep the quotes. Operator Pass is a product name and stays untranslated.',
});

const BlockingNoticeAlert = observer(function BlockingNoticeAlert({notice}: {notice: BlockingNotice}) {
	const {i18n} = useLingui();
	switch (notice) {
		case 'email_unverified':
			if (RuntimeConfig.emailsEnabled) {
				return (
					<EmailVerificationAlert data-flx="premium.operator-pass-checkout.notice">
						{i18n._(EMAIL_UNVERIFIED_NOTICE_DESCRIPTOR, {operatorPassName: OPERATOR_PASS_NAME})}
					</EmailVerificationAlert>
				);
			}
			return (
				<WarningAlert data-flx="premium.operator-pass-checkout.notice">
					{i18n._(EMAIL_UNVERIFIED_NOTICE_DESCRIPTOR, {operatorPassName: OPERATOR_PASS_NAME})}
				</WarningAlert>
			);
		case 'email_undeliverable':
			return (
				<WarningAlert
					actions={
						<Button
							variant="primary"
							small
							onClick={openBouncedEmailChange}
							data-flx="premium.operator-pass-checkout.button.change-email"
						>
							<Trans comment="Button in the Operator Pass checkout dialog that opens the dialog to change the account email.">
								Change email
							</Trans>
						</Button>
					}
					data-flx="premium.operator-pass-checkout.notice"
				>
					{i18n._(EMAIL_UNDELIVERABLE_NOTICE_DESCRIPTOR, {operatorPassName: OPERATOR_PASS_NAME})}
				</WarningAlert>
			);
		case 'purchase_disabled': {
			const supportUrl = Routes.help();
			return (
				<WarningAlert data-flx="premium.operator-pass-checkout.notice">
					{supportUrl ? (
						<Trans comment="Notice in the Operator Pass checkout dialog when purchases are turned off for the account. The linked text opens the support page.">
							Purchases are off for this account.{' '}
							<ExternalLink href={supportUrl} data-flx="premium.operator-pass-checkout.support-link">
								Contact support
							</ExternalLink>
							.
						</Trans>
					) : (
						<Trans comment="Notice in the Operator Pass checkout dialog when purchases are turned off for the account, on an instance without a support page.">
							Purchases are off for this account.
						</Trans>
					)}
				</WarningAlert>
			);
		}
		case 'unclaimed':
			return (
				<WarningAlert data-flx="premium.operator-pass-checkout.notice">
					{i18n._(
						Users.currentUser?.verificationNeed === AccountVerificationNeeds.SET_PASSWORD
							? UNCLAIMED_NOTICE_DESCRIPTOR
							: NO_EMAIL_NOTICE_DESCRIPTOR,
						{operatorPassName: OPERATOR_PASS_NAME},
					)}
				</WarningAlert>
			);
		case 'payment_dispute_open':
			return (
				<WarningAlert data-flx="premium.operator-pass-checkout.notice">
					{i18n._(PAYMENT_DISPUTE_NOTICE_DESCRIPTOR)}
				</WarningAlert>
			);
		case 'removed':
			return (
				<WarningAlert data-flx="premium.operator-pass-checkout.notice">
					{i18n._(REMOVED_NOTICE_DESCRIPTOR, {operatorPassName: OPERATOR_PASS_NAME})}
				</WarningAlert>
			);
	}
});

interface CheckoutNoticeStageProps {
	kind: 'notice' | 'waitlist' | 'owned';
	notice: BlockingNotice | null;
	state: OperatorPassStateResponse | null;
	formatPrice: (amountMinor: number, currency: string) => string;
}

export function CheckoutNoticeStage({kind, notice, state, formatPrice}: CheckoutNoticeStageProps) {
	const {i18n} = useLingui();
	return (
		<div className={styles.stage} data-flx="premium.operator-pass-checkout.notice-stage">
			{kind === 'notice' && notice && (
				<div role="alert" data-flx="premium.operator-pass-checkout.notice-alert">
					<BlockingNoticeAlert
						notice={notice}
						data-flx="premium.operator-pass.checkout.checkout-notice-stage.blocking-notice-alert"
					/>
				</div>
			)}
			{kind !== 'owned' && state && (
				<OperatorPassPriceBlock
					state={state}
					count={1}
					formatPrice={formatPrice}
					data-flx="premium.operator-pass.checkout.checkout-notice-stage.operator-pass-price-block"
				/>
			)}
			{kind === 'waitlist' && (
				<p className={styles.note} data-flx="premium.operator-pass-checkout.waitlist">
					{i18n._(WAITLIST_DESCRIPTION_DESCRIPTOR)}
				</p>
			)}
			{kind === 'owned' && (
				<p className={styles.lead} data-flx="premium.operator-pass-checkout.owned-body">
					{i18n._(OWNED_BODY_DESCRIPTOR, {productName: PRODUCT_NAME, operatorPassName: OPERATOR_PASS_NAME})}
				</p>
			)}
		</div>
	);
}
