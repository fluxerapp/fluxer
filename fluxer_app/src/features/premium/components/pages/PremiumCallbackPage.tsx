// SPDX-License-Identifier: AGPL-3.0-or-later

import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {useLocation} from '@app/features/platform/components/router/RouterReact';
import * as PremiumCommands from '@app/features/premium/commands/PremiumCommands';
import styles from '@app/features/premium/components/pages/PremiumCallbackPage.module.css';
import {Spinner} from '@app/features/ui/components/Spinner';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {CheckCircleIcon, XCircleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useEffect, useState} from 'react';

type PreapprovalState =
	| {status: 'error'}
	| {status: 'expired'}
	| {status: 'pending'}
	| {status: 'redirecting'}
	| {status: 'rejected'; actualCountry: string | null; reason: string};

const CARD_COUNTRY_MISMATCH_WITH_COUNTRY_DESCRIPTOR = msg({
	message:
		'Stripe reported a card issued in {countryName}. To pay in this localized currency, use a card issued in your billing country, or return to {productName} and choose another payment method.',
	comment:
		'Premium checkout error shown when card country does not match billing country. productName is the app name.',
});
const CARD_COUNTRY_MISMATCH_DESCRIPTOR = msg({
	message:
		'This card does not match your billing country, so it cannot be used for this localized currency. Use a local card, or return to {productName} and choose another payment method.',
	comment:
		'Premium checkout error shown when card country does not match billing country and Stripe did not provide the issuing country. productName is the app name.',
});
const CARD_PREAPPROVAL_FAILED_DESCRIPTOR = msg({
	message:
		'Stripe could not verify this card for localized pricing. Use another card, or return to {productName} and choose another payment method.',
	comment:
		'Premium checkout error shown when Stripe cannot verify a card for localized pricing. productName is the app name.',
});
const CARD_VERIFICATION_EXPIRED_DESCRIPTOR = msg({
	message: 'This verification session expired. Return to {productName} and start checkout again.',
	comment: 'Premium checkout error shown when card verification expires before checkout. productName is the app name.',
});
const CARD_VERIFICATION_FAILED_DESCRIPTOR = msg({
	message: 'We could not confirm your card right now. Refresh this page or return to {productName} and try again.',
	comment: 'Premium checkout error shown when card verification fails unexpectedly. productName is the app name.',
});
const PremiumCallbackPage = observer(() => {
	const {i18n} = useLingui();
	const location = useLocation();
	const queryParams = new URLSearchParams(location.search);
	const status = queryParams.get('status');
	const token = queryParams.get('token');
	const [preapprovalState, setPreapprovalState] = useState<PreapprovalState>({status: 'pending'});
	useEffect(() => {
		if (RuntimeConfig.isSelfHosted()) {
			window.location.replace('/');
		}
	}, []);
	useEffect(() => {
		if (status !== 'preapproval-success') {
			return;
		}
		if (!token) {
			setPreapprovalState({status: 'expired'});
			return;
		}
		let cancelled = false;
		let timeoutId: number | null = null;
		const poll = async () => {
			try {
				const result = await PremiumCommands.continueLocalizedCardPreapproval(token);
				if (cancelled) {
					return;
				}
				switch (result.status) {
					case 'pending':
						setPreapprovalState({status: 'pending'});
						timeoutId = window.setTimeout(() => {
							void poll();
						}, 1500);
						return;
					case 'ready':
						setPreapprovalState({status: 'redirecting'});
						window.location.assign(result.url);
						return;
					case 'rejected':
						setPreapprovalState({
							status: 'rejected',
							reason: result.reason,
							actualCountry: result.actual_country ?? null,
						});
						return;
					case 'expired':
						setPreapprovalState({status: 'expired'});
						return;
				}
			} catch {
				if (!cancelled) {
					setPreapprovalState({status: 'error'});
				}
			}
		};
		void poll();
		return () => {
			cancelled = true;
			if (timeoutId != null) {
				window.clearTimeout(timeoutId);
			}
		};
	}, [status, token]);
	if (RuntimeConfig.isSelfHosted()) {
		return null;
	}
	const isSuccess = status === 'success';
	const isCancel = status === 'cancel';
	const isClosedBillingPortal = status === 'closed-billing-portal';
	const isPreapprovalSuccess = status === 'preapproval-success';
	const isPreapprovalCancel = status === 'preapproval-cancel';
	const isPreapprovalRejected = isPreapprovalSuccess && preapprovalState.status === 'rejected';
	const isPreapprovalError = isPreapprovalSuccess && preapprovalState.status === 'error';
	const isPreapprovalExpired = isPreapprovalSuccess && preapprovalState.status === 'expired';
	const isPreapprovalPending =
		isPreapprovalSuccess && (preapprovalState.status === 'pending' || preapprovalState.status === 'redirecting');
	return (
		<div className={styles.container} data-flx="premium.premium-callback-page.container">
			{isSuccess && (
				<>
					<CheckCircleIcon
						className={styles.successIcon}
						weight="fill"
						data-flx="premium.premium-callback-page.success-icon"
					/>
					<div className={styles.content} data-flx="premium.premium-callback-page.content">
						<h1 className={styles.title} data-flx="premium.premium-callback-page.title">
							<Trans>Payment successful</Trans>
						</h1>
						<p className={styles.description} data-flx="premium.premium-callback-page.description">
							<Trans>Your payment was successful. You can now close this tab and return to the app.</Trans>
						</p>
					</div>
				</>
			)}
			{isCancel && (
				<>
					<XCircleIcon className={styles.errorIcon} weight="fill" data-flx="premium.premium-callback-page.error-icon" />
					<div className={styles.content} data-flx="premium.premium-callback-page.content--2">
						<h1 className={styles.title} data-flx="premium.premium-callback-page.title--2">
							<Trans>Payment canceled</Trans>
						</h1>
						<p className={styles.description} data-flx="premium.premium-callback-page.description--2">
							<Trans>Your payment was canceled. You can now close this tab and return to the app.</Trans>
						</p>
					</div>
				</>
			)}
			{isClosedBillingPortal && (
				<>
					<CheckCircleIcon
						className={styles.successIcon}
						weight="fill"
						data-flx="premium.premium-callback-page.success-icon--2"
					/>
					<div className={styles.content} data-flx="premium.premium-callback-page.content--3">
						<h1 className={styles.title} data-flx="premium.premium-callback-page.title--3">
							<Trans>All done</Trans>
						</h1>
						<p className={styles.description} data-flx="premium.premium-callback-page.description--3">
							<Trans>You can now close this tab and return to the app.</Trans>
						</p>
					</div>
				</>
			)}
			{isPreapprovalPending && (
				<>
					<Spinner size="large" data-flx="premium.premium-callback-page.spinner" />
					<div className={styles.content} data-flx="premium.premium-callback-page.content--4">
						<h1 className={styles.title} data-flx="premium.premium-callback-page.title--4">
							<Trans>Verifying card</Trans>
						</h1>
						<p className={styles.description} data-flx="premium.premium-callback-page.description--4">
							{preapprovalState.status === 'redirecting' ? (
								<Trans>Your card is eligible. Redirecting you to the payment page now.</Trans>
							) : (
								<Trans>Your card is being checked for localized pricing. This usually takes a moment.</Trans>
							)}
						</p>
					</div>
				</>
			)}
			{isPreapprovalRejected && (
				<>
					<XCircleIcon
						className={styles.errorIcon}
						weight="fill"
						data-flx="premium.premium-callback-page.error-icon--2"
					/>
					<div className={styles.content} data-flx="premium.premium-callback-page.content--5">
						<h1 className={styles.title} data-flx="premium.premium-callback-page.title--5">
							<Trans>Card not eligible</Trans>
						</h1>
						<p className={styles.description} data-flx="premium.premium-callback-page.description--5">
							{preapprovalState.reason === 'country_mismatch'
								? preapprovalState.actualCountry
									? i18n._(CARD_COUNTRY_MISMATCH_WITH_COUNTRY_DESCRIPTOR, {
											countryName: preapprovalState.actualCountry,
											productName: PRODUCT_NAME,
										})
									: i18n._(CARD_COUNTRY_MISMATCH_DESCRIPTOR, {productName: PRODUCT_NAME})
								: i18n._(CARD_PREAPPROVAL_FAILED_DESCRIPTOR, {productName: PRODUCT_NAME})}
						</p>
					</div>
				</>
			)}
			{isPreapprovalExpired && (
				<>
					<XCircleIcon
						className={styles.errorIcon}
						weight="fill"
						data-flx="premium.premium-callback-page.error-icon--3"
					/>
					<div className={styles.content} data-flx="premium.premium-callback-page.content--6">
						<h1 className={styles.title} data-flx="premium.premium-callback-page.title--6">
							<Trans>Verification expired</Trans>
						</h1>
						<p className={styles.description} data-flx="premium.premium-callback-page.description--6">
							{i18n._(CARD_VERIFICATION_EXPIRED_DESCRIPTOR, {productName: PRODUCT_NAME})}
						</p>
					</div>
				</>
			)}
			{isPreapprovalError && (
				<>
					<XCircleIcon
						className={styles.errorIcon}
						weight="fill"
						data-flx="premium.premium-callback-page.error-icon--4"
					/>
					<div className={styles.content} data-flx="premium.premium-callback-page.content--7">
						<h1 className={styles.title} data-flx="premium.premium-callback-page.title--7">
							<Trans>Verification failed</Trans>
						</h1>
						<p className={styles.description} data-flx="premium.premium-callback-page.description--7">
							{i18n._(CARD_VERIFICATION_FAILED_DESCRIPTOR, {productName: PRODUCT_NAME})}
						</p>
					</div>
				</>
			)}
			{isPreapprovalCancel && (
				<>
					<XCircleIcon
						className={styles.errorIcon}
						weight="fill"
						data-flx="premium.premium-callback-page.error-icon--5"
					/>
					<div className={styles.content} data-flx="premium.premium-callback-page.content--8">
						<h1 className={styles.title} data-flx="premium.premium-callback-page.title--8">
							<Trans>Verification canceled</Trans>
						</h1>
						<p className={styles.description} data-flx="premium.premium-callback-page.description--8">
							<Trans>Card verification was canceled. You can close this tab and return to the app.</Trans>
						</p>
					</div>
				</>
			)}
			{!isSuccess && !isCancel && !isClosedBillingPortal && !isPreapprovalSuccess && !isPreapprovalCancel && (
				<div className={styles.content} data-flx="premium.premium-callback-page.content--9">
					<h1 className={styles.title} data-flx="premium.premium-callback-page.title--9">
						<Trans>Invalid status</Trans>
					</h1>
					<p className={styles.description} data-flx="premium.premium-callback-page.description--9">
						<Trans>An invalid status was provided. You can now close this tab and return to the app.</Trans>
					</p>
				</div>
			)}
		</div>
	);
});

export default PremiumCallbackPage;
