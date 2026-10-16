// SPDX-License-Identifier: AGPL-3.0-or-later

import {useLocation} from '@app/features/platform/components/router/RouterReact';
import styles from '@app/features/premium/components/pages/PremiumCallbackPage.module.css';
import {shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';
import {Trans} from '@lingui/react/macro';
import {CheckCircleIcon, XCircleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useEffect} from 'react';

const PremiumCallbackPage = observer(() => {
	const location = useLocation();
	const queryParams = new URLSearchParams(location.search);
	const status = queryParams.get('status');
	useEffect(() => {
		if (!shouldShowPremiumFeatures()) {
			window.location.replace('/');
		}
	}, []);
	if (!shouldShowPremiumFeatures()) {
		return null;
	}
	const isSuccess = status === 'success';
	const isCancel = status === 'cancel';
	const isClosedBillingPortal = status === 'closed-billing-portal';
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
			{!isSuccess && !isCancel && !isClosedBillingPortal && (
				<div className={styles.content} data-flx="premium.premium-callback-page.content--4">
					<h1 className={styles.title} data-flx="premium.premium-callback-page.title--4">
						<Trans>Invalid status</Trans>
					</h1>
					<p className={styles.description} data-flx="premium.premium-callback-page.description--4">
						<Trans>An invalid status was provided. You can now close this tab and return to the app.</Trans>
					</p>
				</div>
			)}
		</div>
	);
});

export default PremiumCallbackPage;
