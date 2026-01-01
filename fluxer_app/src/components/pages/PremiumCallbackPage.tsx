/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {Trans} from '@lingui/react/macro';
import {CheckCircleIcon, XCircleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useLocation} from '~/lib/router';
import styles from './PremiumCallbackPage.module.css';

const PremiumCallbackPage = observer(() => {
	const location = useLocation();
	const queryParams = new URLSearchParams(location.search);
	const status = queryParams.get('status');

	const isSuccess = status === 'success';
	const isCancel = status === 'cancel';
	const isClosedBillingPortal = status === 'closed-billing-portal';

	return (
		<div className={styles.container}>
			{isSuccess && (
				<>
					<CheckCircleIcon className={styles.successIcon} weight="fill" />
					<div className={styles.content}>
						<h1 className={styles.title}>
							<Trans>Payment Successful</Trans>
						</h1>
						<p className={styles.description}>
							<Trans>Great success! You can now close this tab and return to the app.</Trans>
						</p>
					</div>
				</>
			)}

			{isCancel && (
				<>
					<XCircleIcon className={styles.errorIcon} weight="fill" />
					<div className={styles.content}>
						<h1 className={styles.title}>
							<Trans>Payment Cancelled</Trans>
						</h1>
						<p className={styles.description}>
							<Trans>Your payment was cancelled. You can now close this tab and return to the app.</Trans>
						</p>
					</div>
				</>
			)}

			{isClosedBillingPortal && (
				<>
					<CheckCircleIcon className={styles.successIcon} weight="fill" />
					<div className={styles.content}>
						<h1 className={styles.title}>
							<Trans>All Done</Trans>
						</h1>
						<p className={styles.description}>
							<Trans>You can now close this tab and return to the app.</Trans>
						</p>
					</div>
				</>
			)}

			{!isSuccess && !isCancel && !isClosedBillingPortal && (
				<div className={styles.content}>
					<h1 className={styles.title}>
						<Trans>Invalid Status</Trans>
					</h1>
					<p className={styles.description}>
						<Trans>An invalid status was provided. You can now close this tab and return to the app.</Trans>
					</p>
				</div>
			)}
		</div>
	);
});

export default PremiumCallbackPage;
