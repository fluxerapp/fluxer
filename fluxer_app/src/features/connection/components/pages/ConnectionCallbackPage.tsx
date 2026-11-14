// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/connection/components/pages/ConnectionCallbackPage.module.css';
import {useLocation} from '@app/features/platform/components/router/RouterReact';
import {Trans} from '@lingui/react/macro';
import {CheckCircleIcon, XCircleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';

const ConnectionFailureDescription = ({reason}: {reason: string | null}) => {
	switch (reason) {
		case 'denied':
			return <Trans>The connection was not allowed. You can close this tab and try again.</Trans>;
		case 'limit_reached':
			return <Trans>You already have the most connections an account can have. Remove one and try again.</Trans>;
		default:
			return <Trans>Something went wrong while connecting your account. You can close this tab and try again.</Trans>;
	}
};

const ConnectionCallbackPage = observer(() => {
	const location = useLocation();
	const queryParams = new URLSearchParams(location.search);
	const status = queryParams.get('status');
	const isSuccess = status === 'connected';
	const isError = status === 'error';
	return (
		<div className={styles.container} data-flx="connection.connection-callback-page.container">
			{isSuccess && (
				<>
					<CheckCircleIcon
						className={styles.successIcon}
						weight="fill"
						data-flx="connection.connection-callback-page.success-icon"
					/>
					<div className={styles.content} data-flx="connection.connection-callback-page.content">
						<h1 className={styles.title} data-flx="connection.connection-callback-page.title">
							<Trans>Account connected</Trans>
						</h1>
						<p className={styles.description} data-flx="connection.connection-callback-page.description">
							<Trans>
								Your account has been linked successfully. You can now close this tab and return to the app.
							</Trans>
						</p>
					</div>
				</>
			)}
			{isError && (
				<>
					<XCircleIcon
						className={styles.errorIcon}
						weight="fill"
						data-flx="connection.connection-callback-page.error-icon"
					/>
					<div className={styles.content} data-flx="connection.connection-callback-page.content--2">
						<h1 className={styles.title} data-flx="connection.connection-callback-page.title--2">
							<Trans>Connection failed</Trans>
						</h1>
						<p className={styles.description} data-flx="connection.connection-callback-page.description--2">
							<ConnectionFailureDescription
								reason={queryParams.get('reason')}
								data-flx="connection.connection-callback-page.connection-failure-description"
							/>
						</p>
					</div>
				</>
			)}
			{!isSuccess && !isError && (
				<div className={styles.content} data-flx="connection.connection-callback-page.content--3">
					<h1 className={styles.title} data-flx="connection.connection-callback-page.title--3">
						<Trans>Invalid status</Trans>
					</h1>
					<p className={styles.description} data-flx="connection.connection-callback-page.description--3">
						<Trans>An invalid status was provided. You can now close this tab and return to the app.</Trans>
					</p>
				</div>
			)}
		</div>
	);
});

export default ConnectionCallbackPage;
