// SPDX-License-Identifier: AGPL-3.0-or-later

import {INVALID_STATUS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {useLocation} from '@app/features/platform/components/router/RouterReact';
import styles from '@app/features/premium/components/pages/PremiumCallbackPage.module.css';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CheckCircleIcon, XCircleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';

const MATURE_CONTENT_CHECK_SENT_DESCRIPTOR = msg({
	message: 'Mature content check sent',
	comment: 'Title on the mature content check callback page after a successful check submission.',
});
const MATURE_CONTENT_CHECK_SENT_BODY_DESCRIPTOR = msg({
	message:
		'Your credit card has been checked. If the card is valid, mature content will unlock shortly. You can close this tab and return to the app.',
	comment: 'Body copy on the mature content check callback page after a successful check submission.',
});
const MATURE_CONTENT_CHECK_CANCELED_DESCRIPTOR = msg({
	message: 'Mature content check canceled',
	comment: 'Title on the mature content check callback page after the check was canceled.',
});
const MATURE_CONTENT_CHECK_CANCELED_BODY_DESCRIPTOR = msg({
	message: 'The mature content check was canceled. You can close this tab and try again from the app.',
	comment: 'Body copy on the mature content check callback page after the check was canceled.',
});
const INVALID_STATUS_BODY_DESCRIPTOR = msg({
	message: 'An invalid status was provided. You can close this tab and return to the app.',
	comment: 'Body copy on the mature content check callback page when the callback status is invalid.',
});
const MatureContentCheckCallbackPage = observer(() => {
	const {i18n} = useLingui();
	const location = useLocation();
	const queryParams = new URLSearchParams(location.search);
	const status = queryParams.get('status');
	const isSuccess = status === 'success';
	const isCancel = status === 'cancel';
	return (
		<div className={styles.container} data-flx="auth.mature-content-check-callback-page.container">
			{isSuccess && (
				<>
					<CheckCircleIcon
						className={styles.successIcon}
						weight="fill"
						data-flx="auth.mature-content-check-callback-page.success-icon"
					/>
					<div className={styles.content} data-flx="auth.mature-content-check-callback-page.content">
						<h1 className={styles.title} data-flx="auth.mature-content-check-callback-page.title">
							{i18n._(MATURE_CONTENT_CHECK_SENT_DESCRIPTOR)}
						</h1>
						<p className={styles.description} data-flx="auth.mature-content-check-callback-page.description">
							{i18n._(MATURE_CONTENT_CHECK_SENT_BODY_DESCRIPTOR)}
						</p>
					</div>
				</>
			)}
			{isCancel && (
				<>
					<XCircleIcon
						className={styles.errorIcon}
						weight="fill"
						data-flx="auth.mature-content-check-callback-page.error-icon"
					/>
					<div className={styles.content} data-flx="auth.mature-content-check-callback-page.content--2">
						<h1 className={styles.title} data-flx="auth.mature-content-check-callback-page.title--2">
							{i18n._(MATURE_CONTENT_CHECK_CANCELED_DESCRIPTOR)}
						</h1>
						<p className={styles.description} data-flx="auth.mature-content-check-callback-page.description--2">
							{i18n._(MATURE_CONTENT_CHECK_CANCELED_BODY_DESCRIPTOR)}
						</p>
					</div>
				</>
			)}
			{!isSuccess && !isCancel && (
				<div className={styles.content} data-flx="auth.mature-content-check-callback-page.content--3">
					<h1 className={styles.title} data-flx="auth.mature-content-check-callback-page.title--3">
						{i18n._(INVALID_STATUS_DESCRIPTOR)}
					</h1>
					<p className={styles.description} data-flx="auth.mature-content-check-callback-page.description--3">
						{i18n._(INVALID_STATUS_BODY_DESCRIPTOR)}
					</p>
				</div>
			)}
		</div>
	);
});

export default MatureContentCheckCallbackPage;
