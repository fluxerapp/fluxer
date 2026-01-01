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

import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type {FC} from 'react';
import * as NotificationActionCreators from '~/actions/NotificationActionCreators';
import {Switch} from '~/components/form/Switch';
import {usePushSubscriptions} from '~/hooks/usePushSubscriptions';
import * as PushSubscriptionService from '~/services/push/PushSubscriptionService';
import {isDesktop} from '~/utils/NativeUtils';
import * as NotificationUtils from '~/utils/NotificationUtils';
import {isPwaOnMobileOrTablet} from '~/utils/PwaUtils';
import styles from './Notifications.module.css';

interface NotificationsProps {
	browserNotificationsEnabled: boolean;
	unreadMessageBadgeEnabled: boolean;
}

export const Notifications: FC<NotificationsProps> = observer(
	({browserNotificationsEnabled, unreadMessageBadgeEnabled}) => {
		const {i18n, t} = useLingui();

		const handleToggleNotifications = async (value: boolean) => {
			if (value) {
				await NotificationUtils.requestPermission(i18n);
			} else {
				NotificationActionCreators.permissionDenied(i18n, true);
			}
		};

		const handleToggleUnreadBadge = (value: boolean) => {
			NotificationActionCreators.toggleUnreadMessageBadge(value);
		};

		const isPwaMobile = isPwaOnMobileOrTablet();
		const {subscriptions, loading, refresh} = usePushSubscriptions(isPwaMobile);

		const handleRegisterPushSubscription = async () => {
			if (typeof Notification !== 'undefined' && Notification.permission !== 'granted') {
				await NotificationUtils.requestPermission(i18n);
			}

			if (typeof Notification !== 'undefined' && Notification.permission !== 'granted') {
				return;
			}

			await PushSubscriptionService.registerPushSubscription();
			await refresh();
		};

		const handleForgetPushSubscriptions = async () => {
			await PushSubscriptionService.unregisterAllPushSubscriptions();
			await refresh();
		};

		const pushStatusMessage = loading
			? t`Loading push subscriptions…`
			: subscriptions.length > 0
				? t`${subscriptions.length} active subscription(s)`
				: t`No push subscriptions registered yet.`;

		return (
			<div className={styles.container}>
				<h2 className={styles.title}>{t`Notification Settings`}</h2>
				<p className={styles.description}>
					{isDesktop()
						? t`Configure how you receive desktop notifications.`
						: t`Configure how you receive notifications in your browser.`}
				</p>

				<div className={styles.switchesContainer}>
					<Switch
						label={isDesktop() ? t`Enable Desktop Notifications` : t`Enable Browser Notifications`}
						description={
							isDesktop()
								? t`Uses the OS notification center. For per-channel/per-community controls, right-click a community icon and open Notification Settings.`
								: t`Get notified when you receive messages. You may need to allow notifications in your browser settings. For per-channel/per-community controls, right-click a community icon and open Notification Settings.`
						}
						value={browserNotificationsEnabled}
						onChange={handleToggleNotifications}
					/>

					<Switch
						label={t`Enable Unread Message Badge`}
						description={t`Shows a red badge on the app icon when you have unread messages.`}
						value={unreadMessageBadgeEnabled}
						onChange={handleToggleUnreadBadge}
					/>
				</div>

				{isPwaMobile && (
					<div className={styles.pushSection}>
						<div>
							<h3 className={styles.pushHeading}>{t`Push subscriptions for this device`}</h3>
							<p className={styles.pushDescription}>
								{t`Fluxer uses push notifications when installed as a mobile PWA. Registering ensures the gateway can reach your device even when the browser is backgrounded.`}
							</p>
						</div>

						<div className={styles.pushButtons}>
							<button type="button" className={styles.pushButton} onClick={handleRegisterPushSubscription}>
								{subscriptions.length > 0 ? t`Refresh push subscription` : t`Enable push for this device`}
							</button>

							<button
								type="button"
								className={`${styles.pushButton} ${styles.pushButtonSecondary}`}
								onClick={handleForgetPushSubscriptions}
								disabled={subscriptions.length === 0}
							>
								{t`Forget subscriptions`}
							</button>
						</div>

						<p className={styles.pushStatus}>{pushStatusMessage}</p>

						{subscriptions.length > 0 && (
							<ul className={styles.pushList}>
								{subscriptions.map((subscription) => (
									<li key={subscription.subscription_id} className={styles.pushListItem}>
										<span>{subscription.user_agent ?? t`Unknown device`}</span>
										<span>{subscription.subscription_id}</span>
									</li>
								))}
							</ul>
						)}
					</div>
				)}
			</div>
		);
	},
);
