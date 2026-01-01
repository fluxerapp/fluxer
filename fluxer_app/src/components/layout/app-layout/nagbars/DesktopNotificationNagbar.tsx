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

import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as NagbarActionCreators from '~/actions/NagbarActionCreators';
import {Nagbar} from '~/components/layout/Nagbar';
import {NagbarButton} from '~/components/layout/NagbarButton';
import {NagbarContent} from '~/components/layout/NagbarContent';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {usePushSubscriptions} from '~/hooks/usePushSubscriptions';
import * as PushSubscriptionService from '~/services/push/PushSubscriptionService';
import * as NotificationUtils from '~/utils/NotificationUtils';
import {isPwaOnMobileOrTablet} from '~/utils/PwaUtils';
import styles from './DesktopNotificationNagbar.module.css';

export const DesktopNotificationNagbar = observer(({isMobile}: {isMobile: boolean}) => {
	const {i18n, t} = useLingui();
	const isPwaMobile = isPwaOnMobileOrTablet();
	const {refresh} = usePushSubscriptions(isPwaMobile);

	const handleEnable = () => {
		if (isPwaMobile) {
			void (async () => {
				await PushSubscriptionService.registerPushSubscription();
				await refresh();
			})();
		} else if (typeof Notification === 'undefined') {
			ModalActionCreators.push(
				modal(() => (
					<ConfirmModal
						title={t`Notifications Not Supported`}
						description={
							<p>
								<Trans>Your browser does not support desktop notifications.</Trans>
							</p>
						}
						primaryText={t`OK`}
						primaryVariant="primary"
						secondaryText={false}
						onPrimary={() => {
							NagbarActionCreators.dismissNagbar('desktopNotificationDismissed');
						}}
					/>
				)),
			);
			return;
		} else {
			NotificationUtils.requestPermission(i18n);
		}

		NagbarActionCreators.dismissNagbar('desktopNotificationDismissed');
	};

	const handleDismiss = () => {
		ModalActionCreators.push(
			modal(() => (
				<ConfirmModal
					title={t`Disable Desktop Notifications?`}
					description={
						<>
							<p>
								<Trans>Enable notifications to stay updated on mentions when you're away from the app.</Trans>
							</p>
							<p className={styles.description}>
								<Trans>
									If you dismiss this, you can always enable desktop notifications later under User Settings &gt;
									Notifications.
								</Trans>
							</p>
						</>
					}
					primaryText={t`Enable Notifications`}
					primaryVariant="primary"
					secondaryText={t`Dismiss Anyway`}
					onPrimary={() => {
						handleEnable();
					}}
					onSecondary={() => {
						NagbarActionCreators.dismissNagbar('desktopNotificationDismissed');
					}}
				/>
			)),
		);
	};

	return (
		<Nagbar
			isMobile={isMobile}
			backgroundColor="var(--brand-primary)"
			textColor="var(--text-on-brand-primary)"
			dismissible
			onDismiss={handleDismiss}
		>
			<NagbarContent
				isMobile={isMobile}
				message={
					isPwaMobile ? (
						<Trans>
							Enable push notifications for this installed PWA to keep receiving messages when the browser is
							backgrounded.
						</Trans>
					) : (
						<Trans>Enable desktop notifications to stay updated on new messages.</Trans>
					)
				}
				actions={
					<>
						{isMobile && (
							<NagbarButton isMobile={isMobile} onClick={handleDismiss}>
								<Trans>Dismiss</Trans>
							</NagbarButton>
						)}
						<NagbarButton isMobile={isMobile} onClick={handleEnable}>
							<Trans>Enable Notifications</Trans>
						</NagbarButton>
					</>
				}
			/>
		</Nagbar>
	);
});
