// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import styles from '@app/features/app/components/layout/app_layout/nagbars/DesktopNotificationNagbar.module.css';
import {Nagbar} from '@app/features/app/components/layout/Nagbar';
import {NagbarButton} from '@app/features/app/components/layout/NagbarButton';
import {NagbarContent} from '@app/features/app/components/layout/NagbarContent';
import {NAGBAR_TONES, NagbarToneKind} from '@app/features/app/components/layout/NagbarTones';
import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {ENABLE_NOTIFICATIONS_DESCRIPTOR, OKAY_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {usePushSubscriptions} from '@app/features/notification/hooks/usePushSubscriptions';
import * as NotificationUtils from '@app/features/notification/utils/NotificationUtils';
import * as PushSubscriptionService from '@app/features/platform/push/PushSubscriptionService';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as NagbarCommands from '@app/features/ui/commands/NagbarCommands';
import {isPwaOnMobileOrTablet} from '@app/features/ui/utils/PwaUtils';
import {formatUserSettingsPath} from '@app/features/user/components/settings_utils/SettingsConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const NOTIFICATIONS_NOT_SUPPORTED_DESCRIPTOR = msg({
	message: 'Notifications not supported',
	comment: 'Modal title shown when the current device/browser cannot show notifications.',
});
const DEVICE_NOTIFICATIONS_UNSUPPORTED_DESCRIPTOR = msg({
	message: 'This device does not support notifications.',
	comment: 'Modal body shown when the current device/browser cannot show notifications.',
});
const DISABLE_DESKTOP_NOTIFICATIONS_DESCRIPTOR = msg({
	message: 'Disable desktop notifications?',
	comment: 'Confirmation modal title shown before dismissing the desktop-notification nagbar.',
});
const ENABLE_NOTIFICATIONS_MENTIONS_REMINDER_DESCRIPTOR = msg({
	message: "Enable notifications to stay updated on mentions when you're away from the app.",
	comment: 'Confirmation modal body explaining why notifications are useful before dismissing the nagbar.',
});
const ENABLE_DESKTOP_NOTIFICATIONS_LATER_DESCRIPTOR = msg({
	message: 'If you dismiss this, you can always enable desktop notifications later under {notificationSettingsPath}.',
	comment:
		'Confirmation modal body shown before dismissing the desktop-notification nagbar. {notificationSettingsPath} is the localized settings path.',
});
const DISMISS_ANYWAY_DESCRIPTOR = msg({
	message: 'Dismiss anyway',
	comment: 'Secondary button in the desktop-notification nagbar dismissal confirmation. Dismisses the nagbar.',
});
const ENABLE_PUSH_NOTIFICATIONS_MESSAGE_DESCRIPTOR = msg({
	message: "Enable push notifications for the {productName} app to keep receiving messages when it's backgrounded.",
	comment: 'Nagbar body shown in mobile/PWA contexts. {productName} is the app name.',
});
const ENABLE_DESKTOP_NOTIFICATIONS_MESSAGE_DESCRIPTOR = msg({
	message: 'Enable desktop notifications to stay updated on new messages.',
	comment: 'Nagbar body shown in desktop/web contexts when notification permission is not yet enabled.',
});
export const DesktopNotificationNagbar = observer(({isMobile}: {isMobile: boolean}) => {
	const {i18n} = useLingui();
	const isPwaMobile = isPwaOnMobileOrTablet();
	const {refresh} = usePushSubscriptions(isPwaMobile);
	const notificationSettingsPath = formatUserSettingsPath(i18n, 'notifications', 'notifications');
	const handleEnable = () => {
		if (typeof Notification === 'undefined') {
			ModalCommands.push(
				modal(() => (
					<ConfirmModal
						title={i18n._(NOTIFICATIONS_NOT_SUPPORTED_DESCRIPTOR)}
						description={
							<p data-flx="app.app-layout.nagbars.desktop-notification-nagbar.handle-enable.p">
								{i18n._(DEVICE_NOTIFICATIONS_UNSUPPORTED_DESCRIPTOR)}
							</p>
						}
						primaryText={i18n._(OKAY_DESCRIPTOR)}
						primaryVariant="primary"
						secondaryText={false}
						onPrimary={() => {
							NagbarCommands.dismissNagbar('desktopNotificationDismissed');
						}}
						data-flx="app.app-layout.nagbars.desktop-notification-nagbar.handle-enable.confirm-modal"
					/>
				)),
			);
			return;
		}
		const permissionPromise: Promise<NotificationPermission> =
			Notification.permission === 'granted' ? Promise.resolve('granted') : Notification.requestPermission();
		void (async () => {
			try {
				const permission = await permissionPromise;
				NotificationUtils.handlePermissionResult(i18n, permission);
				if (permission === 'granted' && isPwaMobile) {
					await PushSubscriptionService.registerPushSubscription();
					await refresh();
				}
			} finally {
				NagbarCommands.dismissNagbar('desktopNotificationDismissed');
			}
		})();
	};
	const handleDismiss = () => {
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={i18n._(DISABLE_DESKTOP_NOTIFICATIONS_DESCRIPTOR)}
					description={
						<>
							<p data-flx="app.app-layout.nagbars.desktop-notification-nagbar.handle-dismiss.p">
								{i18n._(ENABLE_NOTIFICATIONS_MENTIONS_REMINDER_DESCRIPTOR)}
							</p>
							<p
								className={styles.description}
								data-flx="app.app-layout.nagbars.desktop-notification-nagbar.handle-dismiss.description"
							>
								{i18n._(ENABLE_DESKTOP_NOTIFICATIONS_LATER_DESCRIPTOR, {notificationSettingsPath})}
							</p>
						</>
					}
					primaryText={i18n._(ENABLE_NOTIFICATIONS_DESCRIPTOR)}
					primaryVariant="primary"
					secondaryText={i18n._(DISMISS_ANYWAY_DESCRIPTOR)}
					onPrimary={() => {
						handleEnable();
					}}
					onSecondary={() => {
						NagbarCommands.dismissNagbar('desktopNotificationDismissed');
					}}
					data-flx="app.app-layout.nagbars.desktop-notification-nagbar.handle-dismiss.confirm-modal"
				/>
			)),
		);
	};
	return (
		<Nagbar
			isMobile={isMobile}
			backgroundColor={NAGBAR_TONES[NagbarToneKind.BRAND].backgroundColor}
			textColor={NAGBAR_TONES[NagbarToneKind.BRAND].textColor}
			dismissible
			onDismiss={handleDismiss}
			data-flx="app.app-layout.nagbars.desktop-notification-nagbar.nagbar"
		>
			<NagbarContent
				isMobile={isMobile}
				onDismiss={handleDismiss}
				message={
					isPwaMobile
						? i18n._(ENABLE_PUSH_NOTIFICATIONS_MESSAGE_DESCRIPTOR, {productName: PRODUCT_NAME})
						: i18n._(ENABLE_DESKTOP_NOTIFICATIONS_MESSAGE_DESCRIPTOR)
				}
				actions={
					<NagbarButton
						isMobile={isMobile}
						onClick={handleEnable}
						data-flx="app.app-layout.nagbars.desktop-notification-nagbar.nagbar-button.enable"
					>
						{i18n._(ENABLE_NOTIFICATIONS_DESCRIPTOR)}
					</NagbarButton>
				}
				data-flx="app.app-layout.nagbars.desktop-notification-nagbar.nagbar-content"
			/>
		</Nagbar>
	);
});
