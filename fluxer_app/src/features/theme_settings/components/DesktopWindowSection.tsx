// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import {useNativePlatform} from '@app/features/app/hooks/useNativePlatform';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import styles from '@app/features/theme_settings/ThemesTab.module.css';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {
	getCachedDesktopWindowBehaviorSettings,
	getDesktopWindowBehaviorSettings,
	relaunchDesktopApp,
	setDesktopWindowBehaviorSettings,
} from '@app/features/ui/utils/DesktopWindowBehaviorUtils';
import {DESKTOP_WINDOW_SECTION_DESCRIPTOR} from '@app/features/user/components/settings_utils/section_registry/ThemesSections';
import type {DesktopWindowBehaviorSettings} from '@app/types/electron.d';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useState} from 'react';

const ALLOW_TRANSPARENT_DESKTOP_WINDOW_DESCRIPTOR = msg({
	message: 'Allow transparent desktop window',
	comment: 'Switch label in Settings > Themes on the desktop app.',
});
const ALLOW_TRANSPARENT_DESKTOP_WINDOW_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Lets custom CSS reveal transparent window regions after restarting the desktop app.',
	comment: 'Description of the "Allow transparent desktop window" switch in Settings > Themes.',
});
const RESTART_DESCRIPTOR = msg({
	message: 'Restart {productName}?',
	comment: 'Confirmation title after changing desktop transparency in Settings > Themes.',
});
const TRANSPARENCY_RESTART_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Desktop window transparency changes apply after restarting the app.',
	comment: 'Confirmation text after changing desktop transparency in Settings > Themes.',
});
const RESTART_NOW_DESCRIPTOR = msg({
	message: 'Restart now',
	comment: 'Confirm button that restarts the desktop app.',
});
const LATER_DESCRIPTOR = msg({
	message: 'Later',
	comment: 'Button that closes the restart prompt without restarting.',
});

export const DesktopWindowSection: React.FC = observer(() => {
	const {i18n} = useLingui();
	const {isLinux, isWindows} = useNativePlatform();
	const [settings, setSettings] = useState<DesktopWindowBehaviorSettings | null>(
		getCachedDesktopWindowBehaviorSettings(),
	);
	const [busy, setBusy] = useState(false);
	useEffect(() => {
		void getDesktopWindowBehaviorSettings().then(setSettings);
	}, []);
	if (!settings) {
		return null;
	}
	const handleChange = (value: boolean) => {
		setBusy(true);
		void setDesktopWindowBehaviorSettings({allowTransparency: value})
			.then((nextSettings) => {
				if (nextSettings) {
					setSettings(nextSettings);
				}
				ModalCommands.push(
					modal(() => (
						<ConfirmModal
							title={i18n._(RESTART_DESCRIPTOR, {productName: RuntimeConfig.productName})}
							description={i18n._(TRANSPARENCY_RESTART_DESCRIPTION_DESCRIPTOR)}
							primaryText={i18n._(RESTART_NOW_DESCRIPTOR)}
							secondaryText={i18n._(LATER_DESCRIPTOR)}
							onPrimary={async () => relaunchDesktopApp()}
							data-flx="theme-settings.desktop-window-section.handle-change.confirm-modal"
						/>
					)),
				);
			})
			.finally(() => setBusy(false));
	};
	return (
		<SettingsSection
			id="transparent-window"
			title={i18n._(DESKTOP_WINDOW_SECTION_DESCRIPTOR)}
			data-flx="theme-settings.desktop-window-section.transparent-window"
		>
			<Switch
				label={i18n._(ALLOW_TRANSPARENT_DESKTOP_WINDOW_DESCRIPTOR)}
				description={i18n._(ALLOW_TRANSPARENT_DESKTOP_WINDOW_DESCRIPTION_DESCRIPTOR)}
				value={settings.allowTransparency}
				disabled={busy}
				onChange={handleChange}
				data-flx="theme-settings.desktop-window-section.switch.change"
			/>
			{isLinux && settings.allowTransparency ? (
				<ul className={styles.notice} data-flx="theme-settings.desktop-window-section.notice">
					<li data-flx="theme-settings.desktop-window-section.li">
						<Trans>
							Resizing, maximizing, and window snapping depend on the compositor. Wayland without client-side
							decorations may not provide handles. Launch with --ozone-platform=x11 if the window feels stuck.
						</Trans>
					</li>
					<li data-flx="theme-settings.desktop-window-section.li--2">
						<Trans>
							Hardware acceleration can cause rendering glitches with transparent windows. Try --disable-gpu if you see
							flicker or smearing.
						</Trans>
					</li>
					<li data-flx="theme-settings.desktop-window-section.li--3">
						<Trans>
							Docked DevTools and windows partially off-screen can render a gray background instead of staying
							transparent.
						</Trans>
					</li>
				</ul>
			) : null}
			{isWindows && settings.allowTransparency ? (
				<ul className={styles.notice} data-flx="theme-settings.desktop-window-section.notice--2">
					<li data-flx="theme-settings.desktop-window-section.li--4">
						<Trans>Aero Snap and Snap Layouts can stop working while transparency is enabled.</Trans>
					</li>
					<li data-flx="theme-settings.desktop-window-section.li--5">
						<Trans>
							Windows cut off at the top or bottom of the screen, and docked DevTools, can render a gray background
							instead of staying transparent.
						</Trans>
					</li>
					<li data-flx="theme-settings.desktop-window-section.li--6">
						<Trans>
							Text is rendered with grayscale antialiasing instead of ClearType while transparency is enabled, so it
							will look softer.
						</Trans>
					</li>
				</ul>
			) : null}
		</SettingsSection>
	);
});
