// SPDX-License-Identifier: AGPL-3.0-or-later

import * as AccessibilityCommands from '@app/features/accessibility/commands/AccessibilityCommands';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {useNativePlatform} from '@app/features/app/hooks/useNativePlatform';
import {CANCEL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {ShareThemeModal} from '@app/features/theme/components/modals/ShareThemeModal';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import Theme from '@app/features/theme/state/Theme';
import ThemeLibrary from '@app/features/theme/state/ThemeLibrary';
import styles from '@app/features/theme_studio/sections/ThemeStudioSettingsSection.module.css';
import {showThemeStudioErrorModal} from '@app/features/theme_studio/utils/ThemeStudioErrorModalUtils';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {
	getCachedDesktopWindowBehaviorSettings,
	getDesktopWindowBehaviorSettings,
	relaunchDesktopApp,
	setDesktopWindowBehaviorSettings,
} from '@app/features/ui/utils/DesktopWindowBehaviorUtils';
import {isDesktop} from '@app/features/ui/utils/NativeUtils';
import type {DesktopWindowBehaviorSettings} from '@app/types/electron.d';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {ArrowCounterClockwiseIcon, ShareNetworkIcon, TrashIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useState} from 'react';
import {broadcastThemeStudioMessage} from '../state/ThemeStudioBroadcast';
import {StudioButton} from '../ui/StudioButton';

const SYNC_CUSTOM_CSS_DESCRIPTOR = msg({
	message: 'Sync custom CSS?',
	comment: 'Confirmation prompt in the theme studio settings section.',
});
const SYNC_CUSTOM_CSS_DESCRIPTION_DESCRIPTOR = msg({
	message:
		'Only the CSS text is synced across devices. Uploaded assets, desktop file references, fonts, and media stay on this device.',
	comment: 'Label in the theme studio settings section.',
});
const SYNC_CSS_TEXT_DESCRIPTOR = msg({
	message: 'Sync CSS text',
	comment: 'Short label in the theme studio settings section. Keep it concise.',
});
const RESTART_DESCRIPTOR = msg({
	message: 'Restart {productName}?',
	comment: 'Confirmation title after changing desktop transparency in Theme Studio.',
});
const TRANSPARENCY_RESTART_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Desktop window transparency changes apply after restarting the app.',
	comment: 'Description text in the theme studio settings section.',
});
const RESTART_NOW_DESCRIPTOR = msg({
	message: 'Restart now',
	comment: 'Short label in the theme studio settings section. Keep it concise.',
});
const LATER_DESCRIPTOR = msg({
	message: 'Later',
	comment: 'Short label in the theme studio settings section. Keep it concise.',
});
const NO_THEME_OVERRIDES_TO_SHARE_DESCRIPTOR = msg({
	message: "You don't have any custom theme overrides to share yet.",
	comment: 'Description text in the theme studio settings section.',
});
const RESET_CUSTOM_THEME_DATA_DESCRIPTOR = msg({
	message: 'Reset custom theme data?',
	comment: 'Confirmation prompt in the theme studio settings section.',
});
const RESET_CUSTOM_THEME_DATA_DESCRIPTION_DESCRIPTOR = msg({
	message: 'This clears quick CSS, imported themes, uploaded assets, and local file references on this device.',
	comment: 'Description text in the theme studio settings section.',
});
const RESET_DATA_DESCRIPTOR = msg({
	message: 'Reset data',
	comment: 'Button or menu action label in the theme studio settings section. Keep it concise.',
});
const SYNC_CUSTOM_CSS_ACROSS_DEVICES_DESCRIPTOR = msg({
	message: 'Sync custom CSS across devices',
	comment: 'Label in the theme studio settings section.',
});
const SYNC_BASE_THEME_ACROSS_DEVICES_DESCRIPTOR = msg({
	message: 'Sync base theme across devices',
	comment: 'Label in the theme studio settings section.',
});
const ALLOW_TRANSPARENT_DESKTOP_WINDOW_DESCRIPTOR = msg({
	message: 'Allow transparent desktop window',
	comment: 'Label in the theme studio settings section.',
});
const TOKEN_OVERRIDES_CLEARED_DESCRIPTOR = msg({
	message: 'Token overrides cleared.',
	comment: 'Short label in the theme studio settings section. Keep it concise. Keep the tone plain and specific.',
});
export const SettingsSection: React.FC = observer(() => {
	const {i18n} = useLingui();
	const {isLinux, isWindows} = useNativePlatform();
	const [desktopWindowBehavior, setDesktopWindowBehavior] = useState<DesktopWindowBehaviorSettings | null>(
		getCachedDesktopWindowBehaviorSettings(),
	);
	const [desktopBusy, setDesktopBusy] = useState(false);
	useEffect(() => {
		if (!isDesktop()) return;
		void getDesktopWindowBehaviorSettings().then((settings) => setDesktopWindowBehavior(settings));
	}, []);
	const handleSyncCustomCss = useCallback(
		(value: boolean) => {
			if (!value) {
				Accessibility.updateSettings({customThemeCssSyncAcrossDevices: false});
				return;
			}
			ModalCommands.push(
				modal(() => (
					<ConfirmModal
						title={i18n._(SYNC_CUSTOM_CSS_DESCRIPTOR)}
						description={i18n._(SYNC_CUSTOM_CSS_DESCRIPTION_DESCRIPTOR)}
						primaryText={i18n._(SYNC_CSS_TEXT_DESCRIPTOR)}
						secondaryText={i18n._(CANCEL_DESCRIPTOR)}
						onPrimary={() => Accessibility.updateSettings({customThemeCssSyncAcrossDevices: true})}
						data-flx="theme-studio.settings-section.handle-sync-custom-css.confirm-modal"
					/>
				)),
			);
		},
		[i18n],
	);
	const handleAllowTransparency = useCallback(
		(value: boolean) => {
			if (!desktopWindowBehavior) return;
			setDesktopBusy(true);
			void setDesktopWindowBehaviorSettings({allowTransparency: value})
				.then((nextSettings) => {
					if (nextSettings) {
						setDesktopWindowBehavior(nextSettings);
					}
					ModalCommands.push(
						modal(() => (
							<ConfirmModal
								title={i18n._(RESTART_DESCRIPTOR, {productName: PRODUCT_NAME})}
								description={i18n._(TRANSPARENCY_RESTART_DESCRIPTION_DESCRIPTOR)}
								primaryText={i18n._(RESTART_NOW_DESCRIPTOR)}
								secondaryText={i18n._(LATER_DESCRIPTOR)}
								onPrimary={async () => relaunchDesktopApp()}
								data-flx="theme-studio.settings-section.handle-allow-transparency.confirm-modal"
							/>
						)),
					);
				})
				.finally(() => setDesktopBusy(false));
		},
		[desktopWindowBehavior, i18n],
	);
	const handleShareTheme = useCallback(() => {
		const css = [ThemeLibrary.activeThemeCss, Accessibility.customThemeCss ?? '']
			.map((value) => value.trim())
			.filter(Boolean)
			.join('\n\n');
		if (!css.trim()) {
			showThemeStudioErrorModal(
				i18n,
				() => i18n._(NO_THEME_OVERRIDES_TO_SHARE_DESCRIPTOR),
				'theme-studio.settings-section.share-theme-empty-error-modal',
			);
			return;
		}
		ModalCommands.push(
			modal(() => (
				<ShareThemeModal themeCss={css} data-flx="theme-studio.settings-section.handle-share-theme.share-theme-modal" />
			)),
		);
	}, [i18n]);
	const handleResetData = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={i18n._(RESET_CUSTOM_THEME_DATA_DESCRIPTOR)}
					description={i18n._(RESET_CUSTOM_THEME_DATA_DESCRIPTION_DESCRIPTOR)}
					primaryText={i18n._(RESET_DATA_DESCRIPTOR)}
					primaryVariant="danger"
					secondaryText={i18n._(CANCEL_DESCRIPTOR)}
					onPrimary={() => {
						AccessibilityCommands.update({customThemeCss: null});
						void ThemeLibrary.resetLibrary().then(() => {
							broadcastThemeStudioMessage({type: 'themeLibrary', revision: ThemeLibrary.revision});
						});
						broadcastThemeStudioMessage({type: 'customThemeCss', value: null});
					}}
					data-flx="theme-studio.settings-section.handle-reset-data.confirm-modal"
				/>
			)),
		);
	}, [i18n]);
	return (
		<div className={styles.section} data-flx="theme-studio.settings-section.section">
			<div className={styles.group} data-flx="theme-studio.settings-section.group">
				<h3 className={styles.groupTitle} data-flx="theme-studio.settings-section.group-title">
					<Trans>Sync</Trans>
				</h3>
				<div className={styles.card} data-flx="theme-studio.settings-section.card">
					<div className={styles.row} data-flx="theme-studio.settings-section.row">
						<div className={styles.rowText} data-flx="theme-studio.settings-section.row-text">
							<p className={styles.rowLabel} data-flx="theme-studio.settings-section.row-label">
								<Trans>Sync custom CSS across devices</Trans>
							</p>
							<p className={styles.rowDescription} data-flx="theme-studio.settings-section.row-description">
								<Trans>
									Only the CSS text is synced. Uploaded assets and local file references stay on this device.
								</Trans>
							</p>
						</div>
						<div className={styles.rowControl} data-flx="theme-studio.settings-section.row-control">
							<Switch
								ariaLabel={i18n._(SYNC_CUSTOM_CSS_ACROSS_DEVICES_DESCRIPTOR)}
								value={Accessibility.customThemeCssSyncAcrossDevices}
								onChange={handleSyncCustomCss}
								data-flx="theme-studio.settings-section.switch.sync-custom-css"
							/>
						</div>
					</div>
					<div className={styles.row} data-flx="theme-studio.settings-section.row--2">
						<div className={styles.rowText} data-flx="theme-studio.settings-section.row-text--2">
							<p className={styles.rowLabel} data-flx="theme-studio.settings-section.row-label--2">
								<Trans>Sync base theme across devices</Trans>
							</p>
							<p className={styles.rowDescription} data-flx="theme-studio.settings-section.row-description--2">
								<Trans>
									Mirror of the Appearance tab toggle for light/dark/coal preference. Disable for a per-device base
									theme.
								</Trans>
							</p>
						</div>
						<div className={styles.rowControl} data-flx="theme-studio.settings-section.row-control--2">
							<Switch
								ariaLabel={i18n._(SYNC_BASE_THEME_ACROSS_DEVICES_DESCRIPTOR)}
								value={Theme.syncAcrossDevices}
								disabled
								onChange={() => {}}
								data-flx="theme-studio.settings-section.switch"
							/>
						</div>
					</div>
				</div>
			</div>

			{isDesktop() && desktopWindowBehavior ? (
				<div className={styles.group} data-flx="theme-studio.settings-section.group--2">
					<h3 className={styles.groupTitle} data-flx="theme-studio.settings-section.group-title--2">
						<Trans>Desktop</Trans>
					</h3>
					<div className={styles.card} data-flx="theme-studio.settings-section.card--2">
						<div className={styles.row} data-flx="theme-studio.settings-section.row--3">
							<div className={styles.rowText} data-flx="theme-studio.settings-section.row-text--3">
								<p className={styles.rowLabel} data-flx="theme-studio.settings-section.row-label--3">
									<Trans>Allow transparent desktop window</Trans>
								</p>
								<p className={styles.rowDescription} data-flx="theme-studio.settings-section.row-description--3">
									<Trans>Lets custom CSS reveal transparent window regions after restarting the desktop app.</Trans>
								</p>
							</div>
							<div className={styles.rowControl} data-flx="theme-studio.settings-section.row-control--3">
								<Switch
									ariaLabel={i18n._(ALLOW_TRANSPARENT_DESKTOP_WINDOW_DESCRIPTOR)}
									value={desktopWindowBehavior.allowTransparency}
									disabled={desktopBusy}
									onChange={handleAllowTransparency}
									data-flx="theme-studio.settings-section.switch.allow-transparency"
								/>
							</div>
						</div>
						{isLinux && desktopWindowBehavior.allowTransparency ? (
							<div className={styles.notice} data-flx="theme-studio.settings-section.linux-transparency-notice">
								<ul className={styles.noticeList} data-flx="theme-studio.settings-section.notice-list--linux">
									<li data-flx="theme-studio.theme-studio-settings-section.settings-section.li">
										<Trans>
											Resize, maximize, and window snapping depend on the compositor. Wayland without client-side
											decorations may not provide handles. Launch with --ozone-platform=x11 if the window feels stuck.
										</Trans>
									</li>
									<li data-flx="theme-studio.theme-studio-settings-section.settings-section.li--2">
										<Trans>
											Hardware acceleration can cause rendering glitches with transparent windows. Try --disable-gpu if
											you see flicker or smearing.
										</Trans>
									</li>
									<li data-flx="theme-studio.theme-studio-settings-section.settings-section.li--3">
										<Trans>
											Docked DevTools and windows partially off-screen can render a gray background instead of staying
											transparent.
										</Trans>
									</li>
								</ul>
							</div>
						) : null}
						{isWindows && desktopWindowBehavior.allowTransparency ? (
							<div className={styles.notice} data-flx="theme-studio.settings-section.windows-transparency-notice">
								<ul className={styles.noticeList} data-flx="theme-studio.settings-section.notice-list--windows">
									<li data-flx="theme-studio.theme-studio-settings-section.settings-section.li--4">
										<Trans>Aero Snap and Snap Layouts can stop working while transparency is enabled.</Trans>
									</li>
									<li data-flx="theme-studio.theme-studio-settings-section.settings-section.li--5">
										<Trans>
											Windows cut off at the top or bottom of the screen, and docked DevTools, can render a gray
											background instead of staying transparent.
										</Trans>
									</li>
									<li data-flx="theme-studio.theme-studio-settings-section.settings-section.li--6">
										<Trans>
											Text is rendered with grayscale antialiasing instead of ClearType while transparency is enabled,
											so it will look softer.
										</Trans>
									</li>
								</ul>
							</div>
						) : null}
					</div>
				</div>
			) : null}

			<div className={styles.group} data-flx="theme-studio.settings-section.group--3">
				<h3 className={styles.groupTitle} data-flx="theme-studio.settings-section.group-title--3">
					<Trans>Share</Trans>
				</h3>
				<div className={styles.card} data-flx="theme-studio.settings-section.card--3">
					<div className={styles.row} data-flx="theme-studio.settings-section.row--4">
						<div className={styles.rowText} data-flx="theme-studio.settings-section.row-text--4">
							<p className={styles.rowLabel} data-flx="theme-studio.settings-section.row-label--4">
								<Trans>Share active custom theme</Trans>
							</p>
							<p className={styles.rowDescription} data-flx="theme-studio.settings-section.row-description--4">
								<Trans>Generate a shareable link or copy your combined custom CSS.</Trans>
							</p>
						</div>
						<div className={styles.rowControl} data-flx="theme-studio.settings-section.row-control--4">
							<StudioButton
								variant="primary"
								leadingIcon={
									<ShareNetworkIcon
										size={remFromPx(13)}
										weight="bold"
										data-flx="theme-studio.settings-section.share-network-icon"
									/>
								}
								onClick={handleShareTheme}
								data-flx="theme-studio.settings-section.studio-button.share-theme"
							>
								<Trans>Share theme</Trans>
							</StudioButton>
						</div>
					</div>
				</div>
			</div>

			<div className={styles.group} data-flx="theme-studio.settings-section.group--4">
				<h3 className={styles.groupTitle} data-flx="theme-studio.settings-section.group-title--4">
					<Trans>Danger zone</Trans>
				</h3>
				<div className={`${styles.card} ${styles.dangerCard}`} data-flx="theme-studio.settings-section.card--4">
					<div className={styles.row} data-flx="theme-studio.settings-section.row--5">
						<div className={styles.rowText} data-flx="theme-studio.settings-section.row-text--5">
							<p className={styles.rowLabel} data-flx="theme-studio.settings-section.row-label--5">
								<Trans>Reset custom theme data</Trans>
							</p>
							<p className={styles.rowDescription} data-flx="theme-studio.settings-section.row-description--5">
								<Trans>
									Clears quick CSS, imported themes, uploaded assets, and local file references on this device.
								</Trans>
							</p>
						</div>
						<div className={styles.rowControl} data-flx="theme-studio.settings-section.row-control--5">
							<StudioButton
								variant="dangerSolid"
								leadingIcon={
									<TrashIcon size={remFromPx(13)} weight="bold" data-flx="theme-studio.settings-section.trash-icon" />
								}
								onClick={handleResetData}
								data-flx="theme-studio.settings-section.studio-button.reset-data"
							>
								<Trans>Reset data</Trans>
							</StudioButton>
						</div>
					</div>
					<div className={styles.row} data-flx="theme-studio.settings-section.row--6">
						<div className={styles.rowText} data-flx="theme-studio.settings-section.row-text--6">
							<p className={styles.rowLabel} data-flx="theme-studio.settings-section.row-label--6">
								<Trans>Clear all token overrides</Trans>
							</p>
							<p className={styles.rowDescription} data-flx="theme-studio.settings-section.row-description--6">
								<Trans>Removes every per-variable override but keeps your library and assets intact.</Trans>
							</p>
						</div>
						<div className={styles.rowControl} data-flx="theme-studio.settings-section.row-control--6">
							<StudioButton
								variant="danger"
								leadingIcon={
									<ArrowCounterClockwiseIcon
										size={13}
										weight="bold"
										data-flx="theme-studio.settings-section.arrow-counter-clockwise-icon"
									/>
								}
								onClick={() => {
									AccessibilityCommands.update({customThemeCss: null});
									broadcastThemeStudioMessage({type: 'customThemeCss', value: null});
									ToastCommands.success(i18n._(TOKEN_OVERRIDES_CLEARED_DESCRIPTOR));
								}}
								data-flx="theme-studio.settings-section.studio-button.update"
							>
								<Trans>Clear overrides</Trans>
							</StudioButton>
						</div>
					</div>
				</div>
			</div>
		</div>
	);
});
