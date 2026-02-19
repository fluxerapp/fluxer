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

import * as UserSettingsActionCreators from '@app/actions/UserSettingsActionCreators';
import * as VoiceSettingsActionCreators from '@app/actions/VoiceSettingsActionCreators';
import {Switch} from '@app/components/form/Switch';
import {SettingsTabContainer, SettingsTabSection} from '@app/components/modals/shared/SettingsTabLayout';
import {WarningAlert} from '@app/components/uikit/warning_alert/WarningAlert';
import NativeWindowStateStore from '@app/stores/NativeWindowStateStore';
import UserSettingsStore from '@app/stores/UserSettingsStore';
import VoiceSettingsStore from '@app/stores/VoiceSettingsStore';
import {getAutostartStatus, setAutostartEnabled} from '@app/utils/AutostartUtils';
import {getNativePlatform, isDesktop, type NativePlatform} from '@app/utils/NativeUtils';
import {Trans} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useLayoutEffect, useState} from 'react';

const AdvancedTab: React.FC = observer(() => {
	const {developerMode} = UserSettingsStore;
	const screenShareHardwareAcceleration = VoiceSettingsStore.getScreenShareHardwareAcceleration();
	const [autostartEnabled, setAutostartEnabledState] = useState(false);
	const [autostartBusy, setAutostartBusy] = useState(false);
	const [platform, setPlatform] = useState<NativePlatform>('unknown');

	useLayoutEffect(() => {
		let mounted = true;

		const initAutostart = async () => {
			if (!isDesktop()) return;

			const detectedPlatform = await getNativePlatform();
			if (!mounted) return;

			setPlatform(detectedPlatform);
			if (detectedPlatform !== 'macos') return;

			setAutostartBusy(true);
			const enabled = await getAutostartStatus();

			if (!mounted) return;

			if (enabled !== null) {
				setAutostartEnabledState(enabled);
			}
			setAutostartBusy(false);
		};

		void initAutostart();

		return () => {
			mounted = false;
		};
	}, []);

	const handleAutostartChange = async (value: boolean) => {
		if (platform !== 'macos') return;
		setAutostartBusy(true);
		const nextState = await setAutostartEnabled(value);
		if (nextState !== null) {
			setAutostartEnabledState(nextState);
		}
		setAutostartBusy(false);
	};

	const showAutostartWarning = platform !== 'unknown' && platform !== 'macos';

	return (
		<SettingsTabContainer>
			{isDesktop() && (
				<SettingsTabSection
					title={<Trans>Desktop Startup</Trans>}
					description={<Trans>Run Fluxer automatically when your computer starts. Or don't. Your choice!</Trans>}
				>
					<Switch
						label={<Trans>Launch Fluxer at Login</Trans>}
						description={<Trans>Applies only to the desktop app on this device.</Trans>}
						value={platform === 'macos' ? autostartEnabled : false}
						disabled={platform !== 'macos' || autostartBusy}
						onChange={handleAutostartChange}
					/>
					{showAutostartWarning && (
						<WarningAlert>
							<Trans>Autostart is coming soon for Windows and Linux. For now, it is only available on macOS.</Trans>
						</WarningAlert>
					)}
				</SettingsTabSection>
			)}
			{isDesktop() && (
				<SettingsTabSection
					title={<Trans>Desktop Window</Trans>}
					description={
						<Trans>Choose what Fluxer remembers about your window between restarts and reloads on this device.</Trans>
					}
				>
					<Switch
						label={<Trans>Remember Size &amp; Position</Trans>}
						description={<Trans>Keep your window dimensions and placement even when you reload the app.</Trans>}
						value={NativeWindowStateStore.rememberSizeAndPosition}
						onChange={NativeWindowStateStore.setRememberSizeAndPosition}
					/>
					<Switch
						label={<Trans>Restore Maximized</Trans>}
						description={<Trans>Reopen in maximized mode if that&rsquo;s how you last used Fluxer.</Trans>}
						value={NativeWindowStateStore.rememberMaximized}
						onChange={NativeWindowStateStore.setRememberMaximized}
					/>
					<Switch
						label={<Trans>Restore Fullscreen</Trans>}
						description={<Trans>Return to fullscreen automatically when you had it enabled last time.</Trans>}
						value={NativeWindowStateStore.rememberFullscreen}
						onChange={NativeWindowStateStore.setRememberFullscreen}
					/>
				</SettingsTabSection>
			)}
			<SettingsTabSection
				title={<Trans>Screen Sharing</Trans>}
				description={<Trans>Choose how Fluxer selects the video codec for screen sharing.</Trans>}
			>
				<Switch
					label={<Trans>Hardware Acceleration</Trans>}
					description={
						<Trans>
							Uses H.265 for screen sharing when enabled. Turn this off to prefer VP9. Changes apply the next time you
							start sharing your screen.
						</Trans>
					}
					value={screenShareHardwareAcceleration}
					onChange={(value) => VoiceSettingsActionCreators.update({screenShareHardwareAcceleration: value})}
				/>
			</SettingsTabSection>
			<SettingsTabSection
				title={<Trans>Developer Options</Trans>}
				description={
					<Trans>
						Enable advanced features for debugging and development. Note that copying snowflake IDs for entities is
						always available to all users without needing developer mode.
					</Trans>
				}
			>
				<Switch
					label={<Trans>Developer Mode</Trans>}
					description={
						<Trans>
							When enabled, reveals debugging menus throughout the app to inspect and copy raw JSON objects of internal
							data structures like messages, channels, users, and communities. Also includes tools to debug the Fluxer
							Markdown parser performance and AST for any given message.
						</Trans>
					}
					value={developerMode}
					onChange={(value) => UserSettingsActionCreators.update({developerMode: value})}
				/>
			</SettingsTabSection>
		</SettingsTabContainer>
	);
});

export default AdvancedTab;
