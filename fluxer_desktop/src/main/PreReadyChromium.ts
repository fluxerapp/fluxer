// SPDX-License-Identifier: AGPL-3.0-or-later

import type {BuildChannel} from '@electron/common/BuildChannel';
import {
	getConfiguredChromiumSwitches,
	getDesktopTroubleshootingSettings,
	getDesktopWindowBehaviorSettings,
} from '@electron/common/DesktopConfig';
import {
	addLinuxHardwareVideoEncodeFeatures,
	addWindowsHardwareVideoEncodeFeatures,
	appendConfiguredChromiumSwitches,
	appendDisabledChromiumFeatures,
	appendEnabledBlinkFeature,
	appendEnabledChromiumFeatures,
	appendLinuxChromiumFlagsConfig,
	BASE_DISABLED_CHROMIUM_FEATURES,
	MIDDLE_CLICK_AUTOSCROLL_BLINK_FEATURE,
} from '@electron/main/ChromiumRuntime';
import {
	getLaunchDesktopTroubleshootingSettings,
	shouldDisableHardwareAccelerationForLaunch,
} from '@electron/main/DesktopDebugInfo';
import {isSafeModeLaunch} from '@electron/main/LaunchOptions';
import {app} from 'electron';
import log from 'electron-log';

let preReadyChromiumConfigured = false;

export function applyPreReadyChromiumConfiguration(channel: BuildChannel, argv: ReadonlyArray<string>): void {
	if (preReadyChromiumConfigured) return;
	preReadyChromiumConfigured = true;
	if (app.isReady()) {
		log.warn('Chromium launch configuration skipped because the app is already ready');
		return;
	}
	const disableHardwareAcceleration = getLaunchDesktopTroubleshootingSettings().disableHardwareAcceleration;
	if (disableHardwareAcceleration) {
		app.disableHardwareAcceleration();
		log.info('Hardware acceleration disabled for this launch', {
			commandLine: shouldDisableHardwareAccelerationForLaunch(argv),
			persistentSetting: getDesktopTroubleshootingSettings().disableHardwareAcceleration,
		});
	}
	app.commandLine.appendSwitch('autoplay-policy', 'no-user-gesture-required');
	const windowBehaviorSettings = getDesktopWindowBehaviorSettings();
	app.commandLine.appendSwitch(
		windowBehaviorSettings.smoothScrolling ? 'enable-smooth-scrolling' : 'disable-smooth-scrolling',
	);
	if (process.platform === 'linux' && windowBehaviorSettings.middleClickAutoscroll) {
		appendEnabledBlinkFeature(MIDDLE_CLICK_AUTOSCROLL_BLINK_FEATURE);
	}
	const disabledChromiumFeatures = new Set(BASE_DISABLED_CHROMIUM_FEATURES);
	const enabledChromiumFeatures = new Set<string>();
	if (!disableHardwareAcceleration) {
		addLinuxHardwareVideoEncodeFeatures(enabledChromiumFeatures);
		addWindowsHardwareVideoEncodeFeatures(enabledChromiumFeatures);
	}
	appendDisabledChromiumFeatures(disabledChromiumFeatures);
	if (enabledChromiumFeatures.size > 0) {
		appendEnabledChromiumFeatures(enabledChromiumFeatures);
	}
	appendConfiguredChromiumSwitches(getConfiguredChromiumSwitches());
	if (!isSafeModeLaunch(argv)) {
		appendLinuxChromiumFlagsConfig(channel);
	}
}
