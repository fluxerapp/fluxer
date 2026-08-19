// SPDX-License-Identifier: AGPL-3.0-or-later

import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {CLOSE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {getElectronAPI, getNativePlatformSync, isDesktop, isNativeMacOS} from '@app/features/ui/utils/NativeUtils';
import type {SearchableSettingDescriptor} from '@app/features/user/components/settings_utils/search_index/SearchIndexTypes';
import {BACKGROUND_DESCRIPTOR} from '@app/features/user/components/settings_utils/search_index/SharedDescriptors';
import {msg} from '@lingui/core/macro';

const HARDWARE_ACCELERATION_DESCRIPTOR = msg({
	message: 'Hardware acceleration',
	comment: 'Settings search entry label. Names the settings search entry in the settings UI.',
});
const HARDWARE_DESCRIPTOR = msg({
	message: 'Hardware',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ACCELERATION_DESCRIPTOR = msg({
	message: 'Acceleration',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const GRAPHICS_DESCRIPTOR = msg({
	message: 'Graphics',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PERFORMANCE_DESCRIPTOR = msg({
	message: 'Performance',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TOGGLE_HARDWARE_ACCELERATION_DESCRIPTOR = msg({
	message: 'Toggle hardware acceleration',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const LAUNCH_AT_LOGIN_DESCRIPTOR = msg({
	message: 'Launch {productName} at login',
	comment: 'Settings search entry label. Names the settings search entry in the settings UI.',
});
const AUTOMATIC_START_DESCRIPTOR = msg({
	message: 'Automatic start',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const LAUNCH_AT_LOGIN_2_DESCRIPTOR = msg({
	message: 'Launch at login',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const STARTUP_DESCRIPTOR = msg({
	message: 'Startup',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DESKTOP_STARTUP_DESCRIPTOR = msg({
	message: 'Desktop startup',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const RUN_AT_LOGIN_DESCRIPTOR = msg({
	message: 'Run at login',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const START_WITH_COMPUTER_DESCRIPTOR = msg({
	message: 'Start with computer',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const BOOT_DESCRIPTOR = msg({
	message: 'Boot',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const RUN_AUTOMATICALLY_WHEN_YOUR_COMPUTER_STARTS_DESCRIPTOR = msg({
	message: 'Run {productName} automatically when your computer starts',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const REMEMBER_SIZE_POSITION_DESCRIPTOR = msg({
	message: 'Remember size & position',
	comment: 'Settings search entry label. Names the settings search entry in the settings UI.',
});
const WINDOW_DESCRIPTOR = msg({
	message: 'Window',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const WINDOW_STATE_DESCRIPTOR = msg({
	message: 'Window state',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const WINDOW_SIZE_DESCRIPTOR = msg({
	message: 'Window size',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const WINDOW_POSITION_DESCRIPTOR = msg({
	message: 'Window position',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const REMEMBER_WINDOW_DESCRIPTOR = msg({
	message: 'Remember window',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DESKTOP_WINDOW_DESCRIPTOR = msg({
	message: 'Desktop window',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SIZE_DESCRIPTOR = msg({
	message: 'Size',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const POSITION_DESCRIPTOR = msg({
	message: 'Position',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const BOUNDS_DESCRIPTOR = msg({
	message: 'Bounds',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const KEEP_WINDOW_DIMENSIONS_AND_PLACEMENT_BETWEEN_RELOADS_DESCRIPTOR = msg({
	message: 'Keep window dimensions and placement between reloads',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const USE_NATIVE_TITLE_BAR_DESCRIPTOR = msg({
	message: 'Use native title bar',
	comment: 'Settings search entry label. Names the settings search entry in the settings UI.',
});
const TITLE_BAR_DESCRIPTOR = msg({
	message: 'Title bar',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TITLEBAR_DESCRIPTOR = msg({
	message: 'Titlebar',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const NATIVE_TITLE_BAR_DESCRIPTOR = msg({
	message: 'Native title bar',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const WINDOW_CHROME_DESCRIPTOR = msg({
	message: 'Window chrome',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const WINDOW_DECORATIONS_DESCRIPTOR = msg({
	message: 'Window decorations',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const FRAME_DESCRIPTOR = msg({
	message: 'Frame',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CUSTOM_TITLE_BAR_DESCRIPTOR = msg({
	message: 'Custom title bar',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const USE_THE_OPERATING_SYSTEM_S_WINDOW_CHROME_DESCRIPTOR = msg({
	message: "Use the operating system's window chrome",
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const SHOW_TRAY_ICON_DESCRIPTOR = msg({
	message: 'Show tray icon',
	comment: 'Settings search entry label. Names the settings search entry in the settings UI.',
});
const TRAY_DESCRIPTOR = msg({
	message: 'Tray',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TRAY_ICON_DESCRIPTOR = msg({
	message: 'Tray icon',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SYSTEM_TRAY_DESCRIPTOR = msg({
	message: 'System tray',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MENU_BAR_DESCRIPTOR = msg({
	message: 'Menu bar',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const STATUS_ICON_DESCRIPTOR = msg({
	message: 'Status icon',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const NOTIFICATION_AREA_DESCRIPTOR = msg({
	message: 'Notification area',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SHOW_A_ICON_IN_THE_SYSTEM_TRAY_OR_DESCRIPTOR = msg({
	message: 'Show a {productName} icon in the system tray or menu bar',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const MINIMIZE_TO_TRAY_DESCRIPTOR = msg({
	message: 'Minimize to tray',
	comment: 'Settings search entry label. Also used as a search synonym in the settings search bar.',
});
const MINIMIZE_DESCRIPTOR = msg({
	message: 'Minimize',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MINIMISE_DESCRIPTOR = msg({
	message: 'Minimise',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const HIDE_WINDOW_DESCRIPTOR = msg({
	message: 'Hide window',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const HIDE_THE_WINDOW_WHEN_MINIMIZED_AND_REOPEN_FROM_DESCRIPTOR = msg({
	message: 'Hide the window when minimized and reopen from the tray',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const CLOSE_TO_TRAY_DESCRIPTOR = msg({
	message: 'Close to tray',
	comment: 'Settings search entry label. Also used as a search synonym in the settings search bar.',
});
const KEEP_RUNNING_DESCRIPTOR = msg({
	message: 'Keep running',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const EXIT_DESCRIPTOR = msg({
	message: 'Exit',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const QUIT_DESCRIPTOR = msg({
	message: 'Quit',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const KEEP_RUNNING_WHEN_THE_WINDOW_IS_CLOSED_DESCRIPTOR = msg({
	message: 'Keep {productName} running when the window is closed',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
export const desktopSettingsIndex: Array<SearchableSettingDescriptor> = [
	{
		id: 'advanced-hardware-acceleration',
		tabType: 'desktop_settings',
		label: HARDWARE_ACCELERATION_DESCRIPTOR,
		keywords: [HARDWARE_DESCRIPTOR, ACCELERATION_DESCRIPTOR, GRAPHICS_DESCRIPTOR, PERFORMANCE_DESCRIPTOR],
		description: TOGGLE_HARDWARE_ACCELERATION_DESCRIPTOR,
		tags: ['desktop'],
		isVisible: () => isDesktop() && getElectronAPI()?.platform !== 'darwin',
	},
	{
		id: 'advanced-autostart',
		tabType: 'desktop_settings',
		label: {...LAUNCH_AT_LOGIN_DESCRIPTOR, values: {productName: PRODUCT_NAME}},
		keywords: [
			AUTOMATIC_START_DESCRIPTOR,
			LAUNCH_AT_LOGIN_2_DESCRIPTOR,
			STARTUP_DESCRIPTOR,
			DESKTOP_STARTUP_DESCRIPTOR,
			RUN_AT_LOGIN_DESCRIPTOR,
			START_WITH_COMPUTER_DESCRIPTOR,
			BOOT_DESCRIPTOR,
		],
		description: {...RUN_AUTOMATICALLY_WHEN_YOUR_COMPUTER_STARTS_DESCRIPTOR, values: {productName: PRODUCT_NAME}},
		audience: 'primary',
		tags: ['desktop'],
		isVisible: isDesktop,
	},
	{
		id: 'advanced-remember-window-state',
		tabType: 'desktop_settings',
		label: REMEMBER_SIZE_POSITION_DESCRIPTOR,
		keywords: [
			WINDOW_DESCRIPTOR,
			WINDOW_STATE_DESCRIPTOR,
			WINDOW_SIZE_DESCRIPTOR,
			WINDOW_POSITION_DESCRIPTOR,
			REMEMBER_WINDOW_DESCRIPTOR,
			DESKTOP_WINDOW_DESCRIPTOR,
			SIZE_DESCRIPTOR,
			POSITION_DESCRIPTOR,
			BOUNDS_DESCRIPTOR,
		],
		description: KEEP_WINDOW_DIMENSIONS_AND_PLACEMENT_BETWEEN_RELOADS_DESCRIPTOR,
		audience: 'primary',
		tags: ['desktop'],
		isVisible: isDesktop,
	},
	{
		id: 'advanced-native-title-bar',
		tabType: 'desktop_settings',
		label: USE_NATIVE_TITLE_BAR_DESCRIPTOR,
		keywords: [
			TITLE_BAR_DESCRIPTOR,
			TITLEBAR_DESCRIPTOR,
			NATIVE_TITLE_BAR_DESCRIPTOR,
			WINDOW_CHROME_DESCRIPTOR,
			WINDOW_DECORATIONS_DESCRIPTOR,
			FRAME_DESCRIPTOR,
			CUSTOM_TITLE_BAR_DESCRIPTOR,
			DESKTOP_WINDOW_DESCRIPTOR,
		],
		description: USE_THE_OPERATING_SYSTEM_S_WINDOW_CHROME_DESCRIPTOR,
		tags: ['desktop', 'appearance'],
		isVisible: () => isDesktop() && !isNativeMacOS(getNativePlatformSync()),
	},
	{
		id: 'advanced-tray-icon',
		tabType: 'desktop_settings',
		label: SHOW_TRAY_ICON_DESCRIPTOR,
		keywords: [
			TRAY_DESCRIPTOR,
			TRAY_ICON_DESCRIPTOR,
			SYSTEM_TRAY_DESCRIPTOR,
			MENU_BAR_DESCRIPTOR,
			STATUS_ICON_DESCRIPTOR,
			NOTIFICATION_AREA_DESCRIPTOR,
			DESKTOP_WINDOW_DESCRIPTOR,
		],
		description: {...SHOW_A_ICON_IN_THE_SYSTEM_TRAY_OR_DESCRIPTOR, values: {productName: PRODUCT_NAME}},
		audience: 'primary',
		tags: ['desktop'],
		isVisible: isDesktop,
	},
	{
		id: 'advanced-minimize-to-tray',
		tabType: 'desktop_settings',
		label: MINIMIZE_TO_TRAY_DESCRIPTOR,
		keywords: [
			MINIMIZE_DESCRIPTOR,
			MINIMISE_DESCRIPTOR,
			TRAY_DESCRIPTOR,
			MINIMIZE_TO_TRAY_DESCRIPTOR,
			HIDE_WINDOW_DESCRIPTOR,
			BACKGROUND_DESCRIPTOR,
			DESKTOP_WINDOW_DESCRIPTOR,
		],
		description: HIDE_THE_WINDOW_WHEN_MINIMIZED_AND_REOPEN_FROM_DESCRIPTOR,
		audience: 'primary',
		tags: ['desktop'],
		isVisible: isDesktop,
	},
	{
		id: 'advanced-close-to-tray',
		tabType: 'desktop_settings',
		label: CLOSE_TO_TRAY_DESCRIPTOR,
		keywords: [
			CLOSE_DESCRIPTOR,
			TRAY_DESCRIPTOR,
			CLOSE_TO_TRAY_DESCRIPTOR,
			BACKGROUND_DESCRIPTOR,
			KEEP_RUNNING_DESCRIPTOR,
			EXIT_DESCRIPTOR,
			QUIT_DESCRIPTOR,
			DESKTOP_WINDOW_DESCRIPTOR,
		],
		description: {...KEEP_RUNNING_WHEN_THE_WINDOW_IS_CLOSED_DESCRIPTOR, values: {productName: PRODUCT_NAME}},
		audience: 'primary',
		tags: ['desktop'],
		isVisible: isDesktop,
	},
];
