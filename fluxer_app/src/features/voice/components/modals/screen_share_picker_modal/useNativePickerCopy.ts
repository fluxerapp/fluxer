// SPDX-License-Identifier: AGPL-3.0-or-later

import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import type {ScreenSharePickerTab} from '@app/features/voice/components/modals/screen_share_picker_modal/shared';
import type {DisplayShareEnvironment} from '@app/features/voice/utils/ScreenShareEnvironment';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {AppWindowIcon, MonitorIcon} from '@phosphor-icons/react';
import {useMemo} from 'react';

const CHOOSE_THE_TAB_OR_WINDOW_IN_YOUR_BROWSER_DESCRIPTOR = msg({
	message: 'Choose the tab or window in your browser',
	comment: 'Empty-state title in the screen-share picker apps tab when running in a web browser.',
});
const BROWSERS_DO_NOT_LET_LIST_APP_WINDOWS_AHEAD_DESCRIPTOR = msg({
	message:
		'Browsers do not let {productName} list app windows ahead of time. Press "Open browser picker", then choose the tab or window there.',
	comment: 'Empty-state explanation in the screen-share picker apps tab on web. {productName} is Fluxer.',
});
const CHOOSE_THE_SCREEN_IN_YOUR_BROWSER_DESCRIPTOR = msg({
	message: 'Choose the screen in your browser',
	comment: 'Empty-state title in the screen-share picker displays tab when running in a web browser.',
});
const BROWSERS_DO_NOT_LET_LIST_DISPLAYS_AHEAD_OF_DESCRIPTOR = msg({
	message:
		'Browsers do not let {productName} list displays ahead of time. Press "Open browser picker", then choose the screen there.',
	comment: 'Empty-state explanation in the screen-share picker displays tab on web. {productName} is Fluxer.',
});
const CHOOSE_A_WINDOW_IN_YOUR_SYSTEM_PICKER_DESCRIPTOR = msg({
	message: 'Choose a window in your system picker',
	comment: 'Empty-state title in the screen-share picker apps tab on Linux/Wayland.',
});
const WAYLAND_S_XDG_DESKTOP_PORTAL_OWNS_THE_PICKER_DESCRIPTOR = msg({
	message:
		'Wayland\'s xdg-desktop-portal owns the picker and shows every window and display together. There is no flag that filters it to windows only. Press "Open system picker", then choose a window there to get per-app audio.',
	comment:
		"Empty-state explanation in the screen-share picker apps tab on Linux/Wayland. Technical surface; keep 'xdg-desktop-portal' as a literal proper noun.",
});
const CHOOSE_A_DISPLAY_IN_YOUR_SYSTEM_PICKER_DESCRIPTOR = msg({
	message: 'Choose a display in your system picker',
	comment: 'Empty-state title in the screen-share picker displays tab on Linux/Wayland.',
});
const WAYLAND_S_XDG_DESKTOP_PORTAL_OWNS_THE_PICKER_2_DESCRIPTOR = msg({
	message:
		'Wayland\'s xdg-desktop-portal owns the picker and shows every window and display together. There is no flag that filters it to displays only. Press "Open system picker", then choose a display there to capture desktop audio.',
	comment:
		"Empty-state explanation in the screen-share picker displays tab on Linux/Wayland. Technical surface; keep 'xdg-desktop-portal' as a literal proper noun.",
});

export interface NativePickerCopy {
	Icon: typeof AppWindowIcon;
	title: string;
	description: string;
}

export function useNativePickerCopy(
	activeTab: ScreenSharePickerTab,
	displayShareEnvironment: DisplayShareEnvironment,
): NativePickerCopy | null {
	const {i18n} = useLingui();
	return useMemo(() => {
		if (activeTab === 'devices') {
			return null;
		}
		if (displayShareEnvironment === 'web') {
			if (activeTab === 'apps') {
				return {
					Icon: AppWindowIcon,
					title: i18n._(CHOOSE_THE_TAB_OR_WINDOW_IN_YOUR_BROWSER_DESCRIPTOR),
					description: i18n._(BROWSERS_DO_NOT_LET_LIST_APP_WINDOWS_AHEAD_DESCRIPTOR, {productName: PRODUCT_NAME}),
				};
			}
			return {
				Icon: MonitorIcon,
				title: i18n._(CHOOSE_THE_SCREEN_IN_YOUR_BROWSER_DESCRIPTOR),
				description: i18n._(BROWSERS_DO_NOT_LET_LIST_DISPLAYS_AHEAD_OF_DESCRIPTOR, {productName: PRODUCT_NAME}),
			};
		}
		if (activeTab === 'apps') {
			return {
				Icon: AppWindowIcon,
				title: i18n._(CHOOSE_A_WINDOW_IN_YOUR_SYSTEM_PICKER_DESCRIPTOR),
				description: i18n._(WAYLAND_S_XDG_DESKTOP_PORTAL_OWNS_THE_PICKER_DESCRIPTOR),
			};
		}
		return {
			Icon: MonitorIcon,
			title: i18n._(CHOOSE_A_DISPLAY_IN_YOUR_SYSTEM_PICKER_DESCRIPTOR),
			description: i18n._(WAYLAND_S_XDG_DESKTOP_PORTAL_OWNS_THE_PICKER_2_DESCRIPTOR),
		};
	}, [activeTab, displayShareEnvironment, i18n.locale]);
}
