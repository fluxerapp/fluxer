// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {Logger} from '@app/features/platform/utils/AppLogger';
import Theme from '@app/features/theme/state/Theme';
import {broadcastThemeStudioMessage} from '@app/features/theme_studio/state/ThemeStudioBroadcast';
import ThemeStudioState from '@app/features/theme_studio/state/ThemeStudioState';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';

const logger = new Logger('ThemeStudioCommands');
const POPOUT_WINDOW_NAME = 'fluxer_theme_studio';
const POPOUT_FEATURES = [
	'popup=yes',
	'width=1240',
	'height=860',
	'minWidth=900',
	'minHeight=620',
	'resizable=yes',
	'scrollbars=no',
	'location=no',
	'toolbar=no',
	'menubar=no',
	'status=no',
	'titlebar=no',
	'personalbar=no',
].join(',');

export function openThemeStudio(): void {
	openThemeStudioPopout();
}

export function closeThemeStudio(): void {
	requestPopoutClose();
}

export function openThemeStudioPopout(): void {
	if (typeof window === 'undefined') return;
	const electronApi = getElectronAPI();
	if (electronApi?.focusThemeStudioPopout) {
		void electronApi
			.focusThemeStudioPopout()
			.then((focused) => {
				if (focused) {
					broadcastThemeStudioMessage({type: 'studio:focus-popout'});
					broadcastThemeStudioMessage({type: 'themePreference', snapshot: Theme.getPreferenceSnapshot()});
					return;
				}
				openThemeStudioPopoutWindow();
			})
			.catch((error) => {
				logger.warn('Failed to focus Theme Studio popout before opening', error);
				openThemeStudioPopoutWindow();
			});
		return;
	}
	openThemeStudioPopoutWindow();
}

function openThemeStudioPopoutWindow(): void {
	try {
		const existingPopup = ThemeStudioState.popupRef;
		if (existingPopup && !existingPopup.closed) {
			existingPopup.focus();
			broadcastThemeStudioMessage({type: 'studio:focus-popout'});
			broadcastThemeStudioMessage({type: 'themePreference', snapshot: Theme.getPreferenceSnapshot()});
			return;
		}
		if (existingPopup?.closed) {
			ThemeStudioState.clearPoppedOut();
		}
		const popup = window.open(Routes.THEME_STUDIO, POPOUT_WINDOW_NAME, POPOUT_FEATURES);
		if (popup) {
			popup.focus();
			ThemeStudioState.markPoppedOut(popup);
			broadcastThemeStudioMessage({type: 'studio:opened-popout'});
			broadcastThemeStudioMessage({type: 'themePreference', snapshot: Theme.getPreferenceSnapshot()});
		}
	} catch (error) {
		logger.warn('Failed to open Theme Studio popout', error);
	}
}

export function focusThemeStudioPopout(): void {
	const electronApi = getElectronAPI();
	if (electronApi?.focusThemeStudioPopout) {
		void electronApi
			.focusThemeStudioPopout()
			.then((focused) => {
				if (focused) return;
				const popup = ThemeStudioState.popupRef;
				if (popup && !popup.closed) {
					popup.focus();
				}
			})
			.catch((error) => {
				logger.warn('Failed to focus Theme Studio popout via Electron', error);
			});
		return;
	}
	const popup = ThemeStudioState.popupRef;
	if (popup && !popup.closed) {
		popup.focus();
		return;
	}
	broadcastThemeStudioMessage({type: 'studio:focus-popout'});
}

export function requestPopoutClose(): void {
	const electronApi = getElectronAPI();
	if (electronApi?.closeThemeStudioPopout) {
		void electronApi.closeThemeStudioPopout().catch((error) => {
			logger.warn('Failed to close Theme Studio popout via Electron', error);
		});
	}
	const popup = ThemeStudioState.popupRef;
	if (popup && !popup.closed) {
		popup.close();
	}
	broadcastThemeStudioMessage({type: 'studio:close-popout'});
	ThemeStudioState.clearPoppedOut();
}
