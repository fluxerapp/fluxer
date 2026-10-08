// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	CANCEL_DESCRIPTOR,
	IDLE_DESCRIPTOR,
	ONLINE_DESCRIPTOR,
	OPEN_SETTINGS_DESCRIPTOR,
	TRY_AGAIN_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {onLocaleChange} from '@app/features/i18n/utils/LocaleChangeListener';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import {VOICE_DEAFEN_DESCRIPTOR, VOICE_UNDEAFEN_DESCRIPTOR} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {i18n, type MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const ABOUT_DESCRIPTOR = msg({
	message: 'About {appName}',
	comment: 'Desktop app menu item. Opens the About panel. {appName} is the desktop app name (typically Fluxer).',
});
const PREFERENCES_DESCRIPTOR = msg({
	message: 'Preferences...',
	comment:
		'macOS app menu item that opens app settings. Trailing ellipsis follows the macOS HIG convention. Keep "Preferences" here even though the in-app term is "settings".',
});
const PREFERENCES_2_DESCRIPTOR = msg({
	message: 'Preferences',
	comment:
		'macOS app menu submenu title. Keep "Preferences" to match the macOS HIG; in-app surfaces still use "settings".',
});
const HIDE_DESCRIPTOR = msg({
	message: 'Hide {appName}',
	comment: 'macOS app menu / tray item. Hides the desktop app window.',
});
const QUIT_DESCRIPTOR = msg({
	message: 'Quit {appName}',
	comment: 'macOS / tray menu item that fully quits the desktop app.',
});
const EXIT_DESCRIPTOR = msg({
	message: 'Exit',
	comment:
		'Windows / Linux File menu item that quits the desktop app. Imperative verb, no app name. Matches the platform wording for leaving an application.',
});
const FILE_DESCRIPTOR = msg({
	message: 'File',
	comment: 'Top-level native menu bar title (File menu).',
});
const EDIT_DESCRIPTOR = msg({
	message: 'Edit',
	comment: 'Top-level native menu bar title (Edit menu, holds cut/copy/paste).',
});
const UNDO_DESCRIPTOR = msg({
	message: 'Undo',
	comment: 'Native Edit menu item that undoes the last edit. Imperative verb.',
});
const REDO_DESCRIPTOR = msg({
	message: 'Redo',
	comment: 'Native Edit menu item that redoes the last undone edit. Imperative verb.',
});
const CUT_DESCRIPTOR = msg({
	message: 'Cut',
	comment: 'Native Edit menu item that cuts the selection to the clipboard. Imperative verb, not the noun.',
});
const COPY_DESCRIPTOR = msg({
	message: 'Copy',
	comment: 'Native Edit menu item that copies the selection to the clipboard. Imperative verb, not the noun.',
});
const PASTE_DESCRIPTOR = msg({
	message: 'Paste',
	comment: 'Native Edit menu item that pastes the clipboard contents. Imperative verb, not the noun.',
});
const PASTE_AND_MATCH_STYLE_DESCRIPTOR = msg({
	message: 'Paste and match style',
	comment:
		'Native macOS Edit menu item that pastes the clipboard contents using the formatting of the surrounding text. Imperative verb.',
});
const DELETE_DESCRIPTOR = msg({
	message: 'Delete',
	comment: 'Native Edit menu item that deletes the current selection. Imperative verb.',
});
const SELECT_ALL_DESCRIPTOR = msg({
	message: 'Select all',
	comment: 'Native Edit menu item that selects everything in the focused field. Imperative verb.',
});
const SPEECH_DESCRIPTOR = msg({
	message: 'Speech',
	comment: 'Native macOS Edit submenu title for text-to-speech actions.',
});
const START_SPEAKING_DESCRIPTOR = msg({
	message: 'Start speaking',
	comment: 'Native macOS Edit > Speech submenu item that starts reading the selected text aloud. Imperative verb.',
});
const STOP_SPEAKING_DESCRIPTOR = msg({
	message: 'Stop speaking',
	comment: 'Native macOS Edit > Speech submenu item that stops reading text aloud. Imperative verb.',
});
const VIEW_DESCRIPTOR = msg({
	message: 'View',
	comment: 'Top-level native menu bar title (View menu, holds zoom and reload).',
});
const FORCE_RELOAD_DESCRIPTOR = msg({
	message: 'Force reload',
	comment: 'Native View menu item that reloads the window and ignores the cache. Imperative verb.',
});
const TOGGLE_DEVELOPER_TOOLS_DESCRIPTOR = msg({
	message: 'Toggle developer tools',
	comment: 'Native View menu item that opens or closes Chromium developer tools.',
});
const ACTUAL_SIZE_DESCRIPTOR = msg({
	message: 'Actual size',
	comment: 'Native View menu item that resets zoom to 100%.',
});
const ZOOM_IN_DESCRIPTOR = msg({
	message: 'Zoom in',
	comment: 'Native View menu item that increases UI zoom level.',
});
const ZOOM_OUT_DESCRIPTOR = msg({
	message: 'Zoom out',
	comment: 'Native View menu item that decreases UI zoom level.',
});
const TOGGLE_FULL_SCREEN_DESCRIPTOR = msg({
	message: 'Toggle full screen',
	comment: 'Native View menu item that enters or leaves full screen. Imperative verb.',
});
const WINDOW_DESCRIPTOR = msg({
	message: 'Window',
	comment: 'Top-level native menu bar title (Window menu).',
});
const MINIMIZE_DESCRIPTOR = msg({
	message: 'Minimize',
	comment: 'Native Window menu item that minimizes the focused desktop window. Imperative verb.',
});
const ZOOM_WINDOW_DESCRIPTOR = msg({
	message: 'Zoom',
	context: 'window-menu',
	comment:
		'Native Window menu item that zooms the focused window, meaning maximize or restore it. Not image zoom and not UI zoom level.',
});
const CLOSE_DESCRIPTOR = msg({
	message: 'Close',
	comment: 'Native Window menu item that closes the focused desktop window.',
});
const HELP_DESCRIPTOR = msg({
	message: 'Help',
	comment: 'Top-level native menu bar title (Help menu).',
});
const WEBSITE_DESCRIPTOR = msg({
	message: 'Website',
	comment: 'Native Help submenu item that opens the Fluxer marketing website.',
});
const REPORT_ISSUE_DESCRIPTOR = msg({
	message: 'Report issue',
	comment: 'Native Help submenu item. Opens the bug / issue reporting flow.',
});
const TROUBLESHOOTING_DESCRIPTOR = msg({
	message: 'Troubleshooting',
	comment: 'Native Help submenu title that groups recovery actions like reload and reset.',
});
const DISABLE_HARDWARE_ACCELERATION_AND_RESTART_DESCRIPTOR = msg({
	message: 'Disable hardware acceleration and restart',
	comment: 'Troubleshooting menu item. Turns off Chromium hardware acceleration and restarts the desktop app.',
});
const ENABLE_HARDWARE_ACCELERATION_AND_RESTART_DESCRIPTOR = msg({
	message: 'Enable hardware acceleration and restart',
	comment: 'Troubleshooting menu item. Turns Chromium hardware acceleration back on and restarts the desktop app.',
});
const RELOAD_DESCRIPTOR = msg({
	message: 'Reload',
	comment: 'Troubleshooting menu item. Reloads the renderer process. Keep short.',
});
const RESET_APP_DATA_AND_RESTART_DESCRIPTOR = msg({
	message: 'Reset app data and restart',
	comment: 'Destructive troubleshooting menu item. Clears local app data, signs out, and restarts.',
});
const RESET_APP_DATA_DESCRIPTOR = msg({
	message: 'Reset app data',
	comment: 'Confirm dialog title for the destructive reset action.',
});
const ARE_YOU_SURE_YOU_WANT_TO_RESET_S_DESCRIPTOR = msg({
	message: "Are you sure you want to reset {appName}'s app data?",
	comment: 'Confirm dialog message for the destructive reset action. Tone stays plain.',
});
const THIS_WILL_SIGN_YOU_OUT_CLEAR_CACHED_FILES_DESCRIPTOR = msg({
	message:
		'This will sign you out, clear cached files and stored sessions, and restart the desktop app. Your account, communities, and messages in {appName} are not affected.',
	comment:
		'Confirm dialog detail text for the destructive reset action. appName is the desktop app name. Reassures users that server-side data is safe.',
});
const RESET_AND_RESTART_DESCRIPTOR = msg({
	message: 'Reset and restart',
	comment: 'Destructive confirm button on the reset-app-data dialog.',
});
const SHOW_DESCRIPTOR = msg({
	message: 'Show {appName}',
	comment: 'Tray menu item that brings the desktop app window to the foreground.',
});
const STATUS_DESCRIPTOR = msg({
	message: 'Status',
	comment: 'Tray menu submenu title for the user presence status picker.',
});
const DO_NOT_DISTURB_DESCRIPTOR = msg({
	message: 'Do not disturb',
	comment: 'Tray status menu item. User presence that suppresses notifications.',
});
const INVISIBLE_DESCRIPTOR = msg({
	message: 'Invisible',
	comment: 'Tray status menu item. User presence that appears offline to others.',
});
const MUTE_MICROPHONE_DESCRIPTOR = msg({
	message: 'Mute microphone',
	comment: 'Tray menu item shown while in a voice call. Mutes the local microphone.',
});
const UNMUTE_MICROPHONE_DESCRIPTOR = msg({
	message: 'Unmute microphone',
	comment: 'Tray menu item shown while in a voice call. Unmutes the local microphone.',
});
const DISCONNECT_FROM_DESCRIPTOR = msg({
	message: 'Disconnect from {channel}',
	comment: 'Tray menu item shown while in a voice call. {channel} is the voice channel name.',
});
const DISCONNECT_FROM_VOICE_DESCRIPTOR = msg({
	message: 'Disconnect from voice',
	comment: 'Tray menu item. Leaves the current voice call when the channel name is unavailable.',
});
const CHECK_FOR_UPDATES_DESCRIPTOR = msg({
	message: 'Check for updates',
	comment: 'Tray menu item. Triggers the auto-updater to look for a new desktop build.',
});
const COPY_BUILD_INFO_DESCRIPTOR = msg({
	message: 'Copy build info',
	comment: 'Tray menu item. Copies build / version diagnostics to the clipboard for bug reports.',
});
const RESTART_DESCRIPTOR = msg({
	message: 'Restart {appName}',
	comment: 'Tray menu item that restarts the desktop app process.',
});
const AUTOSTART_PORTAL_REASON_DESCRIPTOR = msg({
	message: 'Start {appName} automatically when you sign in.',
	comment:
		'Reason shown by the Linux Flatpak background permission portal when enabling launch-at-login. {appName} is the desktop app name.',
});
const OPEN_SETTINGS_2_DESCRIPTOR = msg({
	message: 'Open {appName} settings',
	comment: 'Windows jump-list task description (tooltip text) for the Open settings action.',
});
const NEW_DIRECT_MESSAGE_DESCRIPTOR = msg({
	message: 'New direct message',
	comment: 'Windows jump-list task title. Starts composing a new DM.',
});
const COMPOSE_A_NEW_DIRECT_MESSAGE_DESCRIPTOR = msg({
	message: 'Compose a new direct message',
	comment: 'Windows jump-list task description (tooltip text) for the New direct message action.',
});
const OPEN_NOTIFICATION_DESCRIPTOR = msg({
	message: 'Open',
	comment:
		'Action button on a Linux desktop notification. Imperative verb: opens the message the notification is about.',
});
const ADD_LOCAL_FILES_DESCRIPTOR = msg({
	message: 'Add local files',
	comment: 'Title of the native file picker that selects local theme files to add.',
});
const IMPORT_FOLDER_DESCRIPTOR = msg({
	message: 'Import folder',
	comment: 'Title of the native folder picker that imports a directory of theme files.',
});
const IMPORT_CSS_DESCRIPTOR = msg({
	message: 'Import CSS',
	comment:
		'Title of the native file picker that imports CSS theme files on the desktop app. Imported files stay linked and update live when saved.',
});
const LINK_FILE_DESCRIPTOR = msg({
	message: 'Link file',
	comment: 'Title of the native file picker that links an existing theme to a CSS file on disk.',
});
const CSS_FILES_DESCRIPTOR = msg({
	message: 'CSS files',
	comment: 'Name of the file type filter in the native theme file picker, shown next to the *.css pattern.',
});
const UNREAD_MESSAGES_DESCRIPTOR = msg({
	message: 'Unread messages',
	comment:
		'Accessibility description of the Windows taskbar overlay badge, read by screen readers, when there are unread messages but no exact count to show.',
});
const UNREAD_MESSAGES_COUNT_DESCRIPTOR = msg({
	message: 'Unread messages: {count}',
	comment:
		'Accessibility description of the Windows taskbar overlay badge, read by screen readers. {count} is the number of unread messages, already formatted for the locale.',
});
const APP_STORE_UNREADABLE_TITLE_DESCRIPTOR = msg({
	message: '{appName} cannot open its local data',
	comment:
		'Title of the native error dialog shown when the desktop app cannot open the local data store it used before. {appName} is the desktop app name (typically Fluxer).',
});
const APP_STORE_UNREADABLE_MESSAGE_DESCRIPTOR = msg({
	message: '{appName} cannot open the local data it saved on this computer.',
	comment:
		'Main message of the native error dialog shown when the desktop app cannot open its local data store. {appName} is the desktop app name (typically Fluxer).',
});
const APP_STORE_UNREADABLE_UNCHANGED_DESCRIPTOR = msg({
	message: 'Your accounts and settings have not been changed or deleted.',
	comment: 'Detail line in the native dialog shown when the desktop app cannot open its local data store.',
});
const APP_STORE_UNREADABLE_ADVICE_DESCRIPTOR = msg({
	message: 'Quit {appName}, make sure no other copy is running, and open it again.',
	comment:
		'Detail line in the native dialog shown when the desktop app cannot open its local data store. {appName} is the desktop app name (typically Fluxer).',
});
const APP_STORE_UNREADABLE_DETAILS_DESCRIPTOR = msg({
	message: 'Details: {reason}',
	comment:
		'Detail line in the native dialog shown when the desktop app cannot open its local data store. {reason} is a technical error message.',
});
const LOCAL_APP_RENDERER_UNAVAILABLE_DESCRIPTOR = msg({
	message: '{appName} could not load its app files. Quit and open it again.',
	comment:
		'Plain text page shown in the desktop window when the installed app files cannot be found. {appName} is the desktop app name.',
});
const LOCAL_APP_RENDERER_FILE_UNAVAILABLE_DESCRIPTOR = msg({
	message: '{appName} could not read one of its app files. Quit and open it again.',
	comment:
		'Plain text page shown in the desktop window when one installed app file cannot be read. {appName} is the desktop app name.',
});
const LOCAL_APP_NOT_FOUND_DESCRIPTOR = msg({
	message: 'Not found',
	comment: 'Plain text page shown in the desktop window when a requested app file does not exist.',
});
const LOCAL_APP_API_UNAVAILABLE_DESCRIPTOR = msg({
	message: 'No server is selected for this request.',
	comment:
		'Plain text body returned by the desktop app when a request has no active server to go to. Rarely seen by users.',
});
const SHOW_STORE_FOLDER_DESCRIPTOR = msg({
	message: 'Show store folder',
	comment:
		'Button in the native dialog shown when the desktop app cannot open its local data store. Opens the folder that holds the data file.',
});
const FAILED_TO_START_DESCRIPTOR = msg({
	message: '{appName} failed to start',
	comment:
		'Title of the native error dialog shown when the desktop app cannot boot. {appName} is the desktop app name (typically Fluxer).',
});
const APP_LOAD_FAILED_TITLE_DESCRIPTOR = msg({
	message: "Can't connect",
	comment:
		'Title of the native dialog shown when the desktop app has repeatedly failed to load because it cannot reach the servers.',
});
const APP_LOAD_FAILED_MESSAGE_DESCRIPTOR = msg({
	message: "{appName} can't reach its servers. It will keep trying in the background.",
	comment:
		'Body of the native dialog shown when the desktop app has repeatedly failed to load. {appName} is the desktop app name (typically Fluxer). The app keeps retrying on its own while the dialog is open.',
});
const LINUX_ENTRY_GENERIC_NAME_DESCRIPTOR = msg({
	message: 'Instant Messenger',
	comment:
		'GenericName of the Linux .desktop entry. The category of app shown next to the name in GNOME Activities, KDE Kickoff and software centres. Use the generic term for a chat app in your language.',
});
const LINUX_ENTRY_COMMENT_DESCRIPTOR = msg({
	message: 'Instant messaging and VoIP',
	comment:
		'Comment of the Linux .desktop entry. One-line app description shown as a tooltip in Linux launchers and software centres. VoIP is voice over IP; keep the term if it is used untranslated in your language.',
});
const logger = new Logger('DesktopLocaleBridge');
type NativeMessage = MessageDescriptor | string;

const NATIVE_MESSAGES: Record<string, NativeMessage> = {
	'desktop.appMenu.about': ABOUT_DESCRIPTOR,
	'desktop.appMenu.preferences': PREFERENCES_DESCRIPTOR,
	'desktop.appMenu.preferencesPlain': PREFERENCES_2_DESCRIPTOR,
	'desktop.appMenu.hide': HIDE_DESCRIPTOR,
	'desktop.appMenu.quit': QUIT_DESCRIPTOR,
	'desktop.appMenu.exit': EXIT_DESCRIPTOR,
	'desktop.appMenu.file': FILE_DESCRIPTOR,
	'desktop.appMenu.edit': EDIT_DESCRIPTOR,
	'desktop.appMenu.undo': UNDO_DESCRIPTOR,
	'desktop.appMenu.redo': REDO_DESCRIPTOR,
	'desktop.appMenu.cut': CUT_DESCRIPTOR,
	'desktop.appMenu.copy': COPY_DESCRIPTOR,
	'desktop.appMenu.paste': PASTE_DESCRIPTOR,
	'desktop.appMenu.pasteAndMatchStyle': PASTE_AND_MATCH_STYLE_DESCRIPTOR,
	'desktop.appMenu.delete': DELETE_DESCRIPTOR,
	'desktop.appMenu.selectAll': SELECT_ALL_DESCRIPTOR,
	'desktop.appMenu.speech': SPEECH_DESCRIPTOR,
	'desktop.appMenu.startSpeaking': START_SPEAKING_DESCRIPTOR,
	'desktop.appMenu.stopSpeaking': STOP_SPEAKING_DESCRIPTOR,
	'desktop.appMenu.view': VIEW_DESCRIPTOR,
	'desktop.appMenu.reload': RELOAD_DESCRIPTOR,
	'desktop.appMenu.forceReload': FORCE_RELOAD_DESCRIPTOR,
	'desktop.appMenu.toggleDeveloperTools': TOGGLE_DEVELOPER_TOOLS_DESCRIPTOR,
	'desktop.appMenu.actualSize': ACTUAL_SIZE_DESCRIPTOR,
	'desktop.appMenu.zoomIn': ZOOM_IN_DESCRIPTOR,
	'desktop.appMenu.zoomOut': ZOOM_OUT_DESCRIPTOR,
	'desktop.appMenu.toggleFullScreen': TOGGLE_FULL_SCREEN_DESCRIPTOR,
	'desktop.appMenu.window': WINDOW_DESCRIPTOR,
	'desktop.appMenu.minimize': MINIMIZE_DESCRIPTOR,
	'desktop.appMenu.zoomWindow': ZOOM_WINDOW_DESCRIPTOR,
	'desktop.appMenu.close': CLOSE_DESCRIPTOR,
	'desktop.appMenu.help': HELP_DESCRIPTOR,
	'desktop.appMenu.website': WEBSITE_DESCRIPTOR,
	'desktop.appMenu.github': 'GitHub',
	'desktop.appMenu.reportIssue': REPORT_ISSUE_DESCRIPTOR,
	'desktop.appMenu.troubleshooting': TROUBLESHOOTING_DESCRIPTOR,
	'desktop.troubleshooting.disableHardwareAccelerationAndRestart': DISABLE_HARDWARE_ACCELERATION_AND_RESTART_DESCRIPTOR,
	'desktop.troubleshooting.enableHardwareAccelerationAndRestart': ENABLE_HARDWARE_ACCELERATION_AND_RESTART_DESCRIPTOR,
	'desktop.troubleshooting.reload': RELOAD_DESCRIPTOR,
	'desktop.troubleshooting.resetAppDataAndRestart': RESET_APP_DATA_AND_RESTART_DESCRIPTOR,
	'desktop.troubleshooting.resetTitle': RESET_APP_DATA_DESCRIPTOR,
	'desktop.troubleshooting.resetMessage': ARE_YOU_SURE_YOU_WANT_TO_RESET_S_DESCRIPTOR,
	'desktop.troubleshooting.resetDetail': THIS_WILL_SIGN_YOU_OUT_CLEAR_CACHED_FILES_DESCRIPTOR,
	'desktop.troubleshooting.resetConfirm': RESET_AND_RESTART_DESCRIPTOR,
	'desktop.troubleshooting.resetCancel': CANCEL_DESCRIPTOR,
	'desktop.tray.show': SHOW_DESCRIPTOR,
	'desktop.tray.hide': HIDE_DESCRIPTOR,
	'desktop.tray.openSettings': OPEN_SETTINGS_DESCRIPTOR,
	'desktop.tray.status': STATUS_DESCRIPTOR,
	'desktop.tray.statusOnline': ONLINE_DESCRIPTOR,
	'desktop.tray.statusIdle': IDLE_DESCRIPTOR,
	'desktop.tray.statusDnd': DO_NOT_DISTURB_DESCRIPTOR,
	'desktop.tray.statusInvisible': INVISIBLE_DESCRIPTOR,
	'desktop.tray.muteMic': MUTE_MICROPHONE_DESCRIPTOR,
	'desktop.tray.unmuteMic': UNMUTE_MICROPHONE_DESCRIPTOR,
	'desktop.tray.deafen': VOICE_DEAFEN_DESCRIPTOR,
	'desktop.tray.undeafen': VOICE_UNDEAFEN_DESCRIPTOR,
	'desktop.tray.disconnectFrom': DISCONNECT_FROM_DESCRIPTOR,
	'desktop.tray.disconnectVoice': DISCONNECT_FROM_VOICE_DESCRIPTOR,
	'desktop.tray.checkForUpdates': CHECK_FOR_UPDATES_DESCRIPTOR,
	'desktop.tray.copyBuildInfo': COPY_BUILD_INFO_DESCRIPTOR,
	'desktop.tray.restart': RESTART_DESCRIPTOR,
	'desktop.tray.quit': QUIT_DESCRIPTOR,
	'desktop.autostart.portalReason': AUTOSTART_PORTAL_REASON_DESCRIPTOR,
	'desktop.jumpList.openSettings': OPEN_SETTINGS_DESCRIPTOR,
	'desktop.jumpList.openSettingsDescription': OPEN_SETTINGS_2_DESCRIPTOR,
	'desktop.jumpList.newDirectMessage': NEW_DIRECT_MESSAGE_DESCRIPTOR,
	'desktop.jumpList.newDirectMessageDescription': COMPOSE_A_NEW_DIRECT_MESSAGE_DESCRIPTOR,
	'desktop.notifications.open': OPEN_NOTIFICATION_DESCRIPTOR,
	'desktop.themes.addLocalFiles': ADD_LOCAL_FILES_DESCRIPTOR,
	'desktop.themes.importFolder': IMPORT_FOLDER_DESCRIPTOR,
	'desktop.themes.importCss': IMPORT_CSS_DESCRIPTOR,
	'desktop.themes.linkFile': LINK_FILE_DESCRIPTOR,
	'desktop.themes.cssFilesFilter': CSS_FILES_DESCRIPTOR,
	'desktop.badge.unreadMessages': UNREAD_MESSAGES_DESCRIPTOR,
	'desktop.badge.unreadMessagesCount': UNREAD_MESSAGES_COUNT_DESCRIPTOR,
	'desktop.startup.failedTitle': FAILED_TO_START_DESCRIPTOR,
	'desktop.appLoad.failedTitle': APP_LOAD_FAILED_TITLE_DESCRIPTOR,
	'desktop.appLoad.failedMessage': APP_LOAD_FAILED_MESSAGE_DESCRIPTOR,
	'desktop.appLoad.retry': TRY_AGAIN_DESCRIPTOR,
	'desktop.appStore.unreadableTitle': APP_STORE_UNREADABLE_TITLE_DESCRIPTOR,
	'desktop.appStore.unreadableMessage': APP_STORE_UNREADABLE_MESSAGE_DESCRIPTOR,
	'desktop.appStore.unreadableUnchanged': APP_STORE_UNREADABLE_UNCHANGED_DESCRIPTOR,
	'desktop.appStore.unreadableAdvice': APP_STORE_UNREADABLE_ADVICE_DESCRIPTOR,
	'desktop.appStore.unreadableDetails': APP_STORE_UNREADABLE_DETAILS_DESCRIPTOR,
	'desktop.appStore.showStoreFolder': SHOW_STORE_FOLDER_DESCRIPTOR,
	'desktop.localApp.rendererUnavailable': LOCAL_APP_RENDERER_UNAVAILABLE_DESCRIPTOR,
	'desktop.localApp.rendererFileUnavailable': LOCAL_APP_RENDERER_FILE_UNAVAILABLE_DESCRIPTOR,
	'desktop.localApp.notFound': LOCAL_APP_NOT_FOUND_DESCRIPTOR,
	'desktop.localApp.apiUnavailable': LOCAL_APP_API_UNAVAILABLE_DESCRIPTOR,
	'desktop.linuxEntry.genericName': LINUX_ENTRY_GENERIC_NAME_DESCRIPTOR,
	'desktop.linuxEntry.comment': LINUX_ENTRY_COMMENT_DESCRIPTOR,
};
const PLACEHOLDER_PATTERN = /\{(\w+)\}/g;

export function resolvePreservingPlaceholders(descriptor: NativeMessage): string {
	if (typeof descriptor === 'string') return descriptor;
	const template = descriptor.message ?? descriptor.id ?? '';
	const values: Record<string, string> = {};
	for (const match of template.matchAll(PLACEHOLDER_PATTERN)) {
		values[match[1]] = `{${match[1]}}`;
	}
	const merged = Object.assign({}, descriptor, {values});
	return i18n._(merged);
}

function buildPayload(): Record<string, string> {
	const out: Record<string, string> = {};
	for (const [key, descriptor] of Object.entries(NATIVE_MESSAGES)) {
		try {
			out[key] = resolvePreservingPlaceholders(descriptor);
		} catch (error) {
			logger.warn(`Failed to resolve native string ${key}`, error);
		}
	}
	return out;
}

let pushed = false;

export function pushNativeLocale(): void {
	const electronApi = getElectronAPI();
	if (!electronApi || typeof electronApi.setNativeLocale !== 'function') return;
	try {
		electronApi.setNativeLocale(i18n.locale, buildPayload());
		pushed = true;
	} catch (error) {
		logger.error('Failed to push native strings', error);
	}
}

export function startDesktopLocaleBridge(): void {
	if (pushed) return;
	pushNativeLocale();
	onLocaleChange(() => {
		pushNativeLocale();
	});
}
