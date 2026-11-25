// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	CANCEL_DESCRIPTOR,
	COPIED_DESCRIPTOR,
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
const TURN_OFF_CUSTOM_CSS_DESCRIPTOR = msg({
	message: 'Turn off custom CSS',
	comment:
		'Tray menu item in the desktop app. Stops applying quick CSS and saved themes in the app, for example when CSS breaks it.',
});
const TURN_ON_CUSTOM_CSS_DESCRIPTOR = msg({
	message: 'Turn on custom CSS',
	comment: 'Tray menu item in the desktop app. Applies quick CSS and saved themes in the app again.',
});
const CHECK_FOR_UPDATES_DESCRIPTOR = msg({
	message: 'Check for updates',
	comment: 'Tray menu item. Triggers the auto-updater to look for a new desktop build.',
});
const RELOAD_TO_UPDATE_DESCRIPTOR = msg({
	message: 'Reload {appName} to update',
	comment:
		'Tray menu item shown only while a downloaded update of the app files is waiting. Clicking it reloads the window to apply it. {appName} is the desktop app name (typically Fluxer).',
});
const RESTART_TO_UPDATE_DESCRIPTOR = msg({
	message: 'Restart {appName} to update',
	comment:
		'Tray menu item shown only while a downloaded update of the desktop app itself is waiting. Clicking it restarts the app to apply it. {appName} is the desktop app name (typically Fluxer).',
});
const CHECK_FOR_UPDATES_MENU_DESCRIPTOR = msg({
	message: 'Check for updates...',
	comment:
		'Desktop app menu item that looks for a new desktop version. The trailing ellipsis shows that a dialog follows.',
});
const UPDATE_AVAILABLE_MESSAGE_DESCRIPTOR = msg({
	message: 'An update for {appName} is ready to install.',
	comment: 'Native desktop dialog headline shown once an update has been downloaded and can be applied.',
});
const UPDATE_AVAILABLE_DETAIL_DESCRIPTOR = msg({
	message: '{appName} will restart to finish updating.',
	comment:
		'Native desktop dialog body under the update available headline when the desktop app itself updates. Explains what Update now does.',
});
const UPDATE_RELOAD_DETAIL_DESCRIPTOR = msg({
	message: '{appName} will reload to finish updating.',
	comment:
		'Native desktop dialog body under the update available headline when only the app files update. The window reloads and the app itself keeps running.',
});
const UPDATE_NOW_DESCRIPTOR = msg({
	message: 'Update now',
	comment: 'Native desktop dialog button that installs the waiting desktop update.',
});
const UPDATE_LATER_DESCRIPTOR = msg({
	message: 'Later',
	comment: 'Native desktop dialog button that closes an update dialog without updating.',
});
const UPDATE_OK_DESCRIPTOR = msg({
	message: 'OK',
	comment: 'Native desktop dialog button that closes the dialog saying the app is up to date.',
});
const UPDATE_DOWNLOAD_DESCRIPTOR = msg({
	message: 'Download',
	comment: 'Native desktop dialog button that opens the download page for a new desktop version in the browser.',
});
const UPDATE_MANUAL_MESSAGE_DESCRIPTOR = msg({
	message: '{appName} {version} is available.',
	comment:
		'Native desktop dialog headline on installs that cannot update themselves. {version} is the new version number.',
});
const UPDATE_MANUAL_DETAIL_DESCRIPTOR = msg({
	message: 'Download the new version to update {appName}.',
	comment: 'Native desktop dialog body under the headline announcing a new version that must be downloaded by hand.',
});
const UPDATE_MANAGED_DETAIL_DESCRIPTOR = msg({
	message: 'Update {appName} with your package manager, or download the new version.',
	comment:
		'Native desktop dialog body under the headline announcing a new version, shown on a Linux install managed by a package manager such as a deb, rpm or AUR package. {appName} is the desktop app name (typically Fluxer).',
});
const UPDATE_FLATPAK_DETAIL_DESCRIPTOR = msg({
	message: 'Update {appName} from your software center.',
	comment:
		'Native desktop dialog body under the headline announcing a new version, shown on a Flatpak install. {appName} is the desktop app name (typically Fluxer).',
});
const UPDATE_OPEN_SOFTWARE_CENTER_DESCRIPTOR = msg({
	message: 'Open software center',
	comment: 'Native desktop dialog button on a Flatpak install that opens the software center page for the desktop app.',
});
const UPDATE_UP_TO_DATE_MESSAGE_DESCRIPTOR = msg({
	message: '{appName} is up to date.',
	comment: 'Native desktop dialog headline after a manual update check found nothing newer.',
});
const UPDATE_UP_TO_DATE_DETAIL_DESCRIPTOR = msg({
	message: 'You have version {version}.',
	comment: 'Native desktop dialog body under the up to date headline. {version} is the installed version number.',
});
const UPDATE_CHECK_FAILED_MESSAGE_DESCRIPTOR = msg({
	message: "{appName} couldn't check for updates.",
	comment: 'Native desktop dialog headline when a manual update check could not reach the update server.',
});
const UPDATE_CHECK_FAILED_DETAIL_DESCRIPTOR = msg({
	message: 'Check your internet connection and try again.',
	comment: 'Native desktop dialog body under the headline saying the update check failed.',
});
const UPDATE_FAILED_MESSAGE_DESCRIPTOR = msg({
	message: "{appName} couldn't finish updating.",
	comment: 'Native desktop dialog headline when a desktop update the user started did not complete.',
});
const UPDATE_FAILED_DETAIL_DESCRIPTOR = msg({
	message: 'The update did not download or install. Try again or download it yourself.',
	comment:
		'Native desktop dialog body under the headline saying the update did not finish. The buttons are Try again, Download and Later.',
});
const UPDATE_STILL_DOWNLOADING_DETAIL_DESCRIPTOR = msg({
	message: 'The update is still downloading. Try again in a few minutes.',
	comment: 'Native desktop dialog body shown when the update download is slow and still running in the background.',
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
const TRAY_ACCOUNTS_DESCRIPTOR = msg({
	message: 'Accounts',
	comment: 'Tray and dock menu submenu title listing every signed-in account.',
});
const TRAY_CHOOSE_ACCOUNT_DESCRIPTOR = msg({
	message: 'Choose an account…',
	comment: 'Tray and dock menu item that opens the full-screen account picker.',
});
const TRAY_ACCOUNT_WITH_HOST_DESCRIPTOR = msg({
	message: '{name} ({host})',
	comment:
		'Tray menu account item when the signed-in accounts are on more than one instance. {name} is the account name, {host} is the instance address.',
});
const TRAY_ACCOUNT_NUMBERED_DESCRIPTOR = msg({
	message: 'Account {number}',
	comment:
		'Tray and dock menu account item while streamer mode hides personal information. {number} is the account position in the list, such as 2.',
});
const TRAY_ACCOUNT_WITH_MENTIONS_DESCRIPTOR = msg({
	message: '{label} · {count}',
	comment:
		'Tray menu account item with unread mentions. {label} is the account name, {count} is the number of mentions.',
});
const JUMP_LIST_CHOOSE_ACCOUNT_DESCRIPTOR = msg({
	message: 'Choose an account',
	comment: 'Windows jump-list task title. Opens the full-screen account picker.',
});
const JUMP_LIST_CHOOSE_ACCOUNT_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Switch to another signed-in account',
	comment: 'Windows jump-list task description (tooltip text) for the Choose an account action.',
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
	message: "{appName} can't connect to its servers. It will keep trying in the background.",
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
const SERVICES_DESCRIPTOR = msg({
	message: 'Services',
	context: 'macos-app-menu',
	comment: 'macOS app menu submenu that lists system Services for the selected text. Use the standard macOS term.',
});
const HIDE_OTHERS_DESCRIPTOR = msg({
	message: 'Hide others',
	comment: 'macOS app menu item that hides every other app window. Use the standard macOS term.',
});
const SHOW_ALL_DESCRIPTOR = msg({
	message: 'Show all',
	context: 'macos-app-menu',
	comment: 'macOS app menu item that shows the windows of every hidden app again. Use the standard macOS term.',
});
const BRING_ALL_TO_FRONT_DESCRIPTOR = msg({
	message: 'Bring all to front',
	comment:
		'macOS Window menu item that brings every window of the desktop app to the front. Use the standard macOS term.',
});
const TRAY_TOOLTIP_WITH_UNREAD_DESCRIPTOR = msg({
	message: '{appName} - {description}',
	comment:
		'Tooltip on the desktop tray or menu bar icon while there are unread messages. {appName} is the desktop app name (typically Fluxer). {description} is the unread summary, such as "Unread messages: 3". Change the separator only if your language needs it.',
});
const QUICK_CSS_WINDOW_TITLE_DESCRIPTOR = msg({
	message: '{appName} | Quick CSS',
	comment:
		'Title of the desktop window that holds the popped out Quick CSS editor. {appName} is the desktop app name (typically Fluxer). Use the same term for Quick CSS as in Settings > Themes.',
});
const SAVE_DIALOG_ALL_FILES_DESCRIPTOR = msg({
	message: 'All files',
	comment: 'File type filter in the desktop save dialog on Windows and Linux that shows files of any type.',
});
const SAVE_DIALOG_FILE_TYPE_DESCRIPTOR = msg({
	message: '{extension} file',
	comment:
		'File type filter in the desktop save dialog on Windows and Linux. {extension} is an uppercase file extension such as PNG or PDF.',
});
const SPLASH_WINDOW_TITLE_DESCRIPTOR = msg({
	message: '{appName} Updater',
	comment:
		'Title of the small desktop window that checks for and installs updates before the app opens. {appName} is the desktop app name (typically Fluxer).',
});
const SPLASH_CHECKING_FOR_UPDATES_DESCRIPTOR = msg({
	message: 'Checking for updates…',
	comment: 'Status line in the desktop update window shown while the app looks for updates before it opens.',
});
const SPLASH_DOWNLOADING_UPDATES_DESCRIPTOR = msg({
	message: 'Downloading update {current} of {total}…',
	comment:
		'Status line in the desktop update window while update packages download. {current} is the package being downloaded and {total} is the number of packages. Both are already formatted numbers.',
});
const SPLASH_INSTALLING_UPDATES_DESCRIPTOR = msg({
	message: 'Installing update {current} of {total}…',
	comment:
		'Status line in the desktop update window while update packages install. {current} is the package being installed and {total} is the number of packages. Both are already formatted numbers.',
});
const SPLASH_VERIFYING_DESCRIPTOR = msg({
	message: 'Verifying files…',
	comment: 'Status line in the desktop update window while downloaded update files are checked.',
});
const SPLASH_DOWNLOAD_STALLED_DESCRIPTOR = msg({
	message: 'Download stalled. Retrying in {seconds} sec…',
	comment:
		'Status line in the desktop update window when an update download stopped making progress. {seconds} counts down each second. Use the short unit for seconds so the text reads well for any number, because plural forms are not available here.',
});
const SPLASH_UPDATE_FAILED_DESCRIPTOR = msg({
	message: 'Update failed. Retrying in {seconds} sec…',
	comment:
		'Status line in the desktop update window when an update attempt failed. {seconds} counts down each second. Use the short unit for seconds so the text reads well for any number, because plural forms are not available here.',
});
const SPLASH_SHELL_UPDATE_DOWNLOADING_DESCRIPTOR = msg({
	message: 'Updating {appName}…',
	comment:
		'Status line in the desktop update window while a new version of the desktop app downloads. {appName} is the desktop app name (typically Fluxer).',
});
const SPLASH_SHELL_UPDATE_RESTARTING_DESCRIPTOR = msg({
	message: 'Restarting to finish the update…',
	comment: 'Status line in the desktop update window right before the desktop app restarts to apply an update.',
});
const SPLASH_BLOCKED_UPDATE_REQUIRED_DESCRIPTOR = msg({
	message: 'Update required to continue',
	comment:
		'Status line in the desktop update window when the app cannot open until a required update installs. A Retry now button is shown below.',
});
const SPLASH_BLOCKED_UPDATE_UNREACHABLE_DESCRIPTOR = msg({
	message: "Can't reach the {appName} update server",
	comment:
		'Status line in the desktop update window when the update server cannot be reached. {appName} is the desktop app name (typically Fluxer).',
});
const SPLASH_BLOCKED_SHELL_UPDATE_DESCRIPTOR = msg({
	message: 'A new version of {appName} is required',
	comment:
		'Status line in the desktop update window when this version of the desktop app is too old to continue. A Download update button is shown below. {appName} is the desktop app name (typically Fluxer).',
});
const SPLASH_BLOCKED_UNSUPPORTED_BUILD_DESCRIPTOR = msg({
	message: 'This installation is incomplete. Reinstall {appName}',
	comment:
		'Status line in the desktop update window when the installed desktop app is missing files it needs to start. {appName} is the desktop app name (typically Fluxer).',
});
const SPLASH_LAUNCHING_DESCRIPTOR = msg({
	message: 'Starting…',
	comment: 'Status line in the desktop update window while the desktop app opens after checking for updates.',
});
const SPLASH_UNREACHABLE_LAUNCH_DESCRIPTOR = msg({
	message: 'Starting offline…',
	comment:
		'Status line in the desktop update window when the app opens without checking for updates because the update server cannot be reached.',
});
const SPLASH_WAITING_LOCAL_NETWORK_DESCRIPTOR = msg({
	message: 'Waiting for local network permission…',
	comment:
		'Status line in the desktop update window on macOS while the system asks the user to allow access to the local network.',
});
const SPLASH_DOWNLOAD_AMOUNT_DESCRIPTOR = msg({
	message: '{received} of {total} MB',
	comment:
		'Download progress in the desktop update window. {received} and {total} are already formatted numbers of megabytes. Use the local abbreviation for megabytes.',
});
const SPLASH_DOWNLOAD_AMOUNT_WITH_SPEED_DESCRIPTOR = msg({
	message: '{received} of {total} MB, {speed}',
	comment:
		'Download progress with the current speed in the desktop update window. {received} and {total} are already formatted numbers of megabytes. {speed} is a formatted speed such as "2.4 MB/s". Use the local abbreviation for megabytes.',
});
const SPLASH_SPEED_MEGABYTES_DESCRIPTOR = msg({
	message: '{value} MB/s',
	comment:
		'Download speed in megabytes per second in the desktop update window. {value} is an already formatted number. Use the local abbreviation.',
});
const SPLASH_SPEED_KILOBYTES_DESCRIPTOR = msg({
	message: '{value} KB/s',
	comment:
		'Download speed in kilobytes per second in the desktop update window. {value} is an already formatted number. Use the local abbreviation.',
});
const SPLASH_OPEN_LOGS_DESCRIPTOR = msg({
	message: 'Open logs',
	comment: 'Small link button in the desktop update window that opens the folder with the desktop app log files.',
});
const SPLASH_COPY_DIAGNOSTICS_DESCRIPTOR = msg({
	message: 'Copy diagnostics',
	comment:
		'Small link button in the desktop update window that copies technical details about the update problem to the clipboard, for a support request.',
});
const SPLASH_RETRY_NOW_DESCRIPTOR = msg({
	message: 'Retry now',
	comment: 'Button in the desktop update window that tries the update again right away.',
});
const SPLASH_DOWNLOAD_UPDATE_DESCRIPTOR = msg({
	message: 'Download update',
	comment:
		'Button in the desktop update window that opens the download page for the new version of the desktop app in the browser.',
});
const SPLASH_QUIT_DESCRIPTOR = msg({
	message: 'Quit',
	context: 'desktop-update-window',
	comment: 'Button in the desktop update window that closes the desktop app. Imperative verb.',
});
const SPLASH_UPDATE_SERVER_UNREACHABLE_DESCRIPTOR = msg({
	message:
		"{appName} couldn't reach its update server to download app files. Check your internet connection, proxy or firewall. {appName} keeps retrying.",
	comment:
		'Explanation in the desktop update window when the update server cannot be reached and the app cannot open without an update. {appName} is the desktop app name (typically Fluxer).',
});
const SPLASH_NOT_ENOUGH_DISK_SPACE_DESCRIPTOR = msg({
	message: 'Not enough disk space to install the update',
	comment:
		'Explanation in the desktop update window when the computer has no free disk space left for the update files.',
});
const logger = new Logger('DesktopLocaleBridge');
type NativeMessage = MessageDescriptor | string;

const NATIVE_MESSAGES: Record<string, NativeMessage> = {
	'desktop.appMenu.about': ABOUT_DESCRIPTOR,
	'desktop.appMenu.preferences': PREFERENCES_DESCRIPTOR,
	'desktop.appMenu.preferencesPlain': PREFERENCES_2_DESCRIPTOR,
	'desktop.appMenu.hide': HIDE_DESCRIPTOR,
	'desktop.appMenu.hideOthers': HIDE_OTHERS_DESCRIPTOR,
	'desktop.appMenu.showAll': SHOW_ALL_DESCRIPTOR,
	'desktop.appMenu.services': SERVICES_DESCRIPTOR,
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
	'desktop.appMenu.bringAllToFront': BRING_ALL_TO_FRONT_DESCRIPTOR,
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
	'desktop.tray.reloadToUpdate': RELOAD_TO_UPDATE_DESCRIPTOR,
	'desktop.tray.restartToUpdate': RESTART_TO_UPDATE_DESCRIPTOR,
	'desktop.appMenu.checkForUpdates': CHECK_FOR_UPDATES_MENU_DESCRIPTOR,
	'desktop.update.availableMessage': UPDATE_AVAILABLE_MESSAGE_DESCRIPTOR,
	'desktop.update.availableDetail': UPDATE_AVAILABLE_DETAIL_DESCRIPTOR,
	'desktop.update.reloadDetail': UPDATE_RELOAD_DETAIL_DESCRIPTOR,
	'desktop.update.install': UPDATE_NOW_DESCRIPTOR,
	'desktop.update.later': UPDATE_LATER_DESCRIPTOR,
	'desktop.update.ok': UPDATE_OK_DESCRIPTOR,
	'desktop.update.download': UPDATE_DOWNLOAD_DESCRIPTOR,
	'desktop.update.manualMessage': UPDATE_MANUAL_MESSAGE_DESCRIPTOR,
	'desktop.update.manualDetail': UPDATE_MANUAL_DETAIL_DESCRIPTOR,
	'desktop.update.managedDetail': UPDATE_MANAGED_DETAIL_DESCRIPTOR,
	'desktop.update.flatpakDetail': UPDATE_FLATPAK_DETAIL_DESCRIPTOR,
	'desktop.update.openSoftwareCenter': UPDATE_OPEN_SOFTWARE_CENTER_DESCRIPTOR,
	'desktop.update.upToDateMessage': UPDATE_UP_TO_DATE_MESSAGE_DESCRIPTOR,
	'desktop.update.upToDateDetail': UPDATE_UP_TO_DATE_DETAIL_DESCRIPTOR,
	'desktop.update.checkFailedMessage': UPDATE_CHECK_FAILED_MESSAGE_DESCRIPTOR,
	'desktop.update.checkFailedDetail': UPDATE_CHECK_FAILED_DETAIL_DESCRIPTOR,
	'desktop.update.failedMessage': UPDATE_FAILED_MESSAGE_DESCRIPTOR,
	'desktop.update.failedDetail': UPDATE_FAILED_DETAIL_DESCRIPTOR,
	'desktop.update.stillDownloadingDetail': UPDATE_STILL_DOWNLOADING_DETAIL_DESCRIPTOR,
	'desktop.tray.copyBuildInfo': COPY_BUILD_INFO_DESCRIPTOR,
	'desktop.tray.turnOffCustomCss': TURN_OFF_CUSTOM_CSS_DESCRIPTOR,
	'desktop.tray.turnOnCustomCss': TURN_ON_CUSTOM_CSS_DESCRIPTOR,
	'desktop.tray.restart': RESTART_DESCRIPTOR,
	'desktop.tray.quit': QUIT_DESCRIPTOR,
	'desktop.autostart.portalReason': AUTOSTART_PORTAL_REASON_DESCRIPTOR,
	'desktop.jumpList.openSettings': OPEN_SETTINGS_DESCRIPTOR,
	'desktop.jumpList.openSettingsDescription': OPEN_SETTINGS_2_DESCRIPTOR,
	'desktop.jumpList.newDirectMessage': NEW_DIRECT_MESSAGE_DESCRIPTOR,
	'desktop.jumpList.newDirectMessageDescription': COMPOSE_A_NEW_DIRECT_MESSAGE_DESCRIPTOR,
	'desktop.jumpList.chooseAccount': JUMP_LIST_CHOOSE_ACCOUNT_DESCRIPTOR,
	'desktop.jumpList.chooseAccountDescription': JUMP_LIST_CHOOSE_ACCOUNT_DESCRIPTION_DESCRIPTOR,
	'desktop.tray.toolTipWithUnread': TRAY_TOOLTIP_WITH_UNREAD_DESCRIPTOR,
	'desktop.tray.accounts': TRAY_ACCOUNTS_DESCRIPTOR,
	'desktop.tray.chooseAccount': TRAY_CHOOSE_ACCOUNT_DESCRIPTOR,
	'desktop.tray.accountWithHost': TRAY_ACCOUNT_WITH_HOST_DESCRIPTOR,
	'desktop.tray.accountWithMentions': TRAY_ACCOUNT_WITH_MENTIONS_DESCRIPTOR,
	'desktop.tray.accountNumbered': TRAY_ACCOUNT_NUMBERED_DESCRIPTOR,
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
	'desktop.quickCss.windowTitle': QUICK_CSS_WINDOW_TITLE_DESCRIPTOR,
	'desktop.saveDialog.allFiles': SAVE_DIALOG_ALL_FILES_DESCRIPTOR,
	'desktop.saveDialog.fileType': SAVE_DIALOG_FILE_TYPE_DESCRIPTOR,
	'desktop.splash.windowTitle': SPLASH_WINDOW_TITLE_DESCRIPTOR,
	'desktop.splash.checkingForUpdates': SPLASH_CHECKING_FOR_UPDATES_DESCRIPTOR,
	'desktop.splash.downloadingUpdates': SPLASH_DOWNLOADING_UPDATES_DESCRIPTOR,
	'desktop.splash.installingUpdates': SPLASH_INSTALLING_UPDATES_DESCRIPTOR,
	'desktop.splash.verifying': SPLASH_VERIFYING_DESCRIPTOR,
	'desktop.splash.downloadStalled': SPLASH_DOWNLOAD_STALLED_DESCRIPTOR,
	'desktop.splash.updateFailed': SPLASH_UPDATE_FAILED_DESCRIPTOR,
	'desktop.splash.shellUpdateDownloading': SPLASH_SHELL_UPDATE_DOWNLOADING_DESCRIPTOR,
	'desktop.splash.shellUpdateRestarting': SPLASH_SHELL_UPDATE_RESTARTING_DESCRIPTOR,
	'desktop.splash.blockedUpdateRequired': SPLASH_BLOCKED_UPDATE_REQUIRED_DESCRIPTOR,
	'desktop.splash.blockedUpdateUnreachable': SPLASH_BLOCKED_UPDATE_UNREACHABLE_DESCRIPTOR,
	'desktop.splash.blockedShellUpdate': SPLASH_BLOCKED_SHELL_UPDATE_DESCRIPTOR,
	'desktop.splash.blockedUnsupportedBuild': SPLASH_BLOCKED_UNSUPPORTED_BUILD_DESCRIPTOR,
	'desktop.splash.launching': SPLASH_LAUNCHING_DESCRIPTOR,
	'desktop.splash.unreachableLaunch': SPLASH_UNREACHABLE_LAUNCH_DESCRIPTOR,
	'desktop.splash.waitingLocalNetwork': SPLASH_WAITING_LOCAL_NETWORK_DESCRIPTOR,
	'desktop.splash.downloadAmount': SPLASH_DOWNLOAD_AMOUNT_DESCRIPTOR,
	'desktop.splash.downloadAmountWithSpeed': SPLASH_DOWNLOAD_AMOUNT_WITH_SPEED_DESCRIPTOR,
	'desktop.splash.speedMegabytes': SPLASH_SPEED_MEGABYTES_DESCRIPTOR,
	'desktop.splash.speedKilobytes': SPLASH_SPEED_KILOBYTES_DESCRIPTOR,
	'desktop.splash.openLogs': SPLASH_OPEN_LOGS_DESCRIPTOR,
	'desktop.splash.copyDiagnostics': SPLASH_COPY_DIAGNOSTICS_DESCRIPTOR,
	'desktop.splash.copied': COPIED_DESCRIPTOR,
	'desktop.splash.retryNow': SPLASH_RETRY_NOW_DESCRIPTOR,
	'desktop.splash.downloadUpdate': SPLASH_DOWNLOAD_UPDATE_DESCRIPTOR,
	'desktop.splash.quit': SPLASH_QUIT_DESCRIPTOR,
	'desktop.splash.updateServerUnreachable': SPLASH_UPDATE_SERVER_UNREACHABLE_DESCRIPTOR,
	'desktop.splash.notEnoughDiskSpace': SPLASH_NOT_ENOUGH_DISK_SPACE_DESCRIPTOR,
};
const PLACEHOLDER_PATTERN = /\{(\w+)\}/g;

function resolvePreservingPlaceholders(descriptor: NativeMessage): string {
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

function pushNativeLocale(): void {
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
