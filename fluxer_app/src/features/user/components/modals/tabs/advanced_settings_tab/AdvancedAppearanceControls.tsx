// SPDX-License-Identifier: AGPL-3.0-or-later

import * as AccessibilityCommands from '@app/features/accessibility/commands/AccessibilityCommands';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import HiddenGuildListButtons from '@app/features/guild/state/HiddenGuildListButtons';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {SwitchGroup, SwitchGroupItem} from '@app/features/ui/components/SwitchGroup';
import {
	ENABLE_FAVORITES_DESCRIPTOR,
	KEEP_NEKO_STILL_DESCRIPTOR,
	KEYBOARD_HINTS_DESCRIPTOR,
	SHOW_DOWNLOAD_BUTTON_DESCRIPTOR,
	SHOW_HELP_CENTER_BUTTON_DESCRIPTOR,
	SHOW_NEKO_DESCRIPTOR,
} from '@app/features/user/components/settings_utils/section_registry/SharedDescriptors';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const REQUIRE_DOUBLE_CLICK_TO_JOIN_VOICE_DESCRIPTOR = msg({
	message: 'Require double-click to join voice channels',
	comment: 'Short label for an advanced voice-channel navigation preference.',
});
const CONFIRM_BEFORE_JOINING_VOICE_CHANNELS_DESCRIPTOR = msg({
	message: 'Confirm before joining voice channels',
	comment: 'Short label for an advanced voice-channel safety preference.',
});

export const ShowNekoControl = observer(() => {
	const {i18n} = useLingui();
	return (
		<Switch
			ariaLabel={i18n._(SHOW_NEKO_DESCRIPTOR)}
			value={Accessibility.showNeko}
			onChange={(value) => AccessibilityCommands.update({showNeko: value})}
			compact
			data-flx="user.advanced-settings-tab.switch.show-neko"
		/>
	);
});

export const KeepNekoStillControl = observer(() => {
	const {i18n} = useLingui();
	return (
		<Switch
			ariaLabel={i18n._(KEEP_NEKO_STILL_DESCRIPTOR)}
			value={Accessibility.keepNekoStill}
			onChange={(value) => AccessibilityCommands.update({keepNekoStill: value})}
			compact
			data-flx="user.advanced-settings-tab.switch.keep-neko-still"
		/>
	);
});

export const HideKeyboardHintsControl = observer(() => {
	const {i18n} = useLingui();
	return (
		<Switch
			ariaLabel={i18n._(KEYBOARD_HINTS_DESCRIPTOR)}
			value={!Accessibility.hideKeyboardHints}
			onChange={(value) => AccessibilityCommands.update({hideKeyboardHints: !value})}
			compact
			data-flx="user.advanced-settings-tab.switch.hide-keyboard-hints"
		/>
	);
});

export const VoiceChannelJoinBehaviorControl = observer(() => {
	const {i18n} = useLingui();
	return (
		<SwitchGroup data-flx="user.advanced-settings-tab.switch-group.voice-channel-join-behavior">
			<SwitchGroupItem
				label={i18n._(REQUIRE_DOUBLE_CLICK_TO_JOIN_VOICE_DESCRIPTOR)}
				value={Accessibility.voiceChannelJoinRequiresDoubleClick}
				onChange={(value) => AccessibilityCommands.update({voiceChannelJoinRequiresDoubleClick: value})}
				data-flx="user.advanced-settings-tab.switch-group-item.voice-channel-double-click"
			/>
			<SwitchGroupItem
				label={i18n._(CONFIRM_BEFORE_JOINING_VOICE_CHANNELS_DESCRIPTOR)}
				value={Accessibility.confirmBeforeJoiningVoiceChannels && !Accessibility.voiceChannelJoinRequiresDoubleClick}
				disabled={Accessibility.voiceChannelJoinRequiresDoubleClick}
				onChange={(value) => AccessibilityCommands.update({confirmBeforeJoiningVoiceChannels: value})}
				data-flx="user.advanced-settings-tab.switch-group-item.voice-channel-confirm"
			/>
		</SwitchGroup>
	);
});

export const FavoritesControl = observer(() => {
	const {i18n} = useLingui();
	return (
		<Switch
			ariaLabel={i18n._(ENABLE_FAVORITES_DESCRIPTOR)}
			value={Accessibility.showFavorites}
			onChange={(value) => AccessibilityCommands.update({showFavorites: value})}
			compact
			data-flx="user.advanced-settings-tab.switch.favorites"
		/>
	);
});

export const HelpCenterButtonControl = observer(() => {
	const {i18n} = useLingui();
	return (
		<Switch
			ariaLabel={i18n._(SHOW_HELP_CENTER_BUTTON_DESCRIPTOR)}
			value={!HiddenGuildListButtons.helpButtonHidden}
			onChange={(value) => (value ? HiddenGuildListButtons.showHelpButton() : HiddenGuildListButtons.hideHelpButton())}
			compact
			data-flx="user.advanced-settings-tab.switch.help-center-button"
		/>
	);
});

export const DownloadButtonControl = observer(() => {
	const {i18n} = useLingui();
	return (
		<Switch
			ariaLabel={i18n._(SHOW_DOWNLOAD_BUTTON_DESCRIPTOR)}
			value={!HiddenGuildListButtons.downloadButtonHidden}
			onChange={(value) =>
				value ? HiddenGuildListButtons.showDownloadButton() : HiddenGuildListButtons.hideDownloadButton()
			}
			compact
			data-flx="user.advanced-settings-tab.switch.download-button"
		/>
	);
});
