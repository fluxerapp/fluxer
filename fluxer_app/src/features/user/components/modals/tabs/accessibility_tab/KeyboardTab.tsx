// SPDX-License-Identifier: AGPL-3.0-or-later

import * as AccessibilityCommands from '@app/features/accessibility/commands/AccessibilityCommands';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {
	CONFIRM_BEFORE_STARTING_CALLS_DESCRIPTOR,
	ESCAPE_KEY_EXITS_KEYBOARD_MODE_DESCRIPTOR,
	SHOW_CONTEXT_MENU_SHORTCUTS_DESCRIPTOR,
	SHOW_FOCUS_RING_ON_CHAT_TEXTAREA_DESCRIPTOR,
} from '@app/features/user/utils/UserMessageDescriptors';
import VoicePrompts from '@app/features/voice/state/VoicePrompts';
import {
	VOICE_P2P_ALWAYS_AGREE_SETTING_DESCRIPTION_DESCRIPTOR,
	VOICE_P2P_ALWAYS_AGREE_SETTING_DESCRIPTOR,
} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
export const KeyboardTabContent: React.FC = observer(() => {
	const {i18n} = useLingui();
	const showTextareaFocusRing = Accessibility.showTextareaFocusRing;
	const escapeExitsKeyboardMode = Accessibility.escapeExitsKeyboardMode;
	const showContextMenuShortcuts = Accessibility.showContextMenuShortcuts;
	const confirmBeforeStartingCalls = Accessibility.confirmBeforeStartingCalls;
	const skipP2pJoinConfirm = VoicePrompts.skipP2pJoinConfirm;
	return (
		<>
			<Switch
				label={i18n._(SHOW_FOCUS_RING_ON_CHAT_TEXTAREA_DESCRIPTOR)}
				value={showTextareaFocusRing}
				onChange={(value) => AccessibilityCommands.update({showTextareaFocusRing: value})}
				data-flx="user.accessibility-tab.keyboard-tab.keyboard-tab-content.switch.update"
			/>
			<Switch
				label={i18n._(ESCAPE_KEY_EXITS_KEYBOARD_MODE_DESCRIPTOR)}
				value={escapeExitsKeyboardMode}
				onChange={(value) => AccessibilityCommands.update({escapeExitsKeyboardMode: value})}
				data-flx="user.accessibility-tab.keyboard-tab.keyboard-tab-content.switch.update--2"
			/>
			<Switch
				label={i18n._(SHOW_CONTEXT_MENU_SHORTCUTS_DESCRIPTOR)}
				value={showContextMenuShortcuts}
				onChange={(value) => AccessibilityCommands.update({showContextMenuShortcuts: value})}
				data-flx="user.accessibility-tab.keyboard-tab.keyboard-tab-content.switch.update--3"
			/>
			<Switch
				label={i18n._(CONFIRM_BEFORE_STARTING_CALLS_DESCRIPTOR)}
				value={confirmBeforeStartingCalls}
				onChange={(value) => AccessibilityCommands.update({confirmBeforeStartingCalls: value})}
				data-flx="user.accessibility-tab.keyboard-tab.keyboard-tab-content.switch.update--4"
			/>
			<Switch
				label={i18n._(VOICE_P2P_ALWAYS_AGREE_SETTING_DESCRIPTOR)}
				description={i18n._(VOICE_P2P_ALWAYS_AGREE_SETTING_DESCRIPTION_DESCRIPTOR)}
				value={skipP2pJoinConfirm}
				onChange={(value) => VoicePrompts.setSkipP2pJoinConfirm(value)}
				data-flx="user.accessibility-tab.keyboard-tab.keyboard-tab-content.switch.skip-p2p-join-confirm"
			/>
		</>
	);
});
