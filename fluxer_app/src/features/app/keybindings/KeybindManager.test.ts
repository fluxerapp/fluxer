// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	keyboardEventCanRecoverStaleMacMetaPress,
	keyboardEventMatchesCombo,
	keyboardEventReleasesComboModifier,
	keyboardEventStartsComboPress,
	keyboardEventTriggerMatchesCombo,
	shouldAllowLocalShortcutForChannelTextarea,
} from '@app/features/app/keybindings/KeybindEventUtils';
import {
	isKeybindAllowedDuringVoiceCallFullscreen,
	isKeybindBlockedByCompactVoiceCallView,
} from '@app/features/app/keybindings/KeybindScopeUtils';
import {comboToCombokeysStrings} from '@app/features/app/keybindings/utils/ComboShortcutStrings';
import {keyNameForGlobalHook, physicalKeyNameForGlobalHook} from '@app/features/app/keybindings/utils/GlobalHookKeys';
import {hookShortcutIdForKeybind} from '@app/features/app/keybindings/utils/HookShortcutIds';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {describe, expect, it} from 'vitest';

const keyEvent = (
	overrides: Partial<{
		key: string;
		code: string;
		ctrlKey: boolean;
		altKey: boolean;
		shiftKey: boolean;
		metaKey: boolean;
		repeat: boolean;
	}>,
) => ({
	key: '',
	code: '',
	ctrlKey: false,
	altKey: false,
	shiftKey: false,
	metaKey: false,
	repeat: false,
	...overrides,
});
const runtimeKeybind = (combo: {key: string; code?: string; ctrlOrMeta?: boolean; shift?: boolean; global?: boolean}) =>
	({
		action: 'voice_toggle_mute',
		combo,
	}) as const;

describe('KeybindManager editable shortcut matching', () => {
	it('matches Ctrl+Shift+U exactly for the upload shortcut on non-mac platforms', () => {
		expect(
			keyboardEventMatchesCombo(
				{key: 'u', ctrlOrMeta: true, shift: true},
				keyEvent({key: 'U', code: 'KeyU', ctrlKey: true, shiftKey: true}),
				{isMacOS: false},
			),
		).toBe(true);
	});
	it('treats Command keyup as releasing a macOS ctrlOrMeta shortcut latch', () => {
		expect(
			keyboardEventReleasesComboModifier({key: 'g', ctrlOrMeta: true}, keyEvent({key: 'Meta', code: 'MetaLeft'}), {
				isMacOS: true,
			}),
		).toBe(true);
		expect(
			keyboardEventReleasesComboModifier({key: 'g', ctrlOrMeta: true}, keyEvent({key: 'Meta', code: 'MetaLeft'}), {
				isMacOS: false,
			}),
		).toBe(false);
	});
	it('treats a fresh Command+G keydown as a shortcut start while Command remains held', () => {
		expect(
			keyboardEventStartsComboPress({key: 'g', ctrlOrMeta: true}, keyEvent({key: 'g', code: 'KeyG', metaKey: true}), {
				isMacOS: true,
			}),
		).toBe(true);
		expect(
			keyboardEventStartsComboPress(
				{key: 'g', ctrlOrMeta: true},
				keyEvent({key: 'g', code: 'KeyG', metaKey: true, repeat: true}),
				{isMacOS: true},
			),
		).toBe(false);
	});
	it('only allows stale press recovery for macOS Command shortcuts', () => {
		expect(
			keyboardEventCanRecoverStaleMacMetaPress(
				{key: 'g', ctrlOrMeta: true},
				keyEvent({key: 'g', code: 'KeyG', metaKey: true}),
				{isMacOS: true},
			),
		).toBe(true);
		expect(
			keyboardEventCanRecoverStaleMacMetaPress(
				{key: 'd', ctrlOrMeta: true, shift: true},
				keyEvent({key: 'd', code: 'KeyD', ctrlKey: true, shiftKey: true}),
				{isMacOS: false},
			),
		).toBe(false);
		expect(
			keyboardEventCanRecoverStaleMacMetaPress(
				{key: 'd', ctrlOrMeta: true, shift: true},
				keyEvent({key: 'd', code: 'KeyD', metaKey: true, shiftKey: true, repeat: true}),
				{isMacOS: true},
			),
		).toBe(false);
	});
	it('matches repeated trigger-key presses while macOS modifiers stay held', () => {
		const combo = {key: 'd', ctrlOrMeta: true, shift: true};
		const keyDown = keyEvent({key: 'd', code: 'KeyD', metaKey: true, shiftKey: true});
		const keyUp = keyEvent({key: 'd', code: 'KeyD', metaKey: true, shiftKey: true});
		expect(keyboardEventStartsComboPress(combo, keyDown, {isMacOS: true})).toBe(true);
		expect(keyboardEventTriggerMatchesCombo(combo, keyUp)).toBe(true);
		expect(keyboardEventStartsComboPress(combo, keyDown, {isMacOS: true})).toBe(true);
	});
	it('treats Control keyup as releasing a non-mac ctrlOrMeta shortcut latch', () => {
		expect(
			keyboardEventReleasesComboModifier(
				{key: 'g', ctrlOrMeta: true},
				keyEvent({key: 'Control', code: 'ControlLeft'}),
				{isMacOS: false},
			),
		).toBe(true);
		expect(
			keyboardEventReleasesComboModifier(
				{key: 'g', ctrlOrMeta: true},
				keyEvent({key: 'Control', code: 'ControlLeft'}),
				{isMacOS: true},
			),
		).toBe(false);
	});
	it('does not let extra Shift make Ctrl+U match Ctrl+Shift+U', () => {
		expect(
			keyboardEventMatchesCombo(
				{key: 'u', ctrlOrMeta: true},
				keyEvent({key: 'U', code: 'KeyU', ctrlKey: true, shiftKey: true}),
				{isMacOS: false},
			),
		).toBe(false);
	});
	it('allows Shift when a layout needs it to produce a printable punctuation shortcut', () => {
		expect(
			keyboardEventMatchesCombo(
				{key: '/', ctrlOrMeta: true},
				keyEvent({key: '/', code: 'Digit7', metaKey: true, shiftKey: true}),
				{isMacOS: true},
			),
		).toBe(true);
		expect(
			keyboardEventMatchesCombo(
				{key: '1', ctrlOrMeta: true},
				keyEvent({key: '!', code: 'Digit1', metaKey: true, shiftKey: true}),
				{isMacOS: true},
			),
		).toBe(false);
	});
	it('matches printable shortcuts by layout key before physical code', () => {
		const combo = {key: 'Z', code: 'KeyZ', ctrlOrMeta: true, shift: true};
		expect(
			keyboardEventMatchesCombo(combo, keyEvent({key: 'Z', code: 'KeyY', ctrlKey: true, shiftKey: true}), {
				isMacOS: false,
			}),
		).toBe(true);
		expect(
			keyboardEventMatchesCombo(combo, keyEvent({key: 'Y', code: 'KeyZ', ctrlKey: true, shiftKey: true}), {
				isMacOS: false,
			}),
		).toBe(false);
	});
	it('falls back to physical code for Ctrl+Alt printable shortcuts when the layout key is transformed', () => {
		expect(
			keyboardEventMatchesCombo(
				{key: 'v', code: 'KeyV', ctrlOrMeta: true, alt: true},
				keyEvent({key: '@', code: 'KeyV', ctrlKey: true, altKey: true}),
				{isMacOS: false},
			),
		).toBe(true);
		expect(
			keyboardEventMatchesCombo(
				{key: 'v', code: 'KeyB', ctrlOrMeta: true, alt: true},
				keyEvent({key: '@', code: 'KeyV', ctrlKey: true, altKey: true}),
				{isMacOS: false},
			),
		).toBe(false);
	});
	it('serializes printable global shortcuts with the layout-aware key', () => {
		const combo = {key: 'Z', code: 'KeyY', ctrlOrMeta: true, shift: true, global: true};
		expect(comboToCombokeysStrings(combo)).toEqual(['mod+shift+z']);
		expect(keyNameForGlobalHook(combo)).toBe('Z');
		expect(physicalKeyNameForGlobalHook(combo)).toBe('Y');
		expect(hookShortcutIdForKeybind(runtimeKeybind(combo))).toBe('key:voice_toggle_mute:mod+shift:KeyY:Z');
	});
	it('keeps numpad global hook names code-based', () => {
		expect(keyNameForGlobalHook({key: '1', code: 'Numpad1', ctrlOrMeta: true})).toBe('Numpad1');
	});
	it('maps a CapsLock hold combo to the name the native hooks emit', () => {
		const combo = {key: 'CapsLock', code: 'CapsLock', enabled: true, global: true};
		expect(keyNameForGlobalHook(combo)).toBe('CapsLock');
		expect(physicalKeyNameForGlobalHook(combo)).toBe('CapsLock');
	});
	it('normalizes Pause and Break for global hook registration and matching', () => {
		const combo = {key: 'Pause', code: 'Pause', ctrlOrMeta: true, global: true};
		expect(comboToCombokeysStrings(combo)).toEqual(['mod+pause']);
		expect(keyNameForGlobalHook(combo)).toBe('Pause');
		expect(keyNameForGlobalHook({key: 'Break', code: 'Pause'})).toBe('Pause');
		expect(hookShortcutIdForKeybind(runtimeKeybind(combo))).toBe('key:voice_toggle_mute:mod:Pause:Pause');
		expect(
			keyboardEventMatchesCombo(combo, keyEvent({key: 'Break', code: 'Pause', ctrlKey: true}), {isMacOS: false}),
		).toBe(true);
		expect(keyboardEventTriggerMatchesCombo(combo, keyEvent({key: 'Break', code: 'Pause'}))).toBe(true);
	});
	it('serializes special numpad, media, and native-only keys for the native global hook', () => {
		expect(keyNameForGlobalHook({key: '0', code: 'Numpad0'})).toBe('Numpad0');
		expect(keyNameForGlobalHook({key: 'AudioVolumeMute', code: 'AudioVolumeMute'})).toBe('AudioVolumeMute');
		expect(keyNameForGlobalHook({key: 'BrowserBack', code: 'BrowserBack'})).toBe('BrowserBack');
	});
	it('allows page-scroll shortcuts from an empty channel textarea only', () => {
		expect(shouldAllowLocalShortcutForChannelTextarea({editableFocusBehavior: 'allow_when_empty'}, '')).toBe(true);
		expect(shouldAllowLocalShortcutForChannelTextarea({editableFocusBehavior: 'allow_when_empty'}, 'draft')).toBe(
			false,
		);
	});
	it('allows explicit editable-focus shortcuts even when the textarea has content', () => {
		expect(shouldAllowLocalShortcutForChannelTextarea({editableFocusBehavior: 'allow'}, 'draft')).toBe(true);
	});
	it('limits fullscreen voice call shortcuts to voice controls', () => {
		expect(isKeybindAllowedDuringVoiceCallFullscreen('voice_toggle_mute')).toBe(true);
		expect(isKeybindAllowedDuringVoiceCallFullscreen('voice_disconnect')).toBe(true);
		expect(isKeybindAllowedDuringVoiceCallFullscreen('voice_toggle_compact_call_view')).toBe(false);
		expect(isKeybindAllowedDuringVoiceCallFullscreen('nav_channel_next')).toBe(false);
		expect(isKeybindAllowedDuringVoiceCallFullscreen('chat_toggle_inbox')).toBe(false);
	});
	it('blocks textarea-scoped shortcuts while compact call chat is hidden', () => {
		expect(
			isKeybindBlockedByCompactVoiceCallView({
				action: 'chat_focus_textarea',
				channelType: ChannelTypes.GUILD_VOICE,
				isPrivateChannel: false,
				isGuildVoiceCallExpanded: true,
				isConnectedToPrivateCall: false,
				isPrivateCompactCallExpanded: false,
			}),
		).toBe(true);
		expect(
			isKeybindBlockedByCompactVoiceCallView({
				action: 'chat_toggle_emoji',
				channelType: ChannelTypes.GROUP_DM,
				isPrivateChannel: true,
				isGuildVoiceCallExpanded: false,
				isConnectedToPrivateCall: true,
				isPrivateCompactCallExpanded: true,
			}),
		).toBe(true);
		expect(
			isKeybindBlockedByCompactVoiceCallView({
				action: 'chat_toggle_emoji',
				channelType: ChannelTypes.GROUP_DM,
				isPrivateChannel: true,
				isGuildVoiceCallExpanded: false,
				isConnectedToPrivateCall: true,
				isPrivateCompactCallExpanded: false,
			}),
		).toBe(false);
		expect(
			isKeybindBlockedByCompactVoiceCallView({
				action: 'voice_toggle_mute',
				channelType: ChannelTypes.GUILD_VOICE,
				isPrivateChannel: false,
				isGuildVoiceCallExpanded: true,
				isConnectedToPrivateCall: false,
				isPrivateCompactCallExpanded: false,
			}),
		).toBe(false);
	});
});
