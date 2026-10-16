// SPDX-License-Identifier: AGPL-3.0-or-later

import type {HoldAction} from '@app/features/app/keybindings/utils/RuntimeKeybinds';
import type {Channel} from '@app/features/channel/models/Channel';
import type {KeyCombo} from '@app/features/input/state/InputKeybind';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {msg} from '@lingui/core/macro';

export const YOU_CAN_T_UNMUTE_YOURSELF_BECAUSE_A_MODERATOR_DESCRIPTOR = msg({
	message: "You can't unmute yourself because a moderator muted you.",
	comment: 'Toast error shown when a shortcut tries to unmute the user but a moderator-imposed mute is active.',
});
export const YOU_CAN_T_UNDEAFEN_YOURSELF_BECAUSE_A_MODERATOR_DEAFENED_DESCRIPTOR = msg({
	message: "You can't undeafen yourself because a moderator deafened you.",
	comment:
		'Voice chat error. The user cannot remove moderator-applied deafen themselves. Distinct from microphone mute.',
});
export const PUSH_TO_TALK_WHILE_DEAFENED_TITLE_DESCRIPTOR = msg({
	message: "You're deafened",
	comment: 'Modal title shown when a push-to-talk shortcut is pressed while the user is deafened.',
});
export const PUSH_TO_TALK_WHILE_DEAFENED_DESCRIPTION_DESCRIPTOR = msg({
	message: "Push-to-talk won't unmute your microphone while you're deafened. Undeafen yourself to talk.",
	comment: 'Modal description shown when a push-to-talk shortcut is pressed while the user is self-deafened.',
});

export interface CombokeysInstance {
	bind(keys: string | Array<string>, callback: (event: KeyboardEvent) => void, action?: string): void;
	reset(): void;
	detach(): void;
	stopCallback: (e: Event, element: Element) => boolean;
}

export type ShortcutSource = 'local' | 'global';
export type KeybindHandler = (payload: {
	type: 'press' | 'release';
	source: ShortcutSource;
	sourceId: string;
	context?: {
		focusedMessage?: Message;
		focusedChannel?: Channel | null;
	};
	shiftKey?: boolean;
}) => void;

export interface HoldBindingRuntime {
	action: HoldAction;
	sourceId: string;
	gamepadSourceId: string;
	combo: KeyCombo;
	keycode: number | null;
	keyName: string | null;
	physicalKeyName: string | null;
	mouseButton: number | null;
	gamepadButton: number | null;
	isModifierOnly: boolean;
	ctrlOrMeta: boolean;
	requireBothSides: boolean;
	modifiers: {
		ctrl: boolean;
		alt: boolean;
		shift: boolean;
		meta: boolean;
	};
	routing: 'global' | 'local' | null;
	pressedKeycodes: Set<number>;
	localPressedCodes: Set<string>;
	localActiveCode: string | null;
	localMouseActive: boolean;
	globalMouseActive: boolean;
	localKeyDown: ((event: KeyboardEvent) => void) | null;
	localKeyUp: ((event: KeyboardEvent) => void) | null;
	localMouseDown: ((event: MouseEvent) => void) | null;
	localMouseUp: ((event: MouseEvent) => void) | null;
	gamepadHeld: boolean;
}
