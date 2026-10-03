// SPDX-License-Identifier: AGPL-3.0-or-later

import GlobalShortcuts from '@app/features/input/state/GlobalShortcuts';
import Keybind from '@app/features/input/state/InputKeybind';
import {formatKeyCombo} from '@app/features/input/utils/KeybindUtils';
import type {I18n, MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const PUSH_TO_TALK_ACTIONS = ['voice_push_to_talk', 'voice_push_to_talk_priority'] as const;

const PUSH_TO_TALK_ON_WITHOUT_KEY_DESCRIPTOR = msg({
	message: 'Push-to-talk is on. Hold your push-to-talk key to speak.',
	comment:
		'Tooltip and screen reader label on the mic button when push-to-talk is on but the key has no name to show, for example when the Linux desktop assigns it.',
});

function getPushToTalkHint(i18n: I18n): string | null {
	for (const action of PUSH_TO_TALK_ACTIONS) {
		const trigger = GlobalShortcuts.getPortalTrigger(action);
		if (trigger) return trigger;
		const formatted = formatKeyCombo(i18n, Keybind.getByAction(action).combo);
		if (formatted) return formatted;
	}
	return null;
}

export function getPushToTalkHoldLabel(i18n: I18n, descriptor: MessageDescriptor): string {
	const pushToTalkHint = getPushToTalkHint(i18n);
	if (pushToTalkHint === null) return i18n._(PUSH_TO_TALK_ON_WITHOUT_KEY_DESCRIPTOR);
	return i18n._(descriptor, {pushToTalkHint});
}
