// SPDX-License-Identifier: AGPL-3.0-or-later

import MessageKeyboardFocusRollout from '@app/features/messaging/state/MessageKeyboardFocusRollout';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';

export function focusChannelTextareaAfterNavigation(channelId: string): void {
	const requestFocus = () => {
		ComponentBus.dispatch('FOCUS_TEXTAREA', {channelId});
	};
	window.requestAnimationFrame(requestFocus);
	window.setTimeout(requestFocus, 300);
}

export function focusChannelTextareaFromKeybind(channelId: string): void {
	ComponentBus.dispatch(
		'FOCUS_TEXTAREA',
		MessageKeyboardFocusRollout.enabled ? {channelId, enterKeyboardMode: true} : {channelId},
	);
}
