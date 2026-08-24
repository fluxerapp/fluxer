// SPDX-License-Identifier: AGPL-3.0-or-later

import GuildReadState from '@app/features/guild/state/GuildReadState';
import Idle from '@app/features/ui/state/Idle';
import Notification from '@app/features/ui/state/Notification';
import Window from '@app/features/window/state/Window';

function applyFocusSideEffects(focused: boolean): void {
	GuildReadState.handleWindowFocused();
	Notification.handleWindowFocused({focused});
	if (focused) {
		Idle.recordActivity();
	}
}

export function focused(focused: boolean): void {
	Window.setFocused(focused);
	applyFocusSideEffects(focused);
}

export function resized(): void {
	Window.updateWindowSize();
}

export function visibilityChanged(visible: boolean): void {
	Window.setVisible(visible);
}
