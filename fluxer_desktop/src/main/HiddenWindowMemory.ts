// SPDX-License-Identifier: AGPL-3.0-or-later

import {RELEASE_RENDERER_MEMORY_CHANNEL} from '@electron/common/Constants';
import type {BrowserWindow} from 'electron';

const HIDDEN_MEMORY_RELEASE_DELAY_MS = 30_000;

function isOutOfSight(window: BrowserWindow): boolean {
	return !window.isVisible() || window.isMinimized();
}

export function armHiddenWindowMemoryRelease(window: BrowserWindow): void {
	let timer: NodeJS.Timeout | null = null;
	const cancel = (): void => {
		if (timer == null) return;
		clearTimeout(timer);
		timer = null;
	};
	const schedule = (): void => {
		cancel();
		timer = setTimeout(() => {
			timer = null;
			if (window.isDestroyed() || window.webContents.isDestroyed() || !isOutOfSight(window)) return;
			window.webContents.send(RELEASE_RENDERER_MEMORY_CHANNEL);
		}, HIDDEN_MEMORY_RELEASE_DELAY_MS);
		timer.unref();
	};
	window.on('hide', schedule);
	window.on('minimize', schedule);
	window.on('show', cancel);
	window.on('restore', cancel);
	window.on('closed', cancel);
}
