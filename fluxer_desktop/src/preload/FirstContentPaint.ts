// SPDX-License-Identifier: AGPL-3.0-or-later

import {DESKTOP_FIRST_CONTENT_PAINTED_CHANNEL} from '@electron/common/Constants';

interface FirstContentPaintRenderer {
	send(channel: string): void;
}

type FrameScheduler = (callback: () => void) => unknown;

export function createFirstContentPaintSignal(
	renderer: FirstContentPaintRenderer,
	scheduleFrame: FrameScheduler,
): () => void {
	let signalled = false;
	return () => {
		if (signalled) return;
		signalled = true;
		scheduleFrame(() => {
			scheduleFrame(() => {
				renderer.send(DESKTOP_FIRST_CONTENT_PAINTED_CHANNEL);
			});
		});
	};
}
