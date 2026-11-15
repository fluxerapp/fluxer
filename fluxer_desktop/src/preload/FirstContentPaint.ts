// SPDX-License-Identifier: AGPL-3.0-or-later

import {DESKTOP_FIRST_CONTENT_PAINTED_CHANNEL} from '@electron/common/Constants';

interface FirstContentPaintRenderer {
	send(channel: string): void;
}

type FrameScheduler = (callback: () => void) => unknown;

export function createFirstContentPaintSignal(
	renderer: FirstContentPaintRenderer,
	scheduleFrame: FrameScheduler,
	onContentPainted: (view: 'restored' | 'other') => void,
): (view?: 'restored') => void {
	let signalled = false;
	const signal = (view: 'restored' | 'other'): void => {
		onContentPainted(view);
		renderer.send(DESKTOP_FIRST_CONTENT_PAINTED_CHANNEL);
	};
	return (view) => {
		if (signalled) return;
		signalled = true;
		if (view === 'restored') {
			signal(view);
			return;
		}
		scheduleFrame(() => {
			scheduleFrame(() => signal('other'));
		});
	};
}
