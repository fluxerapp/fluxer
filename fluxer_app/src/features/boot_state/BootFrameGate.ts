// SPDX-License-Identifier: AGPL-3.0-or-later

import {BOOT_FRAME_GLOBAL} from '@app/features/boot_state/BootStateBrowserFormat';

export function whenBootFrameSettled(): Promise<void> {
	const holder = window as unknown as Record<string, {readonly settled: Promise<void>} | undefined>;
	return holder[BOOT_FRAME_GLOBAL]?.settled ?? Promise.resolve();
}
