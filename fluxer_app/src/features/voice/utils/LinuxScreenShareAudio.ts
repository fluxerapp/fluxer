// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';

const logger = new Logger('LinuxScreenShareAudio');
let activeVirtmicSession: symbol | null = null;
let activeVirtmicCleanup: (() => void) | null = null;

function cleanupActiveVirtmicSession(session: symbol): void {
	if (activeVirtmicSession !== session) return;
	const cleanup = activeVirtmicCleanup;
	activeVirtmicSession = null;
	activeVirtmicCleanup = null;
	try {
		cleanup?.();
	} catch (error) {
		logger.warn('virtmic session cleanup failed', {error});
	}
	void getElectronAPI()?.virtmic?.stop();
}

export function disarmVirtmic(): void {
	const session = activeVirtmicSession;
	if (session) {
		cleanupActiveVirtmicSession(session);
	}
}
