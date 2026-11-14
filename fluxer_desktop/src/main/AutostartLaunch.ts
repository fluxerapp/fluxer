// SPDX-License-Identifier: AGPL-3.0-or-later

import {getDesktopWindowBehaviorSettings} from '@electron/common/DesktopConfig';
import {app} from 'electron';

export const AUTOSTART_LAUNCH_ARG = '--autostart';

let autostartLaunch: boolean | null = null;

function isAutostartLaunch(): boolean {
	if (autostartLaunch === null) {
		autostartLaunch =
			process.argv.includes(AUTOSTART_LAUNCH_ARG) ||
			(process.platform === 'darwin' && Boolean(app.getLoginItemSettings().wasOpenedAtLogin));
	}
	return autostartLaunch;
}

export function isStartMinimizedLaunch(): boolean {
	return getDesktopWindowBehaviorSettings().startMinimized && isAutostartLaunch();
}
