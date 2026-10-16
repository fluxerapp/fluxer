// SPDX-License-Identifier: AGPL-3.0-or-later

import {getDesktopWindowBehaviorSettings} from '@electron/common/DesktopConfig';
import {app} from 'electron';

export const AUTOSTART_LAUNCH_ARG = '--autostart';

function isAutostartLaunch(): boolean {
	if (process.argv.includes(AUTOSTART_LAUNCH_ARG)) {
		return true;
	}
	if (process.platform === 'darwin') {
		return Boolean(app.getLoginItemSettings().wasOpenedAtLogin);
	}
	return false;
}

export function isStartMinimizedLaunch(): boolean {
	return isAutostartLaunch() && getDesktopWindowBehaviorSettings().startMinimized;
}
