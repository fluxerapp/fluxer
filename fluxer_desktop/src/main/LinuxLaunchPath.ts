// SPDX-License-Identifier: AGPL-3.0-or-later

import {spawn} from 'node:child_process';
import {existsSync} from 'node:fs';
import {homedir} from 'node:os';
import {createChildLogger} from '@electron/common/Logger';
import {app} from 'electron';

const logger = createChildLogger('LinuxLaunchPath');

const PREFERRED_RELAUNCH_SHELL = '/bin/bash';
const FALLBACK_RELAUNCH_SHELL = '/bin/sh';

const RELAUNCH_AFTER_EXIT =
	'pid="$1"; app="$2"; home="$3"; shift 3; cd /proc/self/fd && for n in *; do case "$n" in 0|1|2) ;; [0-9]) eval "exec $n>&-";; *) [ -n "$BASH_VERSION" ] && eval "exec $n>&-";; esac; done; cd "$home" || cd /; attempt=0; while [ "$attempt" -lt 300 ] && kill -0 "$pid"; do sleep 0.1; attempt=$((attempt + 1)); done; kill -0 "$pid" || exec "$app" "$@"';

function getAppImagePath(): string | null {
	if (process.platform !== 'linux') return null;
	const appImage = process.env.APPIMAGE;
	return appImage && appImage.length > 0 ? appImage : null;
}

export function getStableLinuxLaunchPath(): string {
	return getAppImagePath() ?? process.execPath;
}

export function relaunchStableLaunchPath(): void {
	const appImagePath = getAppImagePath();
	if (appImagePath == null) {
		app.relaunch();
		return;
	}
	try {
		const child = spawn(
			existsSync(PREFERRED_RELAUNCH_SHELL) ? PREFERRED_RELAUNCH_SHELL : FALLBACK_RELAUNCH_SHELL,
			[
				'-c',
				RELAUNCH_AFTER_EXIT,
				'fluxer-relaunch',
				String(process.pid),
				appImagePath,
				homedir(),
				...process.argv.slice(1),
			],
			{
				cwd: homedir(),
				detached: true,
				stdio: 'ignore',
			},
		);
		child.unref();
		logger.info('Relaunching the AppImage once this process exits', {path: appImagePath, pid: child.pid});
	} catch (error) {
		logger.error('Failed to start the AppImage replacement process', {error});
		app.relaunch({execPath: appImagePath});
	}
}
