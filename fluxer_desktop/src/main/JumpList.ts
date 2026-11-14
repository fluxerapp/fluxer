// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChildLogger} from '@electron/common/Logger';
import {onLocaleChange, t} from '@electron/main/MainI18n';
import {app} from 'electron';

const logger = createChildLogger('JumpList');
export const TASK_ARG_PREFIX = '--fluxer-task=';

export type JumpListTaskId = 'open-settings' | 'new-dm' | 'open-account-switcher';

const JUMP_LIST_TASK_IDS: ReadonlySet<string> = new Set<JumpListTaskId>([
	'open-settings',
	'new-dm',
	'open-account-switcher',
]);

let directMessagesAvailable = true;
const availabilitySubscribers = new Set<() => void>();

export function isDirectMessagesAvailable(): boolean {
	return directMessagesAvailable;
}

export function setDirectMessagesAvailable(available: boolean): void {
	if (available === directMessagesAvailable) return;
	directMessagesAvailable = available;
	for (const subscriber of availabilitySubscribers) {
		try {
			subscriber();
		} catch (error) {
			logger.warn('Direct message availability subscriber threw', {error});
		}
	}
}

export function onDirectMessagesAvailabilityChange(callback: () => void): void {
	availabilitySubscribers.add(callback);
}

function buildTasks(): Array<Electron.Task> {
	const exePath = process.execPath;
	const tasks: Array<Electron.Task> = [
		{
			program: exePath,
			arguments: `${TASK_ARG_PREFIX}open-settings`,
			iconPath: exePath,
			iconIndex: 0,
			title: t('desktop.jumpList.openSettings'),
			description: t('desktop.jumpList.openSettingsDescription'),
		},
	];
	if (directMessagesAvailable) {
		tasks.push({
			program: exePath,
			arguments: `${TASK_ARG_PREFIX}new-dm`,
			iconPath: exePath,
			iconIndex: 0,
			title: t('desktop.jumpList.newDirectMessage'),
			description: t('desktop.jumpList.newDirectMessageDescription'),
		});
	}
	tasks.push({
		program: exePath,
		arguments: `${TASK_ARG_PREFIX}open-account-switcher`,
		iconPath: exePath,
		iconIndex: 0,
		title: t('desktop.jumpList.chooseAccount'),
		description: t('desktop.jumpList.chooseAccountDescription'),
	});
	return tasks;
}

let installed = false;

function refreshJumpList(): void {
	if (process.platform !== 'win32') return;
	try {
		const ok = app.setUserTasks(buildTasks());
		if (!ok) {
			logger.warn('setUserTasks returned false — JumpList may be unavailable on this Windows version');
		}
	} catch (error) {
		logger.warn('Failed to install Windows JumpList', {error});
	}
}

export function initializeJumpList(): void {
	if (process.platform !== 'win32') return;
	if (installed) return;
	installed = true;
	refreshJumpList();
	onLocaleChange(refreshJumpList);
	onDirectMessagesAvailabilityChange(refreshJumpList);
}

export function parseJumpListTaskFromArgv(argv: ReadonlyArray<string>): JumpListTaskId | null {
	for (const arg of argv) {
		if (!arg.startsWith(TASK_ARG_PREFIX)) continue;
		const id = arg.slice(TASK_ARG_PREFIX.length);
		if (JUMP_LIST_TASK_IDS.has(id)) return id as JumpListTaskId;
	}
	return null;
}
