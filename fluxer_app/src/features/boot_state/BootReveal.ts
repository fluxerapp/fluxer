// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {BOOT_STATE_EXPECTED_ATTRIBUTE, BOOT_STATE_EXPECTED_VALUE} from '@fluxer/desktop_ipc/src/BootStateContract';

const logger = new Logger('BootReveal');

const RELEASED_TASK_IDLE_TIMEOUT_MS = 1000;

function restoredBootExpected(): boolean {
	return document.documentElement.getAttribute(BOOT_STATE_EXPECTED_ATTRIBUTE) === BOOT_STATE_EXPECTED_VALUE;
}

let held = restoredBootExpected();
const heldTasks = new Set<() => void>();

function runReleasedTasks(tasks: Set<() => void>): void {
	for (const task of tasks) {
		tasks.delete(task);
		try {
			task();
		} catch (error) {
			logger.error('A task held until the boot reveal failed', error);
		}
	}
}

export function isBootRevealHeld(): boolean {
	return held;
}

export function afterBootReveal(task: () => void): () => void {
	if (!held) {
		task();
		return () => {};
	}
	heldTasks.add(task);
	return () => heldTasks.delete(task);
}

export function whenBootRevealed(): Promise<void> {
	return new Promise((resolve) => afterBootReveal(resolve));
}

export function releaseBootReveal(): void {
	if (!held) {
		return;
	}
	held = false;
	const tasks = heldTasks;
	if (tasks.size === 0) {
		return;
	}
	if (typeof requestIdleCallback === 'function') {
		requestIdleCallback(() => runReleasedTasks(tasks), {timeout: RELEASED_TASK_IDLE_TIMEOUT_MS});
		return;
	}
	setTimeout(() => runReleasedTasks(tasks), 0);
}
