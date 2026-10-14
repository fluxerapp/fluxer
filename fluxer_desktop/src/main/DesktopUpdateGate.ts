// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChildLogger} from '@electron/common/Logger';
import type {DesktopUpdateState} from '@fluxer/desktop_ipc/src/ModuleContract';

const logger = createChildLogger('DesktopUpdateGate');

export interface DesktopUpdateCheck {
	readonly shellNewer: boolean;
	readonly modulesChanged: boolean;
}

interface DesktopUpdateController {
	readonly check: (manual: boolean) => Promise<DesktopUpdateCheck>;
	readonly start: () => Promise<void>;
}

type DesktopUpdateStateListener = (state: DesktopUpdateState) => void;

interface DesktopUpdateGateState {
	controller: DesktopUpdateController | null;
	lastCheck: DesktopUpdateCheck | null;
	running: Promise<void> | null;
	checking: {readonly manual: boolean; readonly settled: Promise<void>} | null;
	notifiedAvailable: boolean;
	notifiedUpdating: boolean;
	notifiedReplacesShell: boolean;
	readonly listeners: Set<DesktopUpdateStateListener>;
}

const gateState: DesktopUpdateGateState = {
	controller: null,
	lastCheck: null,
	running: null,
	checking: null,
	notifiedAvailable: false,
	notifiedUpdating: false,
	notifiedReplacesShell: false,
	listeners: new Set(),
};

function isAvailable(check: DesktopUpdateCheck | null): boolean {
	return check != null && (check.shellNewer || check.modulesChanged);
}

export function getDesktopUpdateState(): DesktopUpdateState {
	const available = gateState.running == null && isAvailable(gateState.lastCheck);
	return {
		available,
		updating: gateState.running != null,
		replacesShell: available && gateState.lastCheck?.shellNewer === true,
	};
}

export function desktopUpdateReplacesShell(): boolean {
	return gateState.lastCheck?.shellNewer === true;
}

function notify(): void {
	const state = getDesktopUpdateState();
	const updating = state.updating === true;
	const replacesShell = state.replacesShell === true;
	if (
		state.available === gateState.notifiedAvailable &&
		updating === gateState.notifiedUpdating &&
		replacesShell === gateState.notifiedReplacesShell
	) {
		return;
	}
	gateState.notifiedAvailable = state.available;
	gateState.notifiedUpdating = updating;
	gateState.notifiedReplacesShell = replacesShell;
	for (const listener of Array.from(gateState.listeners)) {
		try {
			listener(state);
		} catch (error) {
			logger.error('A desktop update state listener threw', error);
		}
	}
}

export function armDesktopUpdate(controller: DesktopUpdateController): void {
	gateState.controller = controller;
}

export function publishDesktopUpdateCheck(check: DesktopUpdateCheck): void {
	gateState.lastCheck = check;
	notify();
}

export async function checkDesktopUpdateNow({
	manual = false,
}: {
	readonly manual?: boolean;
} = {}): Promise<DesktopUpdateState> {
	const pending = gateState.checking;
	if (pending != null) {
		if (pending.manual || !manual) {
			await pending.settled;
			return getDesktopUpdateState();
		}
		await pending.settled.catch(() => {});
		return await checkDesktopUpdateNow({manual});
	}
	const controller = gateState.controller;
	if (controller == null || gateState.running != null) {
		return getDesktopUpdateState();
	}
	const settled = controller.check(manual).then((check) => {
		if (gateState.running == null) {
			publishDesktopUpdateCheck(check);
		}
	});
	const checking = {manual, settled};
	gateState.checking = checking;
	try {
		await settled;
	} finally {
		if (gateState.checking === checking) {
			gateState.checking = null;
		}
	}
	return getDesktopUpdateState();
}

export function startDesktopUpdate(): boolean {
	const controller = gateState.controller;
	if (controller == null || gateState.running != null || !isAvailable(gateState.lastCheck)) {
		return false;
	}
	const running = controller
		.start()
		.catch((error: unknown) => {
			logger.error('The desktop update failed', error);
		})
		.finally(() => {
			if (gateState.running === running) {
				gateState.running = null;
				notify();
			}
		});
	gateState.running = running;
	notify();
	return true;
}

export function observeDesktopUpdateState(listener: DesktopUpdateStateListener): () => void {
	gateState.listeners.add(listener);
	return () => {
		gateState.listeners.delete(listener);
	};
}
