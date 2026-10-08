// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChildLogger} from '@electron/common/Logger';
import type {DesktopUpdateState} from '@fluxer/desktop_ipc/src/ModuleContract';

const logger = createChildLogger('DesktopUpdateGate');

export interface DesktopUpdateCheck {
	readonly shellNewer: boolean;
	readonly modulesChanged: boolean;
}

interface DesktopUpdateController {
	readonly check: () => Promise<DesktopUpdateCheck>;
	readonly start: () => Promise<void>;
}

type DesktopUpdateStateListener = (state: DesktopUpdateState) => void;

interface DesktopUpdateGateState {
	controller: DesktopUpdateController | null;
	lastCheck: DesktopUpdateCheck | null;
	running: Promise<void> | null;
	notifiedAvailable: boolean;
	notifiedUpdating: boolean;
	readonly listeners: Set<DesktopUpdateStateListener>;
}

const gateState: DesktopUpdateGateState = {
	controller: null,
	lastCheck: null,
	running: null,
	notifiedAvailable: false,
	notifiedUpdating: false,
	listeners: new Set(),
};

function isAvailable(check: DesktopUpdateCheck | null): boolean {
	return check != null && (check.shellNewer || check.modulesChanged);
}

export function getDesktopUpdateState(): DesktopUpdateState {
	return {
		available: gateState.running == null && isAvailable(gateState.lastCheck),
		updating: gateState.running != null,
	};
}

function notify(): void {
	const state = getDesktopUpdateState();
	const updating = state.updating === true;
	if (state.available === gateState.notifiedAvailable && updating === gateState.notifiedUpdating) {
		return;
	}
	gateState.notifiedAvailable = state.available;
	gateState.notifiedUpdating = updating;
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

export async function checkDesktopUpdateNow(): Promise<DesktopUpdateState> {
	const controller = gateState.controller;
	if (controller == null || gateState.running != null) {
		return getDesktopUpdateState();
	}
	const check = await controller.check();
	if (gateState.running == null) {
		publishDesktopUpdateCheck(check);
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
