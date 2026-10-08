// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UpdaterDownloadOption} from '@app/features/platform/types/Electron';
import {assign, initialTransition, type SnapshotFrom, setup, transition} from 'xstate';

export type UpdaterState = 'idle' | 'checking' | 'available';
export type UpdateType = 'native' | 'web' | 'both' | null;

export interface NativeUpdateInfo {
	available: boolean;
	version: string | null;
}

export interface WebUpdateInfo {
	available: boolean;
	version: string | null;
}

export interface UpdateInfo {
	native: NativeUpdateInfo;
	web: WebUpdateInfo;
}

export interface NativeUnsupportedUpdate {
	reason: 'platform' | 'unpackaged' | 'managed-package';
	downloadUrl: string | null;
}

export interface UpdaterMachineContext {
	updateInfo: UpdateInfo;
	lastCheckedAt: number | null;
	isChecking: boolean;
	nativeCheckFailed: boolean;
	manualNativeDownloadInFlight: boolean;
	nativeUnsupported: NativeUnsupportedUpdate | null;
	nativeManualDownloadUrl: string | null;
	nativeManualDownloadOptions: ReadonlyArray<UpdaterDownloadOption>;
}

export type UpdaterMachineEvent =
	| {type: 'check.started'}
	| {type: 'check.finished'; now: number}
	| {type: 'check.failed'; now: number}
	| {type: 'web.checked'; available: boolean; version: string | null}
	| {
			type: 'native.available';
			version: string | null;
			downloadUrl: string | null;
			downloadOptions: ReadonlyArray<UpdaterDownloadOption>;
	  }
	| {type: 'native.notAvailable'; now: number}
	| {type: 'native.error'}
	| {
			type: 'native.unsupported';
			reason: 'platform' | 'unpackaged' | 'managed-package';
			downloadUrl: string | null;
			now: number;
	  }
	| {type: 'manualDownload.started'}
	| {type: 'manualDownload.finished'}
	| {type: 'reset'};

const EMPTY_DOWNLOAD_OPTIONS: ReadonlyArray<UpdaterDownloadOption> = Object.freeze([]);

function createEmptyNativeUpdateInfo(): NativeUpdateInfo {
	return {
		available: false,
		version: null,
	};
}

function createInitialUpdateInfo(): UpdateInfo {
	return {
		native: createEmptyNativeUpdateInfo(),
		web: {available: false, version: null},
	};
}

export function createInitialUpdaterContext(): UpdaterMachineContext {
	return {
		updateInfo: createInitialUpdateInfo(),
		lastCheckedAt: null,
		isChecking: false,
		nativeCheckFailed: false,
		manualNativeDownloadInFlight: false,
		nativeUnsupported: null,
		nativeManualDownloadUrl: null,
		nativeManualDownloadOptions: EMPTY_DOWNLOAD_OPTIONS,
	};
}

function hasAnyUpdate(context: UpdaterMachineContext): boolean {
	return context.updateInfo.native.available || context.updateInfo.web.available;
}

function clearNativeUpdate(context: UpdaterMachineContext): UpdaterMachineContext {
	return {
		...context,
		updateInfo: {
			...context.updateInfo,
			native: createEmptyNativeUpdateInfo(),
		},
		nativeManualDownloadUrl: null,
		nativeManualDownloadOptions: EMPTY_DOWNLOAD_OPTIONS,
	};
}

export const updaterStateMachine = setup({
	types: {} as {
		context: UpdaterMachineContext;
		events: UpdaterMachineEvent;
	},
	actions: {
		reset: assign(() => createInitialUpdaterContext()),
		markChecking: assign(() => ({
			isChecking: true,
			nativeCheckFailed: false,
		})),
		markCheckFinished: assign(({context, event}) => ({
			lastCheckedAt:
				event.type === 'check.finished' || event.type === 'check.failed' ? event.now : context.lastCheckedAt,
			isChecking: false,
		})),
		applyWebChecked: assign(({context, event}) => {
			if (event.type !== 'web.checked') return {};
			return {
				updateInfo: {
					...context.updateInfo,
					web: {
						available: event.available,
						version: event.version,
					},
				},
			};
		}),
		applyNativeAvailable: assign(({context, event}) => {
			if (event.type !== 'native.available') return {};
			return {
				updateInfo: {
					...context.updateInfo,
					native: {
						available: true,
						version: event.version,
					},
				},
				isChecking: false,
				nativeUnsupported: null,
				nativeManualDownloadUrl: event.downloadUrl,
				nativeManualDownloadOptions: [...event.downloadOptions],
			};
		}),
		applyNativeNotAvailable: assign(({context, event}) => {
			if (event.type !== 'native.notAvailable') return {};
			return {
				...clearNativeUpdate(context),
				lastCheckedAt: event.now,
				isChecking: false,
				nativeUnsupported: null,
			};
		}),
		applyNativeError: assign(() => ({
			isChecking: false,
			nativeCheckFailed: true,
		})),
		applyNativeUnsupported: assign(({context, event}) => {
			if (event.type !== 'native.unsupported') return {};
			return {
				...clearNativeUpdate(context),
				lastCheckedAt: event.now,
				isChecking: false,
				nativeUnsupported: {
					reason: event.reason,
					downloadUrl: event.downloadUrl,
				},
			};
		}),
		startManualDownload: assign(() => ({manualNativeDownloadInFlight: true})),
		finishManualDownload: assign(() => ({manualNativeDownloadInFlight: false})),
	},
	guards: {
		isChecking: ({context}) => context.isChecking,
		isNotChecking: ({context}) => !context.isChecking,
		isNotCheckingAndHasUpdate: ({context}) => !context.isChecking && hasAnyUpdate(context),
		isNotCheckingAndHasNoUpdate: ({context}) => !context.isChecking && !hasAnyUpdate(context),
		hasUpdate: ({context}) => hasAnyUpdate(context),
		hasNoUpdate: ({context}) => !hasAnyUpdate(context),
	},
}).createMachine({
	id: 'updater',
	context: () => createInitialUpdaterContext(),
	initial: 'idle',
	on: {
		'check.started': {actions: 'markChecking'},
		'check.finished': {actions: 'markCheckFinished'},
		'check.failed': {actions: 'markCheckFinished'},
		'web.checked': {actions: 'applyWebChecked'},
		'native.available': {actions: 'applyNativeAvailable'},
		'native.notAvailable': {actions: 'applyNativeNotAvailable'},
		'native.error': {actions: 'applyNativeError'},
		'native.unsupported': {actions: 'applyNativeUnsupported'},
		'manualDownload.started': {actions: 'startManualDownload'},
		'manualDownload.finished': {actions: 'finishManualDownload'},
		reset: {actions: 'reset'},
	},
	states: {
		idle: {
			always: [
				{guard: 'isChecking', target: 'checking'},
				{guard: 'hasUpdate', target: 'available'},
			],
		},
		checking: {
			always: [
				{guard: 'isNotCheckingAndHasUpdate', target: 'available'},
				{guard: 'isNotCheckingAndHasNoUpdate', target: 'idle'},
			],
		},
		available: {
			always: [
				{guard: 'isChecking', target: 'checking'},
				{guard: 'hasNoUpdate', target: 'idle'},
			],
		},
	},
});

export type UpdaterMachineSnapshot = SnapshotFrom<typeof updaterStateMachine>;

export function createUpdaterMachineSnapshot(): UpdaterMachineSnapshot {
	return initialTransition(updaterStateMachine)[0];
}

export function transitionUpdaterMachineSnapshot(
	snapshot: UpdaterMachineSnapshot,
	event: UpdaterMachineEvent,
): UpdaterMachineSnapshot {
	return transition(updaterStateMachine, snapshot, event)[0] as UpdaterMachineSnapshot;
}

export function getUpdaterMachineStateValue(snapshot: UpdaterMachineSnapshot): UpdaterState {
	switch (snapshot.value) {
		case 'checking':
			return 'checking';
		case 'available':
			return 'available';
		default:
			return 'idle';
	}
}

export function getUpdaterUpdateType(snapshot: UpdaterMachineSnapshot): UpdateType {
	const hasNative = snapshot.context.updateInfo.native.available;
	const hasWeb = snapshot.context.updateInfo.web.available;
	if (hasNative && hasWeb) return 'both';
	if (hasNative) return 'native';
	if (hasWeb) return 'web';
	return null;
}

export function hasManualNativeDownload(snapshot: UpdaterMachineSnapshot): boolean {
	return Boolean(snapshot.context.nativeManualDownloadUrl) || snapshot.context.nativeManualDownloadOptions.length > 0;
}

export function getUpdaterDisplayVersion(snapshot: UpdaterMachineSnapshot): string | null {
	if (snapshot.context.updateInfo.native.available && snapshot.context.updateInfo.native.version) {
		return snapshot.context.updateInfo.native.version;
	}
	if (snapshot.context.updateInfo.web.available) {
		return snapshot.context.updateInfo.web.version;
	}
	return null;
}
