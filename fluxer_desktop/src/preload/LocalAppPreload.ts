// SPDX-License-Identifier: AGPL-3.0-or-later

import type {DesktopLegacyHarvest} from '@fluxer/desktop_ipc/src/LegacyHarvestContract';
import {DESKTOP_LEGACY_HARVEST_CHANNELS} from '@fluxer/desktop_ipc/src/LegacyHarvestContract';
import type {DesktopLocalAppUploadProgress} from '@fluxer/desktop_ipc/src/LocalAppRouteContract';
import {LOCAL_APP_UPLOAD_PROGRESS_CHANNELS} from '@fluxer/desktop_ipc/src/LocalAppRouteContract';
import type {
	DesktopRuntimeAbort,
	DesktopRuntimeAbortRequest,
	DesktopRuntimeCancelRequest,
	DesktopRuntimeCommit,
	DesktopRuntimeCommittedTransactionRequest,
	DesktopRuntimeConfigAPI,
	DesktopRuntimeDeactivateRequest,
	DesktopRuntimeDeactivation,
	DesktopRuntimeFinalization,
	DesktopRuntimePlan,
	DesktopRuntimePreparation,
	DesktopRuntimePrepareRequest,
	DesktopRuntimeResolveRequest,
	DesktopRuntimeRollback,
	DesktopRuntimeTransactionRequest,
} from '@fluxer/desktop_ipc/src/LocalAppRuntimeContract';
import {DESKTOP_RUNTIME_CONFIG_CHANNELS} from '@fluxer/desktop_ipc/src/LocalAppRuntimeContract';
import type {
	DesktopModuleAPI,
	DesktopModuleEnsureResult,
	DesktopPendingModuleUpdate,
} from '@fluxer/desktop_ipc/src/ModuleContract';
import {DESKTOP_MODULE_CHANNELS, DESKTOP_MODULE_EVENTS} from '@fluxer/desktop_ipc/src/ModuleContract';

interface LocalAppPreloadRenderer {
	invoke: (channel: string, ...args: Array<unknown>) => Promise<unknown>;
	send: (channel: string, ...args: Array<unknown>) => void;
	on: (channel: string, listener: (event: unknown, ...args: Array<unknown>) => void) => void;
	removeListener: (channel: string, listener: (event: unknown, ...args: Array<unknown>) => void) => void;
}

interface DesktopLegacyHarvestPreloadAPI {
	read: () => Promise<DesktopLegacyHarvest | null>;
	markReplanted: () => Promise<void>;
	discard: () => Promise<void>;
}

interface DesktopLocalAppUploadPreloadAPI {
	subscribe: (listener: (progress: DesktopLocalAppUploadProgress) => void) => () => void;
}

interface LocalAppPreloadAPI {
	readonly desktopLegacyHarvest: DesktopLegacyHarvestPreloadAPI;
	readonly desktopModules: DesktopModuleAPI;
	readonly desktopRuntimeConfig: DesktopRuntimeConfigAPI;
	readonly localAppUpload: DesktopLocalAppUploadPreloadAPI;
}

export function createLocalAppPreloadAPI(renderer: LocalAppPreloadRenderer): LocalAppPreloadAPI {
	const invoke = <T>(channel: string, ...args: Array<unknown>): Promise<T> =>
		renderer.invoke(channel, ...args) as Promise<T>;
	return Object.freeze({
		desktopLegacyHarvest: Object.freeze<DesktopLegacyHarvestPreloadAPI>({
			read: () => invoke<DesktopLegacyHarvest | null>(DESKTOP_LEGACY_HARVEST_CHANNELS.read),
			markReplanted: () => invoke<void>(DESKTOP_LEGACY_HARVEST_CHANNELS.markReplanted),
			discard: () => invoke<void>(DESKTOP_LEGACY_HARVEST_CHANNELS.discard),
		}),
		desktopModules: Object.freeze<DesktopModuleAPI>({
			ensure: (moduleName: string) => invoke<DesktopModuleEnsureResult>(DESKTOP_MODULE_CHANNELS.ensure, moduleName),
			pendingUpdate: () => invoke<DesktopPendingModuleUpdate | null>(DESKTOP_MODULE_CHANNELS.pendingUpdate),
			applyPendingUpdate: () => invoke<boolean>(DESKTOP_MODULE_CHANNELS.applyPendingUpdate),
			onPendingUpdateChanged: (listener: (pending: DesktopPendingModuleUpdate | null) => void) =>
				subscribeToChannel<DesktopPendingModuleUpdate | null>(
					renderer,
					DESKTOP_MODULE_EVENTS.pendingUpdateChanged,
					listener,
				),
			confirmLaunch: () => invoke<void>(DESKTOP_MODULE_CHANNELS.confirmLaunch),
		}),
		desktopRuntimeConfig: Object.freeze<DesktopRuntimeConfigAPI>({
			initialInput: () => invoke<string | null>(DESKTOP_RUNTIME_CONFIG_CHANNELS.initialInput),
			resolve: (request: DesktopRuntimeResolveRequest) =>
				invoke<DesktopRuntimePlan>(DESKTOP_RUNTIME_CONFIG_CHANNELS.resolve, request),
			cancelResolution: (request: DesktopRuntimeCancelRequest) =>
				invoke<void>(DESKTOP_RUNTIME_CONFIG_CHANNELS.cancelResolution, request),
			prepare: (request: DesktopRuntimePrepareRequest) =>
				invoke<DesktopRuntimePreparation>(DESKTOP_RUNTIME_CONFIG_CHANNELS.prepare, request),
			commit: (request: DesktopRuntimeTransactionRequest) =>
				invoke<DesktopRuntimeCommit>(DESKTOP_RUNTIME_CONFIG_CHANNELS.commit, request),
			abort: (request: DesktopRuntimeAbortRequest) =>
				invoke<DesktopRuntimeAbort>(DESKTOP_RUNTIME_CONFIG_CHANNELS.abort, request),
			finalize: (request: DesktopRuntimeCommittedTransactionRequest) =>
				invoke<DesktopRuntimeFinalization>(DESKTOP_RUNTIME_CONFIG_CHANNELS.finalize, request),
			rollback: (request: DesktopRuntimeCommittedTransactionRequest) =>
				invoke<DesktopRuntimeRollback>(DESKTOP_RUNTIME_CONFIG_CHANNELS.rollback, request),
			deactivate: (request: DesktopRuntimeDeactivateRequest) =>
				invoke<DesktopRuntimeDeactivation>(DESKTOP_RUNTIME_CONFIG_CHANNELS.deactivate, request),
		}),
		localAppUpload: Object.freeze<DesktopLocalAppUploadPreloadAPI>({
			subscribe: (listener: (progress: DesktopLocalAppUploadProgress) => void) => {
				renderer.send(LOCAL_APP_UPLOAD_PROGRESS_CHANNELS.subscribe);
				return subscribeToChannel(renderer, LOCAL_APP_UPLOAD_PROGRESS_CHANNELS.progress, listener);
			},
		}),
	});
}

function subscribeToChannel<T>(
	renderer: LocalAppPreloadRenderer,
	channel: string,
	listener: (payload: T) => void,
): () => void {
	const handler = (_event: unknown, ...args: Array<unknown>): void => {
		listener(args[0] as T);
	};
	renderer.on(channel, handler);
	return () => {
		renderer.removeListener(channel, handler);
	};
}
