// SPDX-License-Identifier: AGPL-3.0-or-later

export const DESKTOP_RUNTIME_CONFIG_CHANNELS = Object.freeze({
	initialInput: 'desktop-runtime-config:initial-input',
	resolve: 'desktop-runtime-config:resolve',
	cancelResolution: 'desktop-runtime-config:cancel-resolution',
	prepare: 'desktop-runtime-config:prepare',
	commit: 'desktop-runtime-config:commit',
	abort: 'desktop-runtime-config:abort',
	finalize: 'desktop-runtime-config:finalize',
	rollback: 'desktop-runtime-config:rollback',
	deactivate: 'desktop-runtime-config:deactivate',
} as const);

export const DESKTOP_RUNTIME_DISCOVERY_UNREACHABLE_ERROR_NAME = 'DesktopRuntimeDiscoveryUnreachableError';

export interface DesktopRuntimePlan {
	readonly instanceKey: string;
	readonly apiEndpoint: string;
	readonly remoteApiEndpoint: string;
	readonly document: unknown;
}

export interface DesktopRuntimePreparation extends DesktopRuntimePlan {
	readonly preparationId: string;
	readonly baseRevision: number;
	readonly baseActiveInstanceKey: string | null;
}

export interface DesktopRuntimeCommit extends DesktopRuntimeTransactionRequest {
	readonly revision: number;
}

export type DesktopRuntimeFinalization = DesktopRuntimeCommittedTransactionRequest;

export type DesktopRuntimeAbortDisposition = 'absent' | 'aborted' | 'rolled-back';

export interface DesktopRuntimeAbort {
	readonly disposition: DesktopRuntimeAbortDisposition;
	readonly revision: number;
	readonly activeInstanceKey: string | null;
}

export interface DesktopRuntimeRollback {
	readonly revision: number;
	readonly activeInstanceKey: string | null;
}

export interface DesktopRuntimeDeactivation {
	readonly revision: number;
	readonly deactivatedInstanceKey: string;
}

export interface DesktopRuntimeDeactivateRequest {
	readonly rendererActiveInstanceKey: string;
}

export interface DesktopRuntimeResolveRequest {
	readonly input: string;
	readonly requestId: string;
}

export interface DesktopRuntimeCancelRequest {
	readonly requestId: string;
}

export interface DesktopRuntimePrepareRequest {
	readonly preparationId: string;
	readonly instanceKey: string;
}

export interface DesktopRuntimeAbortRequest {
	readonly preparationId: string;
}

export interface DesktopRuntimeTransactionRequest {
	readonly preparationId: string;
	readonly instanceKey: string;
	readonly baseRevision: number;
}

export interface DesktopRuntimeCommittedTransactionRequest extends DesktopRuntimeTransactionRequest {
	readonly committedRevision: number;
}

export interface DesktopRuntimeConfigAPI {
	initialInput: () => Promise<string | null>;
	resolve: (request: DesktopRuntimeResolveRequest) => Promise<DesktopRuntimePlan>;
	cancelResolution: (request: DesktopRuntimeCancelRequest) => Promise<void>;
	prepare: (request: DesktopRuntimePrepareRequest) => Promise<DesktopRuntimePreparation>;
	commit: (request: DesktopRuntimeTransactionRequest) => Promise<DesktopRuntimeCommit>;
	abort: (request: DesktopRuntimeAbortRequest) => Promise<DesktopRuntimeAbort>;
	finalize: (request: DesktopRuntimeCommittedTransactionRequest) => Promise<DesktopRuntimeFinalization>;
	rollback: (request: DesktopRuntimeCommittedTransactionRequest) => Promise<DesktopRuntimeRollback>;
	deactivate: (request: DesktopRuntimeDeactivateRequest) => Promise<DesktopRuntimeDeactivation>;
}
