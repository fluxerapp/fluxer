// SPDX-License-Identifier: AGPL-3.0-or-later

import {REQUEST_ID_MAX_LENGTH, readBoundedString, readExactPlainRecord} from '@electron/common/PlainRecord';
import {resolveDesktopRuntimePlan} from '@electron/main/DesktopRuntimeDiscovery';
import {projectDesktopRuntimePlan} from '@electron/main/DesktopRuntimePlanProjection';
import {getDesktopLocalAppProtocol} from '@electron/main/LocalAppProtocol';
import type {RendererDocumentOwnerFactory} from '@electron/main/RendererDocumentOwner';
import type {
	RendererDocumentIpcEvent,
	RendererDocumentOwner,
	RendererDocumentOwnerWatcher,
} from '@electron/main/RendererDocumentOwnership';
import type {DesktopRuntimePlan} from '@fluxer/desktop_ipc/src/LocalAppRuntimeContract';

const MAX_PENDING_RESOLUTIONS = 8;
const RESOLVE_INPUT_MAX_LENGTH = 8192;

class InvalidDesktopRuntimeResolutionRequestError extends TypeError {
	public constructor(context: string) {
		super(`Desktop runtime config ${context} is malformed`);
		this.name = 'InvalidDesktopRuntimeResolutionRequestError';
	}
}

class DesktopRuntimeResolutionCapacityError extends Error {
	public constructor() {
		super('Desktop runtime config resolution capacity is exhausted');
		this.name = 'DesktopRuntimeResolutionCapacityError';
	}
}

class DesktopRuntimeResolutionAlreadyExistsError extends Error {
	public constructor(requestId: string) {
		super(`Desktop runtime resolution ${requestId} already exists`);
		this.name = 'DesktopRuntimeResolutionAlreadyExistsError';
	}
}

class DesktopRuntimeResolutionOwnershipError extends Error {
	public constructor(requestId: string) {
		super(`Desktop runtime resolution ${requestId} belongs to another renderer document`);
		this.name = 'DesktopRuntimeResolutionOwnershipError';
	}
}

interface PendingRuntimeResolution {
	readonly requestId: string;
	readonly owner: RendererDocumentOwner;
	readonly watcher: RendererDocumentOwnerWatcher;
	readonly controller: AbortController;
}

export class DesktopRuntimeResolutionRegistry {
	private readonly pending = new Map<string, PendingRuntimeResolution>();

	public constructor(private readonly rendererDocuments: RendererDocumentOwnerFactory) {}

	public async resolve(event: RendererDocumentIpcEvent, request: unknown): Promise<DesktopRuntimePlan> {
		const record = readExactPlainRecord({value: request, expectedKeys: ['input', 'requestId']});
		if (record == null) {
			throw new InvalidDesktopRuntimeResolutionRequestError('resolve request');
		}
		const input = readBoundedString(record.input, RESOLVE_INPUT_MAX_LENGTH);
		const requestId = readBoundedString(record.requestId, REQUEST_ID_MAX_LENGTH);
		if (input == null || requestId == null) {
			throw new InvalidDesktopRuntimeResolutionRequestError('resolve request');
		}
		if (this.pending.has(requestId)) {
			throw new DesktopRuntimeResolutionAlreadyExistsError(requestId);
		}
		if (this.pending.size >= MAX_PENDING_RESOLUTIONS) {
			throw new DesktopRuntimeResolutionCapacityError();
		}
		const owner = this.rendererDocuments.capture(event, 'Desktop runtime config resolve');
		const controller = new AbortController();
		const watcher = owner.watchInvalidation(() => {
			this.dispose(requestId);
		});
		const pending = {requestId, owner, watcher, controller};
		this.pending.set(requestId, pending);
		try {
			const protocol = getDesktopLocalAppProtocol();
			const signal = AbortSignal.any([controller.signal, protocol.getShutdownSignal()]);
			const plan = await resolveDesktopRuntimePlan({input, signal});
			owner.requireCurrent('Desktop runtime config resolve');
			protocol.cacheRuntimePlan(plan);
			return projectDesktopRuntimePlan(plan);
		} finally {
			this.dispose(requestId, pending);
		}
	}

	public cancel(event: RendererDocumentIpcEvent, request: unknown): void {
		const requestId = requireResolutionId(request);
		const pending = this.pending.get(requestId);
		if (pending === undefined) {
			return;
		}
		if (!pending.owner.matchesEvent(event)) {
			throw new DesktopRuntimeResolutionOwnershipError(requestId);
		}
		this.dispose(requestId, pending);
	}

	public cleanup(): void {
		for (const requestId of [...this.pending.keys()]) {
			this.dispose(requestId);
		}
	}

	private dispose(requestId: string, expected?: PendingRuntimeResolution): void {
		const pending = this.pending.get(requestId);
		if (pending === undefined || (expected !== undefined && pending !== expected)) {
			return;
		}
		this.pending.delete(requestId);
		pending.controller.abort();
		pending.watcher.dispose();
	}
}

function requireResolutionId(request: unknown): string {
	const record = readExactPlainRecord({value: request, expectedKeys: ['requestId']});
	const requestId = record == null ? null : readBoundedString(record.requestId, REQUEST_ID_MAX_LENGTH);
	if (requestId === null) {
		throw new InvalidDesktopRuntimeResolutionRequestError('resolution cancellation request');
	}
	return requestId;
}
