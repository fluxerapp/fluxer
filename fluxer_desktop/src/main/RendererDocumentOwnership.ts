// SPDX-License-Identifier: AGPL-3.0-or-later

export const RendererDocumentInvalidationReason = Object.freeze({
	DESTROYED: 'destroyed',
	NAVIGATED: 'navigated',
	RENDER_PROCESS_GONE: 'render-process-gone',
} as const);

export type RendererDocumentInvalidationReason =
	(typeof RendererDocumentInvalidationReason)[keyof typeof RendererDocumentInvalidationReason];

export interface RendererDocumentIpcEvent {
	readonly sender: Electron.WebContents;
	readonly senderFrame?: Electron.WebFrameMain | null;
}

export type RendererDocumentIpcRoutes = Readonly<
	Record<string, (event: RendererDocumentIpcEvent, ...args: Array<unknown>) => Promise<unknown>>
>;

export interface RendererDocumentOwnerWatcher {
	dispose(): void;
}

export interface RendererDocumentOwner {
	readonly frame: Electron.WebFrameMain;
	readonly sender: Electron.WebContents;
	isCurrent(): boolean;
	matchesEvent(event: RendererDocumentIpcEvent): boolean;
	requireCurrent(context: string): void;
	watchInvalidation(onInvalidated: (reason: RendererDocumentInvalidationReason) => void): RendererDocumentOwnerWatcher;
}

export class RendererDocumentCaptureError extends Error {
	public constructor(context: string) {
		super(`${context} requires the current privileged main-frame renderer document`);
		this.name = 'RendererDocumentCaptureError';
	}
}

export class InactiveRendererDocumentOwnerError extends Error {
	public constructor(context: string) {
		super(`${context} owner document is no longer active`);
		this.name = 'InactiveRendererDocumentOwnerError';
	}
}

export function isSameRendererDocument(left: RendererDocumentOwner, right: RendererDocumentOwner): boolean {
	return left.sender === right.sender && left.frame === right.frame;
}
