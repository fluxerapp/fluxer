// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	InactiveRendererDocumentOwnerError,
	RendererDocumentCaptureError,
	RendererDocumentInvalidationReason,
	type RendererDocumentIpcEvent,
	type RendererDocumentOwner,
	type RendererDocumentOwnerWatcher,
} from '@electron/main/RendererDocumentOwnership';
import {webContents as electronWebContents} from 'electron';

interface PrivilegedRendererDocumentRequest {
	readonly sender: Electron.WebContents;
	readonly url: string;
}

interface PrivilegedRendererDocumentPolicy {
	isPrivilegedRendererDocument(request: PrivilegedRendererDocumentRequest): boolean;
}

interface RendererDocumentOwnerFactoryDependencies {
	readonly policy: PrivilegedRendererDocumentPolicy;
	readonly onWatcherFailure: (error: unknown, reason: string) => void;
}

function isExecutingPrivilegedRendererMainFrame(
	sender: Electron.WebContents,
	frame: Electron.WebFrameMain | null,
	policy: PrivilegedRendererDocumentPolicy,
): boolean {
	try {
		if (sender.isDestroyed()) return false;
		if (frame == null || frame.detached) return false;
		if (frame.parent != null) return false;
		if (frame !== sender.mainFrame) return false;
		if (frame.top !== frame) return false;
		if (electronWebContents.fromFrame(frame) !== sender) return false;
		return policy.isPrivilegedRendererDocument({sender, url: frame.url});
	} catch {
		return false;
	}
}

function isCurrentPrivilegedRendererMainFrame(
	sender: Electron.WebContents,
	frame: Electron.WebFrameMain | null,
	policy: PrivilegedRendererDocumentPolicy,
): boolean {
	try {
		if (sender.isLoadingMainFrame()) return false;
	} catch {
		return false;
	}
	return isExecutingPrivilegedRendererMainFrame(sender, frame, policy);
}

class ElectronRendererDocumentOwner implements RendererDocumentOwner {
	public readonly sender: Electron.WebContents;
	public readonly frame: Electron.WebFrameMain;
	private readonly dependencies: RendererDocumentOwnerFactoryDependencies;

	public constructor(
		sender: Electron.WebContents,
		frame: Electron.WebFrameMain,
		dependencies: RendererDocumentOwnerFactoryDependencies,
	) {
		this.sender = sender;
		this.frame = frame;
		this.dependencies = dependencies;
	}

	public isCurrent(): boolean {
		return isCurrentPrivilegedRendererMainFrame(this.sender, this.frame, this.dependencies.policy);
	}

	public requireCurrent(context: string): void {
		if (!this.isCurrent()) {
			throw new InactiveRendererDocumentOwnerError(context);
		}
	}

	public matchesEvent(event: RendererDocumentIpcEvent): boolean {
		if (event.sender !== this.sender) return false;
		if ((event.senderFrame ?? null) !== this.frame) return false;
		return this.isCurrent();
	}

	public watchInvalidation(
		onInvalidated: (reason: RendererDocumentInvalidationReason) => void,
	): RendererDocumentOwnerWatcher {
		this.requireCurrent('Renderer document invalidation watcher');
		return new ActiveRendererDocumentOwnerWatcher(this, onInvalidated, this.dependencies.onWatcherFailure);
	}
}

class ActiveRendererDocumentOwnerWatcher implements RendererDocumentOwnerWatcher {
	private active = true;
	private readonly owner: RendererDocumentOwner;
	private readonly onInvalidated: (reason: RendererDocumentInvalidationReason) => void;
	private readonly onFailure: (error: unknown, reason: string) => void;

	public constructor(
		owner: RendererDocumentOwner,
		onInvalidated: (reason: RendererDocumentInvalidationReason) => void,
		onFailure: (error: unknown, reason: string) => void,
	) {
		this.owner = owner;
		this.onInvalidated = onInvalidated;
		this.onFailure = onFailure;
		owner.sender.once('destroyed', this.handleDestroyed);
		owner.sender.on('did-start-navigation', this.handleDidStartNavigation);
		owner.sender.on('render-process-gone', this.handleRenderProcessGone);
	}

	public dispose(): void {
		if (!this.active) return;
		this.active = false;
		this.owner.sender.removeListener('destroyed', this.handleDestroyed);
		this.owner.sender.removeListener('did-start-navigation', this.handleDidStartNavigation);
		this.owner.sender.removeListener('render-process-gone', this.handleRenderProcessGone);
	}

	private invalidate(reason: RendererDocumentInvalidationReason): void {
		if (!this.active) return;
		this.dispose();
		try {
			this.onInvalidated(reason);
		} catch (error) {
			this.onFailure(error, reason);
		}
	}

	private readonly handleDestroyed = (): void => {
		this.invalidate(RendererDocumentInvalidationReason.DESTROYED);
	};

	private readonly handleRenderProcessGone = (): void => {
		this.invalidate(RendererDocumentInvalidationReason.RENDER_PROCESS_GONE);
	};

	private readonly handleDidStartNavigation = (details: unknown): void => {
		if (typeof details === 'object' && details !== null) {
			const navigation = details as {isMainFrame?: unknown; isSameDocument?: unknown};
			if (navigation.isMainFrame !== true) return;
			if (navigation.isSameDocument === true) return;
		}
		this.invalidate(RendererDocumentInvalidationReason.NAVIGATED);
	};
}

export class RendererDocumentOwnerFactory {
	private readonly dependencies: RendererDocumentOwnerFactoryDependencies;

	public constructor(dependencies: RendererDocumentOwnerFactoryDependencies) {
		this.dependencies = dependencies;
	}

	public capture(event: RendererDocumentIpcEvent, context: string): RendererDocumentOwner {
		const frame = event.senderFrame ?? null;
		if (frame == null) {
			throw new RendererDocumentCaptureError(context);
		}
		const owner = new ElectronRendererDocumentOwner(event.sender, frame, this.dependencies);
		if (!owner.isCurrent()) {
			throw new RendererDocumentCaptureError(context);
		}
		return owner;
	}

	public requireExecutingCurrentMainFrameForRead(event: RendererDocumentIpcEvent, context: string): void {
		const frame = event.senderFrame ?? null;
		if (!isExecutingPrivilegedRendererMainFrame(event.sender, frame, this.dependencies.policy)) {
			throw new RendererDocumentCaptureError(context);
		}
	}
}
