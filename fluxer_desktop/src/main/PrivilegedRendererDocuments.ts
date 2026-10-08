// SPDX-License-Identifier: AGPL-3.0-or-later

import {DESKTOP_APP_ORIGIN} from '@electron/common/Constants';
import {createChildLogger} from '@electron/common/Logger';
import {RendererDocumentOwnerFactory} from '@electron/main/RendererDocumentOwner';
import {isAppDocumentWindowContents, isTrustedOrigin} from '@electron/main/Window';
import type {IpcMainInvokeEvent} from 'electron';

const BLANK_DOCUMENT_URL = 'about:blank';

class UntrustedRendererDocumentSenderError extends Error {
	public constructor(channel: string) {
		super(`${channel} is only reachable from a trusted top-level renderer document`);
		this.name = 'UntrustedRendererDocumentSenderError';
	}
}

function isPrivilegedRendererDocumentSender(event: IpcMainInvokeEvent): boolean {
	const frame = event.senderFrame;
	if (frame == null) {
		return false;
	}
	try {
		if (frame.detached) {
			return false;
		}
		if (frame.parent != null) {
			return false;
		}
		if (isTrustedOrigin(frame.url)) {
			return true;
		}
		if (frame.url !== '' && frame.url !== BLANK_DOCUMENT_URL) {
			return false;
		}
		return frame.origin === DESKTOP_APP_ORIGIN;
	} catch {
		return false;
	}
}

export function requirePrivilegedRendererDocumentSender(event: IpcMainInvokeEvent, channel: string): void {
	if (!isPrivilegedRendererDocumentSender(event)) {
		throw new UntrustedRendererDocumentSenderError(channel);
	}
}

export function createPrivilegedRendererDocumentOwners(componentName: string): RendererDocumentOwnerFactory {
	const log = createChildLogger(componentName);
	return new RendererDocumentOwnerFactory({
		policy: {
			isPrivilegedRendererDocument: ({sender, url}) => isAppDocumentWindowContents(sender) && isTrustedOrigin(url),
		},
		onWatcherFailure: (error, reason) => {
			log.warn('Renderer document invalidation handling failed', {reason, error});
		},
	});
}
