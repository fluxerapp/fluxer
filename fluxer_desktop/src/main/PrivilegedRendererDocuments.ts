// SPDX-License-Identifier: AGPL-3.0-or-later

import {isTrustedOrigin} from '@electron/main/Window';
import type {IpcMainInvokeEvent} from 'electron';

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
		return isTrustedOrigin(frame.url);
	} catch {
		return false;
	}
}

export function requirePrivilegedRendererDocumentSender(event: IpcMainInvokeEvent, channel: string): void {
	if (!isPrivilegedRendererDocumentSender(event)) {
		throw new UntrustedRendererDocumentSenderError(channel);
	}
}
