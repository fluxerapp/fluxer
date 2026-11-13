// SPDX-License-Identifier: AGPL-3.0-or-later

import {getApiAttachmentAuthorization} from '@electron/main/ApiAttachmentAuthorization';
import {isPrivilegedRendererDocumentSender} from '@electron/main/PrivilegedRendererDocuments';
import {
	DESKTOP_API_ATTACHMENT_AUTHORIZATION_CHANNEL,
	readDesktopApiAttachmentAuthorization,
} from '@fluxer/desktop_ipc/src/ApiAttachmentAuthorizationContract';
import {ipcMain, type WebContents} from 'electron';

const watchedSenders = new WeakSet<WebContents>();

function watchSender(sender: WebContents): void {
	if (watchedSenders.has(sender)) return;
	watchedSenders.add(sender);
	const id = sender.id;
	const authorization = getApiAttachmentAuthorization();
	sender.once('destroyed', () => authorization.revoke(id));
	sender.on('render-process-gone', () => authorization.revoke(id));
	sender.on(
		'did-start-navigation',
		(_event: Electron.Event, _url: string, isSameDoc: boolean, isMainFrame: boolean) => {
			if (isMainFrame && !isSameDoc) authorization.revoke(id);
		},
	);
}

export function registerApiAttachmentAuthorizationIpc(): void {
	ipcMain.handle(DESKTOP_API_ATTACHMENT_AUTHORIZATION_CHANNEL, (event, value: unknown): boolean => {
		if (!isPrivilegedRendererDocumentSender(event)) return false;
		const request = readDesktopApiAttachmentAuthorization(value);
		const authorization = getApiAttachmentAuthorization();
		if (request === null || request.token === null) {
			authorization.revoke(event.sender.id);
			return false;
		}
		watchSender(event.sender);
		if (authorization.grant(event.sender.id, request.apiEndpoint, request.token)) return true;
		authorization.revoke(event.sender.id);
		return false;
	});
}
