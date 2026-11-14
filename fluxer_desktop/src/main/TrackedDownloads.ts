// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import fs from 'node:fs';
import path from 'node:path';
import type {DownloadEntryEvent} from '@electron/common/Types';
import {downloadFile} from '@electron/main/FileDownloads';
import {
	isPrivilegedRendererDocumentSender,
	requirePrivilegedRendererDocumentSender,
} from '@electron/main/PrivilegedRendererDocuments';
import {type IpcMainEvent, ipcMain, shell, type WebContents} from 'electron';

const DOWNLOAD_PROGRESS_EMIT_INTERVAL_MS = 250;
const TRACKED_DOWNLOADS_LIMIT = 10;

type TrackedDownloadState = DownloadEntryEvent['state'];

interface TrackedDownload {
	id: string;
	fileName: string;
	filePath: string;
	owner: WebContents;
	controller: AbortController;
	state: TrackedDownloadState;
	receivedBytes: number;
	totalBytes: number | null;
	fileExists: boolean;
}

const trackedDownloads = new Map<string, TrackedDownload>();
const watchedOwners = new WeakSet<WebContents>();

function isInProgress(download: TrackedDownload): boolean {
	return download.state === 'started' || download.state === 'progressing';
}

function toEntry(download: TrackedDownload): DownloadEntryEvent {
	return {
		id: download.id,
		fileName: download.fileName,
		state: download.state,
		receivedBytes: download.receivedBytes,
		totalBytes: download.totalBytes,
		fileExists: download.fileExists,
	};
}

function listDownloads(owner: WebContents): Array<DownloadEntryEvent> {
	const entries: Array<DownloadEntryEvent> = [];
	for (const download of trackedDownloads.values()) {
		if (download.owner === owner) entries.unshift(toEntry(download));
	}
	return entries;
}

function refreshFileExists(download: TrackedDownload): void {
	download.fileExists = download.state === 'completed' && fs.existsSync(download.filePath);
}

function sendDownloads(owner: WebContents): void {
	if (owner.isDestroyed()) return;
	owner.send('downloads-changed', listDownloads(owner));
}

function releaseOwner(owner: WebContents): void {
	for (const [id, download] of trackedDownloads) {
		if (download.owner !== owner) continue;
		if (isInProgress(download)) download.controller.abort();
		trackedDownloads.delete(id);
	}
}

function watchOwner(owner: WebContents): void {
	if (watchedOwners.has(owner)) return;
	watchedOwners.add(owner);
	owner.once('destroyed', () => releaseOwner(owner));
}

function addDownload(download: TrackedDownload): void {
	watchOwner(download.owner);
	for (const [id, tracked] of trackedDownloads) {
		if (tracked.filePath !== download.filePath || isInProgress(tracked)) continue;
		trackedDownloads.delete(id);
		if (tracked.owner !== download.owner) sendDownloads(tracked.owner);
	}
	trackedDownloads.set(download.id, download);
	for (const [id, tracked] of trackedDownloads) {
		if (trackedDownloads.size <= TRACKED_DOWNLOADS_LIMIT) break;
		if (isInProgress(tracked)) continue;
		trackedDownloads.delete(id);
		if (tracked.owner !== download.owner) sendDownloads(tracked.owner);
	}
	sendDownloads(download.owner);
}

function updateDownload(
	download: TrackedDownload,
	state: TrackedDownloadState,
	receivedBytes: number,
	totalBytes: number | null,
): void {
	download.state = state;
	download.receivedBytes = receivedBytes;
	download.totalBytes = totalBytes;
	download.fileExists = state === 'completed';
	sendDownloads(download.owner);
}

export async function downloadTrackedFile(
	owner: WebContents,
	url: string,
	filePath: string,
	sha256: string | null | undefined,
): Promise<'completed' | 'canceled'> {
	const download: TrackedDownload = {
		id: randomUUID(),
		fileName: path.basename(filePath),
		filePath,
		owner,
		controller: new AbortController(),
		state: 'started',
		receivedBytes: 0,
		totalBytes: null,
		fileExists: false,
	};
	addDownload(download);
	let lastEmitAt = 0;
	let lastReceivedBytes = 0;
	try {
		await downloadFile(url, filePath, {
			sha256,
			signal: download.controller.signal,
			onProgress: (receivedBytes, totalBytes) => {
				lastReceivedBytes = receivedBytes;
				const now = Date.now();
				if (now - lastEmitAt < DOWNLOAD_PROGRESS_EMIT_INTERVAL_MS) return;
				lastEmitAt = now;
				updateDownload(download, 'progressing', receivedBytes, totalBytes);
			},
		});
		updateDownload(download, 'completed', lastReceivedBytes, lastReceivedBytes);
		return 'completed';
	} catch (error) {
		if (download.controller.signal.aborted) {
			updateDownload(download, 'canceled', lastReceivedBytes, null);
			return 'canceled';
		}
		updateDownload(download, 'failed', lastReceivedBytes, null);
		throw error;
	}
}

function getOwnedDownload(event: IpcMainEvent, id: unknown): TrackedDownload | null {
	if (!isPrivilegedRendererDocumentSender(event) || typeof id !== 'string') return null;
	const download = trackedDownloads.get(id);
	if (download == null || download.owner !== event.sender) return null;
	return download;
}

function getCompletedFile(event: IpcMainEvent, id: unknown): string | null {
	const download = getOwnedDownload(event, id);
	if (download == null || download.state !== 'completed') return null;
	refreshFileExists(download);
	if (!download.fileExists) {
		sendDownloads(download.owner);
		return null;
	}
	return download.filePath;
}

export function registerTrackedDownloadsIpcHandlers(): void {
	ipcMain.handle('downloads-list', (event): Array<DownloadEntryEvent> => {
		requirePrivilegedRendererDocumentSender(event, 'downloads-list');
		for (const download of trackedDownloads.values()) {
			if (download.owner === event.sender) refreshFileExists(download);
		}
		return listDownloads(event.sender);
	});
	ipcMain.on('download-cancel', (event, id: unknown) => {
		const download = getOwnedDownload(event, id);
		if (download == null || !isInProgress(download)) return;
		download.controller.abort();
	});
	ipcMain.on('download-open', (event, id: unknown) => {
		const filePath = getCompletedFile(event, id);
		if (filePath == null) return;
		void shell.openPath(filePath);
	});
	ipcMain.on('download-reveal', (event, id: unknown) => {
		const filePath = getCompletedFile(event, id);
		if (filePath == null) return;
		shell.showItemInFolder(filePath);
	});
	ipcMain.on('download-dismiss', (event, id: unknown) => {
		const download = getOwnedDownload(event, id);
		if (download == null || isInProgress(download)) return;
		trackedDownloads.delete(download.id);
		sendDownloads(event.sender);
	});
	ipcMain.on('downloads-clear', (event) => {
		if (!isPrivilegedRendererDocumentSender(event)) return;
		for (const [id, download] of trackedDownloads) {
			if (download.owner === event.sender && !isInProgress(download)) trackedDownloads.delete(id);
		}
		sendDownloads(event.sender);
	});
}
