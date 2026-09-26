// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import AttachmentUrlRefresher from '@app/features/messaging/state/AttachmentUrlRefresher';
import {isUrlOnEndpoint, parseEndpoint} from '@app/features/messaging/utils/AttachmentCdnUrl';
import {isMobileOrTabletUserAgent} from '@app/features/platform/notifications/NotificationAlertOptions';
import {supportsShowSaveFilePicker} from '@app/features/platform/types/Browser';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {downloadWithNative, isElectron, isFirefoxBrowser, openExternalUrl} from '@app/features/ui/utils/NativeUtils';

const logger = new Logger('FileDownloadUtils');

type MediaType = 'image' | 'gif' | 'video' | 'audio' | 'file';

function appendMediaProxyDownloadParam(src: string): string {
	let parsedSrc: URL;
	try {
		parsedSrc = new URL(src);
	} catch {
		return src;
	}
	const mediaEndpoint = parseEndpoint(RuntimeConfig.mediaEndpoint);
	if (!mediaEndpoint || !isUrlOnEndpoint(parsedSrc, mediaEndpoint)) {
		return src;
	}
	parsedSrc.searchParams.set('download', 'true');
	return parsedSrc.toString();
}

function deriveSuggestedName(src: string, type: MediaType, providedFilename?: string): string {
	if (providedFilename) return applyDefaultExtension(providedFilename, type);
	try {
		const parsed = new URL(src);
		const segments = parsed.pathname.split('/').filter(Boolean);
		const lastSegment = segments[segments.length - 1];
		if (lastSegment) {
			try {
				const decoded = decodeURIComponent(lastSegment);
				return applyDefaultExtension(decoded, type);
			} catch {
				return applyDefaultExtension(lastSegment, type);
			}
		}
	} catch {}
	return `download${defaultExtensionFor(type)}`;
}

function applyDefaultExtension(fileName: string, type: MediaType): string {
	const defaultExtension = defaultExtensionFor(type);
	if (!defaultExtension) return fileName;
	const lastDotIndex = fileName.lastIndexOf('.');
	if (lastDotIndex === -1) return `${fileName}${defaultExtension}`;
	if (type === 'gif' && fileName.slice(lastDotIndex).toLowerCase() !== '.gif') {
		return `${fileName.slice(0, lastDotIndex)}${defaultExtension}`;
	}
	return fileName;
}

function defaultExtensionFor(type: MediaType): string {
	switch (type) {
		case 'gif':
			return '.gif';
		case 'image':
			return '.png';
		case 'video':
			return '.mp4';
		case 'audio':
			return '.mp3';
		default:
			return '';
	}
}

function mimeForType(type: MediaType): string | undefined {
	switch (type) {
		case 'gif':
			return 'image/gif';
		case 'image':
			return 'image/*';
		case 'video':
			return 'video/*';
		case 'audio':
			return 'audio/*';
		default:
			return undefined;
	}
}

function isUserAbort(error: unknown): boolean {
	return error instanceof DOMException && error.name === 'AbortError';
}

function isMobileOrTabletBrowser(): boolean {
	return typeof navigator !== 'undefined' && isMobileOrTabletUserAgent(navigator.userAgent, navigator.maxTouchPoints);
}

async function downloadViaFileSystemAccess(src: string, suggestedName: string, type: MediaType): Promise<boolean> {
	if (
		isMobileOrTabletBrowser() ||
		!supportsShowSaveFilePicker(window) ||
		typeof window.showSaveFilePicker !== 'function'
	) {
		return false;
	}
	const extension = defaultExtensionFor(type);
	const mime = mimeForType(type);
	let handle: FileSystemFileHandle;
	try {
		handle = await window.showSaveFilePicker({
			suggestedName,
			types:
				mime && extension
					? [
							{
								description: type,
								accept: {[mime]: [extension as `.${string}`]},
							},
						]
					: undefined,
		});
	} catch (error) {
		if (isUserAbort(error)) return true;
		logger.warn('Save picker unavailable, falling back to anchor download', error);
		return false;
	}
	let writable: FileSystemWritableFileStream | undefined;
	try {
		const response = await fetch(src, {credentials: 'omit'});
		if (!response.ok || !response.body) {
			if (response.ok && !response.body) {
				writable = await handle.createWritable();
				await writable.write(new Uint8Array());
				await writable.close();
				return true;
			}
			throw new Error(`Failed to fetch ${src}: ${response.status}`);
		}
		writable = await handle.createWritable();
		await response.body.pipeTo(writable);
		return true;
	} catch (error) {
		try {
			await writable?.abort();
		} catch {}
		logger.warn('File System Access download failed, falling back', error);
		return false;
	}
}

function shouldFetchBlobBeforeAnchor(src: string): boolean {
	if (!isFirefoxBrowser()) return false;
	if (/^(blob|data):/i.test(src)) return false;
	return typeof fetch === 'function' && typeof URL.createObjectURL === 'function';
}

async function downloadViaFetchBlob(src: string, suggestedName: string): Promise<boolean> {
	if (!shouldFetchBlobBeforeAnchor(src)) return false;
	try {
		const response = await fetch(appendMediaProxyDownloadParam(src), {credentials: 'omit'});
		if (!response.ok) {
			throw new Error(`Failed to fetch ${src}: ${response.status}`);
		}
		const blob = await response.blob();
		const blobUrl = URL.createObjectURL(blob);
		try {
			return downloadViaAnchor(blobUrl, suggestedName, {appendDownloadParam: false});
		} finally {
			window.setTimeout(() => URL.revokeObjectURL(blobUrl), 30_000);
		}
	} catch (error) {
		logger.warn('Blob download failed, falling back to anchor download', error);
		return false;
	}
}

function downloadViaAnchor(src: string, suggestedName: string, options?: {appendDownloadParam?: boolean}): boolean {
	if (typeof document === 'undefined') return false;
	const anchor = document.createElement('a');
	anchor.href = options?.appendDownloadParam === false ? src : appendMediaProxyDownloadParam(src);
	anchor.download = suggestedName;
	anchor.rel = 'noopener noreferrer';
	anchor.dataset.routerIgnore = 'true';
	anchor.style.display = 'none';
	document.body.appendChild(anchor);
	try {
		anchor.click();
	} catch (error) {
		anchor.remove();
		logger.warn('Anchor download click failed', error);
		return false;
	}
	anchor.remove();
	return true;
}

export async function downloadFile(src: string, type: MediaType, providedFilename?: string): Promise<void> {
	if (!src) return;
	const suggestedName = deriveSuggestedName(src, type, providedFilename);
	const target = await AttachmentUrlRefresher.refresh(src);
	if (isElectron()) {
		try {
			const outcome = await downloadWithNative({url: target, suggestedName});
			if (outcome !== 'unavailable') return;
		} catch (error) {
			logger.warn('Native download failed', error);
			return;
		}
	}
	if (await downloadViaFileSystemAccess(target, suggestedName, type)) return;
	if (await downloadViaFetchBlob(target, suggestedName)) return;
	if (downloadViaAnchor(target, suggestedName)) return;
	await openExternalUrl(appendMediaProxyDownloadParam(target));
}

export function createDownloadHandler(src: string, type: MediaType, providedFilename?: string) {
	return async () => {
		await downloadFile(src, type, providedFilename);
	};
}
