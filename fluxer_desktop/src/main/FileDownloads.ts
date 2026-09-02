// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import {pipeline} from 'node:stream/promises';
import {
	type DesktopOutboundHTTPMessage,
	getDesktopOutboundHTTP,
	parseDesktopHTTPTarget,
	parseDesktopRedirectTarget,
} from '@electron/main/DesktopOutboundHTTP';

const MAX_DOWNLOAD_REDIRECTS = 5;
const DOWNLOAD_DEADLINE_MS = 600_000;
const DOWNLOAD_MAX_BYTES = 512 * 1024 * 1024;
const DOWNLOAD_CONTEXT = 'File download';

interface DownloadFileOptions {
	maxBytes?: number;
}

async function removePartialDownload(destPath: string): Promise<void> {
	await fs.promises.unlink(destPath).catch(() => {});
}

async function writeCappedResponse(
	response: DesktopOutboundHTTPMessage['message'],
	destPath: string,
	maxBytes: number,
): Promise<void> {
	let received = 0;
	response.on('data', (chunk: Buffer) => {
		received += chunk.length;
		if (received > maxBytes) {
			response.destroy(new Error(`Download exceeds ${maxBytes} bytes`));
		}
	});
	await pipeline(response, fs.createWriteStream(destPath));
}

async function downloadFileWithRedirects(
	url: URL,
	destPath: string,
	redirects: number,
	maxBytes: number,
): Promise<void> {
	const message = await getDesktopOutboundHTTP().get({
		context: DOWNLOAD_CONTEXT,
		timeoutMs: DOWNLOAD_DEADLINE_MS,
		url,
	});
	const statusCode = message.status;
	if (statusCode >= 300 && statusCode < 400) {
		message.message.destroy();
		if (redirects >= MAX_DOWNLOAD_REDIRECTS) {
			throw new Error('Too many download redirects');
		}
		const nextUrl = parseDesktopRedirectTarget(message.url, message.headers.location);
		if (nextUrl == null) {
			throw new Error(`HTTP ${statusCode} redirect target is unusable`);
		}
		await downloadFileWithRedirects(nextUrl, destPath, redirects + 1, maxBytes);
		return;
	}
	if (statusCode === 204 || statusCode === 205) {
		message.message.destroy();
		await fs.promises.writeFile(destPath, new Uint8Array());
		return;
	}
	if (statusCode < 200 || statusCode >= 300) {
		message.message.destroy();
		throw new Error(`HTTP ${statusCode}`);
	}
	try {
		await writeCappedResponse(message.message, destPath, maxBytes);
	} catch (error) {
		await removePartialDownload(destPath);
		throw error;
	}
}

export async function downloadFile(url: string, destPath: string, options: DownloadFileOptions = {}): Promise<void> {
	const target = parseDesktopHTTPTarget(url);
	if (target == null) {
		throw new Error('Download URL must use http or https');
	}
	const maxBytes = options.maxBytes ?? DOWNLOAD_MAX_BYTES;
	await removePartialDownload(destPath);
	await downloadFileWithRedirects(target, destPath, 0, maxBytes);
}
