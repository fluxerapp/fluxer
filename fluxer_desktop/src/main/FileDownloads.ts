// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash, type Hash} from 'node:crypto';
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
const SHA256_PATTERN = /^[0-9a-f]{64}$/;

export class DownloadChecksumError extends Error {
	constructor(expected: string, actual: string) {
		super(`Download checksum mismatch: expected ${expected}, downloaded ${actual}`);
		this.name = 'DownloadChecksumError';
	}
}

interface DownloadFileOptions {
	maxBytes?: number;
	sha256?: string | null;
}

function parseExpectedSha256(value: string | null | undefined): string | null {
	if (value == null) {
		return null;
	}
	const expected = value.trim().toLowerCase();
	if (expected.length === 0) {
		return null;
	}
	if (!SHA256_PATTERN.test(expected)) {
		throw new Error('Download checksum is malformed');
	}
	return expected;
}

async function removePartialDownload(destPath: string): Promise<void> {
	await fs.promises.unlink(destPath).catch(() => {});
}

async function writeCappedResponse(
	response: DesktopOutboundHTTPMessage['message'],
	destPath: string,
	maxBytes: number,
	hash: Hash | null,
): Promise<void> {
	let received = 0;
	response.on('data', (chunk: Buffer) => {
		received += chunk.length;
		if (received > maxBytes) {
			response.destroy(new Error(`Download exceeds ${maxBytes} bytes`));
			return;
		}
		hash?.update(chunk);
	});
	await pipeline(response, fs.createWriteStream(destPath));
}

async function downloadFileWithRedirects(
	url: URL,
	destPath: string,
	redirects: number,
	maxBytes: number,
	hash: Hash | null,
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
		await downloadFileWithRedirects(nextUrl, destPath, redirects + 1, maxBytes, hash);
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
		await writeCappedResponse(message.message, destPath, maxBytes, hash);
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
	const expectedSha256 = parseExpectedSha256(options.sha256);
	const hash = expectedSha256 === null ? null : createHash('sha256');
	const maxBytes = options.maxBytes ?? DOWNLOAD_MAX_BYTES;
	await removePartialDownload(destPath);
	await downloadFileWithRedirects(target, destPath, 0, maxBytes, hash);
	if (expectedSha256 === null || hash === null) {
		return;
	}
	const digest = hash.digest('hex');
	if (digest === expectedSha256) {
		return;
	}
	await removePartialDownload(destPath);
	throw new DownloadChecksumError(expectedSha256, digest);
}
