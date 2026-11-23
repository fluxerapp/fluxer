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
const DOWNLOAD_DEADLINE_MS = 86_400_000;
const DOWNLOAD_STALL_MS = 60_000;
const DOWNLOAD_MAX_BYTES = 512 * 1024 * 1024;
const DOWNLOAD_CONTEXT = 'File download';
const SHA256_PATTERN = /^[0-9a-f]{64}$/;

export class DownloadChecksumError extends Error {
	constructor(expected: string, actual: string) {
		super(`Download checksum mismatch: expected ${expected}, downloaded ${actual}`);
		this.name = 'DownloadChecksumError';
	}
}

class DownloadStalledError extends Error {
	constructor() {
		super(`Download received no data for ${DOWNLOAD_STALL_MS} ms`);
		this.name = 'DownloadStalledError';
	}
}

interface DownloadFileOptions {
	maxBytes?: number | null;
	sha256?: string | null;
	signal?: AbortSignal;
	onProgress?: DownloadProgressHandler;
}

type DownloadProgressHandler = (receivedBytes: number, totalBytes: number | null) => void;

function parseContentLength(value: string | Array<string> | undefined): number | null {
	const raw = Array.isArray(value) ? value[0] : value;
	if (raw == null) {
		return null;
	}
	const parsed = Number.parseInt(raw, 10);
	return Number.isFinite(parsed) && parsed >= 0 ? parsed : null;
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

class StallWatch {
	private readonly controller = new AbortController();
	private timer: NodeJS.Timeout | null = null;

	public get signal(): AbortSignal {
		return this.controller.signal;
	}

	public touch(): void {
		if (this.controller.signal.aborted) return;
		if (this.timer !== null) clearTimeout(this.timer);
		this.timer = setTimeout(() => this.controller.abort(new DownloadStalledError()), DOWNLOAD_STALL_MS);
		this.timer.unref();
	}

	public stop(): void {
		if (this.timer !== null) clearTimeout(this.timer);
		this.timer = null;
	}
}

async function writeCappedResponse(
	response: DesktopOutboundHTTPMessage['message'],
	destPath: string,
	maxBytes: number | null,
	hash: Hash | null,
	totalBytes: number | null,
	onProgress: DownloadProgressHandler | undefined,
	stall: StallWatch,
): Promise<void> {
	let received = 0;
	const onStall = (): void => {
		response.destroy(new DownloadStalledError());
	};
	stall.signal.addEventListener('abort', onStall);
	response.on('data', (chunk: Buffer) => {
		stall.touch();
		received += chunk.length;
		if (maxBytes !== null && received > maxBytes) {
			response.destroy(new Error(`Download exceeds ${maxBytes} bytes`));
			return;
		}
		hash?.update(chunk);
		onProgress?.(received, totalBytes);
	});
	try {
		await pipeline(response, fs.createWriteStream(destPath));
	} finally {
		stall.signal.removeEventListener('abort', onStall);
	}
}

async function downloadFileWithRedirects(
	url: URL,
	destPath: string,
	redirects: number,
	maxBytes: number | null,
	hash: Hash | null,
	signal: AbortSignal | null,
	onProgress: DownloadProgressHandler | undefined,
	stall: StallWatch,
): Promise<void> {
	stall.touch();
	const message = await getDesktopOutboundHTTP()
		.get({
			context: DOWNLOAD_CONTEXT,
			timeoutMs: DOWNLOAD_DEADLINE_MS,
			url,
			signal: signal == null ? stall.signal : AbortSignal.any([signal, stall.signal]),
		})
		.catch((error: unknown) => {
			if (stall.signal.aborted && signal?.aborted !== true) throw new DownloadStalledError();
			throw error;
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
		await downloadFileWithRedirects(nextUrl, destPath, redirects + 1, maxBytes, hash, signal, onProgress, stall);
		return;
	}
	if (statusCode === 204 || statusCode === 205) {
		message.message.destroy();
		await fs.promises.writeFile(destPath, new Uint8Array());
		onProgress?.(0, 0);
		return;
	}
	if (statusCode < 200 || statusCode >= 300) {
		message.message.destroy();
		throw new Error(`HTTP ${statusCode}`);
	}
	try {
		await writeCappedResponse(
			message.message,
			destPath,
			maxBytes,
			hash,
			parseContentLength(message.headers['content-length']),
			onProgress,
			stall,
		);
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
	const maxBytes = options.maxBytes === undefined ? DOWNLOAD_MAX_BYTES : options.maxBytes;
	await removePartialDownload(destPath);
	const stall = new StallWatch();
	try {
		await downloadFileWithRedirects(
			target,
			destPath,
			0,
			maxBytes,
			hash,
			options.signal ?? null,
			options.onProgress,
			stall,
		);
	} finally {
		stall.stop();
	}
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
