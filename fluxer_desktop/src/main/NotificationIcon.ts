// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';
import fs from 'node:fs';
import path from 'node:path';
import {createChildLogger} from '@electron/common/Logger';
import {
	getDesktopOutboundHTTP,
	parseDesktopHTTPTarget,
	parseDesktopRedirectTarget,
	readBoundedMessage,
	readMessageContentLength,
} from '@electron/main/DesktopOutboundHTTP';
import {app, nativeImage} from 'electron';

const logger = createChildLogger('NotificationIcon');
const NOTIFICATION_ICON_DOWNLOAD_TIMEOUT_MS = 10000;
const NOTIFICATION_ICON_MAX_BYTES = 3 * 1024 * 1024;
const NOTIFICATION_ICON_CACHE_MAX_FILES = 512;
const NOTIFICATION_ICON_MAX_CHUNKS = 4096;
const NOTIFICATION_ICON_MAX_REDIRECTS = 5;
const NOTIFICATION_ICON_CONTEXT = 'Notification icon download';
const NOTIFICATION_ICON_REDIRECT_STATUS_CODES: ReadonlySet<number> = new Set([301, 302, 303, 307, 308]);

type ResolvedNotificationIcon = NonNullable<Electron.NotificationConstructorOptions['icon']>;

interface DownloadOptions {
	maxBytes: number;
	timeoutMs: number;
	redirectsRemaining: number;
}

function isHttpUrl(value: string): boolean {
	return value.startsWith('http://') || value.startsWith('https://');
}

function describeIconSource(source: string): string {
	if (source.startsWith('data:')) {
		return 'data-url';
	}
	try {
		const url = new URL(source);
		return `${url.protocol}//${url.host}${url.pathname}`;
	} catch {
		return path.basename(source) || 'local-path';
	}
}

function getNotificationIconCacheDir(): string {
	return path.join(app.getPath('userData'), 'notification-icons');
}

function getErrorCode(error: unknown): string | undefined {
	if (typeof error !== 'object' || error === null || !('code' in error)) {
		return undefined;
	}
	const code = (
		error as {
			code?: unknown;
		}
	).code;
	return typeof code === 'string' ? code : undefined;
}

async function touchFile(filePath: string): Promise<void> {
	const now = new Date();
	await fs.promises.utimes(filePath, now, now).catch(() => {});
}

let trimPromise: Promise<void> | null = null;

function scheduleNotificationIconCacheTrim(cacheDir: string): void {
	if (trimPromise) return;
	trimPromise = trimNotificationIconCache(cacheDir)
		.catch((error) => {
			logger.warn('Failed to trim notification icon cache', {error});
		})
		.finally(() => {
			trimPromise = null;
		});
}

async function trimNotificationIconCache(cacheDir: string): Promise<void> {
	const entries = await fs.promises.readdir(cacheDir, {withFileTypes: true});
	const files = await Promise.all(
		entries
			.filter((entry) => entry.isFile() && entry.name.endsWith('.png'))
			.map(async (entry) => {
				const filePath = path.join(cacheDir, entry.name);
				const stat = await fs.promises.stat(filePath);
				return {filePath, mtimeMs: stat.mtimeMs};
			}),
	);
	if (files.length <= NOTIFICATION_ICON_CACHE_MAX_FILES) {
		return;
	}
	files.sort((a, b) => a.mtimeMs - b.mtimeMs);
	const deleteCount = files.length - NOTIFICATION_ICON_CACHE_MAX_FILES;
	await Promise.all(files.slice(0, deleteCount).map(({filePath}) => fs.promises.rm(filePath, {force: true})));
}

async function cacheNotificationIcon(source: string, image: Electron.NativeImage): Promise<string> {
	const cacheDir = getNotificationIconCacheDir();
	await fs.promises.mkdir(cacheDir, {recursive: true});
	const cacheKey = crypto.createHash('sha256').update(source).digest('hex');
	const filePath = path.join(cacheDir, `${cacheKey}.png`);
	try {
		await fs.promises.access(filePath, fs.constants.R_OK);
		await touchFile(filePath);
		return filePath;
	} catch {}
	const tmpPath = path.join(cacheDir, `${cacheKey}.${process.pid}.${Date.now()}.tmp`);
	await fs.promises.writeFile(tmpPath, image.toPNG(), {mode: 0o600});
	try {
		await fs.promises.rename(tmpPath, filePath);
	} catch (error) {
		await fs.promises.rm(tmpPath, {force: true});
		if (getErrorCode(error) === 'EEXIST') {
			await touchFile(filePath);
			return filePath;
		}
		throw error;
	}
	scheduleNotificationIconCacheTrim(cacheDir);
	return filePath;
}

function decodeNotificationIcon(source: string, buffer: Buffer): Electron.NativeImage | null {
	const image = nativeImage.createFromBuffer(buffer);
	if (image.isEmpty()) {
		logger.warn('Notification icon could not be decoded as PNG/JPEG', {
			source: describeIconSource(source),
			bytes: buffer.length,
		});
		return null;
	}
	return image;
}

async function resolveDecodedNotificationIcon(
	source: string,
	image: Electron.NativeImage,
): Promise<ResolvedNotificationIcon> {
	if (process.platform === 'win32') {
		return cacheNotificationIcon(source, image);
	}
	return image;
}

export async function resolveNotificationIcon(source: string): Promise<ResolvedNotificationIcon | null> {
	if (!source) {
		return null;
	}
	if (isHttpUrl(source)) {
		const buffer = await downloadToBuffer(source, {
			maxBytes: NOTIFICATION_ICON_MAX_BYTES,
			timeoutMs: NOTIFICATION_ICON_DOWNLOAD_TIMEOUT_MS,
			redirectsRemaining: NOTIFICATION_ICON_MAX_REDIRECTS,
		});
		const image = decodeNotificationIcon(source, buffer);
		return image ? resolveDecodedNotificationIcon(source, image) : null;
	}
	if (source.startsWith('data:')) {
		const image = nativeImage.createFromDataURL(source);
		if (image.isEmpty()) {
			logger.warn('Notification icon data URL could not be decoded as PNG/JPEG');
			return null;
		}
		return resolveDecodedNotificationIcon(source, image);
	}
	logger.warn('Rejected non-URL notification icon source', {source: describeIconSource(source)});
	return null;
}

async function downloadToBuffer(url: string, options: DownloadOptions): Promise<Buffer> {
	const initialTarget = parseDesktopHTTPTarget(url);
	if (initialTarget == null) {
		throw new Error('Notification icon URL must use http or https');
	}
	const outboundHTTP = getDesktopOutboundHTTP();
	let target = initialTarget;
	let redirectsRemaining = options.redirectsRemaining;
	for (;;) {
		const message = await outboundHTTP.get({
			context: NOTIFICATION_ICON_CONTEXT,
			timeoutMs: options.timeoutMs,
			url: target,
		});
		const statusCode = message.status;
		if (NOTIFICATION_ICON_REDIRECT_STATUS_CODES.has(statusCode)) {
			message.message.destroy();
			if (redirectsRemaining <= 0) {
				throw new Error('Notification icon download exceeded redirect limit');
			}
			const next = parseDesktopRedirectTarget(message.url, message.headers.location);
			if (next == null) {
				throw new Error(`Notification icon redirect target is unusable (${statusCode})`);
			}
			target = next;
			redirectsRemaining -= 1;
			continue;
		}
		if (statusCode !== 200) {
			message.message.destroy();
			throw new Error(`Notification icon download failed with HTTP ${statusCode}`);
		}
		return await readBoundedMessage({
			declaredBytes: readMessageContentLength(message, NOTIFICATION_ICON_CONTEXT),
			description: NOTIFICATION_ICON_CONTEXT,
			maxBytes: options.maxBytes,
			maxChunks: NOTIFICATION_ICON_MAX_CHUNKS,
			message: message.message,
		});
	}
}
