// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {
	accessSync,
	chmodSync,
	constants,
	createReadStream,
	existsSync,
	mkdtempSync,
	readdirSync,
	renameSync,
	rmSync,
	statfsSync,
	statSync,
} from 'node:fs';
import {open} from 'node:fs/promises';
import {basename, dirname, join, resolve, sep} from 'node:path';

type AppImageUnavailableReason =
	| 'appimage-path-missing'
	| 'appimage-path-gone'
	| 'appimage-path-not-a-file'
	| 'directory-not-writable';

export type AppImageTarget = {
	installedPath: string;
	directory: string;
};

type AppImageTargetResult = {ok: true; target: AppImageTarget} | {ok: false; reason: AppImageUnavailableReason};

export type StagedAppImageUpdate = {
	stagedPath: string;
	stagingDirectory: string;
};

type AppImageDownloadProgress = {
	transferred: number;
	total: number;
};

const STAGING_DIRECTORY_PREFIX = '.fluxer-update-';
const APPDIR_ENTRYPOINT = 'AppRun';
const APPIMAGE_MODE = 0o755;
const SHA256_PATTERN = /^[0-9a-f]{64}$/;
const STAGING_FREE_SPACE_MARGIN_BYTES = 16 * 1024 * 1024;
const NON_RETRYABLE_STAGING_ERRNOS = new Set([
	'ENOSPC',
	'EDQUOT',
	'EROFS',
	'EACCES',
	'EPERM',
	'EIO',
	'EMFILE',
	'EINVAL',
	'ENOSYS',
]);

export class AppImageChecksumError extends Error {
	constructor(expected: string, actual: string) {
		super(`Update checksum mismatch: expected ${expected}, downloaded ${actual}`);
		this.name = 'AppImageChecksumError';
	}
}

export class AppImageStagingError extends Error {
	readonly code: string;

	constructor(code: string, message: string) {
		super(message);
		this.name = 'AppImageStagingError';
		this.code = code;
	}
}

function errnoCode(error: unknown): string | null {
	if (error !== null && typeof error === 'object' && 'code' in error) {
		const code = (error as {code?: unknown}).code;
		if (typeof code === 'string') {
			return code;
		}
	}
	return null;
}

function asStagingError(error: unknown): unknown {
	const code = errnoCode(error);
	if (code === null || !NON_RETRYABLE_STAGING_ERRNOS.has(code)) {
		return error;
	}
	return new AppImageStagingError(code, error instanceof Error ? error.message : String(error));
}

function stagingDirectoryPrefix(installedPath: string): string {
	const scope = createHash('sha256').update(installedPath).digest('hex').slice(0, 8);
	return `${STAGING_DIRECTORY_PREFIX}${scope}-`;
}

export function isRunningFromAppImage(env: NodeJS.ProcessEnv = process.env, execPath = process.execPath): boolean {
	const appDir = env.APPDIR;
	if (typeof appDir !== 'string' || appDir.trim().length === 0) {
		return false;
	}
	const mounted = resolve(appDir.trim());
	const running = resolve(execPath);
	if (running !== mounted && !running.startsWith(`${mounted}${sep}`)) {
		return false;
	}
	return existsSync(join(mounted, APPDIR_ENTRYPOINT));
}

export function resolveAppImageTarget(appImagePath = process.env.APPIMAGE): AppImageTargetResult {
	if (typeof appImagePath !== 'string' || appImagePath.trim().length === 0) {
		return {ok: false, reason: 'appimage-path-missing'};
	}
	let stats: ReturnType<typeof statSync>;
	try {
		stats = statSync(appImagePath);
	} catch {
		return {ok: false, reason: 'appimage-path-gone'};
	}
	if (!stats.isFile()) {
		return {ok: false, reason: 'appimage-path-not-a-file'};
	}
	const directory = dirname(appImagePath);
	try {
		accessSync(directory, constants.W_OK | constants.X_OK);
	} catch {
		return {ok: false, reason: 'directory-not-writable'};
	}
	return {ok: true, target: {installedPath: appImagePath, directory}};
}

function assertRoomToStage(directory: string, total: number): void {
	if (total <= 0) {
		return;
	}
	let available: number;
	try {
		const stats = statfsSync(directory);
		available = Number(stats.bavail) * Number(stats.bsize);
	} catch {
		return;
	}
	if (!Number.isFinite(available) || available >= total + STAGING_FREE_SPACE_MARGIN_BYTES) {
		return;
	}
	throw new AppImageStagingError(
		'ENOSPC',
		`Not enough free space in ${directory} to stage the update: ${available} bytes available, ${total} bytes needed.`,
	);
}

async function digestFile(path: string): Promise<string> {
	const hash = createHash('sha256');
	const stream = createReadStream(path);
	for await (const chunk of stream) {
		hash.update(chunk as Buffer);
	}
	return hash.digest('hex');
}

export async function stageAppImageUpdate(options: {
	target: AppImageTarget;
	url: string;
	expectedSha256: string;
	onProgress?: (progress: AppImageDownloadProgress) => void;
	fetchImpl?: typeof fetch;
}): Promise<StagedAppImageUpdate> {
	const expected = options.expectedSha256.trim().toLowerCase();
	if (!SHA256_PATTERN.test(expected)) {
		throw new Error('Update checksum is missing or malformed.');
	}
	const fetchImpl = options.fetchImpl ?? fetch;
	const response = await fetchImpl(options.url, {cache: 'no-store', redirect: 'follow'});
	if (!response.ok || response.body == null) {
		throw new Error(`Update download failed: ${response.status}`);
	}
	const declaredTotal = Number.parseInt(response.headers.get('content-length') ?? '', 10);
	const total = Number.isFinite(declaredTotal) && declaredTotal > 0 ? declaredTotal : 0;
	try {
		assertRoomToStage(options.target.directory, total);
	} catch (error) {
		await response.body.cancel().catch(() => {});
		throw error;
	}
	let stagingDirectory: string;
	try {
		stagingDirectory = mkdtempSync(
			join(options.target.directory, stagingDirectoryPrefix(options.target.installedPath)),
		);
	} catch (error) {
		await response.body.cancel().catch(() => {});
		throw asStagingError(error);
	}
	const staged: StagedAppImageUpdate = {
		stagedPath: join(stagingDirectory, basename(options.target.installedPath)),
		stagingDirectory,
	};
	try {
		const handle = await open(staged.stagedPath, 'wx', APPIMAGE_MODE);
		const reader = response.body.getReader();
		let transferred = 0;
		try {
			for (;;) {
				const {done, value} = await reader.read();
				if (done) {
					break;
				}
				let written = 0;
				while (written < value.byteLength) {
					const result = await handle.write(value, written, value.byteLength - written);
					if (result.bytesWritten <= 0) {
						throw new AppImageStagingError('ENOSPC', `Staging write stalled after ${transferred} bytes.`);
					}
					written += result.bytesWritten;
					transferred += result.bytesWritten;
				}
				options.onProgress?.({transferred, total});
			}
			if (total > 0 && transferred < total) {
				throw new Error(`Update download ended after ${transferred} of ${total} bytes.`);
			}
			await handle.datasync();
		} catch (error) {
			await reader.cancel().catch(() => {});
			throw error;
		} finally {
			await handle.close();
		}
		const digest = await digestFile(staged.stagedPath);
		if (digest !== expected) {
			throw new AppImageChecksumError(expected, digest);
		}
		chmodSync(staged.stagedPath, APPIMAGE_MODE);
		return staged;
	} catch (error) {
		discardStagedAppImageUpdate(staged);
		throw asStagingError(error);
	}
}

export function applyStagedAppImageUpdate(target: AppImageTarget, staged: StagedAppImageUpdate): void {
	const resolved = resolveAppImageTarget(target.installedPath);
	if (!resolved.ok) {
		discardStagedAppImageUpdate(staged);
		throw new Error(`AppImage cannot be replaced in place: ${resolved.reason}`);
	}
	renameSync(staged.stagedPath, target.installedPath);
	discardStagedAppImageUpdate(staged);
}

export function discardStagedAppImageUpdate(staged: StagedAppImageUpdate): void {
	try {
		rmSync(staged.stagingDirectory, {recursive: true, force: true});
	} catch {}
}

export function sweepAbandonedAppImageUpdates(target: AppImageTarget): Array<string> {
	const prefix = stagingDirectoryPrefix(target.installedPath);
	const reclaimed: Array<string> = [];
	for (const entry of readdirSync(target.directory)) {
		if (!entry.startsWith(prefix)) {
			continue;
		}
		const abandoned = join(target.directory, entry);
		rmSync(abandoned, {recursive: true, force: true});
		reclaimed.push(abandoned);
	}
	return reclaimed;
}
