// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomBytes} from 'node:crypto';
import fs, {type FileHandle} from 'node:fs/promises';
import path from 'node:path';

export const WINDOWS_TRANSIENT_FS_ERROR_CODES: ReadonlySet<string> = new Set(['EPERM', 'EACCES', 'EBUSY']);

export const WINDOWS_TRANSIENT_FS_RETRY_DELAYS_MS: ReadonlyArray<number> = [25, 50, 100, 200, 400, 800];

export class ModuleStoreError extends Error {
	public constructor(message: string, options?: ErrorOptions) {
		super(message, options);
		this.name = 'ModuleStoreError';
	}
}

export class ModuleStoreIOError extends ModuleStoreError {
	public constructor(message: string, options?: ErrorOptions) {
		super(message, options);
		this.name = 'ModuleStoreIOError';
	}
}

export function isWindowsTransientFsError(error: unknown): boolean {
	return (
		process.platform === 'win32' &&
		typeof error === 'object' &&
		error !== null &&
		'code' in error &&
		typeof error.code === 'string' &&
		WINDOWS_TRANSIENT_FS_ERROR_CODES.has(error.code)
	);
}

export async function renameWithRetry(source: string, target: string): Promise<void> {
	for (let attempt = 0; ; attempt += 1) {
		try {
			await fs.rename(source, target);
			return;
		} catch (error) {
			const delay = WINDOWS_TRANSIENT_FS_RETRY_DELAYS_MS[attempt];
			if (delay === undefined || !isWindowsTransientFsError(error)) {
				throw error;
			}
			await new Promise((resolve) => setTimeout(resolve, delay));
		}
	}
}

export async function fsyncDirectory(directory: string): Promise<void> {
	if (process.platform === 'win32') {
		return;
	}
	let handle: FileHandle;
	try {
		handle = await fs.open(directory, 'r');
	} catch (error) {
		throw new ModuleStoreIOError(`failed to open ${directory} for fsync`, {cause: error});
	}
	try {
		await handle.sync();
	} catch (error) {
		throw new ModuleStoreIOError(`failed to fsync ${directory}`, {cause: error});
	} finally {
		await handle.close();
	}
}

export async function writeFileAtomically(target: string, contents: Uint8Array | string, mode: number): Promise<void> {
	const directory = path.dirname(target);
	const temporary = path.join(directory, `${path.basename(target)}.${randomBytes(8).toString('hex')}.tmp`);
	let handle: FileHandle;
	try {
		handle = await fs.open(temporary, 'wx', mode);
	} catch (error) {
		throw new ModuleStoreIOError(`failed to create ${temporary}`, {cause: error});
	}
	try {
		await handle.writeFile(contents, 'utf8');
		await handle.sync();
	} catch (error) {
		await handle.close();
		await fs.rm(temporary, {force: true});
		throw new ModuleStoreIOError(`failed to write ${temporary}`, {cause: error});
	}
	await handle.close();
	try {
		await renameWithRetry(temporary, target);
	} catch (error) {
		await fs.rm(temporary, {force: true});
		throw new ModuleStoreIOError(`failed to publish ${target}`, {cause: error});
	}
	await fsyncDirectory(directory);
}
