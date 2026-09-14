import {createReadStream, createWriteStream, ReadStream} from 'node:fs';
import {lstat, opendir, readlink} from 'node:fs/promises';
import path from 'node:path';
import type {Readable} from 'node:stream';
import {finished, pipeline} from 'node:stream/promises';
import archiver, {type Archiver, type EntryData} from 'archiver';

export interface ArchiveEntryWriter {
	append(source: Readable | Buffer | string, data: EntryData): Promise<void>;
}

interface ArchiveFileWriter extends ArchiveEntryWriter {
	directory(directoryPath: string): Promise<void>;
}

interface ArchiveSymlinkEntry extends EntryData {
	type: 'symlink';
	linkname: string;
}

interface PendingArchiveEntry {
	resolve(): void;
	reject(error: unknown): void;
}

export async function writeZipArchive(
	filePath: string,
	produce: (archive: ArchiveFileWriter) => void | Promise<void>,
): Promise<void> {
	const archive: Archiver = archiver('zip', {zlib: {level: 6}});
	const output = createWriteStream(filePath);
	const outputClosed = new Promise<void>((resolve) => output.once('close', resolve));
	const archiveClosed = new Promise<void>((resolve) => archive.once('close', resolve));
	const inputs = new Map<Readable, Promise<void>>();
	const entries = new Map<EntryData, PendingArchiveEntry>();
	let failure: {error: unknown} | undefined;

	function fail(error: unknown): void {
		if (failure) return;
		failure = {error};
		for (const entry of entries.values()) entry.reject(error);
		entries.clear();
		const streamError = error instanceof Error ? error : new Error('Archive creation failed', {cause: error});
		for (const source of inputs.keys()) source.destroy(streamError);
		archive.abort();
		archive.destroy(streamError);
		output.destroy(streamError);
	}

	function requireWritable(): void {
		if (failure) throw failure.error;
	}

	async function append(source: Readable | Buffer | string, data: EntryData): Promise<void> {
		let completion = Promise.resolve();
		if (typeof source !== 'string' && !Buffer.isBuffer(source)) {
			const closed =
				source instanceof ReadStream && !source.closed
					? new Promise<void>((resolve) => source.once('close', resolve))
					: Promise.resolve();
			source.on('error', fail);
			completion = finished(source, {cleanup: true, writable: false})
				.catch(fail)
				.then(async () => {
					try {
						await source[Symbol.asyncDispose]();
					} catch (error) {
						fail(error);
					}
					await closed;
					inputs.delete(source);
					source.off('error', fail);
				});
			inputs.set(source, completion);
			if (failure) source.destroy(new Error('Archive creation failed', {cause: failure.error}));
		}
		requireWritable();
		const entry = {...data};
		const processed = new Promise<void>((resolve, reject) => entries.set(entry, {resolve, reject}));
		const completed = Promise.all([processed, completion]);
		try {
			archive.append(source, entry);
		} catch (error) {
			fail(error);
		}
		await completed;
		requireWritable();
	}

	async function appendDirectory(directoryPath: string, prefix = ''): Promise<void> {
		requireWritable();
		const directory = await opendir(directoryPath);
		for await (const entry of directory) {
			requireWritable();
			const entryPath = path.join(directoryPath, entry.name);
			const name = path.posix.join(prefix, entry.name);
			const stats = await lstat(entryPath);
			if (stats.isDirectory()) {
				await append(Buffer.alloc(0), {name: `${name}/`, stats});
				await appendDirectory(entryPath, name);
			} else if (stats.isFile()) {
				await append(createReadStream(entryPath), {name, stats});
			} else if (stats.isSymbolicLink()) {
				const target = await readlink(entryPath);
				const data: ArchiveSymlinkEntry = {
					name,
					stats,
					type: 'symlink',
					linkname: path.relative(directoryPath, path.resolve(directoryPath, target)),
				};
				await append(Buffer.alloc(0), data);
			} else {
				throw new Error(`Unsupported archive entry: ${entryPath}`);
			}
		}
	}

	archive.on('warning', fail);
	archive.on('error', fail);
	archive.on('entry', (entry) => {
		if (failure) return;
		const pending = entries.get(entry);
		if (!pending) {
			fail(new Error('Archive completed an unknown entry'));
			return;
		}
		entries.delete(entry);
		pending.resolve();
	});
	output.on('error', fail);
	const written = pipeline(archive, output);
	void written.catch(fail);
	try {
		await produce({
			append,
			directory: (directoryPath) => appendDirectory(directoryPath),
		});
		requireWritable();
		await Promise.all([archive.finalize(), written]);
	} catch (error) {
		fail(error);
	} finally {
		archive.destroy();
		output.destroy();
		for (const source of inputs.keys()) source.destroy();
		await Promise.all([archiveClosed, outputClosed, written.catch(fail), ...inputs.values()]);
	}
	if (failure) throw failure.error;
}
