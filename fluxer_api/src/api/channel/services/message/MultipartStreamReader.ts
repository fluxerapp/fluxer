// SPDX-License-Identifier: AGPL-3.0-or-later

import {Readable} from 'node:stream';
import type {ReadableStream as NodeReadableStream} from 'node:stream/web';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import Busboy, {type BusboyFileStream} from '@fastify/busboy';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {FileSizeTooLargeError} from '@fluxer/errors/src/domains/core/FileSizeTooLargeError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {RequestBodyTooLargeError} from '@fluxer/errors/src/domains/core/RequestBodyTooLargeError';
import type {Context} from 'hono';
import {ms} from 'itty-time';

const MAX_MULTIPART_FIELDS = 64;
const DISCARD_MAX_BYTES = 64 * 1024 * 1024;
const DISCARD_TIMEOUT_MS = ms('30 seconds');

export interface MultipartFilePart {
	fieldName: string;
	filename: string;
	stream: BusboyFileStream;
}

interface ReadMultipartStreamOptions {
	maxFiles: number;
	maxFileSize: number;
	maxFieldBytes: number;
	onField: (name: string, value: string) => void;
	onFile: (part: MultipartFilePart) => Promise<void>;
}

function multipartParseError(): InputValidationError {
	return InputValidationError.fromCode('multipart_form', ValidationErrorCodes.FAILED_TO_PARSE_MULTIPART_FORM_DATA);
}

function multipartBodyBudget({
	maxFiles,
	maxFileSize,
	maxFieldBytes,
}: Pick<ReadMultipartStreamOptions, 'maxFiles' | 'maxFileSize' | 'maxFieldBytes'>): number {
	return Math.max(0, maxFiles) * Math.max(0, maxFileSize) + maxFieldBytes;
}

function discardRest(source: Readable, remaining: number | null): Promise<void> {
	if (source.readableEnded || source.destroyed || (remaining !== null && remaining > DISCARD_MAX_BYTES)) {
		source.destroy();
		return Promise.resolve();
	}
	return new Promise((resolve) => {
		let discarded = 0;
		const done = () => {
			clearTimeout(timer);
			source.off('data', onData);
			source.off('end', done);
			source.off('error', done);
			resolve();
		};
		const stop = () => {
			done();
			source.destroy();
		};
		const onData = (chunk: Buffer) => {
			discarded += chunk.length;
			if (discarded > DISCARD_MAX_BYTES) stop();
		};
		const timer = setTimeout(stop, DISCARD_TIMEOUT_MS);
		source.on('data', onData);
		source.once('end', done);
		source.once('error', done);
		source.resume();
	});
}

export async function readMultipartStream(ctx: Context<HonoEnv>, options: ReadMultipartStreamOptions): Promise<void> {
	const {maxFiles, maxFileSize, maxFieldBytes, onField, onFile} = options;
	const budget = multipartBodyBudget(options);
	const declaredLength = Number(ctx.req.header('content-length'));
	const knownLength = Number.isFinite(declaredLength) ? declaredLength : null;
	const body = ctx.req.raw.body;
	if (!body) {
		return;
	}
	const source = Readable.fromWeb(body as NodeReadableStream<Uint8Array>);
	if (knownLength !== null && knownLength > budget) {
		await discardRest(source, knownLength);
		throw new RequestBodyTooLargeError(budget);
	}
	let parser: ReturnType<typeof Busboy>;
	try {
		parser = Busboy({
			headers: {'content-type': ctx.req.header('content-type') ?? ''},
			isPartAFile: (_fieldName, _contentType, filename) => filename !== undefined,
			limits: {
				fileSize: maxFileSize,
				fields: MAX_MULTIPART_FIELDS,
				fieldSize: maxFieldBytes,
				parts: Math.max(0, maxFiles) + MAX_MULTIPART_FIELDS,
			},
		});
	} catch {
		await discardRest(source, knownLength);
		throw multipartParseError();
	}
	await new Promise<void>((resolve, reject) => {
		let settled = false;
		let bytesRead = 0;
		let fieldBytes = 0;
		let pending: Promise<void> = Promise.resolve();
		let currentFile: BusboyFileStream | null = null;
		const fail = (error: unknown) => {
			if (settled) return;
			settled = true;
			source.unpipe(parser);
			currentFile?.destroy();
			const remaining = knownLength === null ? null : knownLength - bytesRead;
			Promise.all([pending.catch(() => undefined), discardRest(source, remaining)]).then(() => reject(error));
		};
		source.on('data', (chunk: Buffer) => {
			bytesRead += chunk.length;
			if (bytesRead > budget) fail(new RequestBodyTooLargeError(budget));
		});
		parser.on('field', (name, value, _nameTruncated, valueTruncated) => {
			if (settled) return;
			try {
				if (name === undefined) throw multipartParseError();
				fieldBytes += Buffer.byteLength(name) + Buffer.byteLength(value);
				if (valueTruncated || fieldBytes > maxFieldBytes) throw new RequestBodyTooLargeError(maxFieldBytes);
				onField(name, value);
			} catch (error) {
				fail(error);
			}
		});
		parser.on('file', (fieldName, stream, filename) => {
			if (settled || fieldName === undefined) {
				stream.resume();
				fail(multipartParseError());
				return;
			}
			stream.once('limit', () => fail(new FileSizeTooLargeError(maxFileSize)));
			pending = pending.then(async () => {
				if (settled) {
					stream.resume();
					return;
				}
				currentFile = stream;
				try {
					await onFile({fieldName, filename, stream});
				} finally {
					currentFile = null;
				}
				if (stream.truncated) {
					throw new FileSizeTooLargeError(maxFileSize);
				}
			});
			pending.catch(fail);
		});
		parser.on('fieldsLimit', () => fail(multipartParseError()));
		parser.on('partsLimit', () => fail(multipartParseError()));
		parser.on('error', () => fail(multipartParseError()));
		source.on('error', () => fail(multipartParseError()));
		parser.on('finish', () => {
			pending.then(() => {
				if (settled) return;
				settled = true;
				resolve();
			}, fail);
		});
		source.pipe(parser);
	});
}
