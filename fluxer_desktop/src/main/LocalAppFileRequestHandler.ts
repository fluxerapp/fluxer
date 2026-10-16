// SPDX-License-Identifier: AGPL-3.0-or-later

import path from 'node:path';
import {buildLocalAppCSP} from '@electron/main/LocalAppCSP';
import {
	type DesktopLocalAppFiles,
	type LocalAppBufferedResponseFile,
	type LocalAppFile,
	LocalAppFileResolution,
	type LocalAppResolvedFile,
} from '@electron/main/LocalAppFileResolver';
import {randomLocalAppCSPNonce, rewriteLocalAppIndexHTML} from '@electron/main/LocalAppIndexHTML';
import {
	rewriteLocalAppStaticMetadata,
	shouldRewriteLocalAppStaticMetadata,
} from '@electron/main/LocalAppStaticMetadata';
import {t} from '@electron/main/MainI18n';
import {HttpStatus, MimeType} from '@fluxer/constants/src/HttpConstants';

const LOCAL_APP_REWRITTEN_FILE_MAX_BYTES = 4 * 1024 * 1024;

const GET_METHOD = 'GET';
const HEAD_METHOD = 'HEAD';
const TEXT_CONTENT_TYPE = `${MimeType.PLAIN}; charset=utf-8`;
const TEXT_RESPONSE_HEADERS: Readonly<Record<string, string>> = Object.freeze({'Content-Type': TEXT_CONTENT_TYPE});
const METHOD_NOT_ALLOWED_HEADERS: Readonly<Record<string, string>> = Object.freeze({
	Allow: `${GET_METHOD}, ${HEAD_METHOD}`,
	'Content-Type': TEXT_CONTENT_TYPE,
});

export interface LocalAppIndexContext {
	readonly prebootTheme: string | null;
	readonly cleartextInstanceOrigins?: ReadonlyArray<string>;
}

interface LocalAppFileRequestLogger {
	readonly warn: (...args: Array<unknown>) => void;
	readonly error: (...args: Array<unknown>) => void;
}

interface DesktopLocalAppFileRequestHandlerDependencies {
	readonly files: DesktopLocalAppFiles;
	readonly indexContext: () => LocalAppIndexContext;
	readonly logger: LocalAppFileRequestLogger;
}

interface ResolvedLocalAppFileRequest {
	readonly request: Request;
	readonly resolved: LocalAppFile;
	readonly signal: AbortSignal;
}

interface RewrittenLocalAppFileRequest {
	readonly request: Request;
	readonly headers: Headers;
	readonly file: LocalAppBufferedResponseFile;
}

export class DesktopLocalAppFileRequestHandler {
	private readonly files: DesktopLocalAppFiles;
	private readonly indexContext: () => LocalAppIndexContext;
	private readonly logger: LocalAppFileRequestLogger;

	constructor(dependencies: DesktopLocalAppFileRequestHandlerDependencies) {
		this.files = dependencies.files;
		this.indexContext = dependencies.indexContext;
		this.logger = dependencies.logger;
	}

	async handle(request: Request, signal: AbortSignal): Promise<Response> {
		signal.throwIfAborted();
		if (request.method !== GET_METHOD && request.method !== HEAD_METHOD) {
			return new Response('Method not allowed', {
				status: HttpStatus.METHOD_NOT_ALLOWED,
				headers: METHOD_NOT_ALLOWED_HEADERS,
			});
		}
		let resolved: LocalAppResolvedFile;
		try {
			resolved = await this.files.resolve(request.url);
		} catch (error) {
			this.logger.error('Failed to resolve a local app renderer request', error);
			return new Response(t('desktop.localApp.rendererUnavailable'), {
				status: HttpStatus.SERVICE_UNAVAILABLE,
				headers: TEXT_RESPONSE_HEADERS,
			});
		}
		signal.throwIfAborted();
		if (resolved.type === LocalAppFileResolution.BLOCKED) {
			this.logger.warn('Rejected a local app renderer request', resolved.reason);
			return notFoundResponse();
		}
		if (resolved.type === LocalAppFileResolution.NOT_FOUND) {
			return notFoundResponse();
		}
		try {
			return await this.serveFile({request, resolved, signal});
		} catch (error) {
			if (signal.aborted) {
				throw error;
			}
			this.logger.error('Failed to read a local app renderer file', error);
			return new Response(t('desktop.localApp.rendererFileUnavailable'), {
				status: HttpStatus.INTERNAL_SERVER_ERROR,
				headers: TEXT_RESPONSE_HEADERS,
			});
		}
	}

	private async serveFile({request, resolved, signal}: ResolvedLocalAppFileRequest): Promise<Response> {
		const fileName = path.basename(resolved.filePath);
		const staticMetadata = shouldRewriteLocalAppStaticMetadata(fileName);
		const index = this.files.isIndexFile(resolved.filePath);
		const headers = new Headers({
			'Cache-Control': resolved.cacheControl,
			'Content-Type': resolved.contentType,
		});
		if (!staticMetadata && !index) {
			headers.set('Content-Length', String(resolved.size));
			if (request.method === HEAD_METHOD) {
				return new Response(null, {status: HttpStatus.OK, headers});
			}
			const body = await this.files.openStream(resolved.filePath, resolved.size, signal);
			return new Response(body, {status: HttpStatus.OK, headers});
		}
		const file = await this.files.readForResponse(resolved.filePath, LOCAL_APP_REWRITTEN_FILE_MAX_BYTES, signal);
		try {
			if (index) {
				return this.buildIndexResponse({request, headers, file});
			}
			return buildRewrittenResponse(
				request,
				headers,
				file,
				rewriteLocalAppStaticMetadata(fileName, file.contents.toString('utf8')),
			);
		} finally {
			file.release();
		}
	}

	private buildIndexResponse({request, headers, file}: RewrittenLocalAppFileRequest): Response {
		const context = this.indexContext();
		const nonce = randomLocalAppCSPNonce();
		const html = rewriteLocalAppIndexHTML({
			html: file.contents.toString('utf8'),
			nonce,
			prebootTheme: context.prebootTheme,
		});
		headers.set(
			'Content-Security-Policy',
			buildLocalAppCSP({nonce, cleartextInstanceOrigins: context.cleartextInstanceOrigins}),
		);
		return buildRewrittenResponse(request, headers, file, html);
	}
}

function buildRewrittenResponse(
	request: Request,
	headers: Headers,
	file: LocalAppBufferedResponseFile,
	body: string,
): Response {
	const prepared = file.prepareResponseBody(body, request.method !== HEAD_METHOD);
	headers.set('Content-Length', String(prepared.byteLength));
	return new Response(prepared.body, {status: HttpStatus.OK, headers});
}

function notFoundResponse(): Response {
	return new Response(t('desktop.localApp.notFound'), {status: HttpStatus.NOT_FOUND, headers: TEXT_RESPONSE_HEADERS});
}
