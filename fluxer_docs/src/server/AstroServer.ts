// SPDX-License-Identifier: AGPL-3.0-or-later

import {lstat} from 'node:fs/promises';
import {createServer, type IncomingMessage, type Server, type ServerResponse} from 'node:http';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import send from 'send';
import {type AstroServerShutdown, AstroServerTermination} from './AstroServerTermination';
import {
	CanonicalNetworkProtocol,
	getNodeErrorCode,
	HTTPHeader,
	HTTPMethod,
	HTTPStatusCode,
	isCanonicalHTTPNetworkProtocol,
	MIMEType,
	NodeErrorCode,
	OUTBOUND_USER_AGENT,
} from './HTTPConstants';
import {siteSecurityHeaders} from './SecurityHeaders';

const HEALTH_PATH = '/_health';
const HEALTH_RESPONSE_BODY = JSON.stringify({status: 'ok'});
const NOT_READY_RESPONSE_BODY = JSON.stringify({status: 'not_ready'});
const WELL_KNOWN_PATH_PREFIX = '.well-known/';
const IMMUTABLE_ASSET_PATH_PREFIX = '_astro/';
const PARENT_SEGMENT_PATTERN = /(?:^|[\\/])\.\.(?:[\\/]|$)/u;
const IMMUTABLE_ASSET_CACHE_CONTROL = 'public, max-age=31536000, immutable';
const LOCAL_REQUEST_URL_BASE = `${CanonicalNetworkProtocol.HTTP}//localhost`;
const ASTRO_SERVER_SHUTDOWN_TIMEOUT_MS = 15_000;
const ASTRO_SERVER_NAME = 'Fluxer Astro server';
const ASTRO_SERVER_MAX_CONNECTIONS = 256;
const ASTRO_SERVER_MAX_HEADERS = 64;
const ASTRO_SERVER_MAX_HEADER_BYTES = 16 * 1024;
const ASTRO_SERVER_MAX_REQUESTS_PER_SOCKET = 100;
const ASTRO_SERVER_MAX_ACTIVE_REQUESTS = ASTRO_SERVER_MAX_CONNECTIONS;
const ASTRO_SERVER_REQUEST_TARGET_MAX_BYTES = 8 * 1024;
const ASTRO_SERVER_HEADERS_TIMEOUT_MS = 10_000;
const ASTRO_SERVER_REQUEST_TIMEOUT_MS = 15_000;
const ASTRO_SERVER_KEEP_ALIVE_TIMEOUT_MS = 5_000;
const ASTRO_SERVER_SOCKET_TIMEOUT_MS = 30_000;
const ALLOWED_SITE_METHODS = `${HTTPMethod.GET}, ${HTTPMethod.HEAD}`;
const NO_STORE_CACHE_CONTROL = 'no-store';
const TEXT_CONTENT_TYPE = `${MIMEType.PLAIN}; charset=utf-8`;

const AstroServerStartupPhase = Object.freeze({
	ENTRYPOINT: 'entrypoint load',
	INITIALIZE: 'initialization',
	LISTEN: 'listener bind',
} as const);

type AstroServerStartupPhase = (typeof AstroServerStartupPhase)[keyof typeof AstroServerStartupPhase];

const StaticDotfilePolicy = Object.freeze({
	ALLOW: 'allow',
	DENY: 'deny',
} as const);

type StaticDotfilePolicy = (typeof StaticDotfilePolicy)[keyof typeof StaticDotfilePolicy];

type AstroRequestNext = (error: unknown) => void;

type AstroRequestHandler = (
	request: IncomingMessage,
	response: ServerResponse,
	next: AstroRequestNext,
	locals: object,
) => void | Promise<void>;

interface AstroServerEntrypoint {
	readonly handler: AstroRequestHandler;
}

interface StaticRequestPath {
	readonly pathname: string;
	readonly search: string;
	readonly staticPath: string;
}

interface ResolvedStaticFile {
	readonly isDirectory: boolean;
	readonly normalizedPath: string;
}

interface ResolvedPublicEndpoint {
	readonly basePath: string;
	readonly secureOrigin: boolean;
}

type AstroServerInitialize = () => Promise<void>;

type AstroServerReadiness = () => boolean;

type AstroServerRequestLocals = () => object;

interface AstroServerOptions {
	readonly entrypoint: URL;
	readonly initialize: AstroServerInitialize | null;
	readonly listenHost: string;
	readonly listenPort: number;
	readonly publicEndpoint: string;
	readonly readiness: AstroServerReadiness | null;
	readonly requestLocals: AstroServerRequestLocals | null;
	readonly shutdown: AstroServerShutdown | null;
}

interface AstroServerHealthcheckOptions {
	readonly listenHost: string;
	readonly listenPort: number;
	readonly timeoutMs: number;
}

function isAstroServerEntrypoint(value: unknown): value is AstroServerEntrypoint {
	if (typeof value !== 'object') {
		return false;
	}
	if (value == null) {
		return false;
	}
	if (!('handler' in value)) {
		return false;
	}
	return typeof value.handler === 'function';
}

class InvalidAstroServerListenPortError extends Error {
	constructor() {
		super('Astro server listen port must be an integer between 1 and 65535');
		this.name = 'InvalidAstroServerListenPortError';
	}
}

class AstroServerStartupAbandonedError extends Error {
	public readonly phase: AstroServerStartupPhase;

	constructor(phase: AstroServerStartupPhase) {
		super(`${ASTRO_SERVER_NAME} ran out of pending work during ${phase} and would exit without serving`);
		this.name = 'AstroServerStartupAbandonedError';
		this.phase = phase;
	}
}

class InvalidAstroServerHealthcheckTimeoutError extends Error {
	constructor() {
		super('Astro server healthcheck timeout must be a positive integer');
		this.name = 'InvalidAstroServerHealthcheckTimeoutError';
	}
}

class InvalidAstroServerPublicEndpointError extends Error {
	constructor() {
		super('Astro server public endpoint must not contain credentials, a query, or a fragment');
		this.name = 'InvalidAstroServerPublicEndpointError';
	}
}

function validateListener(listenHost: string, listenPort: number): void {
	if (listenHost.length === 0) {
		throw new Error('Astro server listen host must not be empty');
	}
	if (!Number.isInteger(listenPort)) {
		throw new InvalidAstroServerListenPortError();
	}
	if (listenPort < 1) {
		throw new InvalidAstroServerListenPortError();
	}
	if (listenPort > 65_535) {
		throw new InvalidAstroServerListenPortError();
	}
}

function resolvePublicEndpoint(publicEndpoint: string): ResolvedPublicEndpoint {
	const endpoint = new URL(publicEndpoint);
	if (!isCanonicalHTTPNetworkProtocol(endpoint.protocol)) {
		throw new Error('Astro server public endpoint must use HTTP or HTTPS');
	}
	if (endpoint.username.length > 0) {
		throw new InvalidAstroServerPublicEndpointError();
	}
	if (endpoint.password.length > 0) {
		throw new InvalidAstroServerPublicEndpointError();
	}
	if (endpoint.search.length > 0) {
		throw new InvalidAstroServerPublicEndpointError();
	}
	if (endpoint.hash.length > 0) {
		throw new InvalidAstroServerPublicEndpointError();
	}
	const secureOrigin = endpoint.protocol === CanonicalNetworkProtocol.HTTPS;
	if (endpoint.pathname === '/') {
		return {basePath: '', secureOrigin};
	}
	return {basePath: endpoint.pathname.replace(/\/+$/u, ''), secureOrigin};
}

function healthcheckHost(listenHost: string): string {
	if (listenHost === '0.0.0.0') {
		return '127.0.0.1';
	}
	if (listenHost === '::') {
		return '[::1]';
	}
	if (listenHost === '[::]') {
		return '[::1]';
	}
	if (listenHost.includes(':') && !listenHost.startsWith('[')) {
		return `[${listenHost}]`;
	}
	return listenHost;
}

function pathWithinBase(pathname: string, basePath: string): string | null {
	if (basePath.length === 0) {
		return pathname;
	}
	if (pathname === basePath) {
		return '/';
	}
	if (pathname.startsWith(`${basePath}/`)) {
		return pathname.slice(basePath.length);
	}
	return null;
}

function requestURL(request: IncomingMessage): URL | null {
	const requestTarget = request.url;
	if (requestTarget == null) {
		return null;
	}
	if (!requestTarget.startsWith('/')) {
		return null;
	}
	if (requestTarget.includes('#')) {
		return null;
	}
	if (Buffer.byteLength(requestTarget) > ASTRO_SERVER_REQUEST_TARGET_MAX_BYTES) {
		return null;
	}
	try {
		return new URL(`${LOCAL_REQUEST_URL_BASE}${requestTarget}`);
	} catch {
		return null;
	}
}

function resolveStaticRequestPath(
	request: IncomingMessage,
	requestURL: URL,
	basePath: string,
): StaticRequestPath | null {
	if (request.method !== HTTPMethod.GET) {
		if (request.method !== HTTPMethod.HEAD) {
			return null;
		}
	}
	const staticPath = pathWithinBase(requestURL.pathname, basePath);
	if (staticPath == null) {
		return null;
	}
	return {
		pathname: requestURL.pathname,
		search: requestURL.search,
		staticPath,
	};
}

function normalizeServedPath(decodedPath: string): string {
	if (decodedPath.length === 0) {
		return decodedPath;
	}
	return path.normalize(`.${path.sep}${decodedPath}`);
}

async function resolveStaticFile(clientDirectory: string, staticPath: string): Promise<ResolvedStaticFile | null> {
	let decodedPath: string;
	try {
		decodedPath = decodeURIComponent(staticPath);
	} catch {
		return null;
	}
	if (decodedPath.includes('\0')) {
		return null;
	}
	const normalizedPath = normalizeServedPath(decodedPath);
	if (PARENT_SEGMENT_PATTERN.test(normalizedPath)) {
		return null;
	}
	const resolvedClientDirectory = path.resolve(clientDirectory);
	const resolvedPath = path.resolve(resolvedClientDirectory, normalizedPath);
	if (resolvedPath !== resolvedClientDirectory) {
		if (!resolvedPath.startsWith(`${resolvedClientDirectory}${path.sep}`)) {
			return null;
		}
	}
	let isDirectory = false;
	try {
		isDirectory = (await lstat(resolvedPath)).isDirectory();
	} catch (error) {
		const code = getNodeErrorCode(error);
		if (code !== NodeErrorCode.NOT_FOUND && code !== NodeErrorCode.NOT_DIRECTORY) throw error;
	}
	return {isDirectory, normalizedPath};
}

function respondToHealthcheck(
	request: IncomingMessage,
	requestURL: URL,
	response: ServerResponse,
	readiness: AstroServerReadiness | null,
): boolean {
	if (requestURL.pathname !== HEALTH_PATH) {
		return false;
	}
	if (isAstroServerReady(readiness)) {
		return writeHealthcheckResponse(request, response, HTTPStatusCode.OK, HEALTH_RESPONSE_BODY);
	}
	return writeHealthcheckResponse(request, response, HTTPStatusCode.SERVICE_UNAVAILABLE, NOT_READY_RESPONSE_BODY);
}

function isAstroServerReady(readiness: AstroServerReadiness | null): boolean {
	if (readiness == null) {
		return true;
	}
	return readiness();
}

function writeHealthcheckResponse(
	request: IncomingMessage,
	response: ServerResponse,
	status: HTTPStatusCode,
	body: string,
): boolean {
	response.writeHead(status, {
		[HTTPHeader.CONTENT_TYPE]: MIMEType.JSON,
		[HTTPHeader.CONTENT_LENGTH]: Buffer.byteLength(body).toString(),
		[HTTPHeader.CACHE_CONTROL]: NO_STORE_CACHE_CONTROL,
	});
	if (request.method === HTTPMethod.HEAD) {
		response.end();
		return true;
	}
	response.end(body);
	return true;
}

function writeTextResponse(
	request: IncomingMessage,
	response: ServerResponse,
	status: HTTPStatusCode,
	body: string,
): void {
	response.writeHead(status, {
		[HTTPHeader.CONTENT_TYPE]: TEXT_CONTENT_TYPE,
		[HTTPHeader.CONTENT_LENGTH]: Buffer.byteLength(body).toString(),
		[HTTPHeader.CACHE_CONTROL]: NO_STORE_CACHE_CONTROL,
	});
	if (request.method === HTTPMethod.HEAD) {
		response.end();
		return;
	}
	response.end(body);
}

function writeEmptyResponse(response: ServerResponse, status: HTTPStatusCode): void {
	response.writeHead(status, {
		[HTTPHeader.CONTENT_LENGTH]: '0',
		[HTTPHeader.CACHE_CONTROL]: NO_STORE_CACHE_CONTROL,
	});
	response.end();
}

function rejectRequest(response: ServerResponse, status: HTTPStatusCode, allow: string | null): void {
	response.shouldKeepAlive = false;
	response.setHeader(HTTPHeader.CONTENT_LENGTH, '0');
	response.setHeader(HTTPHeader.CACHE_CONTROL, NO_STORE_CACHE_CONTROL);
	response.setHeader(HTTPHeader.CONNECTION, 'close');
	if (allow != null) {
		response.setHeader(HTTPHeader.ALLOW, allow);
	}
	response.writeHead(status);
	response.end();
}

function requestDeclaresBody(request: IncomingMessage): boolean {
	if (request.headers[HTTPHeader.TRANSFER_ENCODING.toLowerCase()] != null) {
		return true;
	}
	if (request.headers[HTTPHeader.EXPECT.toLowerCase()] != null) {
		return true;
	}
	const contentLength = request.headers[HTTPHeader.CONTENT_LENGTH.toLowerCase()];
	if (contentLength == null) {
		return false;
	}
	if (Array.isArray(contentLength)) {
		return true;
	}
	return contentLength.trim() !== '0';
}

function sendFailure(request: IncomingMessage, response: ServerResponse, error: unknown): void {
	if (response.headersSent) {
		let resolvedError: Error;
		if (error instanceof Error) {
			resolvedError = error;
		} else {
			resolvedError = new Error(String(error));
		}
		response.destroy(resolvedError);
		return;
	}
	writeTextResponse(request, response, HTTPStatusCode.INTERNAL_SERVER_ERROR, 'Internal server error');
}

interface AstroHandlerRequest {
	readonly handler: AstroRequestHandler;
	readonly request: IncomingMessage;
	readonly requestLocals: AstroServerRequestLocals | null;
	readonly response: ServerResponse;
}

interface StaticOrRenderRequest extends AstroHandlerRequest {
	readonly clientDirectory: string;
	readonly basePath: string;
	readonly requestURL: URL;
}

async function runAstroHandler({handler, request, requestLocals, response}: AstroHandlerRequest): Promise<void> {
	const next = (error: unknown): void => {
		if (error != null) {
			sendFailure(request, response, error);
			return;
		}
		if (!response.writableEnded) {
			writeEmptyResponse(response, HTTPStatusCode.NOT_FOUND);
		}
	};
	try {
		if (requestLocals == null) {
			await handler(request, response, next, {});
			return;
		}
		await handler(request, response, next, requestLocals());
	} catch (error) {
		sendFailure(request, response, error);
	}
}

function shouldRedirectStaticDirectory(staticFile: ResolvedStaticFile, requestPath: StaticRequestPath): boolean {
	return staticFile.isDirectory && requestPath.pathname !== '/' && !requestPath.pathname.endsWith('/');
}

async function serveStaticOrRender({
	handler,
	clientDirectory,
	basePath,
	request,
	requestURL,
	requestLocals,
	response,
}: StaticOrRenderRequest): Promise<void> {
	const requestPath = resolveStaticRequestPath(request, requestURL, basePath);
	if (requestPath == null) {
		await runAstroHandler({handler, request, requestLocals, response});
		return;
	}
	const staticFile = await resolveStaticFile(clientDirectory, requestPath.staticPath);
	if (staticFile == null) {
		writeTextResponse(request, response, HTTPStatusCode.BAD_REQUEST, 'Bad request');
		return;
	}
	if (shouldRedirectStaticDirectory(staticFile, requestPath)) {
		response.writeHead(HTTPStatusCode.MOVED_PERMANENTLY, {
			[HTTPHeader.LOCATION]: `${requestPath.pathname}/${requestPath.search}`,
			[HTTPHeader.CONTENT_LENGTH]: '0',
			[HTTPHeader.CACHE_CONTROL]: 'no-cache',
		});
		response.end();
		return;
	}
	let encodedPath = requestPath.staticPath;
	let normalizedPath = staticFile.normalizedPath;
	if (staticFile.isDirectory) {
		if (!encodedPath.endsWith('/')) {
			encodedPath = `${encodedPath}/index.html`;
			normalizedPath = normalizeServedPath(`${normalizedPath}/index.html`);
		}
	}
	let dotfiles: StaticDotfilePolicy = StaticDotfilePolicy.DENY;
	if (normalizedPath.startsWith(WELL_KNOWN_PATH_PREFIX)) {
		dotfiles = StaticDotfilePolicy.ALLOW;
	}
	const stream = send(request, encodedPath, {
		dotfiles,
		extensions: ['html'],
		index: ['index.html'],
		root: clientDirectory,
	});
	let fileOpened = false;
	stream.on('file', () => {
		fileOpened = true;
		if (normalizedPath.startsWith(IMMUTABLE_ASSET_PATH_PREFIX)) {
			response.setHeader(HTTPHeader.CACHE_CONTROL, IMMUTABLE_ASSET_CACHE_CONTROL);
		}
	});
	stream.on('error', (error) => {
		if (!fileOpened) {
			runAstroHandler({handler, request, requestLocals, response}).catch((handlerError: unknown) => {
				sendFailure(request, response, handlerError);
			});
			return;
		}
		sendFailure(request, response, error);
	});
	stream.pipe(response);
}

async function loadServerEntrypoint(entrypointURL: URL): Promise<AstroServerEntrypoint> {
	const entrypoint: unknown = await import(entrypointURL.href);
	if (!isAstroServerEntrypoint(entrypoint)) {
		throw new Error(`Astro server entrypoint does not export a handler: ${entrypointURL.href}`);
	}
	return entrypoint;
}

function closeServerListener(server: Server): Promise<void> {
	return new Promise((resolve, reject) => {
		server.close((error) => {
			if (error == null) {
				resolve();
				return;
			}
			reject(error);
		});
		server.closeIdleConnections();
	});
}

export async function runAstroServer(options: AstroServerOptions): Promise<void> {
	validateListener(options.listenHost, options.listenPort);
	const {basePath, secureOrigin} = resolvePublicEndpoint(options.publicEndpoint);
	const securityHeaders = siteSecurityHeaders(secureOrigin);
	let startupPhase: AstroServerStartupPhase | null = AstroServerStartupPhase.ENTRYPOINT;
	const requireStartupProgress = (): void => {
		const phase = startupPhase;
		if (phase == null) {
			return;
		}
		throw new AstroServerStartupAbandonedError(phase);
	};
	process.on('beforeExit', requireStartupProgress);
	try {
		const entrypoint = await loadServerEntrypoint(options.entrypoint);
		if (options.initialize != null) {
			startupPhase = AstroServerStartupPhase.INITIALIZE;
			await options.initialize();
		}
		const clientDirectory = fileURLToPath(new URL('../client/', options.entrypoint));
		let activeRequests = 0;
		const handleRequest = (request: IncomingMessage, response: ServerResponse): void => {
			for (const header of securityHeaders) {
				response.setHeader(header.name, header.value);
			}
			const parsedRequestURL = requestURL(request);
			if (parsedRequestURL == null) {
				rejectRequest(response, HTTPStatusCode.BAD_REQUEST, null);
				return;
			}
			if (request.method !== HTTPMethod.GET && request.method !== HTTPMethod.HEAD) {
				rejectRequest(response, HTTPStatusCode.METHOD_NOT_ALLOWED, ALLOWED_SITE_METHODS);
				return;
			}
			if (requestDeclaresBody(request)) {
				rejectRequest(response, HTTPStatusCode.PAYLOAD_TOO_LARGE, null);
				return;
			}
			if (respondToHealthcheck(request, parsedRequestURL, response, options.readiness)) {
				return;
			}
			if (activeRequests >= ASTRO_SERVER_MAX_ACTIVE_REQUESTS) {
				rejectRequest(response, HTTPStatusCode.SERVICE_UNAVAILABLE, null);
				return;
			}
			activeRequests += 1;
			let retained = true;
			const release = (): void => {
				if (!retained) {
					return;
				}
				retained = false;
				activeRequests -= 1;
			};
			response.once('finish', release);
			response.once('close', release);
			serveStaticOrRender({
				handler: entrypoint.handler,
				clientDirectory,
				basePath,
				request,
				requestURL: parsedRequestURL,
				requestLocals: options.requestLocals,
				response,
			}).catch((error: unknown) => {
				sendFailure(request, response, error);
			});
		};
		const server = createServer({maxHeaderSize: ASTRO_SERVER_MAX_HEADER_BYTES}, handleRequest);
		server.on('checkContinue', handleRequest);
		server.requestTimeout = ASTRO_SERVER_REQUEST_TIMEOUT_MS;
		server.headersTimeout = ASTRO_SERVER_HEADERS_TIMEOUT_MS;
		server.keepAliveTimeout = ASTRO_SERVER_KEEP_ALIVE_TIMEOUT_MS;
		server.timeout = ASTRO_SERVER_SOCKET_TIMEOUT_MS;
		server.maxHeadersCount = ASTRO_SERVER_MAX_HEADERS;
		server.maxRequestsPerSocket = ASTRO_SERVER_MAX_REQUESTS_PER_SOCKET;
		server.maxConnections = ASTRO_SERVER_MAX_CONNECTIONS;
		const done = new Promise<void>((resolve, reject) => {
			server.once('close', resolve);
			server.once('error', reject);
		});
		new AstroServerTermination({
			closeListener: () => closeServerListener(server),
			serverName: ASTRO_SERVER_NAME,
			shutdown: options.shutdown,
			timeoutMs: ASTRO_SERVER_SHUTDOWN_TIMEOUT_MS,
		}).install();
		startupPhase = AstroServerStartupPhase.LISTEN;
		server.once('listening', () => {
			startupPhase = null;
		});
		server.listen(options.listenPort, options.listenHost);
		await done;
	} finally {
		process.off('beforeExit', requireStartupProgress);
	}
}

export async function checkAstroServerHealth(options: AstroServerHealthcheckOptions): Promise<void> {
	validateListener(options.listenHost, options.listenPort);
	if (!Number.isInteger(options.timeoutMs)) {
		throw new InvalidAstroServerHealthcheckTimeoutError();
	}
	if (options.timeoutMs < 1) {
		throw new InvalidAstroServerHealthcheckTimeoutError();
	}
	const endpoint = [
		CanonicalNetworkProtocol.HTTP,
		'//',
		healthcheckHost(options.listenHost),
		':',
		options.listenPort.toString(),
		HEALTH_PATH,
	].join('');
	const response = await fetch(endpoint, {
		cache: 'no-store',
		credentials: 'omit',
		headers: {[HTTPHeader.USER_AGENT]: OUTBOUND_USER_AGENT},
		redirect: 'error',
		referrerPolicy: 'no-referrer',
		signal: AbortSignal.timeout(options.timeoutMs),
	});
	if (response.body != null) {
		await response.body.cancel();
	}
	if (!response.ok) {
		throw new Error(`Astro server healthcheck failed with status ${response.status.toString()}`);
	}
}
