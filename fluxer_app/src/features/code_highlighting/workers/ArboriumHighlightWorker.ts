// SPDX-License-Identifier: AGPL-3.0-or-later

import {ARBORIUM_GRAMMAR_LOADERS} from '@app/features/code_highlighting/utils/ArboriumGrammars';
import {desktopModuleNameForAsset} from '@app/features/platform/utils/DesktopModuleAssetName';
import {resolveWorkerAssetUrl} from '@app/features/platform/utils/WorkerAssetUrl';
import hostWasmUrl from '@arborium/arborium/arborium_host_bg.wasm';
import {MAX_CODE_HIGHLIGHT_OUTPUT_LENGTH, MAX_CODE_HIGHLIGHT_SOURCE_LENGTH} from '@fluxer/constants/src/LimitConstants';

type ArboriumModule = typeof import('@arborium/arborium');

export interface ArboriumHighlightWorkerRequest {
	id: number;
	language: string;
	source: string;
}

export interface ArboriumHighlightWorkerModuleReply {
	moduleRequestId: number;
	available: boolean;
}

export type ArboriumHighlightWorkerResponse =
	| {
			id: number;
			status: 'progress';
			phase: 'initializing' | 'loading' | 'highlighting';
	  }
	| {
			id: number;
			status: 'module';
			moduleRequestId: number;
			assetUrl: string;
	  }
	| {
			id: number;
			status: 'success';
			highlightedHtml: string;
	  }
	| {
			id: number;
			status: 'skipped';
			reason: 'language' | 'output_limit' | 'source_limit';
	  }
	| {
			id: number;
			status: 'error';
			message: string;
	  };

const AUTO_DETECT_LANGUAGE_CODE = 'auto';
const MAX_LANGUAGE_LENGTH = 128;
const ARBORIUM_WASM_MAX_BYTES = 32 * 1024 * 1024;
const ARBORIUM_WASM_MAX_CHUNKS = 8192;
const ARBORIUM_WASM_REQUEST_TIMEOUT_MS = 30_000;
const workerScope = self as unknown as DedicatedWorkerGlobalScope;
let arboriumPromise: Promise<ArboriumModule> | null = null;
let requestInFlight = false;
let activeRequestId: number | null = null;
let grammarModuleUnavailable = false;
let nextModuleRequestId = 1;
const pendingModuleRequests = new Map<number, (available: boolean) => void>();

class ArboriumWASMResponseError extends Error {
	constructor(description: string, status: number) {
		super(`${description} returned ${status}`);
		this.name = 'ArboriumWASMResponseError';
	}
}

class ArboriumWASMSizeLimitError extends Error {
	constructor(description: string) {
		super(`${description} exceeded its size limit`);
		this.name = 'ArboriumWASMSizeLimitError';
	}
}

async function cancelArboriumWASMBody(body: ReadableStream<Uint8Array>): Promise<void> {
	try {
		await body.cancel();
	} catch (error) {
		console.warn('[ArboriumHighlightWorker]', 'Failed to cancel an oversized WASM response body', error);
	}
}

async function fetchArboriumWASM(url: string, description: string): Promise<Response> {
	const controller = new AbortController();
	const timeout = setTimeout(() => controller.abort(), ARBORIUM_WASM_REQUEST_TIMEOUT_MS);
	try {
		const response = await fetch(url, {
			cache: 'force-cache',
			credentials: 'omit',
			redirect: 'error',
			referrerPolicy: 'no-referrer',
			signal: controller.signal,
		});
		if (!response.ok) {
			throw new ArboriumWASMResponseError(description, response.status);
		}
		const contentLength = response.headers.get('content-length');
		if (contentLength !== null) {
			const parsedContentLength = Number(contentLength);
			if (Number.isFinite(parsedContentLength) && parsedContentLength > ARBORIUM_WASM_MAX_BYTES) {
				if (response.body !== null) {
					await cancelArboriumWASMBody(response.body);
				}
				throw new ArboriumWASMSizeLimitError(description);
			}
		}
		const body = response.body;
		if (body === null) {
			throw new Error(`${description} has no readable body`);
		}
		const reader = body.getReader();
		const chunks: Array<Uint8Array> = [];
		let totalBytes = 0;
		try {
			while (true) {
				const result = await reader.read();
				if (result.done) {
					break;
				}
				if (result.value === undefined) {
					continue;
				}
				if (chunks.length >= ARBORIUM_WASM_MAX_CHUNKS) {
					throw new ArboriumWASMSizeLimitError(description);
				}
				totalBytes += result.value.byteLength;
				if (totalBytes > ARBORIUM_WASM_MAX_BYTES) {
					throw new ArboriumWASMSizeLimitError(description);
				}
				chunks.push(result.value);
			}
		} catch (error) {
			try {
				await reader.cancel();
			} catch (cancelError) {
				console.warn('[ArboriumHighlightWorker]', 'Failed to cancel a bounded WASM response body', cancelError);
			}
			throw error;
		} finally {
			reader.releaseLock();
		}
		const bytes = new Uint8Array(totalBytes);
		let offset = 0;
		for (const chunk of chunks) {
			bytes.set(chunk, offset);
			offset += chunk.byteLength;
		}
		const contentType = response.headers.get('content-type');
		if (contentType === null) {
			return new Response(bytes);
		}
		return new Response(bytes, {headers: {'content-type': contentType}});
	} finally {
		clearTimeout(timeout);
	}
}

function requireGrammarModule(assetUrl: string): Promise<void> {
	const requestId = activeRequestId;
	if (requestId === null || desktopModuleNameForAsset(assetUrl) === null) {
		return Promise.resolve();
	}
	return new Promise((resolve, reject) => {
		const moduleRequestId = nextModuleRequestId++;
		pendingModuleRequests.set(moduleRequestId, (available) => {
			if (available) {
				resolve();
				return;
			}
			grammarModuleUnavailable = true;
			reject(new Error(`The grammar module for ${assetUrl} is not available`));
		});
		workerScope.postMessage({
			id: requestId,
			status: 'module',
			moduleRequestId,
			assetUrl,
		} satisfies ArboriumHighlightWorkerResponse);
	});
}

function isModuleReply(value: unknown): value is ArboriumHighlightWorkerModuleReply {
	if (typeof value !== 'object' || value === null) {
		return false;
	}
	const reply = value as Partial<ArboriumHighlightWorkerModuleReply>;
	return typeof reply.moduleRequestId === 'number' && typeof reply.available === 'boolean';
}

function postProgress(id: number, phase: 'initializing' | 'loading' | 'highlighting'): void {
	workerScope.postMessage({id, status: 'progress', phase} satisfies ArboriumHighlightWorkerResponse);
}

function loadArborium(): Promise<ArboriumModule> {
	if (!arboriumPromise) {
		arboriumPromise = import('@arborium/arborium')
			.then((arborium) => {
				arborium.setConfig({
					resolveHostJs: () => import('@arborium/arborium/arborium_host.js'),
					resolveHostWasm: () => fetchArboriumWASM(resolveWorkerAssetUrl(hostWasmUrl), 'Arborium host WASM request'),
					resolveJs: ({language}) => {
						const loader = ARBORIUM_GRAMMAR_LOADERS[language];
						if (!loader) {
							throw new Error(`No bundled arborium grammar for language '${language}'`);
						}
						return loader.loadJs();
					},
					resolveWasm: async ({language}) => {
						const loader = ARBORIUM_GRAMMAR_LOADERS[language];
						if (!loader) {
							throw new Error(`No bundled arborium grammar for language '${language}'`);
						}
						await requireGrammarModule(loader.wasmUrl);
						return fetchArboriumWASM(
							resolveWorkerAssetUrl(loader.wasmUrl),
							`Arborium ${language} grammar WASM request`,
						);
					},
					logger: console,
				});
				return arborium;
			})
			.catch((error) => {
				arboriumPromise = null;
				throw error;
			});
	}
	return arboriumPromise;
}

function isHighlightRequest(value: unknown): value is ArboriumHighlightWorkerRequest {
	if (typeof value !== 'object' || value === null) {
		return false;
	}
	const request = value as Partial<ArboriumHighlightWorkerRequest>;
	return (
		typeof request.id === 'number' &&
		Number.isSafeInteger(request.id) &&
		request.id > 0 &&
		typeof request.language === 'string' &&
		request.language.length <= MAX_LANGUAGE_LENGTH &&
		typeof request.source === 'string'
	);
}

function getErrorMessage(error: unknown): string {
	const message = error instanceof Error ? error.message : String(error);
	return message.length <= 4_096 ? message : message.slice(0, 4_096);
}

async function highlightSource(request: ArboriumHighlightWorkerRequest): Promise<ArboriumHighlightWorkerResponse> {
	if (request.source.length >= MAX_CODE_HIGHLIGHT_SOURCE_LENGTH) {
		return {id: request.id, status: 'skipped', reason: 'source_limit'};
	}
	postProgress(request.id, 'initializing');
	const arborium = await loadArborium();
	const detectedLanguage =
		request.language === AUTO_DETECT_LANGUAGE_CODE ? arborium.detectLanguage(request.source) : request.language;
	const normalizedLanguage = detectedLanguage ? arborium.normalizeLanguage(detectedLanguage) : null;
	if (!normalizedLanguage || !arborium.availableLanguages.includes(normalizedLanguage)) {
		return {id: request.id, status: 'skipped', reason: 'language'};
	}
	postProgress(request.id, 'loading');
	const grammar = await arborium.loadGrammar(normalizedLanguage);
	if (!grammar) {
		return {id: request.id, status: 'skipped', reason: 'language'};
	}
	await arborium.highlight(normalizedLanguage, '');
	postProgress(request.id, 'highlighting');
	const highlightedHtml = await arborium.highlight(normalizedLanguage, request.source);
	if (highlightedHtml.length > MAX_CODE_HIGHLIGHT_OUTPUT_LENGTH) {
		return {id: request.id, status: 'skipped', reason: 'output_limit'};
	}
	return {id: request.id, status: 'success', highlightedHtml};
}

workerScope.addEventListener('message', (event: MessageEvent<unknown>) => {
	if (isModuleReply(event.data)) {
		const settle = pendingModuleRequests.get(event.data.moduleRequestId);
		pendingModuleRequests.delete(event.data.moduleRequestId);
		settle?.(event.data.available);
		return;
	}
	if (!isHighlightRequest(event.data)) {
		return;
	}
	if (requestInFlight) {
		workerScope.postMessage({
			id: event.data.id,
			status: 'error',
			message: 'Arborium highlight worker received a request while busy',
		} satisfies ArboriumHighlightWorkerResponse);
		return;
	}
	requestInFlight = true;
	const request = event.data;
	activeRequestId = request.id;
	grammarModuleUnavailable = false;
	void highlightSource(request)
		.then((response) => workerScope.postMessage(response))
		.catch((error) => {
			if (grammarModuleUnavailable) {
				workerScope.postMessage({
					id: request.id,
					status: 'skipped',
					reason: 'language',
				} satisfies ArboriumHighlightWorkerResponse);
				return;
			}
			workerScope.postMessage({
				id: request.id,
				status: 'error',
				message: getErrorMessage(error),
			} satisfies ArboriumHighlightWorkerResponse);
		})
		.finally(() => {
			requestInFlight = false;
			activeRequestId = null;
		});
});
