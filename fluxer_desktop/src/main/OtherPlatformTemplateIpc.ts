// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChildLogger} from '@electron/common/Logger';
import {requirePrivilegedRendererDocumentSender} from '@electron/main/PrivilegedRendererDocuments';
import {
	DESKTOP_OTHER_PLATFORM_TEMPLATE_CHANNELS,
	isOtherPlatformTemplateCode,
	type OtherPlatformTemplateFetchError,
	OtherPlatformTemplateFetchFailure,
	type OtherPlatformTemplateFetchResult,
} from '@fluxer/desktop_ipc/src/OtherPlatformTemplateContract';
import {isRecord} from '@fluxer/record_utils/src/RecordGuards';
import {ipcMain, net} from 'electron';

const logger = createChildLogger('OtherPlatformTemplate');

const TEMPLATE_ENDPOINT_ORIGIN = 'https://discord.com';
const TEMPLATE_ENDPOINT_PATH = '/api/v10/guilds/templates/';
const FETCH_TIMEOUT_MS = 15_000;
export const MAX_TEMPLATE_RESPONSE_BYTES = 4 * 1024 * 1024;

let handlersRegistered = false;

class ResponseTooLargeError extends Error {
	public constructor() {
		super('Template response exceeded the size cap');
		this.name = 'ResponseTooLargeError';
	}
}

function browserPlatformToken(): string {
	switch (process.platform) {
		case 'darwin':
			return 'Macintosh; Intel Mac OS X 10_15_7';
		case 'win32':
			return 'Windows NT 10.0; Win64; x64';
		default:
			return 'X11; Linux x86_64';
	}
}

export function genericBrowserUserAgent(): string {
	const chromeMajor = Number.parseInt(process.versions.chrome ?? '', 10);
	const version = Number.isFinite(chromeMajor) && chromeMajor > 0 ? chromeMajor : 140;
	return `Mozilla/5.0 (${browserPlatformToken()}) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/${version}.0.0.0 Safari/537.36`;
}

export function otherPlatformTemplateUrl(code: string): string {
	return `${TEMPLATE_ENDPOINT_ORIGIN}${TEMPLATE_ENDPOINT_PATH}${code}`;
}

function failure(kind: OtherPlatformTemplateFetchError['failure']) {
	return {ok: false, failure: kind} satisfies OtherPlatformTemplateFetchError;
}

async function readBoundedText(response: Response): Promise<string> {
	const declaredLength = Number.parseInt(response.headers.get('content-length') ?? '', 10);
	if (Number.isFinite(declaredLength) && declaredLength > MAX_TEMPLATE_RESPONSE_BYTES) {
		throw new ResponseTooLargeError();
	}
	if (response.body === null) return '';
	const reader = response.body.getReader();
	const chunks: Array<Uint8Array> = [];
	let received = 0;
	try {
		for (;;) {
			const {done, value} = await reader.read();
			if (done) break;
			received += value.byteLength;
			if (received > MAX_TEMPLATE_RESPONSE_BYTES) {
				throw new ResponseTooLargeError();
			}
			chunks.push(value);
		}
	} finally {
		reader.cancel().catch(() => undefined);
	}
	return Buffer.concat(chunks).toString('utf8');
}

export async function fetchOtherPlatformTemplate(code: unknown): Promise<OtherPlatformTemplateFetchResult> {
	if (!isOtherPlatformTemplateCode(code)) {
		return failure(OtherPlatformTemplateFetchFailure.INVALID_CODE);
	}
	const controller = new AbortController();
	const timeout = setTimeout(() => controller.abort(), FETCH_TIMEOUT_MS);
	try {
		const response = await net.fetch(otherPlatformTemplateUrl(code), {
			method: 'GET',
			headers: {
				Accept: 'application/json',
				'User-Agent': genericBrowserUserAgent(),
			},
			cache: 'no-store',
			credentials: 'omit',
			redirect: 'error',
			signal: controller.signal,
			bypassCustomProtocolHandlers: true,
		});
		if (response.status === 404) {
			return failure(OtherPlatformTemplateFetchFailure.NOT_FOUND);
		}
		if (response.status === 429) {
			return failure(OtherPlatformTemplateFetchFailure.RATE_LIMITED);
		}
		if (!response.ok) {
			logger.warn('Template endpoint answered with an unexpected status', {status: response.status});
			return failure(OtherPlatformTemplateFetchFailure.NETWORK);
		}
		let template: unknown;
		try {
			template = JSON.parse(await readBoundedText(response));
		} catch (error) {
			if (controller.signal.aborted) {
				throw error;
			}
			logger.warn('Template endpoint returned an unreadable body', error);
			return failure(OtherPlatformTemplateFetchFailure.INVALID_RESPONSE);
		}
		if (!isRecord(template)) {
			return failure(OtherPlatformTemplateFetchFailure.INVALID_RESPONSE);
		}
		return {ok: true, template};
	} catch (error) {
		logger.warn('Template fetch failed', error);
		return failure(OtherPlatformTemplateFetchFailure.NETWORK);
	} finally {
		clearTimeout(timeout);
	}
}

export function registerOtherPlatformTemplateHandlers(): void {
	if (handlersRegistered) return;
	handlersRegistered = true;
	ipcMain.handle(DESKTOP_OTHER_PLATFORM_TEMPLATE_CHANNELS.fetch, (event, code: unknown) => {
		requirePrivilegedRendererDocumentSender(event, DESKTOP_OTHER_PLATFORM_TEMPLATE_CHANNELS.fetch);
		return fetchOtherPlatformTemplate(code);
	});
}

export function cleanupOtherPlatformTemplateHandlers(): void {
	if (!handlersRegistered) return;
	handlersRegistered = false;
	ipcMain.removeHandler(DESKTOP_OTHER_PLATFORM_TEMPLATE_CHANNELS.fetch);
}
