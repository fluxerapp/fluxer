// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import {ResettableStates} from '@app/features/app/state/ResettableStates';
import {
	apiAttachmentFileKey,
	readAttachmentToken,
	withAttachmentToken,
	withoutAttachmentToken,
} from '@app/features/messaging/utils/ApiAttachmentUrl';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {AttachmentAccessTokenResponse} from '@fluxer/schema/src/domains/message/AttachmentSchemas';

const logger = new Logger('ApiAttachmentFileTokens');

const RENEW_MARGIN_MS = 30_000;
const RETRY_DELAY_MS = 10_000;
const MAX_ISSUED_TOKENS = 64;
const TOKEN_REQUEST_RETRIES = 2;

interface IssuedFileToken {
	readonly token: string;
	readonly expiresAtMs: number;
}

type FileTokenState = IssuedFileToken | 'failed' | null;

function readIssuedToken(body: AttachmentAccessTokenResponse | undefined): IssuedFileToken | null {
	if (!body?.token) return null;
	const expiresAtMs = Date.parse(body.expires_at);
	return Number.isFinite(expiresAtMs) ? {token: body.token, expiresAtMs} : null;
}

export function isFreshFileToken(issued: IssuedFileToken, nowMs = Date.now()): boolean {
	return issued.expiresAtMs - RENEW_MARGIN_MS > nowMs;
}

class ApiAttachmentFileTokens {
	private issued = new Map<string, IssuedFileToken>();
	private readonly failedUntilMs = new Map<string, number>();
	private readonly pending = new Map<string, Promise<string | null>>();
	private readonly epochs = new Map<string, number>();
	private readonly listeners = new Set<() => void>();
	private generation = 0;

	subscribe = (listener: () => void): (() => void) => {
		this.listeners.add(listener);
		return () => {
			this.listeners.delete(listener);
		};
	};

	read(url: string): FileTokenState {
		const key = apiAttachmentFileKey(url);
		if (key === null) return null;
		return this.issued.get(key) ?? (this.failedUntilMs.has(key) ? 'failed' : null);
	}

	epochOf(url: string): number {
		const key = apiAttachmentFileKey(url);
		return key === null ? 0 : (this.epochs.get(key) ?? 0);
	}

	retryInMs(url: string): number {
		const key = apiAttachmentFileKey(url);
		return key === null ? 0 : Math.max(0, (this.failedUntilMs.get(key) ?? 0) - Date.now());
	}

	ensure(url: string): void {
		const key = apiAttachmentFileKey(url);
		if (key === null) return;
		const issued = this.issued.get(key);
		if (issued !== undefined && isFreshFileToken(issued)) return;
		if (Date.now() < (this.failedUntilMs.get(key) ?? 0)) return;
		void this.request(key);
	}

	async linkUrl(url: string): Promise<string> {
		const key = apiAttachmentFileKey(url);
		if (key === null) return url;
		const bare = withoutAttachmentToken(url);
		const issued = this.issued.get(key);
		const token = issued !== undefined && isFreshFileToken(issued) ? issued.token : await this.request(key);
		return token === null ? bare : withAttachmentToken(bare, token);
	}

	renew(url: string): boolean {
		const key = apiAttachmentFileKey(url);
		const token = readAttachmentToken(url);
		if (key === null || token === null) return false;
		const issued = this.issued.get(key);
		if (issued !== undefined && isFreshFileToken(issued) && issued.token === token) return false;
		this.invalidate(key);
		void this.request(key);
		return true;
	}

	invalidate(url: string): void {
		const key = apiAttachmentFileKey(url);
		if (key === null) return;
		this.failedUntilMs.delete(key);
		this.issued.delete(key);
		this.epochs.set(key, (this.epochs.get(key) ?? 0) + 1);
		this.notify();
	}

	reset(): void {
		this.generation += 1;
		this.issued = new Map();
		this.failedUntilMs.clear();
		this.pending.clear();
		this.epochs.clear();
		this.notify();
	}

	private request(key: string): Promise<string | null> {
		const existing = this.pending.get(key);
		if (existing) return existing;
		const generation = this.generation;
		const pending = http
			.post<AttachmentAccessTokenResponse>(Endpoints.USER_ATTACHMENT_TOKEN, {
				body: {url: key},
				mode: 'auto-retry',
				retries: TOKEN_REQUEST_RETRIES,
			})
			.then((response) => {
				const issued = readIssuedToken(response.body);
				if (issued === null) throw new Error('The attachment token response was empty');
				if (generation !== this.generation) return null;
				this.failedUntilMs.delete(key);
				this.remember(key, issued);
				return issued.token;
			})
			.catch((error: unknown) => {
				if (generation !== this.generation) return null;
				logger.warn('Could not get a token for an attachment', error);
				this.failedUntilMs.set(key, Date.now() + RETRY_DELAY_MS);
				this.notify();
				return null;
			})
			.finally(() => {
				if (this.pending.get(key) === pending) this.pending.delete(key);
			});
		this.pending.set(key, pending);
		return pending;
	}

	private remember(key: string, issued: IssuedFileToken): void {
		this.issued.delete(key);
		this.issued.set(key, issued);
		for (const key of this.issued.keys()) {
			if (this.issued.size <= MAX_ISSUED_TOKENS) break;
			this.issued.delete(key);
		}
		this.notify();
	}

	private notify(): void {
		for (const listener of this.listeners) listener();
	}
}

const apiAttachmentFileTokens = new ApiAttachmentFileTokens();
ResettableStates.register(apiAttachmentFileTokens, () => apiAttachmentFileTokens.reset());

export default apiAttachmentFileTokens;
