// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import {ResettableStates} from '@app/features/app/state/ResettableStates';
import {
	isApiAttachmentUrl,
	isApiProxiedAttachmentsEnabled,
	withAttachmentToken,
	withoutAttachmentToken,
} from '@app/features/messaging/utils/ApiAttachmentUrl';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {AttachmentAccessTokenResponse} from '@fluxer/schema/src/domains/message/AttachmentSchemas';
import {makeAutoObservable, runInAction} from 'mobx';

const logger = new Logger('AttachmentAccessToken');

const RETRY_DELAY_MS = 30_000;
const MAX_TIMER_DELAY_MS = 2_147_483_647;

interface IssuedToken {
	token: string;
	expiresAtMs: number;
}

function readIssuedToken(body: AttachmentAccessTokenResponse | undefined): IssuedToken | null {
	if (!body?.token) return null;
	const expiresAtMs = Date.parse(body.expires_at);
	return Number.isFinite(expiresAtMs) ? {token: body.token, expiresAtMs} : null;
}

async function requestToken(url?: string): Promise<IssuedToken | null> {
	const response = await http.post<AttachmentAccessTokenResponse>(Endpoints.USER_ATTACHMENT_TOKEN, {
		body: url === undefined ? {} : {url},
	});
	return readIssuedToken(response.body);
}

class AttachmentAccessToken {
	issued: IssuedToken | null = null;
	pending: Promise<string | null> | null = null;
	renewTimer: ReturnType<typeof setTimeout> | null = null;
	retryAtMs = 0;
	generation = 0;

	constructor() {
		makeAutoObservable(
			this,
			{pending: false, renewTimer: false, retryAtMs: false, generation: false},
			{autoBind: true},
		);
	}

	current(): string | null {
		if (!isApiProxiedAttachmentsEnabled()) return null;
		const nowMs = Date.now();
		if (this.issued !== null && this.issued.expiresAtMs > nowMs) return this.issued.token;
		if (nowMs >= this.retryAtMs) void this.ensure();
		return null;
	}

	displayUrl(url: string): string | null {
		if (!isApiAttachmentUrl(url)) return url;
		const token = this.current();
		return token === null ? null : withAttachmentToken(url, token);
	}

	async loadableUrl(url: string): Promise<string> {
		if (!isApiAttachmentUrl(url)) return url;
		const token = this.current() ?? (await this.ensure());
		return token === null ? url : withAttachmentToken(url, token);
	}

	async shareableUrl(url: string): Promise<string> {
		if (!isApiAttachmentUrl(url)) return url;
		const bare = withoutAttachmentToken(url);
		try {
			const issued = await requestToken(bare);
			return issued === null ? bare : withAttachmentToken(bare, issued.token);
		} catch (error) {
			logger.warn('Could not get a link token for an attachment', error);
			return bare;
		}
	}

	ensure(): Promise<string | null> {
		if (this.pending !== null) return this.pending;
		const generation = this.generation;
		const pending = requestToken()
			.then((issued) => {
				if (generation !== this.generation) return null;
				runInAction(() => {
					this.issued = issued;
				});
				if (issued !== null) this.scheduleRenewal(issued);
				return issued?.token ?? null;
			})
			.catch((error: unknown) => {
				logger.warn('Could not get an attachment token', error);
				this.retryAtMs = Date.now() + RETRY_DELAY_MS;
				return null;
			})
			.finally(() => {
				if (this.pending === pending) this.pending = null;
			});
		this.pending = pending;
		return pending;
	}

	reset(): void {
		this.generation += 1;
		this.issued = null;
		this.pending = null;
		this.retryAtMs = 0;
		if (this.renewTimer !== null) {
			clearTimeout(this.renewTimer);
			this.renewTimer = null;
		}
	}

	private scheduleRenewal(issued: IssuedToken): void {
		if (this.renewTimer !== null) clearTimeout(this.renewTimer);
		const nowMs = Date.now();
		const delayMs = Math.min(MAX_TIMER_DELAY_MS, Math.max(0, (issued.expiresAtMs - nowMs) / 2));
		this.renewTimer = setTimeout(() => {
			this.renewTimer = null;
			if (isApiProxiedAttachmentsEnabled()) void this.ensure();
		}, delayMs);
	}
}

const attachmentAccessToken = new AttachmentAccessToken();
ResettableStates.register(attachmentAccessToken, () => attachmentAccessToken.reset());

export default attachmentAccessToken;
