// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Embed} from './Wire';

export type MessageBody = {
	content?: string;
	embeds?: Array<Embed>;
	message_reference?: {message_id: string};
	allowed_mentions?: {parse: []};
};

export interface ChannelInfo {
	id: string;
	guildId: string | null;
	type: number;
	bitrate: number | null;
}

const MAX_TRIES = 3;
const DEFAULT_RETRY_MS = 1000;

export class FluxerRest {
	private readonly baseUrl: string;
	private readonly token: string;
	private readonly fetchImpl: typeof fetch;
	private apiBase: string | null = null;

	constructor(opts: {baseUrl: string; token: string; fetch?: typeof fetch}) {
		this.baseUrl = opts.baseUrl.replace(/\/+$/, '');
		this.token = opts.token;
		this.fetchImpl = opts.fetch ?? fetch;
	}

	async discover(): Promise<void> {
		const res = await this.fetchImpl(`${this.baseUrl}/.well-known/fluxer`);
		if (!res.ok) throw new Error(`Fluxer discovery failed: HTTP ${res.status}`);
		const doc = (await res.json()) as {endpoints?: {api_public?: string}};
		const api = doc.endpoints?.api_public;
		if (!api) throw new Error('Fluxer discovery has no endpoints.api_public');
		this.apiBase = api.replace(/\/+$/, '');
	}

	async gatewayUrl(): Promise<string> {
		const body = (await this.request('GET', '/v1/gateway/bot')) as {url: string};
		return body.url;
	}

	async getChannel(id: string): Promise<ChannelInfo> {
		const c = (await this.request('GET', `/v1/channels/${encodeURIComponent(id)}`)) as {
			id: string;
			guild_id?: string;
			type: number;
			bitrate?: number | null;
		};
		return {id: c.id, guildId: c.guild_id ?? null, type: c.type, bitrate: c.bitrate ?? null};
	}

	async sendMessage(channelId: string, body: MessageBody): Promise<{id: string}> {
		const msg = (await this.request('POST', `/v1/channels/${encodeURIComponent(channelId)}/messages`, {
			...body,
			allowed_mentions: {parse: []},
		})) as {id: string};
		return {id: msg.id};
	}

	async editMessage(channelId: string, messageId: string, body: MessageBody): Promise<void> {
		await this.request('PATCH', `${messagePath(channelId, messageId)}`, {...body, allowed_mentions: {parse: []}});
	}

	async deleteMessage(channelId: string, messageId: string): Promise<void> {
		await this.request('DELETE', messagePath(channelId, messageId));
	}

	async addReaction(channelId: string, messageId: string, emoji: string): Promise<void> {
		await this.request('PUT', `${messagePath(channelId, messageId)}/reactions/${encodeURIComponent(emoji)}/@me`);
	}

	async removeUserReaction(
		channelId: string,
		messageId: string,
		emoji: string,
		userId: string,
		sessionId?: string,
	): Promise<void> {
		const query = sessionId === undefined ? '' : `?session_id=${encodeURIComponent(sessionId)}`;
		await this.request(
			'DELETE',
			`${messagePath(channelId, messageId)}/reactions/${encodeURIComponent(emoji)}/${encodeURIComponent(userId)}${query}`,
		);
	}

	private async request(method: string, path: string, body?: unknown): Promise<unknown> {
		if (this.apiBase === null) await this.discover();
		const url = `${this.apiBase}${path}`;
		const headers: Record<string, string> = {Authorization: `Bot ${this.token}`};
		if (body !== undefined) headers['Content-Type'] = 'application/json';
		for (let attempt = 1; ; attempt++) {
			const res = await this.fetchImpl(url, {
				method,
				headers,
				body: body === undefined ? undefined : JSON.stringify(body),
			});
			if (res.status === 429 && attempt < MAX_TRIES) {
				await sleep(await retryDelayMs(res));
				continue;
			}
			if (!res.ok) {
				const err = (await res.json().catch(() => null)) as {code?: unknown} | null;
				const code = typeof err?.code === 'string' ? ` ${err.code}` : '';
				throw new Error(`Fluxer ${method} ${path.split('?')[0]} failed: HTTP ${res.status}${code}`);
			}
			if (res.status === 204) return null;
			return res.json().catch(() => null);
		}
	}
}

function messagePath(channelId: string, messageId: string): string {
	return `/v1/channels/${encodeURIComponent(channelId)}/messages/${encodeURIComponent(messageId)}`;
}

async function retryDelayMs(res: Response): Promise<number> {
	const body = (await res.json().catch(() => null)) as {retry_after?: unknown} | null;
	if (typeof body?.retry_after === 'number') return Math.ceil(body.retry_after * 1000);
	const header = Number(res.headers.get('Retry-After'));
	return Number.isFinite(header) && header > 0 ? header * 1000 : DEFAULT_RETRY_MS;
}

function sleep(ms: number): Promise<void> {
	return new Promise((resolve) => setTimeout(resolve, ms));
}
