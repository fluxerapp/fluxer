/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {makeAutoObservable} from 'mobx';
import type {Webhook} from '~/records/WebhookRecord';
import {WebhookRecord} from '~/records/WebhookRecord';

type FetchStatus = 'idle' | 'pending' | 'success' | 'error';

class WebhookStore {
	webhooks: Map<string, WebhookRecord> = new Map();
	channelWebhooks: Map<string, Array<string>> = new Map();
	channelGuildMap: Map<string, string> = new Map();
	channelFetchStatus: Map<string, FetchStatus> = new Map();
	guildFetchStatus: Map<string, FetchStatus> = new Map();

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	getWebhook(webhookId: string): WebhookRecord | null {
		return this.webhooks.get(webhookId) ?? null;
	}

	getChannelWebhooks(channelId: string): Array<WebhookRecord> | null {
		const ids = this.channelWebhooks.get(channelId);
		if (!ids) return null;
		return ids.map((id) => this.webhooks.get(id)).filter((record): record is WebhookRecord => record != null);
	}

	getGuildWebhooks(guildId: string): Array<WebhookRecord> {
		return Array.from(this.webhooks.values()).filter((webhook) => webhook.guildId === guildId);
	}

	getChannelFetchStatus(channelId: string): FetchStatus {
		return this.channelFetchStatus.get(channelId) ?? 'idle';
	}

	getGuildFetchStatus(guildId: string): FetchStatus {
		return this.guildFetchStatus.get(guildId) ?? 'idle';
	}

	handleChannelWebhooksFetchPending(channelId: string): void {
		this.channelFetchStatus = new Map(this.channelFetchStatus).set(channelId, 'pending');
	}

	handleChannelWebhooksFetchSuccess(channelId: string, guildId: string, webhooks: Array<Webhook>): void {
		const records = webhooks.map((webhook) => new WebhookRecord(webhook));
		const recordIds = records.map((record) => record.id);

		const nextWebhooks = new Map(this.webhooks);
		const previousIds = this.channelWebhooks.get(channelId) ?? [];

		for (const id of previousIds) {
			const record = nextWebhooks.get(id);
			if (record?.channelId === channelId) {
				nextWebhooks.delete(id);
			}
		}

		for (const record of records) {
			nextWebhooks.set(record.id, record);
		}

		this.webhooks = nextWebhooks;
		this.channelWebhooks = new Map(this.channelWebhooks).set(channelId, recordIds);
		this.channelGuildMap = new Map(this.channelGuildMap).set(channelId, guildId);
		this.channelFetchStatus = new Map(this.channelFetchStatus).set(channelId, 'success');
		this.guildFetchStatus = new Map(this.guildFetchStatus).set(guildId, 'idle');
	}

	handleChannelWebhooksFetchError(channelId: string): void {
		this.channelFetchStatus = new Map(this.channelFetchStatus).set(channelId, 'error');
	}

	handleGuildWebhooksFetchPending(guildId: string): void {
		this.guildFetchStatus = new Map(this.guildFetchStatus).set(guildId, 'pending');
	}

	handleGuildWebhooksFetchSuccess(guildId: string, webhooks: Array<Webhook>): void {
		const records = webhooks.map((webhook) => new WebhookRecord(webhook));
		const newWebhookIds = new Set(records.map((record) => record.id));
		const channelBuckets = new Map<string, Array<string>>();

		for (const record of records) {
			const existing = channelBuckets.get(record.channelId);
			if (existing) {
				existing.push(record.id);
			} else {
				channelBuckets.set(record.channelId, [record.id]);
			}
		}

		const nextWebhooks = new Map(this.webhooks);
		const nextChannelWebhooks = new Map(this.channelWebhooks);
		const nextChannelGuildMap = new Map(this.channelGuildMap);
		const nextChannelFetchStatus = new Map(this.channelFetchStatus);

		for (const [id, record] of nextWebhooks) {
			if (record.guildId === guildId && !newWebhookIds.has(id)) {
				nextWebhooks.delete(id);
			}
		}

		for (const record of records) {
			nextWebhooks.set(record.id, record);
		}

		for (const [channelId, ids] of channelBuckets) {
			nextChannelWebhooks.set(channelId, ids);
			nextChannelGuildMap.set(channelId, guildId);
			nextChannelFetchStatus.set(channelId, 'success');
		}

		const channelEntries = Array.from(nextChannelWebhooks.entries());
		for (const [channelId, ids] of channelEntries) {
			if (channelBuckets.has(channelId)) {
				continue;
			}
			if (nextChannelGuildMap.get(channelId) !== guildId) {
				continue;
			}
			const filtered = ids.filter((id) => {
				const record = nextWebhooks.get(id);
				return record?.guildId === guildId && record.channelId === channelId;
			});
			if (filtered.length === 0) {
				nextChannelWebhooks.set(channelId, []);
				nextChannelFetchStatus.set(channelId, 'success');
			} else {
				nextChannelWebhooks.set(channelId, filtered);
				nextChannelFetchStatus.set(channelId, 'success');
			}
		}

		this.webhooks = nextWebhooks;
		this.channelWebhooks = nextChannelWebhooks;
		this.channelGuildMap = nextChannelGuildMap;
		this.channelFetchStatus = nextChannelFetchStatus;
		this.guildFetchStatus = new Map(this.guildFetchStatus).set(guildId, 'success');
	}

	handleGuildWebhooksFetchError(guildId: string): void {
		this.guildFetchStatus = new Map(this.guildFetchStatus).set(guildId, 'error');
	}

	handleWebhookCreate(webhook: Webhook): void {
		const record = new WebhookRecord(webhook);

		this.webhooks = new Map(this.webhooks).set(record.id, record);
		const channelWebhooks = this.channelWebhooks.get(record.channelId) ?? [];
		this.channelWebhooks = new Map(this.channelWebhooks).set(
			record.channelId,
			channelWebhooks.includes(record.id) ? channelWebhooks : [...channelWebhooks, record.id],
		);
		this.channelGuildMap = new Map(this.channelGuildMap).set(record.channelId, record.guildId);
		this.channelFetchStatus = new Map(this.channelFetchStatus).set(record.channelId, 'success');
		this.guildFetchStatus = new Map(this.guildFetchStatus).set(record.guildId, 'idle');
	}

	handleWebhookDelete(webhookId: string, channelId: string | null, guildId: string | null): void {
		const nextWebhooks = new Map(this.webhooks);
		const record = nextWebhooks.get(webhookId);

		if (!record) {
			return;
		}

		nextWebhooks.delete(webhookId);

		const actualChannelId = channelId ?? record.channelId;
		const nextChannelWebhooks = new Map(this.channelWebhooks);
		const channelList = nextChannelWebhooks.get(actualChannelId);
		if (channelList) {
			nextChannelWebhooks.set(
				actualChannelId,
				channelList.filter((id) => id !== webhookId),
			);
		}

		const nextGuildFetchStatus = new Map(this.guildFetchStatus);
		if (guildId ?? record.guildId) {
			nextGuildFetchStatus.set(guildId ?? record.guildId, 'idle');
		}

		const nextChannelFetchStatus = new Map(this.channelFetchStatus);
		nextChannelFetchStatus.set(actualChannelId, 'idle');

		this.webhooks = nextWebhooks;
		this.channelWebhooks = nextChannelWebhooks;
		this.guildFetchStatus = nextGuildFetchStatus;
		this.channelFetchStatus = nextChannelFetchStatus;
	}

	handleWebhooksUpdate(guildId: string, channelId: string): void {
		const nextWebhooks = new Map(this.webhooks);
		for (const [id, record] of nextWebhooks) {
			if (record.guildId === guildId && record.channelId === channelId) {
				nextWebhooks.delete(id);
			}
		}

		const nextChannelWebhooks = new Map(this.channelWebhooks);
		nextChannelWebhooks.delete(channelId);

		this.webhooks = nextWebhooks;
		this.channelWebhooks = nextChannelWebhooks;
		this.channelGuildMap = new Map(this.channelGuildMap).set(channelId, guildId);
		this.channelFetchStatus = new Map(this.channelFetchStatus).set(channelId, 'idle');
		this.guildFetchStatus = new Map(this.guildFetchStatus).set(guildId, 'idle');
	}

	handleChannelDelete(channelId: string): void {
		const guildId = this.channelGuildMap.get(channelId);
		const nextWebhooks = new Map(this.webhooks);
		for (const [id, record] of nextWebhooks) {
			if (record.channelId === channelId) {
				nextWebhooks.delete(id);
			}
		}

		const nextChannelWebhooks = new Map(this.channelWebhooks);
		nextChannelWebhooks.delete(channelId);

		const nextChannelGuildMap = new Map(this.channelGuildMap);
		nextChannelGuildMap.delete(channelId);

		const nextChannelFetchStatus = new Map(this.channelFetchStatus);
		nextChannelFetchStatus.delete(channelId);

		this.webhooks = nextWebhooks;
		this.channelWebhooks = nextChannelWebhooks;
		this.channelGuildMap = nextChannelGuildMap;
		this.channelFetchStatus = nextChannelFetchStatus;
		if (guildId != null) {
			this.guildFetchStatus = new Map(this.guildFetchStatus).set(guildId, 'idle');
		}
	}

	handleGuildDelete(guildId: string): void {
		const nextWebhooks = new Map(this.webhooks);
		for (const [id, record] of nextWebhooks) {
			if (record.guildId === guildId) {
				nextWebhooks.delete(id);
			}
		}

		const nextChannelWebhooks = new Map(this.channelWebhooks);
		const nextChannelGuildMap = new Map(this.channelGuildMap);
		const nextChannelFetchStatus = new Map(this.channelFetchStatus);

		for (const [channelId, guild] of nextChannelGuildMap) {
			if (guild === guildId) {
				nextChannelWebhooks.delete(channelId);
				nextChannelGuildMap.delete(channelId);
				nextChannelFetchStatus.delete(channelId);
			}
		}

		const nextGuildFetchStatus = new Map(this.guildFetchStatus);
		nextGuildFetchStatus.delete(guildId);

		this.webhooks = nextWebhooks;
		this.channelWebhooks = nextChannelWebhooks;
		this.channelGuildMap = nextChannelGuildMap;
		this.channelFetchStatus = nextChannelFetchStatus;
		this.guildFetchStatus = nextGuildFetchStatus;
	}

	handleLogout(): void {
		this.webhooks = new Map();
		this.channelWebhooks = new Map();
		this.channelGuildMap = new Map();
		this.channelFetchStatus = new Map();
		this.guildFetchStatus = new Map();
	}
}

export default new WebhookStore();
