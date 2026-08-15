// SPDX-License-Identifier: AGPL-3.0-or-later

import type {SearchableMessage} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';
import {describe, expect, it} from 'vitest';
import type {MeilisearchClient, MeilisearchTask} from './MeilisearchClient';
import {MeilisearchMessageAdapter} from './MeilisearchDomainAdapters';

interface RecordedMeilisearchRequest {
	method: string;
	path: string;
	body: unknown;
}

class FakeMeilisearchClient implements MeilisearchClient {
	readonly requests: Array<RecordedMeilisearchRequest> = [];
	readonly waitedTaskUids: Array<number> = [];
	private nextTaskUid = 1;
	indexExists = false;

	async request<TResponse>(method: string, path: string, body?: unknown): Promise<TResponse> {
		this.requests.push({method, path, body});
		if (method === 'GET' && path.startsWith('/indexes/')) {
			if (!this.indexExists) {
				throw new Error('404 index_not_found');
			}
			return {uid: path.slice('/indexes/'.length)} as TResponse;
		}
		if (method === 'POST' && path === '/indexes') {
			this.indexExists = true;
			return this.nextTask() as TResponse;
		}
		if ((method === 'PUT' || method === 'PATCH') && path.includes('/settings/')) {
			return this.nextTask() as TResponse;
		}
		if (method === 'POST' && path.endsWith('/documents')) {
			return this.nextTask() as TResponse;
		}
		if (method === 'POST' && path.endsWith('/search')) {
			return {
				hits: [{id: 'message-1'}],
				estimatedTotalHits: 1,
			} as TResponse;
		}
		throw new Error(`Unhandled fake Meilisearch request: ${method} ${path}`);
	}

	async waitForTask(taskUid: number): Promise<void> {
		this.waitedTaskUids.push(taskUid);
	}

	clear(): void {
		this.requests.length = 0;
		this.waitedTaskUids.length = 0;
	}

	private nextTask(): MeilisearchTask {
		return {
			taskUid: this.nextTaskUid++,
			status: 'enqueued',
		};
	}
}

describe('MeilisearchMessageAdapter', () => {
	it('creates missing indexes and applies settings before becoming available', async () => {
		const client = new FakeMeilisearchClient();
		const adapter = new MeilisearchMessageAdapter({client});

		await adapter.initialize();

		expect(adapter.isAvailable()).toBe(true);
		expect(client.requests.map((request) => `${request.method} ${request.path}`)).toEqual([
			'GET /indexes/messages',
			'POST /indexes',
			'PUT /indexes/messages/settings/searchable-attributes',
			'PUT /indexes/messages/settings/filterable-attributes',
			'PUT /indexes/messages/settings/sortable-attributes',
			'PATCH /indexes/messages/settings/pagination',
		]);
		expect(client.waitedTaskUids).toEqual([1, 2, 3, 4, 5]);
		expect(client.requests.find((request) => request.path.endsWith('/settings/pagination'))?.body).toEqual({
			maxTotalHits: 10000,
		});
	});

	it('builds Meilisearch search requests from message filters', async () => {
		const client = new FakeMeilisearchClient();
		client.indexExists = true;
		const adapter = new MeilisearchMessageAdapter({client});
		await adapter.initialize();
		client.clear();

		const result = await adapter.search(
			'hello',
			{
				guildId: 'guild-1',
				channelIds: ['channel-"quoted"', 'channel-2'],
				mentions: ['user-1'],
				sortBy: 'timestamp',
				sortOrder: 'asc',
			},
			{limit: 10, offset: 20},
		);

		expect(result).toEqual({hits: [{id: 'message-1'}], total: 1});
		expect(client.requests).toHaveLength(1);
		expect(client.requests[0]).toEqual({
			method: 'POST',
			path: '/indexes/messages/search',
			body: {
				q: 'hello',
				filter:
					'(guildId = "guild-1") AND ((channelId = "channel-\\"quoted\\"" OR channelId = "channel-2")) AND (mentionedUserIds = "user-1")',
				limit: 10,
				offset: 20,
				sort: ['createdAt:asc', 'id:desc'],
				attributesToSearchOn: ['content', 'embedContent'],
				showRankingScore: false,
			},
		});
	});

	it('waits for queued write tasks when refreshed', async () => {
		const client = new FakeMeilisearchClient();
		client.indexExists = true;
		const adapter = new MeilisearchMessageAdapter({client});
		await adapter.initialize();
		client.clear();

		await adapter.indexDocument({id: 'message-1'} as SearchableMessage);

		expect(client.waitedTaskUids).toEqual([]);
		await adapter.refreshIndex();
		expect(client.waitedTaskUids).toEqual([5]);
	});
});
