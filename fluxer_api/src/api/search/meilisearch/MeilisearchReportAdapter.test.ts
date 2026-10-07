// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MeilisearchClient, MeilisearchTask} from '@app/api/search/meilisearch/MeilisearchClient';
import {MeilisearchReportAdapter} from '@app/api/search/meilisearch/MeilisearchDomainAdapters';
import type {ReportSearchFilters} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';
import {describe, expect, it} from 'vitest';

interface RecordedMeilisearchRequest {
	method: string;
	path: string;
	body: unknown;
}

interface MeilisearchSearchBody {
	q: string;
	filter: string | undefined;
	sort: Array<string> | undefined;
}

class FakeMeilisearchClient implements MeilisearchClient {
	readonly requests: Array<RecordedMeilisearchRequest> = [];
	private nextTaskUid = 1;

	async request<TResponse>(method: string, path: string, body?: unknown): Promise<TResponse> {
		this.requests.push({method, path, body});
		if (method === 'GET' && path.startsWith('/indexes/')) {
			return {uid: path.slice('/indexes/'.length)} as TResponse;
		}
		if ((method === 'PUT' || method === 'PATCH') && path.includes('/settings/')) {
			const task: MeilisearchTask = {taskUid: this.nextTaskUid++, status: 'enqueued'};
			return task as TResponse;
		}
		if (method === 'POST' && path.endsWith('/search')) {
			return {hits: [{id: 'report-1'}], estimatedTotalHits: 1} as TResponse;
		}
		throw new Error(`Unhandled fake Meilisearch request: ${method} ${path}`);
	}

	async waitForTask(_taskUid: number): Promise<void> {}
}

async function searchBody(filters: ReportSearchFilters): Promise<MeilisearchSearchBody> {
	const client = new FakeMeilisearchClient();
	const adapter = new MeilisearchReportAdapter({client});
	await adapter.initialize();
	await adapter.search('', filters, {limit: 10, offset: 0});
	const request = client.requests.find((candidate) => candidate.path === '/indexes/reports/search');
	return request!.body as MeilisearchSearchBody;
}

describe('MeilisearchReportAdapter', () => {
	it('declares the reported channel as a filterable attribute', async () => {
		const client = new FakeMeilisearchClient();
		const adapter = new MeilisearchReportAdapter({client});

		await adapter.initialize();

		const settings = client.requests.find(
			(request) => request.method === 'PUT' && request.path === '/indexes/reports/settings/filterable-attributes',
		);
		expect(settings?.body).toContain('reportedChannelId');
	});

	it('emits a reported channel term', async () => {
		const body = await searchBody({reportedChannelId: '1234567890'});

		expect(body.filter).toBe('(reportedChannelId = "1234567890")');
		expect(body.sort).toEqual(['reportedAt:desc', 'id:desc']);
	});

	it('combines the reported channel term with the other report filters', async () => {
		const body = await searchBody({
			status: 0,
			reportType: 0,
			reportedGuildId: '11',
			reportedMessageId: '22',
			reportedChannelId: '33',
			guildContextId: '44',
		});

		expect(body.filter).toBe(
			'(status = 0) AND (reportType = 0) AND (reportedGuildId = "11") AND (reportedMessageId = "22") AND (reportedChannelId = "33") AND (guildContextId = "44")',
		);
	});

	it('omits the reported channel term when no channel is given', async () => {
		const body = await searchBody({status: 1});

		expect(body.filter).toBe('(status = 1)');
	});

	it('matches resolved reports on a resolution time that is present and not null', async () => {
		const body = await searchBody({isResolved: true});

		expect(body.filter).toBe('((resolvedAt EXISTS AND resolvedAt IS NOT NULL))');
	});

	it('matches unresolved reports on a resolution time that is absent or null', async () => {
		const body = await searchBody({isResolved: false});

		expect(body.filter).toBe('((resolvedAt NOT EXISTS OR resolvedAt IS NULL))');
	});

	it('keeps the resolution clause grouped when combined with other filters', async () => {
		const body = await searchBody({reportedChannelId: '33', isResolved: false});

		expect(body.filter).toBe('(reportedChannelId = "33") AND ((resolvedAt NOT EXISTS OR resolvedAt IS NULL))');
	});
});
