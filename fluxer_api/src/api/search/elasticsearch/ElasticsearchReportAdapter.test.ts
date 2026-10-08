// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ReportSearchFilters} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';
import {
	ElasticsearchReportAdapter,
	type ElasticsearchReportAdapterOptions,
} from '@pkgs/elasticsearch_search/src/adapters/ElasticsearchReportAdapter';
import {ELASTICSEARCH_INDEX_DEFINITIONS} from '@pkgs/elasticsearch_search/src/ElasticsearchIndexDefinitions';
import {describe, expect, it} from 'vitest';

interface RecordedSearchRequest {
	index?: unknown;
	query?: {
		bool?: {
			filter?: unknown;
		};
	};
}

class FakeElasticsearchClient {
	readonly searches: Array<RecordedSearchRequest> = [];

	readonly indices = {
		exists: async (): Promise<boolean> => true,
		putMapping: async (): Promise<void> => {},
	};

	async search(request: RecordedSearchRequest): Promise<unknown> {
		this.searches.push(request);
		return {hits: {total: {value: 1}, hits: [{_id: 'report-1', _source: {id: 'report-1'}, sort: [1, 'report-1']}]}};
	}
}

async function searchFilter(filters: ReportSearchFilters): Promise<unknown> {
	const client = new FakeElasticsearchClient();
	const adapter = new ElasticsearchReportAdapter({
		client: client as unknown as ElasticsearchReportAdapterOptions['client'],
	});
	await adapter.initialize();
	await adapter.search('', filters, {limit: 10, offset: 0});
	expect(client.searches).toHaveLength(1);
	expect(client.searches[0].index).toBe('reports');
	return client.searches[0].query?.bool?.filter;
}

describe('ElasticsearchReportAdapter', () => {
	it('maps the reported channel as a keyword', () => {
		expect(ELASTICSEARCH_INDEX_DEFINITIONS.reports.mappings.properties['reportedChannelId']).toEqual({
			type: 'keyword',
		});
	});

	it('emits a reported channel term', async () => {
		expect(await searchFilter({reportedChannelId: '1234567890'})).toEqual([{term: {reportedChannelId: '1234567890'}}]);
	});

	it('combines the reported channel term with the other report filters', async () => {
		expect(
			await searchFilter({
				status: 0,
				reportType: 0,
				reportedGuildId: '11',
				reportedMessageId: '22',
				reportedChannelId: '33',
				guildContextId: '44',
			}),
		).toEqual([
			{term: {status: 0}},
			{term: {reportType: 0}},
			{term: {reportedGuildId: '11'}},
			{term: {reportedMessageId: '22'}},
			{term: {reportedChannelId: '33'}},
			{term: {guildContextId: '44'}},
		]);
	});

	it('omits the reported channel term when no channel is given', async () => {
		expect(await searchFilter({status: 1})).toEqual([{term: {status: 1}}]);
	});
});
