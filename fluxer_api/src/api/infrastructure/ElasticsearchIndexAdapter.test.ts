// SPDX-License-Identifier: AGPL-3.0-or-later

import {createServer} from 'node:http';
import type {Socket} from 'node:net';
import {Client} from '@elastic/elasticsearch';
import type {SortCombinations} from '@elastic/elasticsearch/lib/api/types';
import {HttpStatus} from '@fluxer/constants/src/HttpConstants';
import type {SearchOptions} from '@fluxer/schema/src/contracts/search/SearchAdapterTypes';
import {ElasticsearchIndexAdapter} from '@pkgs/elasticsearch_search/src/adapters/ElasticsearchIndexAdapter';
import type {ElasticsearchFilter} from '@pkgs/elasticsearch_search/src/ElasticsearchFilterUtils';
import {afterEach, describe, expect, it} from 'vitest';

interface SearchDocument {
	id: string;
	content: string;
}

interface SearchFilters {
	clauses: Array<ElasticsearchFilter | undefined>;
	sort: Array<SortCombinations>;
}

interface SearchConfiguration {
	fuzziness?: string;
	minimumShouldMatch?: string;
	searchType?: 'best_fields' | 'bool_prefix';
	operator?: 'and' | 'or';
}

interface RecordedRequest {
	method: string | undefined;
	path: string;
	query: string;
	body: unknown;
}

interface QueryCase extends SearchConfiguration {
	name: string;
	query: string;
	must: Array<Record<string, unknown>>;
}

interface PaginationCase {
	name: string;
	options: SearchOptions;
	pagination: Record<string, unknown>;
}

const clients = new Set<Client>();
const endpoints = new Set<{close(): Promise<void>}>();

async function createSearchFixture(configuration: SearchConfiguration) {
	const requests: Array<RecordedRequest> = [];
	const pending = new Set<Promise<void>>();
	const socketClosures: Array<Promise<void>> = [];
	const sockets = new Set<Socket>();
	const protocolErrors: Array<unknown> = [];
	const endpoint = createServer((request, response) => {
		const operation = (async () => {
			request.setEncoding('utf8');
			let body = '';
			for await (const chunk of request) {
				body += chunk;
			}
			const url = new URL(request.url ?? '', 'http://127.0.0.1');
			const parsedBody: unknown = body.length > 0 ? JSON.parse(body) : null;
			requests.push({method: request.method, path: url.pathname, query: url.search, body: parsedBody});
			response.setHeader('x-elastic-product', 'Elasticsearch');
			response.setHeader('content-type', 'application/json');
			if (request.method === 'HEAD' && url.pathname === '/messages') {
				response.end();
				return;
			}
			if (request.method === 'PUT' && url.pathname === '/messages/_mapping') {
				response.end('{"acknowledged":true}');
				return;
			}
			if (request.method === 'POST' && url.pathname === '/messages/_search') {
				response.end(
					JSON.stringify({
						took: 1,
						timed_out: false,
						_shards: {total: 1, successful: 1, skipped: 0, failed: 0},
						hits: {
							total: {value: 3, relation: 'eq'},
							max_score: null,
							hits: [
								{
									_index: 'messages',
									_id: 'event-42',
									_score: null,
									_source: {id: 'source-id', content: 'A search result'},
									sort: [17, 'event-42'],
								},
							],
						},
					}),
				);
				return;
			}
			protocolErrors.push(new Error(`Unexpected request: ${request.method} ${request.url}`));
			response.statusCode = HttpStatus.NOT_FOUND;
			response.end('{"error":"unexpected request"}');
		})();
		pending.add(operation);
		void operation
			.catch((error: unknown) => {
				protocolErrors.push(error);
				response.destroy(error instanceof Error ? error : new Error(String(error)));
			})
			.finally(() => pending.delete(operation));
	});
	endpoint.on('connection', (socket) => {
		sockets.add(socket);
		socketClosures.push(
			new Promise<void>((resolve) => {
				socket.once('close', () => {
					sockets.delete(socket);
					resolve();
				});
			}),
		);
	});
	await new Promise<void>((resolve, reject) => {
		endpoint.once('error', reject);
		endpoint.listen(0, '127.0.0.1', () => {
			endpoint.removeListener('error', reject);
			resolve();
		});
	});
	endpoints.add({
		async close() {
			await Promise.allSettled(pending);
			const closed = new Promise<void>((resolve, reject) => {
				endpoint.close((error) => {
					if (error) {
						reject(error);
						return;
					}
					resolve();
				});
			});
			for (const socket of sockets) {
				socket.destroy();
			}
			try {
				await closed;
			} finally {
				await Promise.all(socketClosures);
			}
			expect(protocolErrors).toEqual([]);
		},
	});
	const address = endpoint.address();
	if (address === null || typeof address === 'string') {
		throw new Error('Search fixture did not bind a loopback port');
	}
	const client = new Client({node: `http://127.0.0.1:${address.port}`, maxRetries: 0});
	clients.add(client);
	const adapter = new ElasticsearchIndexAdapter<SearchFilters, SearchDocument>({
		client,
		index: {indexName: 'messages', mappings: {properties: {id: {type: 'keyword'}, content: {type: 'text'}}}},
		searchableFields: ['content^2', 'id'],
		...configuration,
		buildFilters: (filters) => filters.clauses,
		buildSort: (filters) => filters.sort,
	});
	return {adapter, requests};
}

afterEach(async () => {
	try {
		const clientResults = await Promise.allSettled([...clients].map((client) => client.close()));
		const endpointResults = await Promise.allSettled([...endpoints].map((endpoint) => endpoint.close()));
		for (const result of [...clientResults, ...endpointResults]) {
			if (result.status === 'rejected') {
				throw result.reason;
			}
		}
	} finally {
		clients.clear();
		endpoints.clear();
	}
});

const queryCases: Array<QueryCase> = [
	{name: 'empty query, fuzziness omitted, minimum omitted', query: '', must: [{match_all: {}}]},
	{name: "empty query, fuzziness omitted, minimum '2'", query: '', minimumShouldMatch: '2', must: [{match_all: {}}]},
	{name: "empty query, fuzziness omitted, minimum ''", query: '', minimumShouldMatch: '', must: [{match_all: {}}]},
	{name: "empty query, fuzziness 'AUTO', minimum omitted", query: '', fuzziness: 'AUTO', must: [{match_all: {}}]},
	{
		name: "empty query, fuzziness 'AUTO', minimum '2'",
		query: '',
		fuzziness: 'AUTO',
		minimumShouldMatch: '2',
		must: [{match_all: {}}],
	},
	{
		name: "empty query, fuzziness 'AUTO', minimum ''",
		query: '',
		fuzziness: 'AUTO',
		minimumShouldMatch: '',
		must: [{match_all: {}}],
	},
	{name: "empty query, fuzziness '', minimum omitted", query: '', fuzziness: '', must: [{match_all: {}}]},
	{
		name: "empty query, fuzziness '', minimum '2'",
		query: '',
		fuzziness: '',
		minimumShouldMatch: '2',
		must: [{match_all: {}}],
	},
	{
		name: "empty query, fuzziness '', minimum ''",
		query: '',
		fuzziness: '',
		minimumShouldMatch: '',
		must: [{match_all: {}}],
	},
	{
		name: 'text query, fuzziness omitted, minimum omitted',
		query: 'needle',
		must: [{multi_match: {query: 'needle', fields: ['content^2', 'id'], type: 'best_fields', operator: 'or'}}],
	},
	{
		name: 'text query with every word required',
		query: 'good idea',
		operator: 'and',
		must: [{multi_match: {query: 'good idea', fields: ['content^2', 'id'], type: 'best_fields', operator: 'and'}}],
	},
	{
		name: "text query, fuzziness omitted, minimum '2'",
		query: 'needle',
		minimumShouldMatch: '2',
		must: [
			{
				multi_match: {
					query: 'needle',
					fields: ['content^2', 'id'],
					type: 'best_fields',
					operator: 'or',
					minimum_should_match: '2',
				},
			},
		],
	},
	{
		name: "text query, fuzziness omitted, minimum ''",
		query: 'needle',
		minimumShouldMatch: '',
		must: [
			{
				multi_match: {
					query: 'needle',
					fields: ['content^2', 'id'],
					type: 'best_fields',
					operator: 'or',
					minimum_should_match: '',
				},
			},
		],
	},
	{
		name: "text query, fuzziness 'AUTO', minimum omitted",
		query: 'needle',
		fuzziness: 'AUTO',
		must: [
			{
				multi_match: {
					query: 'needle',
					fields: ['content^2', 'id'],
					type: 'best_fields',
					operator: 'or',
					fuzziness: 'AUTO',
				},
			},
		],
	},
	{
		name: "text query, fuzziness 'AUTO', minimum '2'",
		query: 'needle',
		fuzziness: 'AUTO',
		minimumShouldMatch: '2',
		must: [
			{
				multi_match: {
					query: 'needle',
					fields: ['content^2', 'id'],
					type: 'best_fields',
					operator: 'or',
					fuzziness: 'AUTO',
					minimum_should_match: '2',
				},
			},
		],
	},
	{
		name: "text query, fuzziness 'AUTO', minimum ''",
		query: 'needle',
		fuzziness: 'AUTO',
		minimumShouldMatch: '',
		must: [
			{
				multi_match: {
					query: 'needle',
					fields: ['content^2', 'id'],
					type: 'best_fields',
					operator: 'or',
					fuzziness: 'AUTO',
					minimum_should_match: '',
				},
			},
		],
	},
	{
		name: "text query, fuzziness '', minimum omitted",
		query: 'needle',
		fuzziness: '',
		must: [
			{multi_match: {query: 'needle', fields: ['content^2', 'id'], type: 'best_fields', operator: 'or', fuzziness: ''}},
		],
	},
	{
		name: "text query, fuzziness '', minimum '2'",
		query: 'needle',
		fuzziness: '',
		minimumShouldMatch: '2',
		must: [
			{
				multi_match: {
					query: 'needle',
					fields: ['content^2', 'id'],
					type: 'best_fields',
					operator: 'or',
					fuzziness: '',
					minimum_should_match: '2',
				},
			},
		],
	},
	{
		name: "text query, fuzziness '', minimum ''",
		query: 'needle',
		fuzziness: '',
		minimumShouldMatch: '',
		must: [
			{
				multi_match: {
					query: 'needle',
					fields: ['content^2', 'id'],
					type: 'best_fields',
					operator: 'or',
					fuzziness: '',
					minimum_should_match: '',
				},
			},
		],
	},
	{
		name: 'whitespace is a text query',
		query: ' ',
		must: [{multi_match: {query: ' ', fields: ['content^2', 'id'], type: 'best_fields', operator: 'or'}}],
	},
	{
		name: 'explicit prefix search type',
		query: 'needle',
		searchType: 'bool_prefix',
		fuzziness: 'AUTO',
		minimumShouldMatch: '2',
		must: [
			{
				multi_match: {
					query: 'needle',
					fields: ['content^2', 'id'],
					type: 'bool_prefix',
					operator: 'or',
					fuzziness: 'AUTO',
					minimum_should_match: '2',
				},
			},
		],
	},
];

const paginationCases: Array<PaginationCase> = [
	{name: 'explicit offset', options: {limit: 2, offset: 7}, pagination: {size: 2, from: 7}},
	{name: 'page offset', options: {page: 2, hitsPerPage: 3}, pagination: {size: 3, from: 3}},
	{
		name: 'cursor',
		options: {limit: 2, offset: 7, cursor: ['12', 'previous']},
		pagination: {size: 2, search_after: ['12', 'previous']},
	},
];

describe('Elasticsearch search HTTP requests', () => {
	it.each(queryCases)('$name', async ({query, must, name: _name, ...configuration}) => {
		const {adapter, requests} = await createSearchFixture(configuration);
		expect(adapter.isAvailable()).toBe(false);
		await adapter.initialize();
		expect(adapter.isAvailable()).toBe(true);
		expect(await adapter.search(query, {clauses: [], sort: []})).toEqual({
			hits: [{id: 'event-42', content: 'A search result'}],
			total: 3,
			cursor: ['17', 'event-42'],
		});
		expect(requests).toEqual([
			{method: 'HEAD', path: '/messages', query: '', body: null},
			{
				method: 'PUT',
				path: '/messages/_mapping',
				query: '',
				body: {properties: {id: {type: 'keyword'}, content: {type: 'text'}}},
			},
			{
				method: 'POST',
				path: '/messages/_search',
				query: '',
				body: {
					query: {bool: {must}},
					sort: [{id: {order: 'desc'}}],
					size: 25,
					track_total_hits: true,
					from: 0,
				},
			},
		]);
		await adapter.shutdown();
		expect(adapter.isAvailable()).toBe(false);
		await expect(adapter.search('after shutdown', {clauses: [], sort: []})).rejects.toThrow(
			'Elasticsearch adapter not initialised',
		);
		expect(requests).toHaveLength(3);
	});

	it.each(paginationCases)('sends filters and sorting with $name', async ({options, pagination}) => {
		const {adapter, requests} = await createSearchFixture({});
		await adapter.initialize();
		expect(
			await adapter.search(
				'needle',
				{
					clauses: [undefined, {term: {channelId: 'channel-7'}}],
					sort: [{createdAt: {order: 'asc'}}],
				},
				options,
			),
		).toEqual({
			hits: [{id: 'event-42', content: 'A search result'}],
			total: 3,
			cursor: ['17', 'event-42'],
		});
		expect(requests).toEqual([
			{method: 'HEAD', path: '/messages', query: '', body: null},
			{
				method: 'PUT',
				path: '/messages/_mapping',
				query: '',
				body: {properties: {id: {type: 'keyword'}, content: {type: 'text'}}},
			},
			{
				method: 'POST',
				path: '/messages/_search',
				query: '',
				body: {
					query: {
						bool: {
							must: [
								{multi_match: {query: 'needle', fields: ['content^2', 'id'], type: 'best_fields', operator: 'or'}},
							],
							filter: [{term: {channelId: 'channel-7'}}],
						},
					},
					sort: [{createdAt: {order: 'asc'}}, {id: {order: 'desc'}}],
					track_total_hits: true,
					...pagination,
				},
			},
		]);
		await adapter.shutdown();
	});
});
