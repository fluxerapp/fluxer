// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAuthHarness} from '@app/api/auth/tests/AuthTestUtils';
import {getConfig} from '@app/api/Config';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {server} from '@app/api/test/msw/server';
import {createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {parseInstanceDiscoveryDocument} from '@fluxer/instance_bootstrap/src/Discovery';
import type {WellKnownFluxerResponse} from '@fluxer/schema/src/domains/instance/InstanceSchemas';
import {HttpResponse, http} from 'msw';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const INSTATUS_SUMMARY = {page: {name: 'Status', status: 'UP'}, activeIncidents: [], activeMaintenances: []};

describe('discovery fields for self-hosted clients', () => {
	let harness: ApiTestHarness;

	async function withSelfHostedStatusPage(statusPageUrl: string): Promise<WellKnownFluxerResponse> {
		const config = getConfig().instance;
		const originalSelfHosted = config.selfHosted;
		config.selfHosted = true;
		try {
			await getInstanceConfigRepository().setAppPublicConfig({branding: {status_page_url: statusPageUrl}});
			return await createBuilderWithoutAuth<WellKnownFluxerResponse>(harness).get('/.well-known/fluxer').execute();
		} finally {
			config.selfHosted = originalSelfHosted;
		}
	}

	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterEach(() => {
		getInstanceConfigRepository().clearCacheForTesting();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	it('keeps friend requests off on an instance that turned direct messages off before the split', async () => {
		await getInstanceConfigRepository().setConfig(
			'instance_policy_config',
			JSON.stringify({direct_messages_disabled: true}),
		);
		const discovery = await createBuilderWithoutAuth<WellKnownFluxerResponse>(harness)
			.get('/.well-known/fluxer')
			.execute();
		expect(discovery.community).toMatchObject({direct_messages_disabled: true, friends_disabled: true});
	});

	it('detects an existing Instatus status page on a self-hosted instance with no extra setting', async () => {
		const statusPageUrl = 'https://93.184.215.14';
		server.use(
			http.get(`${statusPageUrl}/summary.json`, () =>
				HttpResponse.json(INSTATUS_SUMMARY, {headers: {'access-control-allow-origin': '*'}}),
			),
		);
		const discovery = await withSelfHostedStatusPage(statusPageUrl);
		expect(discovery.app_public.branding).toMatchObject({
			status_page_url: statusPageUrl,
			status_page_instatus: true,
		});
		expect(parseInstanceDiscoveryDocument(discovery).appPublic?.branding.status_page_instatus).toBe(true);
	});

	it('tells clients not to read incidents from a status page that is not Instatus', async () => {
		const statusPageUrl = 'https://93.184.215.15';
		server.use(
			http.get(`${statusPageUrl}/summary.json`, () => new HttpResponse('<!doctype html><html></html>', {status: 404})),
		);
		const discovery = await withSelfHostedStatusPage(statusPageUrl);
		expect(discovery.app_public.branding).toMatchObject({
			status_page_url: statusPageUrl,
			status_page_instatus: false,
		});
	});

	it('fills friends_disabled from the direct message flag for an older server', () => {
		const document = parseInstanceDiscoveryDocument({
			api_code_version: 1,
			endpoints: {api: 'https://chat.example.com/api', gateway: 'wss://chat.example.com/gateway'},
			community: {single_community: false, single_community_guild_id: null, direct_messages_disabled: true},
		});
		expect(document.community?.friends_disabled).toBe(true);
		expect(document.notice).toBeNull();
	});
});
