// SPDX-License-Identifier: AGPL-3.0-or-later

import type {TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {getConfig} from '@app/api/Config';
import {setCachedConfiguredLegalUrls} from '@app/api/instance/LegalUrls';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {InstanceConfigResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import type {WellKnownFluxerResponse} from '@fluxer/schema/src/domains/instance/InstanceSchemas';
import type {ReportFlowResponse} from '@fluxer/schema/src/domains/report/ReportFlowSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const RULES_URL = 'https://rules.example.org/community';

function useInstanceHarness(selfHosted: boolean) {
	const context: {harness: ApiTestHarness} = {harness: undefined as unknown as ApiTestHarness};
	let originalSelfHosted: boolean;
	beforeAll(async () => {
		originalSelfHosted = getConfig().instance.selfHosted;
		getConfig().instance.selfHosted = selfHosted;
		context.harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await context.harness.reset();
		setCachedConfiguredLegalUrls({terms_url: null, guidelines_url: null});
	});
	afterAll(async () => {
		await context.harness.shutdown();
		getConfig().instance.selfHosted = originalSelfHosted;
		setCachedConfiguredLegalUrls({terms_url: null, guidelines_url: null});
	});
	return context;
}

async function createAdmin(harness: ApiTestHarness): Promise<TestAccount> {
	return await setUserACLs(harness, await createTestAccount(harness), [
		AdminACLs.AUTHENTICATE,
		AdminACLs.INSTANCE_CONFIG_VIEW,
		AdminACLs.INSTANCE_CONFIG_UPDATE,
	]);
}

function patchLegal(harness: ApiTestHarness, admin: TestAccount, legal: Record<string, unknown>) {
	return createBuilder<InstanceConfigResponse>(harness, admin.token)
		.patch('/admin/instance/config')
		.body({app_public: {legal}});
}

function getUserFlow(harness: ApiTestHarness): Promise<ReportFlowResponse> {
	return createBuilderWithoutAuth<ReportFlowResponse>(harness)
		.get('/reports/flows/user?surface=in_app&locale=en-US')
		.expect(HTTP_STATUS.OK)
		.execute();
}

describe('community guidelines URL on a self-hosted instance', () => {
	const context = useInstanceHarness(true);

	it('is unset by default, so no guidelines link is offered', async () => {
		const admin = await createAdmin(context.harness);
		const config = await createBuilder<InstanceConfigResponse>(context.harness, admin.token)
			.get('/admin/instance/config')
			.execute();
		expect(config.app_public.legal.guidelines_url).toBeNull();
		const flow = await getUserFlow(context.harness);
		expect(flow.guidelines_url).toBeNull();
		expect(flow.screens[0].options).toEqual([]);
	});

	it('round-trips through the admin API and reaches discovery and the report flow', async () => {
		const admin = await createAdmin(context.harness);
		const before = await getUserFlow(context.harness);
		const updated = await patchLegal(context.harness, admin, {guidelines_url: ` ${RULES_URL} `}).execute();
		expect(updated.app_public.legal).toEqual({terms_url: null, privacy_url: null, guidelines_url: RULES_URL});
		const reread = await createBuilder<InstanceConfigResponse>(context.harness, admin.token)
			.get('/admin/instance/config')
			.execute();
		expect(reread.app_public.legal.guidelines_url).toBe(RULES_URL);
		const discovery = await createBuilderWithoutAuth<WellKnownFluxerResponse>(context.harness)
			.get('/.well-known/fluxer')
			.execute();
		expect(discovery.app_public.legal.guidelines_url).toBe(RULES_URL);
		const flow = await getUserFlow(context.harness);
		expect(flow.guidelines_url).toBe(RULES_URL);
		expect(flow.screens[0].options.map((option) => option.outcome.url)).toEqual([RULES_URL]);
		expect(flow.revision_hash).not.toBe(before.revision_hash);
		expect(flow.screens.find((screen) => screen.id === 'root_user')?.options.map((option) => option.id)).not.toContain(
			'dsa',
		);
		await patchLegal(context.harness, admin, {terms_url: 'https://rules.example.org/tos'}).execute();
		expect((await getUserFlow(context.harness)).guidelines_url).toBe(RULES_URL);
		const cleared = await patchLegal(context.harness, admin, {guidelines_url: null}).execute();
		expect(cleared.app_public.legal.guidelines_url).toBeNull();
		expect(cleared.app_public.legal.terms_url).toBe('https://rules.example.org/tos');
		const after = await getUserFlow(context.harness);
		expect(after.guidelines_url).toBeNull();
		expect(after.revision_hash).toBe(before.revision_hash);
	});

	it('clears every legal URL at once, and an empty string clears like null', async () => {
		const admin = await createAdmin(context.harness);
		await patchLegal(context.harness, admin, {
			terms_url: 'https://rules.example.org/tos',
			privacy_url: 'https://rules.example.org/privacy',
			guidelines_url: RULES_URL,
		}).execute();
		const cleared = await patchLegal(context.harness, admin, {
			terms_url: null,
			privacy_url: '',
			guidelines_url: '',
		}).execute();
		expect(cleared.app_public.legal).toEqual({terms_url: null, privacy_url: null, guidelines_url: null});
		expect((await getUserFlow(context.harness)).guidelines_url).toBeNull();
	});

	it.each(['rules.example.org', 'javascript:alert(1)', 'ftp://rules.example.org', '/guidelines'])(
		'rejects %j as a guidelines URL',
		async (value) => {
			const admin = await createAdmin(context.harness);
			await patchLegal(context.harness, admin, {guidelines_url: value})
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_FORM_BODY)
				.execute();
		},
	);
});

describe('community guidelines URL on a hosted instance', () => {
	const context = useInstanceHarness(false);

	it('defaults to the marketing page and can be overridden', async () => {
		const admin = await createAdmin(context.harness);
		const hosted = await getUserFlow(context.harness);
		expect(hosted.guidelines_url).toBe(`${getConfig().endpoints.marketing}/guidelines`);
		await patchLegal(context.harness, admin, {guidelines_url: RULES_URL}).execute();
		const flow = await getUserFlow(context.harness);
		expect(flow.guidelines_url).toBe(RULES_URL);
		expect(flow.revision_hash).toBe(hosted.revision_hash);
	});
});
