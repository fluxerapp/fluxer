// SPDX-License-Identifier: AGPL-3.0-or-later

import type {TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {Config, getConfig} from '@app/api/Config';
import {getPngDataUrl} from '@app/api/emoji/tests/EmojiTestUtils';
import {NoopPurgeQueue} from '@app/api/infrastructure/CachePurgeQueue';
import {
	getAssetDeletionQueue,
	getGuildRepository,
	getInstanceConfigRepository,
	getUserRepository,
} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createTestWorkerHelpers} from '@app/api/test/SyncTaskWorkerService';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import processAssetDeletionQueue from '@app/api/worker/tasks/ProcessAssetDeletionQueue';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '@app/api/worker/WorkerContext';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import type {InstanceConfigResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import sharp from 'sharp';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

type BrandingKind = 'icon' | 'logo' | 'sign_in_backdrop';

async function solidPng(background: string): Promise<string> {
	const buffer = await sharp({create: {width: 4, height: 4, channels: 4, background}})
		.png()
		.toBuffer();
	return getPngDataUrl(buffer.toString('base64'));
}

function storageKey(url: string | null): string {
	const match = /\/(branding(?:-backdrops)?\/0)\/(?:a_)?([0-9a-f]{8})\.[a-z0-9]+$/.exec(url ?? '');
	if (!match) throw new Error(`Not an uploaded branding URL: ${url}`);
	return `${match[1]}/${match[2]}`;
}

describe('branding image cleanup', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;
	let originalSelfHosted: boolean;

	beforeAll(async () => {
		originalSelfHosted = getConfig().instance.selfHosted;
		getConfig().instance.selfHosted = true;
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_CONFIG_VIEW,
			AdminACLs.INSTANCE_CONFIG_UPDATE,
		]);
		setWorkerDependenciesForTest({
			assetDeletionQueue: getAssetDeletionQueue(),
			purgeQueue: new NoopPurgeQueue(),
			storageService: harness.storageService,
			instanceConfigRepository: getInstanceConfigRepository(),
			userRepository: getUserRepository(),
			guildRepository: getGuildRepository(),
		});
	});

	afterEach(() => {
		clearWorkerDependencies();
	});

	afterAll(async () => {
		await harness.shutdown();
		getConfig().instance.selfHosted = originalSelfHosted;
	});

	async function upload(kind: BrandingKind, image: string | null): Promise<InstanceConfigResponse> {
		return await createBuilder<InstanceConfigResponse>(harness, admin.token)
			.post('/admin/instance/config/branding-assets')
			.body({kind, image})
			.execute();
	}

	async function patchBranding(branding: Record<string, string | null>): Promise<InstanceConfigResponse> {
		return await createBuilder<InstanceConfigResponse>(harness, admin.token)
			.patch('/admin/instance/config')
			.body({app_public: {branding}})
			.execute();
	}

	async function storedAfterCleanup(key: string): Promise<boolean> {
		await processAssetDeletionQueue({}, createTestWorkerHelpers());
		return harness.storageService.hasObject(Config.s3.buckets.cdn, key);
	}

	it('keeps the image while another branding field still uses it', async () => {
		const image = await solidPng('#ff0000');
		const shared = storageKey((await upload('icon', image)).app_public.branding.icon_url);
		expect(storageKey((await upload('logo', image)).app_public.branding.logo_url)).toBe(shared);

		await upload('icon', await solidPng('#00ff00'));
		expect(await storedAfterCleanup(shared)).toBe(true);

		await patchBranding({logo_url: null});
		expect(await storedAfterCleanup(shared)).toBe(false);
	});

	it('keeps the image when the same file is uploaded again', async () => {
		const image = await solidPng('#ff0000');
		const key = storageKey((await upload('icon', image)).app_public.branding.icon_url);
		await upload('icon', image);
		expect(await storedAfterCleanup(key)).toBe(true);
	});

	it('keeps the image when it is set again before the cleanup runs', async () => {
		const image = await solidPng('#ff0000');
		const key = storageKey((await upload('icon', image)).app_public.branding.icon_url);
		await upload('icon', null);
		await upload('logo', image);
		expect(await storedAfterCleanup(key)).toBe(true);
	});
});
