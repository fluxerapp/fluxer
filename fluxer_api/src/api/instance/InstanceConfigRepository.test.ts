// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {createUserID} from '@app/api/BrandedTypes';
import {getConfig} from '@app/api/Config';
import type {CassandraQueryExecutorForTesting} from '@app/api/database/CassandraQueryExecution';
import {setCassandraQueryExecutorForTesting} from '@app/api/database/CassandraQueryExecution';
import {
	INSTANCE_CONFIG_WRITE_ATTEMPTS,
	InstanceConfigRepository,
	InstanceConfigWriteConflictError,
} from '@app/api/instance/InstanceConfigRepository';
import {ensureSetupToken, isSetupTokenEnforced} from '@app/api/instance/SetupToken';
import {InstanceConfigWriteRaceExecutor} from '@app/api/instance/tests/InstanceConfigWriteRaceExecutor';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {MockKVProvider} from '@pkgs/kv_client/src/testing/MockKVProvider';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

const REGISTRATION_CONFIG_KEY = 'registration_config';
const REGISTRATION_URLS_KEY = 'registration_urls';
const REGISTRATION_PENDING_APPROVALS_KEY = 'registration_pending_approvals';
const SETUP_TOKEN_HASH_KEY = 'setup_token_hash';
const ENV_SETUP_TOKEN = 'setup-token-for-tests-0123456789-abcdefghijklmnopqrstuvwxyz';
describe('InstanceConfigRepository', () => {
	const repositories: Array<InstanceConfigRepository> = [];

	afterEach(() => {
		for (const repository of repositories) {
			repository.shutdown();
		}
		repositories.length = 0;
	});

	function createRepository(kvProvider: MockKVProvider): InstanceConfigRepository {
		const repository = new InstanceConfigRepository(kvProvider);
		repositories.push(repository);
		return repository;
	}

	it('clears an invalid allowed domain list only while SSO is disabled', async () => {
		const executor = new InMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		const cleared = await repository.setSsoConfig({enabled: false, allowedEmailDomains: ['nope@example.com']});
		expect(cleared.allowedEmailDomains).toEqual([]);

		await expect(repository.setSsoConfig({enabled: true, allowedEmailDomains: ['nope@example.com']})).rejects.toThrow();
	});

	it('keeps an all-invalid stored allowed domain list non-empty so SSO still rejects every domain', async () => {
		const executor = new InMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		await repository.setConfig('sso_allowed_domains', JSON.stringify(['nope@example.com', 'bad/domain']));

		const domains = (await repository.getSsoConfig()).allowedEmailDomains;
		expect(domains.length).toBeGreaterThan(0);
		expect(domains).not.toContain('example.com');
	});
});

function describeConcurrentInstanceConfigWrites(prepareBase: () => Promise<CassandraQueryExecutorForTesting>): void {
	const pods: Array<InstanceConfigRepository> = [];
	let executor: InstanceConfigWriteRaceExecutor;

	beforeEach(async () => {
		executor = new InstanceConfigWriteRaceExecutor(await prepareBase());
		setCassandraQueryExecutorForTesting(executor);
	});

	afterEach(async () => {
		await Promise.all(pods.map((pod) => pod.shutdown()));
		pods.length = 0;
	});

	function createPod(): InstanceConfigRepository {
		const pod = new InstanceConfigRepository(new MockKVProvider());
		pods.push(pod);
		return pod;
	}

	async function readStoredRegistrationConfig(): Promise<unknown> {
		const raw = await executor.readDirectly(REGISTRATION_CONFIG_KEY);
		return raw === null ? null : JSON.parse(raw);
	}

	it('applies two concurrent patches on top of each other instead of dropping one', async () => {
		const first = createPod();
		const second = createPod();
		await first.setRegistrationConfig({mode: 'open', admin_registration_urls_enabled: true});
		await second.getRegistrationConfig();
		executor.watch(REGISTRATION_CONFIG_KEY);
		executor.pauseWritesUntil(2);

		await Promise.all([
			first.setRegistrationConfig({mode: 'closed'}),
			second.setRegistrationConfig({admin_registration_urls_enabled: false}),
		]);

		expect(executor.events.filter((event) => event === 'write rejected')).toHaveLength(1);
		expect(executor.events.filter((event) => event === 'write')).toHaveLength(2);
		expect(await readStoredRegistrationConfig()).toEqual({
			mode: 'closed',
			admin_registration_urls_enabled: false,
			allowed_email_domains: [],
		});
	});

	it('lets one of two concurrent first writes create the config and applies the other on top', async () => {
		const first = createPod();
		const second = createPod();
		await first.getRegistrationConfig();
		await second.getRegistrationConfig();
		executor.watch(REGISTRATION_CONFIG_KEY);
		executor.pauseWritesUntil(2);

		await Promise.all([
			first.setRegistrationConfig({mode: 'closed'}),
			second.setRegistrationConfig({admin_registration_urls_enabled: false}),
		]);

		expect(executor.events.filter((event) => event === 'write rejected')).toHaveLength(1);
		expect(executor.events.filter((event) => event === 'write')).toHaveLength(2);
		expect(await readStoredRegistrationConfig()).toEqual({
			mode: 'closed',
			admin_registration_urls_enabled: false,
			allowed_email_domains: [],
		});
	});

	it('re-reads the database, not its stale cache, when a concurrent write lands between its read and its write', async () => {
		const stale = createPod();
		const other = createPod();
		await stale.setRegistrationConfig({mode: 'open', admin_registration_urls_enabled: true});
		await stale.getRegistrationConfig();
		await other.setRegistrationConfig({mode: 'approval'});
		expect(await stale.getRegistrationConfig()).toEqual({
			mode: 'open',
			admin_registration_urls_enabled: true,
			allowed_email_domains: [],
		});
		executor.watch(REGISTRATION_CONFIG_KEY);
		let competed = false;
		executor.competeBeforeEachWrite(async () => {
			if (competed) return;
			competed = true;
			await executor.writeDirectly(
				REGISTRATION_CONFIG_KEY,
				JSON.stringify({mode: 'approval', admin_registration_urls_enabled: false}),
			);
		});

		await stale.setRegistrationConfig({mode: 'closed'});

		expect(executor.events).toEqual(['read', 'write rejected', 'read', 'write']);
		expect(await readStoredRegistrationConfig()).toEqual({
			mode: 'closed',
			admin_registration_urls_enabled: false,
			allowed_email_domains: [],
		});
	});

	it('fails loudly and writes nothing once every attempt has lost the race', async () => {
		const pod = createPod();
		await pod.setRegistrationConfig({mode: 'open', admin_registration_urls_enabled: true});
		executor.watch(REGISTRATION_CONFIG_KEY);
		let competingWrites = 0;
		executor.competeBeforeEachWrite(async () => {
			competingWrites++;
			await executor.writeDirectly(
				REGISTRATION_CONFIG_KEY,
				JSON.stringify({mode: 'approval', admin_registration_urls_enabled: competingWrites % 2 === 0}),
			);
		});

		const write = pod.setRegistrationConfig({mode: 'closed'});

		await expect(write).rejects.toBeInstanceOf(InstanceConfigWriteConflictError);
		await expect(write).rejects.toMatchObject({
			status: 409,
			code: 'CONFLICT',
			message: expect.stringContaining(REGISTRATION_CONFIG_KEY),
		});
		expect(executor.events.filter((event) => event === 'write rejected')).toHaveLength(INSTANCE_CONFIG_WRITE_ATTEMPTS);
		expect(executor.events).not.toContain('write');
		expect(await readStoredRegistrationConfig()).toEqual({
			mode: 'approval',
			admin_registration_urls_enabled: INSTANCE_CONFIG_WRITE_ATTEMPTS % 2 === 0,
		});
	});

	it('keeps a pending registration another pod added while this pod held a stale list', async () => {
		const first = createPod();
		const second = createPod();
		await first.getPendingRegistrations();
		await second.getPendingRegistrations();

		await first.addPendingRegistration(pendingRegistration('1400000000000000011'));
		await second.addPendingRegistration(pendingRegistration('1400000000000000012'));

		const listed = await createPod().getPendingRegistrations();
		expect(listed.map((entry) => entry.user_id)).toEqual(['1400000000000000011', '1400000000000000012']);
	});

	it('keeps a pending registration another pod stored and removes it once decided', async () => {
		const pod = createPod();
		await executor.writeDirectly(
			REGISTRATION_PENDING_APPROVALS_KEY,
			JSON.stringify([pendingRegistration('1400000000000000021')]),
		);
		await pod.addPendingRegistration(pendingRegistration('1400000000000000022'));

		expect((await createPod().getPendingRegistrations()).map((entry) => entry.user_id)).toEqual([
			'1400000000000000021',
			'1400000000000000022',
		]);

		await pod.removePendingRegistration('1400000000000000021');
		await pod.removePendingRegistration('1400000000000000022');

		expect(await createPod().getPendingRegistrations()).toEqual([]);
		expect(await executor.readDirectly(REGISTRATION_PENDING_APPROVALS_KEY)).toBe('[]');
	});

	it('keeps a registration URL another pod created while this pod held a stale list', async () => {
		const first = createPod();
		const second = createPod();
		await first.getRegistrationUrls();
		await second.getRegistrationUrls();

		const created = [
			await first.createRegistrationUrl(registrationUrlParams(null)),
			await second.createRegistrationUrl(registrationUrlParams(null)),
		];

		const listed = await createPod().getRegistrationUrlsForAdmin();
		expect(listed.map((url) => url.id).toSorted()).toEqual(created.map((entry) => entry.registrationUrl.id).toSorted());
	});

	it('refuses a registration URL another pod revoked while this pod held a stale list', async () => {
		const admin = createPod();
		const signup = createPod();
		const {code, registrationUrl} = await admin.createRegistrationUrl(registrationUrlParams(null));
		expect(await signup.resolveRegistrationUrlCode(code)).not.toBeNull();

		await admin.revokeOrRemoveRegistrationUrl(registrationUrl.id);

		await expect(signup.resolveRegistrationUrlCode(code)).resolves.toBeNull();
		await expect(signup.claimRegistrationUrlUse(registrationUrl.id, '1400000000000000501')).resolves.toBeNull();
		const [listed] = await createPod().getRegistrationUrlsForAdmin();
		expect(listed?.use_count).toBe(0);
	});

	it('admits a registration URL another pod created while this pod held a stale list', async () => {
		const admin = createPod();
		const signup = createPod();
		await signup.getRegistrationUrls();

		const {code, registrationUrl} = await admin.createRegistrationUrl(registrationUrlParams(1));

		await expect(signup.resolveRegistrationUrlCode(code)).resolves.toMatchObject({
			id: registrationUrl.id,
			approval_required: false,
		});
		await expect(signup.claimRegistrationUrlUse(registrationUrl.id, '1400000000000000601')).resolves.not.toBeNull();
		const [listed] = await createPod().getRegistrationUrlsForAdmin();
		expect(listed?.use_count).toBe(1);
	});

	it('never seats more signups than max_uses when pods claim the same registration URL at once', async () => {
		const pods = [createPod(), createPod(), createPod()];
		const {code} = await pods[0]!.createRegistrationUrl(registrationUrlParams(2));
		const registrationUrl = await pods[0]!.resolveRegistrationUrlCode(code);
		if (registrationUrl === null) throw new Error('registration URL did not resolve');

		const claims = await Promise.all(
			Array.from({length: 6}, (_, index) =>
				pods[index % pods.length]!.claimRegistrationUrlUse(registrationUrl.id, `14000000000000001${index}0`),
			),
		);

		expect(claims.filter((claim) => claim !== null)).toHaveLength(2);
		const [listed] = await createPod().getRegistrationUrlsForAdmin();
		expect(listed?.use_count).toBe(2);
		await expect(createPod().resolveRegistrationUrlCode(code)).resolves.toBeNull();
	});

	it('refuses a claim whose retry finds the registration URL exhausted, rather than reporting the lost attempt', async () => {
		const pod = createPod();
		const {code, registrationUrl} = await pod.createRegistrationUrl(registrationUrlParams(1));
		expect(await pod.resolveRegistrationUrlCode(code)).not.toBeNull();
		executor.watch(REGISTRATION_URLS_KEY);
		let competed = false;
		executor.competeBeforeEachWrite(async () => {
			if (competed) return;
			competed = true;
			const stored = JSON.parse((await executor.readDirectly(REGISTRATION_URLS_KEY)) ?? 'null') as Array<
				Record<string, unknown>
			>;
			await executor.writeDirectly(
				REGISTRATION_URLS_KEY,
				JSON.stringify(
					stored.map((entry) => ({
						...entry,
						use_count: 1,
						last_used_at: '2026-09-20T00:00:00.000Z',
						last_used_by_user_id: '1400000000000000901',
					})),
				),
			);
		});

		await expect(pod.claimRegistrationUrlUse(registrationUrl.id, '1400000000000000902')).resolves.toBeNull();

		expect(executor.events).toEqual(['read', 'write rejected', 'read']);
		const [listed] = await createPod().getRegistrationUrlsForAdmin();
		expect(listed).toMatchObject({use_count: 1, last_used_by_user_id: '1400000000000000901'});
	});

	it('counts concurrent uses of an uncapped registration URL without ever refusing one', async () => {
		const pods = [createPod(), createPod()];
		const {code} = await pods[0]!.createRegistrationUrl(registrationUrlParams(null));
		const registrationUrl = await pods[0]!.resolveRegistrationUrlCode(code);
		if (registrationUrl === null) throw new Error('registration URL did not resolve');

		const claims = await Promise.all(
			Array.from({length: 5}, (_, index) =>
				pods[index % pods.length]!.claimRegistrationUrlUse(registrationUrl.id, `14000000000000002${index}0`),
			),
		);

		expect(claims.every((claim) => claim !== null)).toBe(true);
		const [listed] = await createPod().getRegistrationUrlsForAdmin();
		expect(listed?.use_count).toBe(5);
	});

	it('frees a released seat for the next signup', async () => {
		const pod = createPod();
		const {code} = await pod.createRegistrationUrl(registrationUrlParams(1));
		const registrationUrl = await pod.resolveRegistrationUrlCode(code);
		if (registrationUrl === null) throw new Error('registration URL did not resolve');

		const failedSignup = await pod.claimRegistrationUrlUse(registrationUrl.id, '1400000000000000301');
		if (failedSignup === null) throw new Error('the first claim was refused');
		await expect(pod.claimRegistrationUrlUse(registrationUrl.id, '1400000000000000302')).resolves.toBeNull();
		await pod.releaseRegistrationUrlUse(failedSignup);

		await expect(pod.claimRegistrationUrlUse(registrationUrl.id, '1400000000000000303')).resolves.not.toBeNull();
		const [listed] = await createPod().getRegistrationUrlsForAdmin();
		expect(listed).toMatchObject({use_count: 1, last_used_by_user_id: '1400000000000000303'});
	});

	it('enforces max_uses against the use count already stored in the blob', async () => {
		const pod = createPod();
		const id = 'b3c4f0b2-8a6e-4c41-9f55-3f0c2a7d1e91';
		await executor.writeDirectly(
			REGISTRATION_URLS_KEY,
			JSON.stringify([
				{
					id,
					label: 'Issued earlier',
					code_hash: createHash('sha256').update(id).digest('hex'),
					created_by_user_id: '1400000000000000001',
					created_at: '2026-09-01T00:00:00.000Z',
					expires_at: null,
					max_uses: 3,
					use_count: 2,
					revoked_at: null,
					approval_required: true,
					last_used_at: '2026-09-02T00:00:00.000Z',
					last_used_by_user_id: '1400000000000000002',
				},
			]),
		);

		expect(await pod.getRegistrationUrlsForAdmin()).toEqual([
			expect.objectContaining({
				id,
				use_count: 2,
				max_uses: 3,
				approval_required: true,
				last_used_at: '2026-09-02T00:00:00.000Z',
				last_used_by_user_id: '1400000000000000002',
			}),
		]);
		const registrationUrl = await pod.resolveRegistrationUrlCode(id);
		if (registrationUrl === null) throw new Error('stored registration URL did not resolve');
		expect(registrationUrl).toMatchObject({id, approval_required: true});

		await expect(pod.claimRegistrationUrlUse(registrationUrl.id, '1400000000000000401')).resolves.not.toBeNull();
		await expect(pod.claimRegistrationUrlUse(registrationUrl.id, '1400000000000000402')).resolves.toBeNull();

		const [listed] = await createPod().getRegistrationUrlsForAdmin();
		expect(listed).toMatchObject({use_count: 3, max_uses: 3, last_used_by_user_id: '1400000000000000401'});
		await expect(pod.resolveRegistrationUrlCode(id)).resolves.toBeNull();
		expect(JSON.parse((await executor.readDirectly(REGISTRATION_URLS_KEY)) ?? 'null')[0]).toMatchObject({
			use_count: 3,
			max_uses: 3,
		});
	});

	it('keeps an SSO field another pod changed while this pod held a stale snapshot', async () => {
		const first = createPod();
		const second = createPod();
		await first.getSsoConfig();
		await second.getSsoConfig();

		await first.setSsoConfig({displayName: 'Set by the first pod'});
		await second.setSsoConfig({clientId: 'set-by-the-second-pod'});

		expect(await createPod().getSsoConfig()).toMatchObject({
			displayName: 'Set by the first pod',
			clientId: 'set-by-the-second-pod',
		});
	});

	it('leaves an SSO row alone when another pod wrote it between this pod reading and writing it', async () => {
		const pod = createPod();
		await pod.getSsoConfig();
		executor.watch('sso_enforced');
		let competed = false;
		executor.competeBeforeEachWrite(async () => {
			if (competed) return;
			competed = true;
			await executor.writeDirectly('sso_enforced', 'true');
		});

		await pod.setSsoConfig({displayName: 'Only the display name'});

		expect(await executor.readDirectly('sso_enforced')).toBe('true');
		expect(await executor.readDirectly('sso_display_name')).toBe('Only the display name');
	});
}

function pendingRegistration(userId: string) {
	return {
		user_id: userId,
		username: `pending_${userId.slice(-3)}`,
		discriminator: 1,
		global_name: null,
		email: `${userId}@example.com`,
		requested_at: `2026-09-01T00:00:${userId.slice(-2)}.000Z`,
		registration_url_id: null,
		client_ip: '127.0.0.1',
		invite_code: null,
	};
}

function registrationUrlParams(maxUses: number | null) {
	return {
		label: maxUses === null ? 'Uncapped' : `Capped at ${maxUses}`,
		createdByUserId: '1400000000000000001',
		expiresAt: null,
		maxUses,
		approvalRequired: false,
	};
}

describe('InstanceConfigRepository setup token', () => {
	const repositories: Array<InstanceConfigRepository> = [];
	const config = getConfig();
	let executor: InstanceConfigWriteRaceExecutor;
	let kvProvider: MockKVProvider;
	let originalSelfHosted: boolean;
	let originalNodeEnv: typeof config.nodeEnv;
	let originalSetupToken: typeof config.instance.setup.token;
	let originalSetupConfigured: boolean;

	beforeEach(() => {
		executor = new InstanceConfigWriteRaceExecutor(new InMemoryCassandraQueryExecutor());
		setCassandraQueryExecutorForTesting(executor);
		kvProvider = new MockKVProvider();
		originalSelfHosted = config.instance.selfHosted;
		originalNodeEnv = config.nodeEnv;
		originalSetupToken = config.instance.setup.token;
		originalSetupConfigured = config.instance.setup.configured;
	});

	afterEach(() => {
		config.instance.selfHosted = originalSelfHosted;
		config.nodeEnv = originalNodeEnv;
		config.instance.setup.token = originalSetupToken;
		config.instance.setup.configured = originalSetupConfigured;
		for (const repository of repositories) {
			repository.shutdown();
		}
		repositories.length = 0;
		setCassandraQueryExecutorForTesting(new InMemoryCassandraQueryExecutor());
	});

	function createPod(): InstanceConfigRepository {
		const repository = new InstanceConfigRepository(kvProvider);
		repositories.push(repository);
		return repository;
	}

	function useSelfHostedProduction(): void {
		config.instance.selfHosted = true;
		config.nodeEnv = 'production';
		config.instance.setup.token = null;
		config.instance.setup.configured = false;
	}

	function sha256(value: string): string {
		return createHash('sha256').update(value).digest('hex');
	}

	const accountExists = async () => true;
	const accountMissing = async () => false;

	it('generates one setup token when two pods start together and stores only its hash', async () => {
		const tokens = await Promise.all([createPod().createSetupTokenIfAbsent(), createPod().createSetupTokenIfAbsent()]);
		const generated = tokens.filter((token) => token !== null);
		expect(generated).toHaveLength(1);
		expect(tokens.filter((token) => token === null)).toHaveLength(1);
		const stored = await executor.readDirectly(SETUP_TOKEN_HASH_KEY);
		expect(stored).toBe(sha256(generated[0]!));
		expect(stored).not.toBe(generated[0]);
		expect(await createPod().createSetupTokenIfAbsent()).toBeNull();
	});

	it('verifies only the generated setup token', async () => {
		const pod = createPod();
		for (const presented of ['anything', '', null, undefined]) {
			expect(await pod.verifySetupToken(presented)).toBe(false);
		}
		const token = await pod.createSetupTokenIfAbsent();
		expect(await pod.verifySetupToken(token)).toBe(true);
		expect(await createPod().verifySetupToken(token)).toBe(true);
		for (const presented of [`${token}x`, sha256(token!), '', null, undefined]) {
			expect(await pod.verifySetupToken(presented)).toBe(false);
		}
	});

	it('takes the environment setup token over a generated one', async () => {
		const pod = createPod();
		const generated = await pod.createSetupTokenIfAbsent();
		config.instance.setup.token = ENV_SETUP_TOKEN;
		expect(await pod.verifySetupToken(ENV_SETUP_TOKEN)).toBe(true);
		expect(await pod.verifySetupToken(generated)).toBe(false);
		expect(await executor.readDirectly(SETUP_TOKEN_HASH_KEY)).toBe(sha256(generated!));
	});

	it('burns the stored setup token and leaves it alone when the admin bootstrap is only claimed', async () => {
		useSelfHostedProduction();
		const pod = createPod();
		const token = await pod.createSetupTokenIfAbsent();
		await pod.claimAdminBootstrap();
		expect(await executor.readDirectly(SETUP_TOKEN_HASH_KEY)).toBe(sha256(token!));
		await pod.burnSetupToken();
		expect(await executor.readDirectly(SETUP_TOKEN_HASH_KEY)).toBe('');
		expect(await pod.verifySetupToken(token)).toBe(false);
		expect(await pod.verifySetupToken('')).toBe(false);
		await ensureSetupToken(pod, accountExists);
		expect(await executor.readDirectly(SETUP_TOKEN_HASH_KEY)).toBe('');
	});

	it('writes nothing when there is no stored setup token to burn', async () => {
		await createPod().burnSetupToken();
		expect(await executor.readDirectly(SETUP_TOKEN_HASH_KEY)).toBeNull();
	});

	it('releases a claimed admin bootstrap so it can be claimed again', async () => {
		const pod = createPod();
		await pod.releaseAdminBootstrap();
		expect(await executor.readDirectly('admin_bootstrapped')).toBeNull();
		expect(await pod.claimAdminBootstrap()).toBe(true);
		expect(await pod.isAdminBootstrapped()).toBe(true);
		await pod.releaseAdminBootstrap();
		expect(await pod.isAdminBootstrapped()).toBe(false);
		expect(await createPod().isAdminBootstrapped()).toBe(false);
		expect(await pod.claimAdminBootstrap()).toBe(true);
		expect(await pod.claimAdminBootstrap()).toBe(false);
	});

	it('enforces the setup token only on self-hosted instances that run in production or configure one', async () => {
		const pod = createPod();
		const gate = async () => ({
			enforced: isSetupTokenEnforced(config),
			open: await pod.isSetupTokenGateOpen(),
			required: await pod.isSetupTokenRequired(),
		});
		config.instance.setup.configured = false;
		config.instance.selfHosted = false;
		config.nodeEnv = 'production';
		config.instance.setup.token = ENV_SETUP_TOKEN;
		expect(await gate()).toEqual({enforced: false, open: false, required: false});
		config.instance.selfHosted = true;
		config.nodeEnv = 'development';
		config.instance.setup.token = null;
		expect(await gate()).toEqual({enforced: false, open: false, required: false});
		config.instance.setup.token = ENV_SETUP_TOKEN;
		expect(await gate()).toEqual({enforced: true, open: true, required: true});
		config.nodeEnv = 'production';
		config.instance.setup.token = null;
		expect(await gate()).toEqual({enforced: true, open: true, required: true});
		config.instance.setup.configured = true;
		expect(await gate()).toEqual({enforced: true, open: true, required: false});
		config.instance.setup.configured = false;
		expect(await gate()).toEqual({enforced: true, open: true, required: true});
		await pod.setAppPublicConfig({setup: {configured: true}});
		expect(await gate()).toEqual({enforced: true, open: true, required: false});
		await pod.claimAdminBootstrap();
		expect(await gate()).toEqual({enforced: true, open: false, required: false});
	});

	it('stores a setup token on the first production start and keeps it on the next', async () => {
		useSelfHostedProduction();
		await ensureSetupToken(createPod(), accountExists);
		const stored = await executor.readDirectly(SETUP_TOKEN_HASH_KEY);
		expect(stored).toMatch(/^[a-f0-9]{64}$/);
		await ensureSetupToken(createPod(), accountExists);
		expect(await executor.readDirectly(SETUP_TOKEN_HASH_KEY)).toBe(stored);
	});

	it('stores a setup token when setup is configured but no admin exists', async () => {
		useSelfHostedProduction();
		config.instance.setup.configured = true;
		await ensureSetupToken(createPod(), accountExists);
		expect(await executor.readDirectly(SETUP_TOKEN_HASH_KEY)).toMatch(/^[a-f0-9]{64}$/);
	});

	it('stores no setup token when one is set in the environment', async () => {
		useSelfHostedProduction();
		config.instance.setup.token = ENV_SETUP_TOKEN;
		await ensureSetupToken(createPod(), accountExists);
		expect(await executor.readDirectly(SETUP_TOKEN_HASH_KEY)).toBeNull();
	});

	it('stores no setup token on the hosted service or in self-hosted development', async () => {
		useSelfHostedProduction();
		config.instance.selfHosted = false;
		await ensureSetupToken(createPod(), accountExists);
		config.instance.selfHosted = true;
		config.nodeEnv = 'development';
		await ensureSetupToken(createPod(), accountExists);
		expect(await executor.readDirectly(SETUP_TOKEN_HASH_KEY)).toBeNull();
	});

	it('stores no setup token once the admin exists', async () => {
		useSelfHostedProduction();
		const pod = createPod();
		await pod.claimAdminBootstrap();
		await ensureSetupToken(pod, accountExists);
		expect(await executor.readDirectly(SETUP_TOKEN_HASH_KEY)).toBeNull();
	});

	it('releases an admin bootstrap claim at start when the claiming account was never created', async () => {
		useSelfHostedProduction();
		const pod = createPod();
		expect(await pod.claimAdminBootstrap(createUserID(4242n))).toBe(true);
		expect(await executor.readDirectly('admin_bootstrap_user_id')).toBe('4242');
		await ensureSetupToken(createPod(), accountExists);
		expect(await executor.readDirectly('admin_bootstrapped')).toBe('true');
		expect(await executor.readDirectly(SETUP_TOKEN_HASH_KEY)).toBeNull();
		const restarted = createPod();
		await ensureSetupToken(restarted, accountMissing);
		expect(await executor.readDirectly('admin_bootstrapped')).toBe('false');
		expect(await restarted.isSetupTokenGateOpen()).toBe(true);
		expect(await executor.readDirectly(SETUP_TOKEN_HASH_KEY)).toMatch(/^[a-f0-9]{64}$/);
	});

	it('keeps an admin bootstrap claim at start when no claiming account is recorded', async () => {
		useSelfHostedProduction();
		const pod = createPod();
		await pod.claimAdminBootstrap();
		expect(await executor.readDirectly('admin_bootstrap_user_id')).toBeNull();
		await ensureSetupToken(createPod(), accountMissing);
		expect(await executor.readDirectly('admin_bootstrapped')).toBe('true');
		await pod.releaseAdminBootstrap();
		await pod.claimAdminBootstrap(createUserID(4242n));
		await pod.releaseAdminBootstrap();
		await pod.claimAdminBootstrap();
		expect(await executor.readDirectly('admin_bootstrap_user_id')).toBe('');
		await ensureSetupToken(createPod(), accountMissing);
		expect(await executor.readDirectly('admin_bootstrapped')).toBe('true');
	});

	it('keeps a won admin bootstrap claim when the claiming account cannot be recorded', async () => {
		const pod = createPod();
		executor.watch('admin_bootstrap_user_id');
		executor.competeBeforeEachWrite(async () => {
			throw new Error('database down');
		});
		expect(await pod.claimAdminBootstrap(createUserID(4242n))).toBe(true);
		expect(await pod.isAdminBootstrapped()).toBe(true);
		expect(await executor.readDirectly('admin_bootstrapped')).toBe('true');
		expect(await executor.readDirectly('admin_bootstrap_user_id')).toBeNull();
	});

	it('confirms the admin bootstrap against the database and repairs a stale snapshot', async () => {
		const stale = new InstanceConfigRepository(new MockKVProvider());
		repositories.push(stale);
		const writer = createPod();
		expect(await stale.isAdminBootstrapped()).toBe(false);
		await writer.claimAdminBootstrap();
		expect(await stale.isAdminBootstrapped()).toBe(false);
		expect(await stale.confirmAdminBootstrapped()).toBe(true);
		expect(await stale.isAdminBootstrapped()).toBe(true);
		await writer.releaseAdminBootstrap();
		expect(await stale.isAdminBootstrapped()).toBe(true);
		expect(await stale.confirmAdminBootstrapped()).toBe(false);
		expect(await stale.isAdminBootstrapped()).toBe(false);
	});

	it('refuses to start when the setup token cannot be stored', async () => {
		useSelfHostedProduction();
		executor.watch(SETUP_TOKEN_HASH_KEY);
		executor.competeBeforeEachWrite(async () => {
			throw new Error('database down');
		});
		await expect(ensureSetupToken(createPod(), accountExists)).rejects.toThrow('database down');
		expect(await executor.readDirectly(SETUP_TOKEN_HASH_KEY)).toBeNull();
	});
});

describe('InstanceConfigRepository concurrent writes', () => {
	describe('in memory', () => {
		describeConcurrentInstanceConfigWrites(async () => new InMemoryCassandraQueryExecutor());
	});
});
