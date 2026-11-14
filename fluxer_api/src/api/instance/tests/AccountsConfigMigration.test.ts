// SPDX-License-Identifier: AGPL-3.0-or-later

import {getConfig} from '@app/api/Config';
import {setCassandraQueryExecutorForTesting, upsertOne} from '@app/api/database/CassandraQueryExecution';
import {setCachedAccountIdentity} from '@app/api/instance/AccountIdentityModeCache';
import {getEmailDeliveryState, setCachedEmailDeliveryState} from '@app/api/instance/AccountsConfigCache';
import {InstanceConfigRepository} from '@app/api/instance/InstanceConfigRepository';
import {AdminAuditLogs} from '@app/api/Tables';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {
	CleanupModes,
	DateOfBirthPolicies,
	EmailDeliveryModes,
	EmailDeliveryStates,
	GuildJoinGates,
	IdentifierFirstModes,
	LastSessionCleanupModes,
} from '@fluxer/constants/src/AccountConfigConstants';
import {AccountIdentityModes, TagStyles} from '@fluxer/constants/src/AccountIdentityConstants';
import {MockKVProvider} from '@pkgs/kv_client/src/testing/MockKVProvider';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

const AUTH_CONFIG_KEY = 'auth_config';
const ACCOUNTS_CONFIG_KEY = 'accounts_config';
const APP_PUBLIC_CONFIG_KEY = 'app_public_config';
const INTEGRATIONS_CONFIG_KEY = 'instance_integrations_config';

const LEGACY_HOSTED_ACCOUNTS = {
	date_of_birth_enforcement: 'new_accounts',
	interaction_tier: 'legacy',
	guild_join_gate: 'always',
	abandoned_account_cleanup: false,
};

describe('account config migrations', () => {
	const config = getConfig();
	const repositories: Array<InstanceConfigRepository> = [];
	let original: {
		selfHosted: boolean;
		threshold: number | undefined;
		email: typeof config.email;
	};

	beforeEach(() => {
		setCassandraQueryExecutorForTesting(new InMemoryCassandraQueryExecutor());
		original = {
			selfHosted: config.instance.selfHosted,
			threshold: config.inactivityDeletionThresholdDays,
			email: {...config.email},
		};
	});

	afterEach(() => {
		for (const repository of repositories) {
			repository.clearCacheForTesting();
			repository.shutdown();
		}
		repositories.length = 0;
		config.instance.selfHosted = original.selfHosted;
		config.inactivityDeletionThresholdDays = original.threshold;
		Object.assign(config.email, original.email);
		setCachedAccountIdentity(null);
		setCachedEmailDeliveryState(null);
	});

	function createRepository(): InstanceConfigRepository {
		const repository = new InstanceConfigRepository(new MockKVProvider());
		repositories.push(repository);
		return repository;
	}

	async function storedJson(repository: InstanceConfigRepository, key: string): Promise<unknown> {
		const raw = await repository.getConfig(key);
		return raw === null ? null : JSON.parse(raw);
	}

	describe('identifier_first', () => {
		it('reads a stored true as on and a stored false as off', async () => {
			const repository = createRepository();
			await repository.setConfig(AUTH_CONFIG_KEY, JSON.stringify({identifier_first: true}));
			expect((await repository.getAuthConfig()).identifier_first).toBe(IdentifierFirstModes.ON);
			await repository.setConfig(AUTH_CONFIG_KEY, JSON.stringify({identifier_first: false}));
			expect((await repository.getAuthConfig()).identifier_first).toBe(IdentifierFirstModes.OFF);
		});

		it('leaves the hosted false alone at boot', async () => {
			config.instance.selfHosted = false;
			const repository = createRepository();
			await repository.setConfig(AUTH_CONFIG_KEY, JSON.stringify({identifier_first: false}));
			await repository.setConfig(ACCOUNTS_CONFIG_KEY, JSON.stringify(LEGACY_HOSTED_ACCOUNTS));
			await repository.ensureAccountsDefaults();
			expect(await storedJson(repository, AUTH_CONFIG_KEY)).toEqual({identifier_first: false});
			expect((await repository.getAuthConfig()).identifier_first).toBe(IdentifierFirstModes.OFF);
		});

		it('moves a self-hosted false that no admin chose to auto at boot', async () => {
			config.instance.selfHosted = true;
			const repository = createRepository();
			await repository.setConfig(AUTH_CONFIG_KEY, JSON.stringify({identifier_first: false}));
			await repository.setConfig(ACCOUNTS_CONFIG_KEY, JSON.stringify({}));
			await repository.ensureAccountsDefaults();
			expect((await repository.getAuthConfig()).identifier_first).toBe(IdentifierFirstModes.AUTO);
			expect(await storedJson(repository, AUTH_CONFIG_KEY)).toMatchObject({
				identifier_first: IdentifierFirstModes.AUTO,
			});
		});

		it('keeps a self-hosted false that an admin saved as off', async () => {
			config.instance.selfHosted = true;
			const repository = createRepository();
			await upsertOne(
				AdminAuditLogs.insert({
					log_id: 1n,
					admin_user_id: 2n,
					target_type: 'instance_config',
					target_id: 0n,
					action: 'update_instance_config',
					audit_log_reason: null,
					metadata: new Map([['sections', 'auth,captcha']]),
					created_at: new Date(),
				}),
			);
			await repository.setConfig(AUTH_CONFIG_KEY, JSON.stringify({identifier_first: false}));
			await repository.setConfig(ACCOUNTS_CONFIG_KEY, JSON.stringify({}));
			await repository.ensureAccountsDefaults();
			expect((await repository.getAuthConfig()).identifier_first).toBe(IdentifierFirstModes.OFF);
			expect(await storedJson(repository, AUTH_CONFIG_KEY)).toMatchObject({identifier_first: IdentifierFirstModes.OFF});
		});

		it('turns on for a fresh install once email is configured after boot', async () => {
			config.instance.selfHosted = true;
			config.email.enabled = false;
			const repository = createRepository();
			await repository.ensureAccountsDefaults();
			expect(await repository.getSignInFeatures()).toEqual({identifierFirst: false, emailCodes: false});
			await repository.setInstanceIntegrationsConfig({
				email: {
					delivery: EmailDeliveryModes.SMTP,
					from_email: 'noreply@example.com',
					smtp: {host: 'smtp.example.com', port: 587, username: 'user', password: 'secret'},
				},
			});
			expect(await repository.getSignInFeatures()).toEqual({identifierFirst: true, emailCodes: true});
		});
	});

	describe('date_of_birth', () => {
		const cases: Array<[boolean | undefined, string | undefined, boolean, string]> = [
			[false, 'all', true, DateOfBirthPolicies.OFF],
			[true, 'off', true, DateOfBirthPolicies.ASK],
			[true, 'new_accounts', true, DateOfBirthPolicies.REQUIRE_NEW_ACCOUNTS],
			[true, 'all', true, DateOfBirthPolicies.REQUIRE_ALL],
			[undefined, 'new_accounts', true, DateOfBirthPolicies.OFF],
			[undefined, 'new_accounts', false, DateOfBirthPolicies.REQUIRE_NEW_ACCOUNTS],
			[undefined, undefined, false, DateOfBirthPolicies.REQUIRE_NEW_ACCOUNTS],
		];
		for (const [collect, enforcement, selfHosted, expected] of cases) {
			it(`reads collect=${collect} with enforcement=${enforcement} ${selfHosted ? 'self-hosted' : 'hosted'} as ${expected}`, async () => {
				config.instance.selfHosted = selfHosted;
				const repository = createRepository();
				if (collect !== undefined) {
					await repository.setConfig(
						APP_PUBLIC_CONFIG_KEY,
						JSON.stringify({registration: {collect_date_of_birth: collect}}),
					);
				}
				await repository.setConfig(
					ACCOUNTS_CONFIG_KEY,
					JSON.stringify(enforcement === undefined ? {} : {date_of_birth_enforcement: enforcement}),
				);
				expect((await repository.getAccountsConfig()).date_of_birth).toBe(expected);
				expect((await repository.getAppPublicConfig()).registration.collect_date_of_birth).toBe(
					expected !== DateOfBirthPolicies.OFF,
				);
			});
		}

		it('translates the deprecated collection switch and enforcement on write', async () => {
			const repository = createRepository();
			await repository.updateAccountsConfig({date_of_birth: DateOfBirthPolicies.REQUIRE_NEW_ACCOUNTS});
			await repository.updateAccountsConfig({date_of_birth_enforcement: 'all'});
			expect((await repository.getAccountsConfig()).date_of_birth).toBe(DateOfBirthPolicies.REQUIRE_ALL);
			await repository.setDateOfBirthCollection(false);
			expect((await repository.getAccountsConfig()).date_of_birth).toBe(DateOfBirthPolicies.OFF);
			await repository.updateAccountsConfig({date_of_birth_enforcement: 'all'});
			expect((await repository.getAccountsConfig()).date_of_birth).toBe(DateOfBirthPolicies.OFF);
			await repository.setDateOfBirthCollection(true);
			expect((await repository.getAccountsConfig()).date_of_birth).toBe(DateOfBirthPolicies.REQUIRE_NEW_ACCOUNTS);
		});
	});

	describe('cleanup', () => {
		it('turns both cleanup policies on with the defaults from a stored true', async () => {
			const repository = createRepository();
			await repository.setConfig(ACCOUNTS_CONFIG_KEY, JSON.stringify({abandoned_account_cleanup: true}));
			const accounts = await repository.getAccountsConfig();
			expect(accounts.unclaimed_account_cleanup).toEqual({
				mode: CleanupModes.AFTER_INACTIVITY,
				inactive_days: 90,
				on_last_session_end: LastSessionCleanupModes.AFTER_GRACE,
			});
			expect(accounts.dob_missing_cleanup).toEqual({mode: CleanupModes.AFTER_INACTIVITY, inactive_days: 7});
		});

		it('turns both cleanup policies off from a stored false', async () => {
			config.instance.selfHosted = true;
			const repository = createRepository();
			await repository.setConfig(ACCOUNTS_CONFIG_KEY, JSON.stringify({abandoned_account_cleanup: false}));
			const accounts = await repository.getAccountsConfig();
			expect(accounts.unclaimed_account_cleanup.mode).toBe(CleanupModes.OFF);
			expect(accounts.unclaimed_account_cleanup.on_last_session_end).toBe(LastSessionCleanupModes.OFF);
			expect(accounts.dob_missing_cleanup.mode).toBe(CleanupModes.OFF);
		});

		it('seeds a missing inactivity policy from the deprecated threshold, never below two years', async () => {
			const repository = createRepository();
			await repository.setConfig(ACCOUNTS_CONFIG_KEY, JSON.stringify(LEGACY_HOSTED_ACCOUNTS));
			config.inactivityDeletionThresholdDays = 365;
			expect((await repository.getAccountsConfig()).inactivity_deletion).toEqual({
				mode: CleanupModes.AFTER_INACTIVITY,
				inactive_days: 730,
				warning_lead_days: 30,
			});
			config.inactivityDeletionThresholdDays = 900;
			expect((await repository.getAccountsConfig()).inactivity_deletion.inactive_days).toBe(900);
		});

		it('keeps a stored inactivity threshold below two years', async () => {
			const repository = createRepository();
			await repository.updateAccountsConfig({inactivity_deletion: {inactive_days: 200}});
			expect((await repository.getAccountsConfig()).inactivity_deletion.inactive_days).toBe(200);
		});
	});

	it('reads an unknown guild join gate as always', async () => {
		const repository = createRepository();
		await repository.setConfig(ACCOUNTS_CONFIG_KEY, JSON.stringify({guild_join_gate: 'sometimes'}));
		const accounts = await repository.getAccountsConfig();
		expect(accounts.guild_join_gate).toBe(GuildJoinGates.ALWAYS);
		expect(accounts.interaction_tier).toBeDefined();
	});

	it('keeps the hosted accounts row exactly as it behaves today', async () => {
		config.instance.selfHosted = false;
		const repository = createRepository();
		await repository.setConfig(ACCOUNTS_CONFIG_KEY, JSON.stringify(LEGACY_HOSTED_ACCOUNTS));
		expect(await repository.getAccountsConfig()).toEqual({
			date_of_birth: DateOfBirthPolicies.REQUIRE_NEW_ACCOUNTS,
			interaction_tier: 'legacy',
			verification_basis: 'auto',
			guild_join_gate: GuildJoinGates.ALWAYS,
			unclaimed_account_cleanup: {
				mode: CleanupModes.OFF,
				inactive_days: 90,
				on_last_session_end: LastSessionCleanupModes.OFF,
			},
			dob_missing_cleanup: {mode: CleanupModes.OFF, inactive_days: 7},
			inactivity_deletion: {
				mode: CleanupModes.AFTER_INACTIVITY,
				inactive_days: Math.max(730, config.inactivityDeletionThresholdDays ?? 0),
				warning_lead_days: 30,
			},
		});
	});

	describe('email delivery', () => {
		const smtp = {host: 'smtp.example.com', port: 587, username: 'user', password: 'secret'};
		const cases: Array<[boolean | null, 'smtp' | 'none' | null, string | null]> = [
			[false, 'smtp', EmailDeliveryModes.OFF],
			[true, 'none', EmailDeliveryModes.OFF],
			[true, 'smtp', EmailDeliveryModes.SMTP],
			[null, null, null],
		];
		for (const [enabled, provider, expected] of cases) {
			it(`reads enabled=${enabled} with provider=${provider} as ${expected}`, async () => {
				const repository = createRepository();
				await repository.setConfig(
					INTEGRATIONS_CONFIG_KEY,
					JSON.stringify({email: {enabled, provider, from_email: 'noreply@example.com', smtp}}),
				);
				expect((await repository.getInstanceIntegrationsConfig()).email.delivery).toBe(expected);
			});
		}

		it('translates the deprecated switches on write and stores only delivery', async () => {
			const repository = createRepository();
			await repository.setInstanceIntegrationsConfig({
				email: {enabled: true, provider: 'smtp', from_email: 'noreply@example.com', smtp},
			});
			expect((await repository.getInstanceIntegrationsConfig()).email.delivery).toBe(EmailDeliveryModes.SMTP);
			expect(await repository.getEmailDeliveryState()).toBe(EmailDeliveryStates.ON);
			await repository.setInstanceIntegrationsConfig({email: {enabled: false}});
			const stored = (await storedJson(repository, INTEGRATIONS_CONFIG_KEY)) as {email: Record<string, unknown>};
			expect(stored.email.delivery).toBe(EmailDeliveryModes.OFF);
			expect(stored.email).not.toHaveProperty('enabled');
			expect(stored.email).not.toHaveProperty('provider');
			expect(await repository.getEmailDeliveryState()).toBe(EmailDeliveryStates.OFF_DISABLED);
		});

		it('reports incomplete settings separately from delivery being off', async () => {
			const repository = createRepository();
			await repository.setInstanceIntegrationsConfig({email: {delivery: EmailDeliveryModes.SMTP, from_email: null}});
			config.email.fromEmail = '';
			expect(await repository.getEmailDeliveryState()).toBe(EmailDeliveryStates.OFF_INCOMPLETE);
		});

		it('falls back to the env settings and the cached identity mode before the first sync', () => {
			Object.assign(config.email, {
				enabled: true,
				provider: 'smtp',
				fromEmail: 'noreply@example.com',
				smtp: {...smtp, tlsMode: null},
			});
			setCachedEmailDeliveryState(null);
			setCachedAccountIdentity(null);
			expect(getEmailDeliveryState()).toBe(EmailDeliveryStates.ON);
			config.instance.selfHosted = true;
			setCachedAccountIdentity({mode: AccountIdentityModes.USERNAME, tagStyle: TagStyles.NONE});
			expect(getEmailDeliveryState()).toBe(EmailDeliveryStates.OFF_USERNAME_MODE);
		});

		it('turns delivery and identifier-first off when an instance switches to usernames', async () => {
			config.instance.selfHosted = true;
			const repository = createRepository();
			await repository.updateAuthConfig({identifier_first: IdentifierFirstModes.ON});
			await repository.setInstanceIntegrationsConfig({email: {delivery: EmailDeliveryModes.SMTP}});
			await repository.setAccountIdentityMode(AccountIdentityModes.USERNAME, 'setup', TagStyles.RANDOM);
			expect((await repository.getAccountIdentity()).tagStyle).toBe(TagStyles.NONE);
			expect((await repository.getInstanceIntegrationsConfig()).email.delivery).toBe(EmailDeliveryModes.OFF);
			expect((await repository.getAuthConfig()).identifier_first).toBe(IdentifierFirstModes.OFF);
			expect(await repository.getEmailDeliveryState()).toBe(EmailDeliveryStates.OFF_USERNAME_MODE);
		});
	});
});
