// SPDX-License-Identifier: AGPL-3.0-or-later

import {setCassandraQueryExecutorForTesting} from '@app/api/database/CassandraQueryExecution';
import type {PreparedQuery} from '@app/api/database/CassandraTypes';
import {
	INSTANCE_CONFIG_REFRESH_CHANNEL,
	InstanceConfigRepository,
	type InstanceRegistrationConfig,
} from '@app/api/instance/InstanceConfigRepository';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {MockKVProvider} from '@app/api/test/mocks/MockKVProvider';
import {
	DEFAULT_VOICE_NOISE_SUPPRESSION_CONFIG,
	type VoiceNoiseSuppressionConfig,
} from '@fluxer/schema/src/domains/admin/VoiceNoiseSuppressionSchemas';
import {
	type BlockedMessageGroupsConfig,
	DEFAULT_BLOCKED_MESSAGE_GROUPS_CONFIG,
} from '@fluxer/schema/src/domains/experiment/BlockedMessageGroupsSchemas';
import {
	DEFAULT_EXPERIMENT_DELIVERY_CONFIG,
	type ExperimentDeliveryConfig,
} from '@fluxer/schema/src/domains/experiment/ExperimentSchemas';
import {
	DEFAULT_EXPRESSION_INFO_CARD_CONFIG,
	type ExpressionInfoCardConfig,
} from '@fluxer/schema/src/domains/experiment/ExpressionInfoCardSchemas';
import {
	DEFAULT_GUILD_ACTIVITY_LOG_PRESENTATION_CONFIG,
	type GuildActivityLogPresentationConfig,
} from '@fluxer/schema/src/domains/experiment/GuildActivityLogPresentationSchemas';
import {
	DEFAULT_GUILD_HEADER_COLLAPSE_CONFIG,
	type GuildHeaderCollapseConfig,
} from '@fluxer/schema/src/domains/experiment/GuildHeaderCollapseSchemas';
import {
	DEFAULT_MESSAGE_HOVER_TRACKING_CONFIG,
	type MessageHoverTrackingConfig,
} from '@fluxer/schema/src/domains/experiment/MessageHoverTrackingSchemas';
import {
	DEFAULT_MESSAGE_KEYBOARD_FOCUS_CONFIG,
	type MessageKeyboardFocusConfig,
} from '@fluxer/schema/src/domains/experiment/MessageKeyboardFocusSchemas';
import {
	DEFAULT_TYPING_INDICATOR_REWORK_CONFIG,
	type TypingIndicatorReworkConfig,
} from '@fluxer/schema/src/domains/experiment/TypingIndicatorReworkSchemas';
import {afterEach, describe, expect, it, vi} from 'vitest';

const VOICE_NOISE_SUPPRESSION_CONFIG_KEY = 'voice_noise_suppression_config';
const MESSAGE_HOVER_TRACKING_CONFIG_KEY = 'message_hover_tracking_config';
const MESSAGE_KEYBOARD_FOCUS_CONFIG_KEY = 'message_keyboard_focus_config';
const BLOCKED_MESSAGE_GROUPS_CONFIG_KEY = 'blocked_message_groups_config';
const GUILD_ACTIVITY_LOG_PRESENTATION_CONFIG_KEY = 'guild_activity_log_presentation_config';
const EXPRESSION_INFO_CARD_CONFIG_KEY = 'expression_info_card_config';
const GUILD_HEADER_COLLAPSE_CONFIG_KEY = 'guild_header_collapse_config';
const TYPING_INDICATOR_REWORK_CONFIG_KEY = 'typing_indicator_rework_config';
const EXPERIMENT_DELIVERY_CONFIG_KEY = 'experiment_delivery_config';
const APP_PUBLIC_CONFIG_KEY = 'app_public_config';
const INSTANCE_POLICY_CONFIG_KEY = 'instance_policy_config';
const INSTANCE_INTEGRATIONS_CONFIG_KEY = 'instance_integrations_config';

class CountingInMemoryCassandraQueryExecutor extends InMemoryCassandraQueryExecutor {
	instanceConfigSelects = 0;

	override async executeQuery<T = Record<string, unknown>>(query: PreparedQuery): Promise<Array<T>> {
		if (query.kvMeta?.action === 'select' && query.kvMeta.table.name === 'instance_configuration') {
			this.instanceConfigSelects++;
		}
		return super.executeQuery<T>(query);
	}
}

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

	it('serves repeated config reads from the hydrated in-memory cache', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		await repository.setRegistrationConfig({mode: 'closed'});
		executor.instanceConfigSelects = 0;

		expect(await repository.getRegistrationConfig()).toEqual({
			mode: 'closed',
			admin_registration_urls_enabled: true,
		} satisfies InstanceRegistrationConfig);
		expect(await repository.getRegistrationConfig()).toEqual({
			mode: 'closed',
			admin_registration_urls_enabled: true,
		} satisfies InstanceRegistrationConfig);
		expect(executor.instanceConfigSelects).toBe(0);
		expect(kvProvider.getSubscription().subscribedChannels).toContain(INSTANCE_CONFIG_REFRESH_CHANNEL);
	});

	it('refreshes a hydrated cache after another repository publishes a config update', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const reader = createRepository(kvProvider);
		const writer = createRepository(kvProvider);

		expect(await reader.getRegistrationConfig()).toEqual({
			mode: 'open',
			admin_registration_urls_enabled: true,
		} satisfies InstanceRegistrationConfig);

		await writer.setRegistrationConfig({mode: 'approval'});

		await vi.waitFor(async () => {
			expect(await reader.getRegistrationConfig()).toEqual({
				mode: 'approval',
				admin_registration_urls_enabled: true,
			} satisfies InstanceRegistrationConfig);
		});
	});

	it('reuses the memoized effective bluesky config until the integrations blob changes', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		const first = await repository.getEffectiveBlueskyConfig();
		expect(await repository.getEffectiveBlueskyConfig()).toBe(first);

		await repository.setInstanceIntegrationsConfig({bluesky: {client_name: 'Memoized Instance'}});

		const updated = await repository.getEffectiveBlueskyConfig();
		expect(updated).not.toBe(first);
		expect(updated.client_name).toBe('Memoized Instance');
		expect(await repository.getEffectiveBlueskyConfig()).toBe(updated);
	});

	it('recomputes the effective bluesky config after another repository publishes an integrations update', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const reader = createRepository(kvProvider);
		const writer = createRepository(kvProvider);

		const before = await reader.getEffectiveBlueskyConfig();
		expect(await reader.getEffectiveBlueskyConfig()).toBe(before);

		await writer.setInstanceIntegrationsConfig({bluesky: {client_name: 'Refreshed Instance'}});

		await vi.waitFor(async () => {
			expect((await reader.getEffectiveBlueskyConfig()).client_name).toBe('Refreshed Instance');
		});
	});

	it('reports the effective captcha provider as none while the selected pair is incomplete', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		await repository.setInstanceIntegrationsConfig({
			captcha: {
				provider: 'turnstile',
				hcaptcha_site_key: 'hcaptcha-site-key',
				hcaptcha_secret_key: 'hcaptcha-secret-key',
			},
		});

		await expect(repository.getEffectiveCaptchaConfig()).resolves.toMatchObject({
			enabled: false,
			provider: 'none',
		});

		await repository.setInstanceIntegrationsConfig({
			captcha: {
				turnstile_site_key: 'turnstile-site-key',
				turnstile_secret_key: 'turnstile-secret-key',
			},
		});

		await expect(repository.getEffectiveCaptchaConfig()).resolves.toMatchObject({
			enabled: true,
			provider: 'turnstile',
		});
	});

	it('keeps the stored setup state when a branding field is invalid', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		await repository.setConfig(
			APP_PUBLIC_CONFIG_KEY,
			JSON.stringify({branding: {product_name: 'Kept', icon_url: 42}, setup: {configured: false}}),
		);

		const config = await repository.getAppPublicConfig();
		expect(config.setup.configured).toBe(false);
		expect(config.branding.product_name).toBe('Kept');
	});

	it('keeps valid stored instance policy flags when one field is invalid', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		await repository.setConfig(
			INSTANCE_POLICY_CONFIG_KEY,
			JSON.stringify({direct_messages_disabled: true, single_community_enabled: true, premium_mode: 'nonsense'}),
		);

		const policy = await repository.getInstancePolicyConfig();
		expect(policy.direct_messages_disabled).toBe(true);
		expect(policy.single_community_enabled).toBe(true);
		expect(policy.premium_mode).toBe('everyone');
	});

	it('keeps valid stored integration settings when one field is invalid', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		await repository.setConfig(
			INSTANCE_INTEGRATIONS_CONFIG_KEY,
			JSON.stringify({bluesky: {client_name: 'Kept Instance', enabled: 'yes'}}),
		);

		expect((await repository.getEffectiveBlueskyConfig()).client_name).toBe('Kept Instance');
		expect((await repository.getInstanceIntegrationsConfig()).bluesky.enabled).toBeNull();
	});

	it('drops invalid stored SSO allowed domains and keeps the valid ones', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		await repository.setConfig('sso_allowed_domains', JSON.stringify(['example.com', 'nope@example.com', 'Kept.ORG']));

		expect((await repository.getSsoConfig()).allowedEmailDomains).toEqual(['example.com', 'kept.org']);
	});

	it('falls back to the default SSO flags when a stored flag is not a boolean', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		await repository.setConfig('sso_enabled', 'yes');
		await repository.setConfig('sso_enforced', 'sometimes');
		await repository.setConfig('sso_auto_provision', 'maybe');

		const config = await repository.getSsoConfig();
		expect(config.enabled).toBe(false);
		expect(config.enforced).toBe(false);
		expect(config.autoProvision).toBe(true);
	});

	it('clears an invalid allowed domain list only while SSO is disabled', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		const cleared = await repository.setSsoConfig({enabled: false, allowedEmailDomains: ['nope@example.com']});
		expect(cleared.allowedEmailDomains).toEqual([]);

		await expect(repository.setSsoConfig({enabled: true, allowedEmailDomains: ['nope@example.com']})).rejects.toThrow();
	});

	it('keeps an all-invalid stored allowed domain list non-empty so SSO still rejects every domain', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		await repository.setConfig('sso_allowed_domains', JSON.stringify(['nope@example.com', 'bad/domain']));

		const domains = (await repository.getSsoConfig()).allowedEmailDomains;
		expect(domains.length).toBeGreaterThan(0);
		expect(domains).not.toContain('example.com');
	});

	it('returns the default voice noise suppression config when the key is absent', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		await expect(repository.getVoiceNoiseSuppressionConfig()).resolves.toEqual(DEFAULT_VOICE_NOISE_SUPPRESSION_CONFIG);
	});

	it.each([
		{name: 'unparseable text', stored: 'not-json'},
		{name: 'a json array', stored: '[]'},
		{name: 'out-of-range values', stored: '{"rollout_basis_points":99999}'},
		{name: 'an unknown backend', stored: '{"default_backend":"magic"}'},
	])('falls back to the default voice noise suppression config for $name', async ({stored}) => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		await repository.setConfig(VOICE_NOISE_SUPPRESSION_CONFIG_KEY, stored);

		await expect(repository.getVoiceNoiseSuppressionConfig()).resolves.toEqual(DEFAULT_VOICE_NOISE_SUPPRESSION_CONFIG);
	});

	it('round-trips a stored voice noise suppression config', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		const config: VoiceNoiseSuppressionConfig = {
			...DEFAULT_VOICE_NOISE_SUPPRESSION_CONFIG,
			enabled: true,
			config_version: 3,
			default_backend: 'rnnoise',
			enabled_backends: ['none', 'standard', 'rnnoise'],
			allow_user_override: false,
			rollout_basis_points: 2500,
			rollout_salt: 'voice-ns-v2',
			included_user_ids: ['1400000000000000001'],
			excluded_user_ids: ['1400000000000000002'],
			guild_overrides: [{guild_id: '2400000000000000001', backend: 'rnnoise'}],
			stereo_enabled: true,
			suppression_strength: 55,
		};
		await repository.setVoiceNoiseSuppressionConfig(config);

		await expect(repository.getVoiceNoiseSuppressionConfig()).resolves.toEqual(config);
	});

	it('returns the default message hover tracking config when the key is absent', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		await expect(repository.getMessageHoverTrackingConfig()).resolves.toEqual(DEFAULT_MESSAGE_HOVER_TRACKING_CONFIG);
	});

	it.each([
		{name: 'unparseable text', stored: 'not-json'},
		{name: 'a json array', stored: '[]'},
		{name: 'out-of-range values', stored: '{"rollout_basis_points":99999}'},
		{name: 'a target that is not a snowflake', stored: '{"included_user_ids":["nope"]}'},
	])('falls back to the default message hover tracking config for $name', async ({stored}) => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		await repository.setConfig(MESSAGE_HOVER_TRACKING_CONFIG_KEY, stored);

		await expect(repository.getMessageHoverTrackingConfig()).resolves.toEqual(DEFAULT_MESSAGE_HOVER_TRACKING_CONFIG);
	});

	it('round-trips a stored message hover tracking config', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		const config: MessageHoverTrackingConfig = {
			...DEFAULT_MESSAGE_HOVER_TRACKING_CONFIG,
			enabled: true,
			config_version: 5,
			rollout_basis_points: 2500,
			rollout_salt: 'message-hover-tracking-v2',
			included_user_ids: ['1400000000000000001'],
			excluded_user_ids: ['1400000000000000002'],
		};
		await repository.setMessageHoverTrackingConfig(config);

		await expect(repository.getMessageHoverTrackingConfig()).resolves.toEqual(config);
	});

	it('returns the default message keyboard focus config when the key is absent', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		await expect(repository.getMessageKeyboardFocusConfig()).resolves.toEqual(DEFAULT_MESSAGE_KEYBOARD_FOCUS_CONFIG);
	});

	it.each([
		{name: 'unparseable text', stored: 'not-json'},
		{name: 'a json array', stored: '[]'},
		{name: 'out-of-range values', stored: '{"rollout_basis_points":99999}'},
		{name: 'a target that is not a snowflake', stored: '{"included_user_ids":["nope"]}'},
	])('falls back to the default message keyboard focus config for $name', async ({stored}) => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		await repository.setConfig(MESSAGE_KEYBOARD_FOCUS_CONFIG_KEY, stored);

		await expect(repository.getMessageKeyboardFocusConfig()).resolves.toEqual(DEFAULT_MESSAGE_KEYBOARD_FOCUS_CONFIG);
	});

	it('round-trips a stored message keyboard focus config', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		const config: MessageKeyboardFocusConfig = {
			...DEFAULT_MESSAGE_KEYBOARD_FOCUS_CONFIG,
			enabled: true,
			config_version: 5,
			rollout_basis_points: 2500,
			rollout_salt: 'message-keyboard-focus-v2',
			included_user_ids: ['1400000000000000001'],
			excluded_user_ids: ['1400000000000000002'],
		};
		await repository.setMessageKeyboardFocusConfig(config);

		await expect(repository.getMessageKeyboardFocusConfig()).resolves.toEqual(config);
	});

	it('returns the default blocked message groups config when the key is absent', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		await expect(repository.getBlockedMessageGroupsConfig()).resolves.toEqual(DEFAULT_BLOCKED_MESSAGE_GROUPS_CONFIG);
	});

	it.each([
		{name: 'unparseable text', stored: 'not-json'},
		{name: 'a json array', stored: '[]'},
		{name: 'out-of-range values', stored: '{"rollout_basis_points":99999}'},
		{name: 'a target that is not a snowflake', stored: '{"included_user_ids":["nope"]}'},
	])('falls back to the default blocked message groups config for $name', async ({stored}) => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		await repository.setConfig(BLOCKED_MESSAGE_GROUPS_CONFIG_KEY, stored);

		await expect(repository.getBlockedMessageGroupsConfig()).resolves.toEqual(DEFAULT_BLOCKED_MESSAGE_GROUPS_CONFIG);
	});

	it('round-trips a stored blocked message groups config', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		const config: BlockedMessageGroupsConfig = {
			...DEFAULT_BLOCKED_MESSAGE_GROUPS_CONFIG,
			enabled: true,
			config_version: 5,
			rollout_basis_points: 2500,
			rollout_salt: 'blocked-message-groups-v2',
			included_user_ids: ['1400000000000000001'],
			excluded_user_ids: ['1400000000000000002'],
		};
		await repository.setBlockedMessageGroupsConfig(config);

		await expect(repository.getBlockedMessageGroupsConfig()).resolves.toEqual(config);
	});

	it('returns the default guild activity log presentation config when the key is absent', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		await expect(repository.getGuildActivityLogPresentationConfig()).resolves.toEqual(
			DEFAULT_GUILD_ACTIVITY_LOG_PRESENTATION_CONFIG,
		);
	});

	it.each([
		{name: 'unparseable text', stored: 'not-json'},
		{name: 'a json array', stored: '[]'},
		{name: 'out-of-range values', stored: '{"rollout_basis_points":99999}'},
		{name: 'a target that is not a snowflake', stored: '{"included_user_ids":["nope"]}'},
	])('falls back to the default guild activity log presentation config for $name', async ({stored}) => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		await repository.setConfig(GUILD_ACTIVITY_LOG_PRESENTATION_CONFIG_KEY, stored);

		await expect(repository.getGuildActivityLogPresentationConfig()).resolves.toEqual(
			DEFAULT_GUILD_ACTIVITY_LOG_PRESENTATION_CONFIG,
		);
	});

	it('round-trips a stored guild activity log presentation config', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		const config: GuildActivityLogPresentationConfig = {
			...DEFAULT_GUILD_ACTIVITY_LOG_PRESENTATION_CONFIG,
			enabled: true,
			config_version: 5,
			rollout_basis_points: 2500,
			rollout_salt: 'guild-activity-log-presentation-v2',
			included_user_ids: ['1400000000000000001'],
			excluded_user_ids: ['1400000000000000002'],
		};
		await repository.setGuildActivityLogPresentationConfig(config);

		await expect(repository.getGuildActivityLogPresentationConfig()).resolves.toEqual(config);
	});

	it('returns the default expression info card config when the key is absent', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		await expect(repository.getExpressionInfoCardConfig()).resolves.toEqual(DEFAULT_EXPRESSION_INFO_CARD_CONFIG);
	});

	it.each([
		{name: 'unparseable text', stored: 'not-json'},
		{name: 'a json array', stored: '[]'},
		{name: 'out-of-range values', stored: '{"rollout_basis_points":99999}'},
		{name: 'a target that is not a snowflake', stored: '{"included_user_ids":["nope"]}'},
	])('falls back to the default expression info card config for $name', async ({stored}) => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		await repository.setConfig(EXPRESSION_INFO_CARD_CONFIG_KEY, stored);

		await expect(repository.getExpressionInfoCardConfig()).resolves.toEqual(DEFAULT_EXPRESSION_INFO_CARD_CONFIG);
	});

	it('round-trips a stored expression info card config', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		const config: ExpressionInfoCardConfig = {
			...DEFAULT_EXPRESSION_INFO_CARD_CONFIG,
			enabled: true,
			config_version: 5,
			rollout_basis_points: 2500,
			rollout_salt: 'expression-info-card-v2',
			included_user_ids: ['1400000000000000001'],
			excluded_user_ids: ['1400000000000000002'],
		};
		await repository.setExpressionInfoCardConfig(config);

		await expect(repository.getExpressionInfoCardConfig()).resolves.toEqual(config);
	});

	it('returns the default guild header collapse config when the key is absent', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		await expect(repository.getGuildHeaderCollapseConfig()).resolves.toEqual(DEFAULT_GUILD_HEADER_COLLAPSE_CONFIG);
	});

	it.each([
		{name: 'unparseable text', stored: 'not-json'},
		{name: 'a json array', stored: '[]'},
		{name: 'out-of-range values', stored: '{"rollout_basis_points":99999}'},
		{name: 'a target that is not a snowflake', stored: '{"included_user_ids":["nope"]}'},
	])('falls back to the default guild header collapse config for $name', async ({stored}) => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		await repository.setConfig(GUILD_HEADER_COLLAPSE_CONFIG_KEY, stored);

		await expect(repository.getGuildHeaderCollapseConfig()).resolves.toEqual(DEFAULT_GUILD_HEADER_COLLAPSE_CONFIG);
	});

	it('round-trips a stored guild header collapse config', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		const config: GuildHeaderCollapseConfig = {
			...DEFAULT_GUILD_HEADER_COLLAPSE_CONFIG,
			enabled: true,
			config_version: 5,
			rollout_basis_points: 1500,
			rollout_salt: 'guild-header-collapse-v2',
			included_user_ids: ['1400000000000000001'],
			excluded_user_ids: ['1400000000000000002'],
		};
		await repository.setGuildHeaderCollapseConfig(config);

		await expect(repository.getGuildHeaderCollapseConfig()).resolves.toEqual(config);
	});

	it('fills newly added guild header collapse fields from the schema defaults', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		await repository.setConfig(
			GUILD_HEADER_COLLAPSE_CONFIG_KEY,
			JSON.stringify({enabled: true, config_version: 2, rollout_basis_points: 1000}),
		);

		await expect(repository.getGuildHeaderCollapseConfig()).resolves.toEqual({
			...DEFAULT_GUILD_HEADER_COLLAPSE_CONFIG,
			enabled: true,
			config_version: 2,
			rollout_basis_points: 1000,
		});
	});

	it('returns the default typing indicator rework config when the key is absent', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		await expect(repository.getTypingIndicatorReworkConfig()).resolves.toEqual(DEFAULT_TYPING_INDICATOR_REWORK_CONFIG);
	});

	it.each([
		{name: 'unparseable text', stored: 'not-json'},
		{name: 'a json array', stored: '[]'},
		{name: 'out-of-range values', stored: '{"rollout_basis_points":99999}'},
		{name: 'a target that is not a snowflake', stored: '{"included_user_ids":["nope"]}'},
	])('falls back to the default typing indicator rework config for $name', async ({stored}) => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		await repository.setConfig(TYPING_INDICATOR_REWORK_CONFIG_KEY, stored);

		await expect(repository.getTypingIndicatorReworkConfig()).resolves.toEqual(DEFAULT_TYPING_INDICATOR_REWORK_CONFIG);
	});

	it('round-trips a stored typing indicator rework config', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		const config: TypingIndicatorReworkConfig = {
			...DEFAULT_TYPING_INDICATOR_REWORK_CONFIG,
			enabled: true,
			config_version: 5,
			rollout_basis_points: 1500,
			rollout_salt: 'typing-indicator-rework-v2',
			included_user_ids: ['1400000000000000001'],
			excluded_user_ids: ['1400000000000000002'],
		};
		await repository.setTypingIndicatorReworkConfig(config);

		await expect(repository.getTypingIndicatorReworkConfig()).resolves.toEqual(config);
	});

	it('fills newly added typing indicator rework fields from the schema defaults', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		await repository.setConfig(
			TYPING_INDICATOR_REWORK_CONFIG_KEY,
			JSON.stringify({enabled: true, config_version: 2, rollout_basis_points: 1000}),
		);

		await expect(repository.getTypingIndicatorReworkConfig()).resolves.toEqual({
			...DEFAULT_TYPING_INDICATOR_REWORK_CONFIG,
			enabled: true,
			config_version: 2,
			rollout_basis_points: 1000,
		});
	});

	it('fills newly added voice noise suppression fields from the schema defaults', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		await repository.setConfig(
			VOICE_NOISE_SUPPRESSION_CONFIG_KEY,
			JSON.stringify({enabled: true, config_version: 2, rollout_basis_points: 1000}),
		);

		await expect(repository.getVoiceNoiseSuppressionConfig()).resolves.toEqual({
			...DEFAULT_VOICE_NOISE_SUPPRESSION_CONFIG,
			enabled: true,
			config_version: 2,
			rollout_basis_points: 1000,
		});
	});

	it('returns the default experiment delivery config when the key is absent', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		await expect(repository.getExperimentDeliveryConfig()).resolves.toEqual(DEFAULT_EXPERIMENT_DELIVERY_CONFIG);
	});

	it.each([
		{name: 'unparseable text', stored: 'not-json'},
		{name: 'a json array', stored: '[]'},
		{name: 'an out-of-range poll interval', stored: '{"poll_interval_seconds":1}'},
		{name: 'an out-of-range jitter', stored: '{"poll_jitter_percent":99}'},
		{name: 'a non-numeric poll interval', stored: '{"poll_interval_seconds":"often"}'},
	])('falls back to the default experiment delivery config for $name', async ({stored}) => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		await repository.setConfig(EXPERIMENT_DELIVERY_CONFIG_KEY, stored);

		await expect(repository.getExperimentDeliveryConfig()).resolves.toEqual(DEFAULT_EXPERIMENT_DELIVERY_CONFIG);
	});

	it('round-trips a stored experiment delivery config', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		const config: ExperimentDeliveryConfig = {poll_interval_seconds: 900, poll_jitter_percent: 0};
		await repository.setExperimentDeliveryConfig(config);

		await expect(repository.getExperimentDeliveryConfig()).resolves.toEqual(config);
	});

	it('fills missing experiment delivery fields from the schema defaults', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		await repository.setConfig(EXPERIMENT_DELIVERY_CONFIG_KEY, JSON.stringify({poll_interval_seconds: 3600}));

		await expect(repository.getExperimentDeliveryConfig()).resolves.toEqual({
			...DEFAULT_EXPERIMENT_DELIVERY_CONFIG,
			poll_interval_seconds: 3600,
		});
	});

	it('publishes a refresh so another repository observes the voice noise suppression config', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const reader = createRepository(kvProvider);
		const writer = createRepository(kvProvider);

		await expect(reader.getVoiceNoiseSuppressionConfig()).resolves.toEqual(DEFAULT_VOICE_NOISE_SUPPRESSION_CONFIG);

		await writer.setVoiceNoiseSuppressionConfig({
			...DEFAULT_VOICE_NOISE_SUPPRESSION_CONFIG,
			enabled: true,
			config_version: 1,
		});

		await vi.waitFor(async () => {
			expect(await reader.getVoiceNoiseSuppressionConfig()).toMatchObject({enabled: true, config_version: 1});
		});
	});

	it('uses the registration URL id as the admin-visible registration code', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		const created = await repository.createRegistrationUrl({
			label: 'Support invite',
			createdByUserId: '1500000000000000000',
			expiresAt: null,
			maxUses: null,
			approvalRequired: false,
		});

		expect(created.code).toBe(created.registrationUrl.id);
		expect(created.registrationUrl).not.toHaveProperty('code_hash');
		await expect(repository.resolveRegistrationUrlCode(created.registrationUrl.id)).resolves.toMatchObject({
			id: created.registrationUrl.id,
		});
	});
});
