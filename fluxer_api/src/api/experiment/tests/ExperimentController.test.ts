// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {
	DEFAULT_VOICE_NOISE_SUPPRESSION_CONFIG,
	INERT_VOICE_NOISE_SUPPRESSION_ASSIGNMENT,
} from '@fluxer/schema/src/domains/admin/VoiceNoiseSuppressionSchemas';
import {
	DEFAULT_BLOCKED_MESSAGE_GROUPS_CONFIG,
	INERT_BLOCKED_MESSAGE_GROUPS_ASSIGNMENT,
} from '@fluxer/schema/src/domains/experiment/BlockedMessageGroupsSchemas';
import {
	DEFAULT_EXPERIMENT_POLL_INTERVAL_SECONDS,
	DEFAULT_EXPERIMENT_POLL_JITTER_PERCENT,
	type ExperimentAssignmentsResponse,
	type ExperimentDeliveryConfigResponse,
	readBlockedMessageGroupsAssignment,
	readExpressionInfoCardAssignment,
	readGuildActivityLogPresentationAssignment,
	readGuildHeaderCollapseAssignment,
	readMessageHoverTrackingAssignment,
	readMessageKeyboardFocusAssignment,
	readTypingIndicatorReworkAssignment,
	readVoiceNoiseSuppressionAssignment,
} from '@fluxer/schema/src/domains/experiment/ExperimentSchemas';
import {
	DEFAULT_EXPRESSION_INFO_CARD_CONFIG,
	INERT_EXPRESSION_INFO_CARD_ASSIGNMENT,
} from '@fluxer/schema/src/domains/experiment/ExpressionInfoCardSchemas';
import {
	DEFAULT_GUILD_ACTIVITY_LOG_PRESENTATION_CONFIG,
	INERT_GUILD_ACTIVITY_LOG_PRESENTATION_ASSIGNMENT,
} from '@fluxer/schema/src/domains/experiment/GuildActivityLogPresentationSchemas';
import {
	DEFAULT_GUILD_HEADER_COLLAPSE_CONFIG,
	INERT_GUILD_HEADER_COLLAPSE_ASSIGNMENT,
} from '@fluxer/schema/src/domains/experiment/GuildHeaderCollapseSchemas';
import {
	DEFAULT_MESSAGE_HOVER_TRACKING_CONFIG,
	INERT_MESSAGE_HOVER_TRACKING_ASSIGNMENT,
} from '@fluxer/schema/src/domains/experiment/MessageHoverTrackingSchemas';
import {
	DEFAULT_MESSAGE_KEYBOARD_FOCUS_CONFIG,
	INERT_MESSAGE_KEYBOARD_FOCUS_ASSIGNMENT,
} from '@fluxer/schema/src/domains/experiment/MessageKeyboardFocusSchemas';
import {
	DEFAULT_TYPING_INDICATOR_REWORK_CONFIG,
	INERT_TYPING_INDICATOR_REWORK_ASSIGNMENT,
} from '@fluxer/schema/src/domains/experiment/TypingIndicatorReworkSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const NOT_MODIFIED = 304;
const ENDPOINT = '/experiments';

describe('GET /experiments', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	it('rejects an unauthenticated caller', async () => {
		await createBuilderWithoutAuth(harness).get(ENDPOINT).expect(HTTP_STATUS.UNAUTHORIZED).execute();
	});

	it('returns the default delivery cadence and the inert assignment while the feature is disabled', async () => {
		const account = await createTestAccount(harness);

		const body = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token).get(ENDPOINT).execute();

		expect(body).toEqual({
			poll_interval_seconds: DEFAULT_EXPERIMENT_POLL_INTERVAL_SECONDS,
			poll_jitter_percent: DEFAULT_EXPERIMENT_POLL_JITTER_PERCENT,
			assignments: {
				voice_noise_suppression: INERT_VOICE_NOISE_SUPPRESSION_ASSIGNMENT,
				message_hover_tracking: INERT_MESSAGE_HOVER_TRACKING_ASSIGNMENT,
				message_keyboard_focus: INERT_MESSAGE_KEYBOARD_FOCUS_ASSIGNMENT,
				blocked_message_groups: INERT_BLOCKED_MESSAGE_GROUPS_ASSIGNMENT,
				guild_activity_log_presentation: INERT_GUILD_ACTIVITY_LOG_PRESENTATION_ASSIGNMENT,
				expression_info_card: INERT_EXPRESSION_INFO_CARD_ASSIGNMENT,
				guild_header_collapse: INERT_GUILD_HEADER_COLLAPSE_ASSIGNMENT,
				typing_indicator_rework: INERT_TYPING_INDICATOR_REWORK_ASSIGNMENT,
			},
		});
	});

	it('returns the inert assignment while the stored config is disabled but populated', async () => {
		const account = await createTestAccount(harness);
		await getInstanceConfigRepository().setVoiceNoiseSuppressionConfig({
			...DEFAULT_VOICE_NOISE_SUPPRESSION_CONFIG,
			enabled: false,
			config_version: 9,
			rollout_basis_points: 10000,
			included_user_ids: [account.userId],
		});

		const body = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token).get(ENDPOINT).execute();

		expect(body.assignments.voice_noise_suppression).toEqual({
			...INERT_VOICE_NOISE_SUPPRESSION_ASSIGNMENT,
			config_version: 9,
		});
	});

	it('populates the voice assignment key even when the rollout is disabled', async () => {
		const account = await createTestAccount(harness);

		const body = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token).get(ENDPOINT).execute();

		expect(Object.hasOwn(body.assignments, 'voice_noise_suppression')).toBe(true);
		expect(readVoiceNoiseSuppressionAssignment(body).enabled).toBe(false);
	});

	it('populates the message hover tracking key even when the rollout is disabled', async () => {
		const account = await createTestAccount(harness);

		const body = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token).get(ENDPOINT).execute();

		expect(Object.hasOwn(body.assignments, 'message_hover_tracking')).toBe(true);
		expect(readMessageHoverTrackingAssignment(body)).toEqual(INERT_MESSAGE_HOVER_TRACKING_ASSIGNMENT);
	});

	it('targets an allowlisted account for message hover tracking', async () => {
		const account = await createTestAccount(harness);
		await getInstanceConfigRepository().setMessageHoverTrackingConfig({
			...DEFAULT_MESSAGE_HOVER_TRACKING_CONFIG,
			enabled: true,
			config_version: 4,
			included_user_ids: [account.userId],
		});

		const body = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token).get(ENDPOINT).execute();

		expect(readMessageHoverTrackingAssignment(body)).toEqual({
			enabled: true,
			config_version: 4,
			user_targeted: true,
			source: 'user_rule',
		});
	});

	it('leaves an account outside a zero-width message hover tracking rollout', async () => {
		const account = await createTestAccount(harness);
		await getInstanceConfigRepository().setMessageHoverTrackingConfig({
			...DEFAULT_MESSAGE_HOVER_TRACKING_CONFIG,
			enabled: true,
			config_version: 2,
		});

		const body = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token).get(ENDPOINT).execute();

		expect(readMessageHoverTrackingAssignment(body)).toEqual({
			enabled: true,
			config_version: 2,
			user_targeted: false,
			source: null,
		});
	});

	it('populates the message keyboard focus key even when the rollout is disabled', async () => {
		const account = await createTestAccount(harness);

		const body = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token).get(ENDPOINT).execute();

		expect(Object.hasOwn(body.assignments, 'message_keyboard_focus')).toBe(true);
		expect(readMessageKeyboardFocusAssignment(body)).toEqual(INERT_MESSAGE_KEYBOARD_FOCUS_ASSIGNMENT);
	});

	it('targets an allowlisted account for message keyboard focus', async () => {
		const account = await createTestAccount(harness);
		await getInstanceConfigRepository().setMessageKeyboardFocusConfig({
			...DEFAULT_MESSAGE_KEYBOARD_FOCUS_CONFIG,
			enabled: true,
			config_version: 4,
			included_user_ids: [account.userId],
		});

		const body = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token).get(ENDPOINT).execute();

		expect(readMessageKeyboardFocusAssignment(body)).toEqual({
			enabled: true,
			config_version: 4,
			user_targeted: true,
			source: 'user_rule',
		});
	});

	it('leaves an account outside a zero-width message keyboard focus rollout', async () => {
		const account = await createTestAccount(harness);
		await getInstanceConfigRepository().setMessageKeyboardFocusConfig({
			...DEFAULT_MESSAGE_KEYBOARD_FOCUS_CONFIG,
			enabled: true,
			config_version: 2,
		});

		const body = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token).get(ENDPOINT).execute();

		expect(readMessageKeyboardFocusAssignment(body)).toEqual({
			enabled: true,
			config_version: 2,
			user_targeted: false,
			source: null,
		});
	});

	it('populates the blocked message groups key even when the rollout is disabled', async () => {
		const account = await createTestAccount(harness);

		const body = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token).get(ENDPOINT).execute();

		expect(Object.hasOwn(body.assignments, 'blocked_message_groups')).toBe(true);
		expect(readBlockedMessageGroupsAssignment(body)).toEqual(INERT_BLOCKED_MESSAGE_GROUPS_ASSIGNMENT);
	});

	it('targets an allowlisted account for blocked message groups', async () => {
		const account = await createTestAccount(harness);
		await getInstanceConfigRepository().setBlockedMessageGroupsConfig({
			...DEFAULT_BLOCKED_MESSAGE_GROUPS_CONFIG,
			enabled: true,
			config_version: 4,
			included_user_ids: [account.userId],
		});

		const body = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token).get(ENDPOINT).execute();

		expect(readBlockedMessageGroupsAssignment(body)).toEqual({
			enabled: true,
			config_version: 4,
			user_targeted: true,
			source: 'user_rule',
		});
	});

	it('leaves an account outside a zero-width blocked message groups rollout', async () => {
		const account = await createTestAccount(harness);
		await getInstanceConfigRepository().setBlockedMessageGroupsConfig({
			...DEFAULT_BLOCKED_MESSAGE_GROUPS_CONFIG,
			enabled: true,
			config_version: 2,
		});

		const body = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token).get(ENDPOINT).execute();

		expect(readBlockedMessageGroupsAssignment(body)).toEqual({
			enabled: true,
			config_version: 2,
			user_targeted: false,
			source: null,
		});
	});

	it('populates the guild activity log presentation key even when the rollout is disabled', async () => {
		const account = await createTestAccount(harness);

		const body = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token).get(ENDPOINT).execute();

		expect(Object.hasOwn(body.assignments, 'guild_activity_log_presentation')).toBe(true);
		expect(readGuildActivityLogPresentationAssignment(body)).toEqual(INERT_GUILD_ACTIVITY_LOG_PRESENTATION_ASSIGNMENT);
	});

	it('targets an allowlisted account for guild activity log presentation', async () => {
		const targeted = await createTestAccount(harness);
		const untargeted = await createTestAccount(harness);
		await getInstanceConfigRepository().setGuildActivityLogPresentationConfig({
			...DEFAULT_GUILD_ACTIVITY_LOG_PRESENTATION_CONFIG,
			enabled: true,
			config_version: 6,
			included_user_ids: [targeted.userId],
		});

		const targetedBody = await createBuilder<ExperimentAssignmentsResponse>(harness, targeted.token)
			.get(ENDPOINT)
			.execute();
		expect(readGuildActivityLogPresentationAssignment(targetedBody)).toEqual({
			enabled: true,
			config_version: 6,
			user_targeted: true,
			source: 'user_rule',
		});

		const untargetedBody = await createBuilder<ExperimentAssignmentsResponse>(harness, untargeted.token)
			.get(ENDPOINT)
			.execute();
		expect(readGuildActivityLogPresentationAssignment(untargetedBody)).toEqual({
			enabled: true,
			config_version: 6,
			user_targeted: false,
			source: null,
		});
	});

	it('bumps the guild activity log presentation config version on every admin update', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_CONFIG_VIEW,
			AdminACLs.INSTANCE_CONFIG_UPDATE,
		]);

		const updated = await createBuilder<{
			guild_activity_log_presentation: {config_version: number; enabled: boolean; rollout_basis_points: number};
		}>(harness, admin.token)
			.patch('/admin/instance/config')
			.body({guild_activity_log_presentation: {enabled: true, rollout_basis_points: 10000}})
			.execute();
		expect(updated.guild_activity_log_presentation).toMatchObject({
			config_version: 1,
			enabled: true,
			rollout_basis_points: 10000,
		});

		const body = await createBuilder<ExperimentAssignmentsResponse>(harness, admin.token).get(ENDPOINT).execute();
		expect(readGuildActivityLogPresentationAssignment(body)).toEqual({
			enabled: true,
			config_version: 1,
			user_targeted: true,
			source: 'canary',
		});
	});

	it('populates the expression info card key even when the rollout is disabled', async () => {
		const account = await createTestAccount(harness);

		const body = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token).get(ENDPOINT).execute();

		expect(Object.hasOwn(body.assignments, 'expression_info_card')).toBe(true);
		expect(readExpressionInfoCardAssignment(body)).toEqual(INERT_EXPRESSION_INFO_CARD_ASSIGNMENT);
	});

	it('targets an allowlisted account for the expression info card', async () => {
		const account = await createTestAccount(harness);
		await getInstanceConfigRepository().setExpressionInfoCardConfig({
			...DEFAULT_EXPRESSION_INFO_CARD_CONFIG,
			enabled: true,
			config_version: 4,
			included_user_ids: [account.userId],
		});

		const body = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token).get(ENDPOINT).execute();

		expect(readExpressionInfoCardAssignment(body)).toEqual({
			enabled: true,
			config_version: 4,
			user_targeted: true,
			source: 'user_rule',
		});
	});

	it('leaves an account outside a zero-width expression info card rollout', async () => {
		const account = await createTestAccount(harness);
		await getInstanceConfigRepository().setExpressionInfoCardConfig({
			...DEFAULT_EXPRESSION_INFO_CARD_CONFIG,
			enabled: true,
			config_version: 2,
		});

		const body = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token).get(ENDPOINT).execute();

		expect(readExpressionInfoCardAssignment(body)).toEqual({
			enabled: true,
			config_version: 2,
			user_targeted: false,
			source: null,
		});
	});

	it('populates the guild header collapse key even when the rollout is disabled', async () => {
		const account = await createTestAccount(harness);

		const body = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token).get(ENDPOINT).execute();

		expect(Object.hasOwn(body.assignments, 'guild_header_collapse')).toBe(true);
		expect(readGuildHeaderCollapseAssignment(body)).toEqual(INERT_GUILD_HEADER_COLLAPSE_ASSIGNMENT);
	});

	it('targets an allowlisted account for guild header collapse', async () => {
		const account = await createTestAccount(harness);
		await getInstanceConfigRepository().setGuildHeaderCollapseConfig({
			...DEFAULT_GUILD_HEADER_COLLAPSE_CONFIG,
			enabled: true,
			config_version: 4,
			included_user_ids: [account.userId],
		});

		const body = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token).get(ENDPOINT).execute();

		expect(readGuildHeaderCollapseAssignment(body)).toEqual({
			enabled: true,
			config_version: 4,
			user_targeted: true,
			source: 'user_rule',
		});
	});

	it('leaves an account outside a zero-width guild header collapse rollout', async () => {
		const account = await createTestAccount(harness);
		await getInstanceConfigRepository().setGuildHeaderCollapseConfig({
			...DEFAULT_GUILD_HEADER_COLLAPSE_CONFIG,
			enabled: true,
			config_version: 2,
		});

		const body = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token).get(ENDPOINT).execute();

		expect(readGuildHeaderCollapseAssignment(body)).toEqual({
			enabled: true,
			config_version: 2,
			user_targeted: false,
			source: null,
		});
	});

	it('populates the typing indicator rework key even when the rollout is disabled', async () => {
		const account = await createTestAccount(harness);

		const body = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token).get(ENDPOINT).execute();

		expect(Object.hasOwn(body.assignments, 'typing_indicator_rework')).toBe(true);
		expect(readTypingIndicatorReworkAssignment(body)).toEqual(INERT_TYPING_INDICATOR_REWORK_ASSIGNMENT);
	});

	it('targets an allowlisted account for typing indicator rework', async () => {
		const account = await createTestAccount(harness);
		await getInstanceConfigRepository().setTypingIndicatorReworkConfig({
			...DEFAULT_TYPING_INDICATOR_REWORK_CONFIG,
			enabled: true,
			config_version: 6,
			included_user_ids: [account.userId],
		});

		const body = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token).get(ENDPOINT).execute();

		expect(readTypingIndicatorReworkAssignment(body)).toEqual({
			enabled: true,
			config_version: 6,
			user_targeted: true,
			source: 'user_rule',
		});
	});

	it('leaves an account outside a zero-width typing indicator rework rollout', async () => {
		const account = await createTestAccount(harness);
		await getInstanceConfigRepository().setTypingIndicatorReworkConfig({
			...DEFAULT_TYPING_INDICATOR_REWORK_CONFIG,
			enabled: true,
			config_version: 2,
		});

		const body = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token).get(ENDPOINT).execute();

		expect(readTypingIndicatorReworkAssignment(body)).toEqual({
			enabled: true,
			config_version: 2,
			user_targeted: false,
			source: null,
		});
	});

	it('serves a fresh body once the typing indicator rework config changes', async () => {
		const account = await createTestAccount(harness);

		const first = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token)
			.get(ENDPOINT)
			.executeWithResponse();
		const staleEtag = first.response.headers.get('etag') as string;

		await getInstanceConfigRepository().setTypingIndicatorReworkConfig({
			...DEFAULT_TYPING_INDICATOR_REWORK_CONFIG,
			enabled: true,
			config_version: 1,
			rollout_basis_points: 10000,
		});

		const refreshed = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token)
			.get(ENDPOINT)
			.header('If-None-Match', staleEtag)
			.executeWithResponse();
		expect(refreshed.response.status).toBe(HTTP_STATUS.OK);
		expect(refreshed.response.headers.get('etag')).not.toBe(staleEtag);
		expect(refreshed.json?.assignments.typing_indicator_rework).toMatchObject({
			enabled: true,
			config_version: 1,
			user_targeted: true,
		});
	});

	it('resolves all eight experiments independently', async () => {
		const account = await createTestAccount(harness);
		await getInstanceConfigRepository().setMessageHoverTrackingConfig({
			...DEFAULT_MESSAGE_HOVER_TRACKING_CONFIG,
			enabled: true,
			rollout_basis_points: 10000,
		});
		await getInstanceConfigRepository().setMessageKeyboardFocusConfig({
			...DEFAULT_MESSAGE_KEYBOARD_FOCUS_CONFIG,
			enabled: true,
			rollout_basis_points: 10000,
		});
		await getInstanceConfigRepository().setBlockedMessageGroupsConfig({
			...DEFAULT_BLOCKED_MESSAGE_GROUPS_CONFIG,
			enabled: true,
			rollout_basis_points: 10000,
		});
		await getInstanceConfigRepository().setGuildActivityLogPresentationConfig({
			...DEFAULT_GUILD_ACTIVITY_LOG_PRESENTATION_CONFIG,
			enabled: true,
			rollout_basis_points: 10000,
		});
		await getInstanceConfigRepository().setExpressionInfoCardConfig({
			...DEFAULT_EXPRESSION_INFO_CARD_CONFIG,
			enabled: true,
			rollout_basis_points: 10000,
		});
		await getInstanceConfigRepository().setGuildHeaderCollapseConfig({
			...DEFAULT_GUILD_HEADER_COLLAPSE_CONFIG,
			enabled: true,
			rollout_basis_points: 10000,
		});
		await getInstanceConfigRepository().setTypingIndicatorReworkConfig({
			...DEFAULT_TYPING_INDICATOR_REWORK_CONFIG,
			enabled: true,
			rollout_basis_points: 10000,
		});

		const body = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token).get(ENDPOINT).execute();

		expect(readMessageHoverTrackingAssignment(body).user_targeted).toBe(true);
		expect(readMessageKeyboardFocusAssignment(body).user_targeted).toBe(true);
		expect(readBlockedMessageGroupsAssignment(body).user_targeted).toBe(true);
		expect(readGuildActivityLogPresentationAssignment(body).user_targeted).toBe(true);
		expect(readExpressionInfoCardAssignment(body).user_targeted).toBe(true);
		expect(readGuildHeaderCollapseAssignment(body).user_targeted).toBe(true);
		expect(readTypingIndicatorReworkAssignment(body).user_targeted).toBe(true);
		expect(readVoiceNoiseSuppressionAssignment(body).enabled).toBe(false);
	});

	it('serves the delivery cadence from the delivery config and not from the voice config', async () => {
		const account = await createTestAccount(harness);
		await getInstanceConfigRepository().setExperimentDeliveryConfig({
			poll_interval_seconds: 7200,
			poll_jitter_percent: 45,
		});
		await getInstanceConfigRepository().setVoiceNoiseSuppressionConfig({
			...DEFAULT_VOICE_NOISE_SUPPRESSION_CONFIG,
			enabled: true,
			config_version: 3,
			rollout_basis_points: 10000,
		});

		const body = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token).get(ENDPOINT).execute();

		expect(body.poll_interval_seconds).toBe(7200);
		expect(body.poll_jitter_percent).toBe(45);
		expect(body.assignments.voice_noise_suppression).toMatchObject({enabled: true, config_version: 3});
		expect(body.assignments.voice_noise_suppression).not.toHaveProperty('poll_interval_seconds');
		expect(body.assignments.voice_noise_suppression).not.toHaveProperty('poll_jitter_percent');
	});

	it('echoes the config version and resolves the caller through the allowlist', async () => {
		const targeted = await createTestAccount(harness);
		const untargeted = await createTestAccount(harness);
		await getInstanceConfigRepository().setVoiceNoiseSuppressionConfig({
			...DEFAULT_VOICE_NOISE_SUPPRESSION_CONFIG,
			enabled: true,
			config_version: 14,
			default_backend: 'rnnoise',
			rollout_basis_points: 0,
			included_user_ids: [targeted.userId],
		});

		const targetedBody = await createBuilder<ExperimentAssignmentsResponse>(harness, targeted.token)
			.get(ENDPOINT)
			.execute();
		expect(targetedBody.assignments.voice_noise_suppression).toMatchObject({
			enabled: true,
			config_version: 14,
			user_targeted: true,
			backend: 'rnnoise',
			source: 'user_rule',
		});

		const untargetedBody = await createBuilder<ExperimentAssignmentsResponse>(harness, untargeted.token)
			.get(ENDPOINT)
			.execute();
		expect(untargetedBody.assignments.voice_noise_suppression).toMatchObject({
			enabled: true,
			config_version: 14,
			user_targeted: false,
			backend: null,
			source: null,
		});
	});

	it('revalidates with a strong etag and answers 304 when nothing changed', async () => {
		const account = await createTestAccount(harness);

		const first = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token)
			.get(ENDPOINT)
			.executeWithResponse();
		const etag = first.response.headers.get('etag');
		expect(etag).toMatch(/^"[0-9a-f]{64}"$/);
		expect(first.response.headers.get('cache-control')).toBe('private, no-cache');
		expect(first.response.headers.get('vary')).toBe('Authorization, Origin');

		const revalidated = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token)
			.get(ENDPOINT)
			.header('If-None-Match', etag as string)
			.expect(NOT_MODIFIED)
			.executeWithResponse();
		expect(revalidated.response.status).toBe(NOT_MODIFIED);
		expect(revalidated.json).toBeUndefined();
		expect(revalidated.response.headers.get('etag')).toBe(etag);
		expect(revalidated.response.headers.get('vary')).toBe('Authorization, Origin');
	});

	it('lets a cross-origin client send If-None-Match and read the etag back', async () => {
		const preflight = await harness.requestJson({path: ENDPOINT, method: 'OPTIONS'});

		expect(preflight.headers.get('access-control-allow-headers')).toContain('If-None-Match');
		expect(preflight.headers.get('access-control-expose-headers')).toContain('ETag');
	});

	it('serves a fresh body once the voice config changes', async () => {
		const account = await createTestAccount(harness);

		const first = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token)
			.get(ENDPOINT)
			.executeWithResponse();
		const staleEtag = first.response.headers.get('etag') as string;

		await getInstanceConfigRepository().setVoiceNoiseSuppressionConfig({
			...DEFAULT_VOICE_NOISE_SUPPRESSION_CONFIG,
			enabled: true,
			config_version: 1,
			rollout_basis_points: 10000,
		});

		const refreshed = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token)
			.get(ENDPOINT)
			.header('If-None-Match', staleEtag)
			.executeWithResponse();
		expect(refreshed.response.status).toBe(HTTP_STATUS.OK);
		expect(refreshed.response.headers.get('etag')).not.toBe(staleEtag);
		expect(refreshed.json?.assignments.voice_noise_suppression).toMatchObject({
			enabled: true,
			config_version: 1,
			user_targeted: true,
		});
	});

	it('serves a fresh body once the delivery config changes', async () => {
		const account = await createTestAccount(harness);

		const first = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token)
			.get(ENDPOINT)
			.executeWithResponse();
		const staleEtag = first.response.headers.get('etag') as string;

		await getInstanceConfigRepository().setExperimentDeliveryConfig({
			poll_interval_seconds: 1800,
			poll_jitter_percent: 5,
		});

		const refreshed = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token)
			.get(ENDPOINT)
			.header('If-None-Match', staleEtag)
			.executeWithResponse();
		expect(refreshed.response.status).toBe(HTTP_STATUS.OK);
		expect(refreshed.response.headers.get('etag')).not.toBe(staleEtag);
		expect(refreshed.json?.poll_interval_seconds).toBe(1800);
		expect(refreshed.json?.poll_jitter_percent).toBe(5);
	});

	it('bumps the config version on every admin update without the client sending one', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_CONFIG_VIEW,
			AdminACLs.INSTANCE_CONFIG_UPDATE,
		]);

		const afterFirst = await createBuilder<{voice_noise_suppression: {config_version: number; enabled: boolean}}>(
			harness,
			admin.token,
		)
			.patch('/admin/instance/config')
			.body({voice_noise_suppression: {enabled: true, rollout_basis_points: 10000}})
			.execute();
		expect(afterFirst.voice_noise_suppression).toMatchObject({config_version: 1, enabled: true});

		const afterSecond = await createBuilder<{voice_noise_suppression: {config_version: number; enabled: boolean}}>(
			harness,
			admin.token,
		)
			.patch('/admin/instance/config')
			.body({voice_noise_suppression: {suppression_strength: 42}})
			.execute();
		expect(afterSecond.voice_noise_suppression).toMatchObject({config_version: 2, enabled: true});

		const body = await createBuilder<ExperimentAssignmentsResponse>(harness, admin.token).get(ENDPOINT).execute();
		expect(body.assignments.voice_noise_suppression).toMatchObject({
			enabled: true,
			config_version: 2,
			suppression_strength: 42,
		});
	});

	it('leaves the config version alone for an admin update that sets no field', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_CONFIG_VIEW,
			AdminACLs.INSTANCE_CONFIG_UPDATE,
		]);

		const afterFirst = await createBuilder<{voice_noise_suppression: {config_version: number; enabled: boolean}}>(
			harness,
			admin.token,
		)
			.patch('/admin/instance/config')
			.body({voice_noise_suppression: {enabled: true}})
			.execute();
		expect(afterFirst.voice_noise_suppression).toMatchObject({config_version: 1, enabled: true});

		const afterEmpty = await createBuilder<{voice_noise_suppression: {config_version: number; enabled: boolean}}>(
			harness,
			admin.token,
		)
			.patch('/admin/instance/config')
			.body({voice_noise_suppression: {}})
			.execute();
		expect(afterEmpty.voice_noise_suppression).toMatchObject({config_version: 1, enabled: true});

		const afterUndefined = await createBuilder<{voice_noise_suppression: {config_version: number; enabled: boolean}}>(
			harness,
			admin.token,
		)
			.patch('/admin/instance/config')
			.body({voice_noise_suppression: {enabled: undefined}})
			.execute();
		expect(afterUndefined.voice_noise_suppression).toMatchObject({config_version: 1, enabled: true});
	});

	it('serves the delivery cadence an admin set through the instance config', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_CONFIG_VIEW,
			AdminACLs.INSTANCE_CONFIG_UPDATE,
		]);

		const updated = await createBuilder<{experiment_delivery: ExperimentDeliveryConfigResponse}>(harness, admin.token)
			.patch('/admin/instance/config')
			.body({experiment_delivery: {poll_interval_seconds: 3600}})
			.execute();
		expect(updated.experiment_delivery).toEqual({
			poll_interval_seconds: 3600,
			poll_jitter_percent: DEFAULT_EXPERIMENT_POLL_JITTER_PERCENT,
		});

		const body = await createBuilder<ExperimentAssignmentsResponse>(harness, admin.token).get(ENDPOINT).execute();
		expect(body.poll_interval_seconds).toBe(3600);
		expect(body.poll_jitter_percent).toBe(DEFAULT_EXPERIMENT_POLL_JITTER_PERCENT);
	});
});
