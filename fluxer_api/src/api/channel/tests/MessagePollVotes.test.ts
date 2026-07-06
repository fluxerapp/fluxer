// SPDX-License-Identifier: AGPL-3.0-or-later

import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {GuildAuditLogListResponse} from '@fluxer/schema/src/domains/guild/GuildAuditLogSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import {createTestAccount, setUserACLs} from '../../auth/tests/AuthTestUtils';
import {ensureSessionStarted} from '../../message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';
import {createMultipartFormData, sendMessageWithAttachments} from './AttachmentTestUtils';
import {createPermissionOverwrite, setupTestGuildWithMembers} from './ChannelTestUtils';

async function createPollMessage(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
	options: {
		allowCustomAnswers?: boolean;
		allowRankedChoice?: boolean;
		anonymous?: boolean;
		optionTexts?: Array<string>;
	} = {},
): Promise<MessageResponse> {
	await ensureSessionStarted(harness, token);
	const message = await createBuilder<MessageResponse>(harness, token)
		.post(`/channels/${channelId}/messages`)
		.body({
			poll: {
				title: 'Choose a deploy window',
				options: (options.optionTexts ?? ['Morning', 'Afternoon']).map((text) => ({text})),
				duration_seconds: 3600,
				anonymous: options.anonymous ?? false,
				allow_ranked_choice: options.allowRankedChoice ?? false,
				allow_custom_answers: options.allowCustomAnswers ?? false,
			},
		})
		.expect(HTTP_STATUS.OK)
		.execute();
	if (!message.poll) {
		throw new Error('Expected created message to include poll');
	}
	return message;
}

async function getMessage(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
	messageId: string,
): Promise<MessageResponse> {
	return createBuilder<MessageResponse>(harness, token)
		.get(`/channels/${channelId}/messages/${messageId}`)
		.expect(HTTP_STATUS.OK)
		.execute();
}

describe('Message poll votes', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	it('sets, replaces, and removes the current user vote', async () => {
		const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const voter = members[0]!;
		await ensureSessionStarted(harness, voter.token);
		const message = await createPollMessage(harness, owner.token, systemChannel.id);
		const [morning, afternoon] = message.poll!.options;

		await createBuilder<void>(harness, voter.token)
			.put(`/channels/${systemChannel.id}/messages/${message.id}/poll/votes/@me`)
			.body({option_ids: [morning.id]})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();

		let updated = await getMessage(harness, voter.token, systemChannel.id, message.id);
		expect(updated.poll?.options[0]).toMatchObject({
			id: morning.id,
			vote_count: 1,
			me: true,
			voter_ids: [voter.userId],
		});
		expect(updated.poll?.options[1]).toMatchObject({
			id: afternoon.id,
			vote_count: 0,
			voter_ids: [],
		});

		await createBuilder<void>(harness, voter.token)
			.put(`/channels/${systemChannel.id}/messages/${message.id}/poll/votes/@me`)
			.body({option_ids: [afternoon.id]})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();

		updated = await getMessage(harness, voter.token, systemChannel.id, message.id);
		expect(updated.poll?.options[0]).toMatchObject({id: morning.id, vote_count: 0});
		expect(updated.poll?.options[1]).toMatchObject({
			id: afternoon.id,
			vote_count: 1,
			me: true,
			voter_ids: [voter.userId],
		});

		await createBuilder<void>(harness, voter.token)
			.delete(`/channels/${systemChannel.id}/messages/${message.id}/poll/votes/@me`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();

		updated = await getMessage(harness, voter.token, systemChannel.id, message.id);
		expect(updated.poll?.options[0]).toMatchObject({id: morning.id, vote_count: 0, voter_ids: []});
		expect(updated.poll?.options[1]).toMatchObject({id: afternoon.id, vote_count: 0, voter_ids: []});
		expect(updated.poll?.options.some((option) => option.me)).toBe(false);
	});

	it('rejects invalid simple votes and rejects votes after close', async () => {
		const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const voter = members[0]!;
		await ensureSessionStarted(harness, voter.token);
		const message = await createPollMessage(harness, owner.token, systemChannel.id);
		const [morning, afternoon] = message.poll!.options;

		await createBuilder<void>(harness, voter.token)
			.put(`/channels/${systemChannel.id}/messages/${message.id}/poll/votes/@me`)
			.body({option_ids: [morning.id, afternoon.id]})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();

		await createBuilder<void>(harness, owner.token)
			.post(`/channels/${systemChannel.id}/messages/${message.id}/poll/close`)
			.body({})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();

		const closedMessage = await getMessage(harness, voter.token, systemChannel.id, message.id);
		expect(closedMessage.poll?.closed).toBe(true);

		await createBuilder<void>(harness, voter.token)
			.put(`/channels/${systemChannel.id}/messages/${message.id}/poll/votes/@me`)
			.body({option_ids: [morning.id]})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});

	it('accepts ranked-choice votes and exposes aggregate ranked results', async () => {
		const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 2);
		const [firstVoter, secondVoter] = members;
		await ensureSessionStarted(harness, firstVoter!.token);
		await ensureSessionStarted(harness, secondVoter!.token);
		const message = await createPollMessage(harness, owner.token, systemChannel.id, {
			allowRankedChoice: true,
			optionTexts: ['Morning', 'Afternoon', 'Evening'],
		});
		const [morning, afternoon, evening] = message.poll!.options;

		await createBuilder<void>(harness, firstVoter!.token)
			.put(`/channels/${systemChannel.id}/messages/${message.id}/poll/votes/@me`)
			.body({option_ids: [morning.id, afternoon.id, evening.id]})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		await createBuilder<void>(harness, secondVoter!.token)
			.put(`/channels/${systemChannel.id}/messages/${message.id}/poll/votes/@me`)
			.body({option_ids: [afternoon.id, morning.id]})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();

		const updated = await getMessage(harness, secondVoter!.token, systemChannel.id, message.id);
		expect(updated.poll?.options[0]).toMatchObject({
			id: morning.id,
			vote_count: 1,
			rank_counts: [1, 1, 0],
			ranked_score: 5,
			me: true,
			voter_ids: [firstVoter!.userId],
		});
		expect(updated.poll?.options[1]).toMatchObject({
			id: afternoon.id,
			vote_count: 1,
			rank_counts: [1, 1, 0],
			ranked_score: 5,
			me: true,
			voter_ids: [secondVoter!.userId],
		});
		expect(updated.poll?.options[2]).toMatchObject({
			id: evening.id,
			vote_count: 0,
			rank_counts: [0, 0, 1],
			ranked_score: 1,
			voter_ids: [],
		});
	});

	it('hides voters but keeps own choice and aggregate ranked results for anonymous polls', async () => {
		const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 2);
		const [firstVoter, secondVoter] = members;
		await ensureSessionStarted(harness, firstVoter!.token);
		await ensureSessionStarted(harness, secondVoter!.token);
		const message = await createPollMessage(harness, owner.token, systemChannel.id, {
			anonymous: true,
			allowRankedChoice: true,
			optionTexts: ['Morning', 'Afternoon', 'Evening'],
		});
		const [morning, afternoon, evening] = message.poll!.options;

		await createBuilder<void>(harness, firstVoter!.token)
			.put(`/channels/${systemChannel.id}/messages/${message.id}/poll/votes/@me`)
			.body({option_ids: [morning.id, afternoon.id, evening.id]})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		await createBuilder<void>(harness, secondVoter!.token)
			.put(`/channels/${systemChannel.id}/messages/${message.id}/poll/votes/@me`)
			.body({option_ids: [afternoon.id, morning.id]})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();

		const updated = await getMessage(harness, secondVoter!.token, systemChannel.id, message.id);
		expect(updated.poll?.anonymous).toBe(true);
		expect(updated.poll?.options[0]).toMatchObject({
			id: morning.id,
			rank_counts: [1, 1, 0],
			ranked_score: 5,
			me: true,
			voter_ids: null,
		});
		expect(updated.poll?.options[1]).toMatchObject({
			id: afternoon.id,
			rank_counts: [1, 1, 0],
			ranked_score: 5,
			me: true,
			voter_ids: null,
		});
		expect(updated.poll?.options[2]).toMatchObject({
			id: evening.id,
			rank_counts: [0, 0, 1],
			ranked_score: 1,
			voter_ids: null,
		});
	});

	it('adds a custom option when the poll allows custom answers', async () => {
		const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const voter = members[0]!;
		await ensureSessionStarted(harness, voter.token);
		const message = await createPollMessage(harness, owner.token, systemChannel.id, {allowCustomAnswers: true});

		await createBuilder<void>(harness, voter.token)
			.post(`/channels/${systemChannel.id}/messages/${message.id}/poll/options`)
			.body({text: 'Evening'})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();

		const updated = await getMessage(harness, voter.token, systemChannel.id, message.id);
		const customOption = updated.poll?.options.find((option) => option.text === 'Evening');
		expect(customOption).toMatchObject({
			vote_count: 0,
			voter_ids: [],
		});

		await createBuilder<void>(harness, voter.token)
			.put(`/channels/${systemChannel.id}/messages/${message.id}/poll/votes/@me`)
			.body({option_ids: [customOption!.id]})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();

		const voted = await getMessage(harness, voter.token, systemChannel.id, message.id);
		expect(voted.poll?.options.find((option) => option.id === customOption!.id)).toMatchObject({
			text: 'Evening',
			vote_count: 1,
			me: true,
			voter_ids: [voter.userId],
		});
	});

	it('adds a custom option with an image attachment', async () => {
		const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const voter = members[0]!;
		await ensureSessionStarted(harness, voter.token);
		const message = await createPollMessage(harness, owner.token, systemChannel.id, {allowCustomAnswers: true});
		const {body, contentType} = createMultipartFormData({text: 'Evening'}, [
			{index: 0, filename: 'evening.png', data: Buffer.from('not a real png'), contentType: 'image/png'},
		]);
		const headers = new Headers();
		headers.set('Content-Type', contentType);
		headers.set('Authorization', voter.token);
		headers.set('x-forwarded-for', '127.0.0.1');

		const response = await harness.app.request(`/channels/${systemChannel.id}/messages/${message.id}/poll/options`, {
			method: 'POST',
			headers,
			body,
		});
		expect(response.status).toBe(HTTP_STATUS.NO_CONTENT);

		const updated = await getMessage(harness, voter.token, systemChannel.id, message.id);
		const attachment = updated.attachments?.[0];
		if (!attachment) {
			throw new Error('Expected custom poll option image attachment');
		}
		const customOption = updated.poll?.options.find((option) => option.text === 'Evening');
		expect(customOption).toMatchObject({
			text: 'Evening',
			attachment_id: attachment.id,
			vote_count: 0,
			voter_ids: [],
		});
	});

	it('rejects a custom option attachment that is not an image', async () => {
		const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const voter = members[0]!;
		await ensureSessionStarted(harness, voter.token);
		const message = await createPollMessage(harness, owner.token, systemChannel.id, {allowCustomAnswers: true});
		const {body, contentType} = createMultipartFormData({text: 'Evening'}, [
			{index: 0, filename: 'notes.txt', data: Buffer.from('not an image'), contentType: 'text/plain'},
		]);
		const headers = new Headers();
		headers.set('Content-Type', contentType);
		headers.set('Authorization', voter.token);
		headers.set('x-forwarded-for', '127.0.0.1');

		const response = await harness.app.request(`/channels/${systemChannel.id}/messages/${message.id}/poll/options`, {
			method: 'POST',
			headers,
			body,
		});
		expect(response.status).toBe(HTTP_STATUS.BAD_REQUEST);

		const updated = await getMessage(harness, voter.token, systemChannel.id, message.id);
		expect(updated.poll?.options.some((option) => option.text === 'Evening')).toBe(false);
		expect(updated.attachments ?? []).toEqual([]);
	});

	it('records custom options and poll closes in the guild audit log', async () => {
		const {owner, members, guild, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const voter = members[0]!;
		await ensureSessionStarted(harness, voter.token);
		const message = await createPollMessage(harness, owner.token, systemChannel.id, {allowCustomAnswers: true});

		await createBuilder<void>(harness, voter.token)
			.post(`/channels/${systemChannel.id}/messages/${message.id}/poll/options`)
			.body({text: 'Evening'})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();

		const updated = await getMessage(harness, voter.token, systemChannel.id, message.id);
		const customOption = updated.poll?.options.find((option) => option.text === 'Evening');
		expect(customOption).toBeTruthy();

		await createBuilder<void>(harness, owner.token)
			.post(`/channels/${systemChannel.id}/messages/${message.id}/poll/close`)
			.body({})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();

		const optionLogs = await createBuilder<GuildAuditLogListResponse>(harness, owner.token)
			.get(`/guilds/${guild.id}/audit-logs?action_type=${AuditLogActionType.POLL_OPTION_CREATE}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(optionLogs.audit_log_entries[0]).toMatchObject({
			action_type: AuditLogActionType.POLL_OPTION_CREATE,
			user_id: voter.userId,
			target_id: customOption!.id,
			options: {
				channel_id: systemChannel.id,
				message_id: message.id,
				poll_id: message.poll!.id,
				option_id: customOption!.id,
			},
		});

		const closeLogs = await createBuilder<GuildAuditLogListResponse>(harness, owner.token)
			.get(`/guilds/${guild.id}/audit-logs?action_type=${AuditLogActionType.POLL_CLOSE}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(closeLogs.audit_log_entries[0]).toMatchObject({
			action_type: AuditLogActionType.POLL_CLOSE,
			user_id: owner.userId,
			target_id: message.poll!.id,
			options: {
				channel_id: systemChannel.id,
				message_id: message.id,
				poll_id: message.poll!.id,
			},
		});
	});

	it('requires create polls permission for guild poll messages', async () => {
		const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		await ensureSessionStarted(harness, member.token);
		await createPermissionOverwrite(harness, owner.token, systemChannel.id, member.userId, {
			type: 1,
			allow: '0',
			deny: Permissions.CREATE_POLLS.toString(),
		});

		await createBuilder<MessageResponse>(harness, member.token)
			.post(`/channels/${systemChannel.id}/messages`)
			.body({content: 'Plain message still allowed'})
			.expect(HTTP_STATUS.OK)
			.execute();

		await createBuilder<void>(harness, member.token)
			.post(`/channels/${systemChannel.id}/messages`)
			.body({
				poll: {
					title: 'Choose one',
					options: [{text: 'A'}, {text: 'B'}],
					duration_seconds: 3600,
				},
			})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});

	it('keeps poll messages when deleting an image-backed poll option attachment', async () => {
		const {owner, systemChannel} = await setupTestGuildWithMembers(harness, 0);
		const {response, json: message} = await sendMessageWithAttachments(
			harness,
			owner.token,
			systemChannel.id,
			{
				poll: {
					title: 'Choose the concept',
					options: [{text: 'Mockup', attachment_id: 0}, {text: 'Text only'}],
					duration_seconds: 3600,
				},
				attachments: [{id: 0, filename: 'mockup.png'}],
			},
			[{index: 0, filename: 'mockup.png', data: Buffer.from('not a real png'), contentType: 'image/png'}],
		);
		expect(response.status).toBe(HTTP_STATUS.OK);
		const attachment = message.attachments?.[0];
		if (!attachment) {
			throw new Error('Expected poll image attachment');
		}
		expect(message.poll?.options[0].attachment_id).toBe(attachment.id);

		await createBuilder<void>(harness, owner.token)
			.delete(`/channels/${systemChannel.id}/messages/${message.id}/attachments/${attachment.id}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();

		const updated = await getMessage(harness, owner.token, systemChannel.id, message.id);
		expect(updated.attachments ?? []).toEqual([]);
		expect(updated.poll?.options[0]).toMatchObject({
			text: 'Mockup',
			attachment_id: null,
		});
		expect(updated.poll?.options[1]).toMatchObject({
			text: 'Text only',
		});
	});

	it('rejects non-image attachments for image-backed poll options', async () => {
		const {owner, systemChannel} = await setupTestGuildWithMembers(harness, 0);
		const {response} = await sendMessageWithAttachments(
			harness,
			owner.token,
			systemChannel.id,
			{
				poll: {
					title: 'Choose the artifact',
					options: [{text: 'Notes', attachment_id: 0}, {text: 'Text only'}],
					duration_seconds: 3600,
				},
				attachments: [{id: 0, filename: 'notes.txt'}],
			},
			[{index: 0, filename: 'notes.txt', data: Buffer.from('not an image'), contentType: 'text/plain'}],
		);

		expect(response.status).toBe(HTTP_STATUS.BAD_REQUEST);
	});

	it('rejects missing attachment references for image-backed poll options', async () => {
		const {owner, systemChannel} = await setupTestGuildWithMembers(harness, 0);
		await ensureSessionStarted(harness, owner.token);

		await createBuilder<void>(harness, owner.token)
			.post(`/channels/${systemChannel.id}/messages`)
			.body({
				poll: {
					title: 'Choose the concept',
					options: [{text: 'Mockup', attachment_id: 0}, {text: 'Text only'}],
					duration_seconds: 3600,
				},
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});

	it('lets admins delete an image-backed poll option attachment and audits poll context', async () => {
		const {owner, systemChannel} = await setupTestGuildWithMembers(harness, 0);
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'message:delete', 'audit_log:view']);
		const {response, json: message} = await sendMessageWithAttachments(
			harness,
			owner.token,
			systemChannel.id,
			{
				poll: {
					title: 'Choose the concept',
					options: [{text: 'Mockup', attachment_id: 0}, {text: 'Text only'}],
					duration_seconds: 3600,
				},
				attachments: [{id: 0, filename: 'mockup.png'}],
			},
			[{index: 0, filename: 'mockup.png', data: Buffer.from('not a real png'), contentType: 'image/png'}],
		);
		expect(response.status).toBe(HTTP_STATUS.OK);
		const attachment = message.attachments?.[0];
		if (!attachment) {
			throw new Error('Expected poll image attachment');
		}

		await createBuilder<void>(harness, admin.token)
			.post('/admin/messages/delete-attachment')
			.body({
				channel_id: systemChannel.id,
				message_id: message.id,
				attachment_id: attachment.id,
			})
			.expect(HTTP_STATUS.OK)
			.execute();

		const updated = await getMessage(harness, owner.token, systemChannel.id, message.id);
		expect(updated.attachments ?? []).toEqual([]);
		expect(updated.poll?.options[0]).toMatchObject({
			text: 'Mockup',
			attachment_id: null,
		});

		const auditLogs = await createBuilder<{
			logs: Array<{
				action: string;
				target_type: string;
				target_id: string;
				metadata: Record<string, string>;
			}>;
		}>(harness, admin.token)
			.post('/admin/audit-logs')
			.body({target_type: 'message_attachment', target_id: attachment.id, limit: 10})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(auditLogs.logs[0]).toMatchObject({
			action: 'delete_message_attachment',
			target_type: 'message_attachment',
			target_id: attachment.id,
			metadata: {
				channel_id: systemChannel.id,
				message_id: message.id,
				attachment_id: attachment.id,
				filename: 'mockup.png',
				poll_id: message.poll!.id,
				poll_option_id: message.poll!.options[0].id,
			},
		});
	});

	it('rejects custom options when disabled or duplicate', async () => {
		const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const voter = members[0]!;
		await ensureSessionStarted(harness, voter.token);
		const message = await createPollMessage(harness, owner.token, systemChannel.id);

		await createBuilder<void>(harness, voter.token)
			.post(`/channels/${systemChannel.id}/messages/${message.id}/poll/options`)
			.body({text: 'Evening'})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();

		const customMessage = await createPollMessage(harness, owner.token, systemChannel.id, {allowCustomAnswers: true});
		await createBuilder<void>(harness, voter.token)
			.post(`/channels/${systemChannel.id}/messages/${customMessage.id}/poll/options`)
			.body({text: 'morning'})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
});
