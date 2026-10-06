// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditCoverageCase} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';
import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannel, createGuild} from '@app/api/channel/tests/AttachmentTestUtils';
import {markChannelAsIndexed, sendMessage} from '@app/api/message/tests/MessageTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createMessageWithImageAttachment} from '@app/api/user/tests/FavoriteMemeTestUtils';
import {expect} from 'vitest';

const UNKNOWN_ATTACHMENT_ID = '900000000000000003';
const UNKNOWN_SHRED_JOB_ID = '900000000000000004';

async function createAuthorChannel(harness: ApiTestHarness): Promise<{author: TestAccount; channelId: string}> {
	const author = await createTestAccount(harness);
	const guild = await createGuild(harness, author.token, 'Audit Message Guild');
	const channel = await createChannel(harness, author.token, guild.id, 'audit-messages');
	return {author, channelId: channel.id};
}

export const MessageAdminAuditCases: ReadonlyArray<AdminAuditCoverageCase> = [
	{
		method: 'GET',
		route: '/admin/messages',
		name: 'search',
		search: 'enabled',
		async prepare({harness}) {
			const {author, channelId} = await createAuthorChannel(harness);
			await markChannelAsIndexed(harness, channelId);
			await sendMessage(harness, author.token, channelId, 'quarterly harbour report');
			return {
				request: {path: `/admin/messages?channel_id=${channelId}&q=harbour&limit=10`},
				expected: {
					action: 'search_messages',
					targetType: 'channel',
					targetId: channelId,
					metadata: {mode: 'search', has_query: 'true', limit: '10', result_count: '1', total: '1'},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/messages',
		name: 'message',
		async prepare({harness}) {
			const {author, channelId} = await createAuthorChannel(harness);
			const message = await sendMessage(harness, author.token, channelId, 'lookup target');
			return {
				request: {path: `/admin/messages?channel_id=${channelId}&message_id=${message.id}&context_limit=10`},
				expected: {
					action: 'search_messages',
					targetType: 'message',
					targetId: message.id,
					metadata: {
						mode: 'message',
						channel_id: channelId,
						context_limit: '10',
						found: 'true',
						result_count: '1',
					},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/messages',
		name: 'attachment',
		async prepare({harness}) {
			const {author, channelId} = await createAuthorChannel(harness);
			const message = await createMessageWithImageAttachment(harness, author.token, channelId);
			const attachment = message.attachments[0];
			return {
				request: {
					path: `/admin/messages?channel_id=${channelId}&attachment_id=${attachment.id}&filename=${encodeURIComponent(attachment.filename)}`,
				},
				expected: {
					action: 'search_messages',
					targetType: 'channel',
					targetId: channelId,
					metadata: {
						mode: 'attachment',
						attachment_id: attachment.id,
						message_id: message.id,
						context_limit: '50',
						found: 'true',
						result_count: '1',
					},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/messages',
		name: 'attachment not found',
		async prepare({harness}) {
			const {channelId} = await createAuthorChannel(harness);
			return {
				request: {
					path: `/admin/messages?channel_id=${channelId}&attachment_id=${UNKNOWN_ATTACHMENT_ID}&filename=missing.png`,
				},
				expected: {
					action: 'search_messages',
					targetType: 'channel',
					targetId: channelId,
					metadata: {
						mode: 'attachment',
						attachment_id: UNKNOWN_ATTACHMENT_ID,
						context_limit: '50',
						found: 'false',
						result_count: '0',
					},
				},
			};
		},
	},
	{
		method: 'POST',
		route: '/admin/messages/ncmec-reports',
		auditLogReason: expect.stringMatching(/^NCMEC Report \S+ - \S+$/),
		async prepare({harness}) {
			const {author, channelId} = await createAuthorChannel(harness);
			const message = await createMessageWithImageAttachment(harness, author.token, channelId);
			const attachment = message.attachments[0];
			return {
				request: {
					path: '/admin/messages/ncmec-reports',
					body: {
						channel_id: channelId,
						message_id: message.id,
						attachment_id: attachment.id,
						filename: attachment.filename,
						reporter_full_name: 'Audit Coverage Reporter',
						confirmed_viewed: true,
					},
				},
				expected: {
					action: 'NCMEC Report',
					targetType: 'user',
					targetId: author.userId,
					metadata: {
						channel_id: channelId,
						message_id: message.id,
						attachment_id: attachment.id,
						reported_user_email: author.email,
						reported_user_email_verified: 'true',
						reported_user_date_of_birth: '2000-01-01',
						reported_user_last_active_ip: '127.0.0.1',
						attachment_upload_mode: 'form_data',
						attachment_upload_request_ip: '127.0.0.1',
						attachment_upload_requested_at: expect.any(String),
						attachment_upload_ip_source: 'attachment upload request handled directly by Fluxer API',
					},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/messages/shreds/:job_id',
		async prepare() {
			return {
				request: {path: `/admin/messages/shreds/${UNKNOWN_SHRED_JOB_ID}`},
				expected: {
					action: 'get_message_shred_status',
					targetType: 'message_shred',
					targetId: '0',
					metadata: {job_id: UNKNOWN_SHRED_JOB_ID, status: 'not_found'},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/channels/:channel_id/messages',
		async prepare({harness}) {
			const {author, channelId} = await createAuthorChannel(harness);
			const first = await sendMessage(harness, author.token, channelId, 'first');
			await sendMessage(harness, author.token, channelId, 'second');
			await sendMessage(harness, author.token, channelId, 'third');
			return {
				request: {path: `/admin/channels/${channelId}/messages?limit=1&after=${first.id}`},
				expected: {
					action: 'list_channel_messages',
					targetType: 'channel',
					targetId: channelId,
					metadata: {limit: '1', after: first.id, result_count: '1', has_more: 'true'},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/channels/:channel_id/messages/:message_id',
		async prepare({harness}) {
			const {author, channelId} = await createAuthorChannel(harness);
			await sendMessage(harness, author.token, channelId, 'context before');
			const message = await sendMessage(harness, author.token, channelId, 'detail target');
			return {
				request: {path: `/admin/channels/${channelId}/messages/${message.id}`},
				expected: {
					action: 'get_message',
					targetType: 'message',
					targetId: message.id,
					metadata: {channel_id: channelId, context_limit: '50', found: 'true', result_count: '2'},
				},
			};
		},
	},
	{
		method: 'DELETE',
		route: '/admin/channels/:channel_id/messages/:message_id',
		async prepare({harness}) {
			const {author, channelId} = await createAuthorChannel(harness);
			const message = await sendMessage(harness, author.token, channelId, 'delete target');
			return {
				request: {path: `/admin/channels/${channelId}/messages/${message.id}`},
				expected: {
					action: 'delete_message',
					targetType: 'message',
					targetId: message.id,
					metadata: {channel_id: channelId, message_id: message.id},
				},
			};
		},
	},
	{
		method: 'POST',
		route: '/admin/users/:user_id/message-shreds',
		async prepare({harness}) {
			const {author, channelId} = await createAuthorChannel(harness);
			const message = await sendMessage(harness, author.token, channelId, 'shred target');
			return {
				request: {
					path: `/admin/users/${author.userId}/message-shreds`,
					body: {entries: [{channel_id: channelId, message_id: message.id}]},
				},
				expected: {
					action: 'queue_message_shred',
					targetType: 'message_shred',
					targetId: author.userId,
					metadata: {user_id: author.userId, job_id: expect.any(String), requested_entries: '1'},
				},
			};
		},
	},
	{
		method: 'DELETE',
		route: '/admin/users/:user_id/messages',
		async prepare({harness}) {
			const {author, channelId} = await createAuthorChannel(harness);
			await sendMessage(harness, author.token, channelId, 'counted message');
			return {
				request: {path: `/admin/users/${author.userId}/messages`},
				expected: {
					action: 'delete_all_user_messages_dry_run',
					targetType: 'message_deletion',
					targetId: author.userId,
					metadata: {user_id: author.userId, channel_count: '1', message_count: '1', dry_run: 'true'},
				},
			};
		},
	},
];
