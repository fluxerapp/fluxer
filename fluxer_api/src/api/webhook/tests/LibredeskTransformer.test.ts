// SPDX-License-Identifier: AGPL-3.0-or-later

import {transformLibredeskWebhook} from '@app/api/webhook/transformers/LibredeskTransformer';
import type {RichEmbedRequest} from '@fluxer/schema/src/domains/message/MessageRequestSchemas';
import {LibredeskWebhook} from '@fluxer/schema/src/domains/webhook/LibredeskWebhookSchemas';
import {describe, expect, it} from 'vitest';

const CONVERSATION_UUID = '3f1c2a7e-8b4d-4e6a-9c1f-5d2e7b8a9c01';
const EMBED_TOTAL_MAX_LENGTH = 6000;
const COMBINING_ACUTE = String.fromCodePoint(0x301);
const RIGHT_TO_LEFT_OVERRIDE = String.fromCodePoint(0x202e);
const LINE_SEPARATOR = String.fromCodePoint(0x2028);

function createContact(overrides: Record<string, unknown> = {}) {
	return {
		id: 17,
		created_at: '2026-10-15T09:12:30.118204Z',
		updated_at: '2026-10-15T09:12:30.118204Z',
		first_name: 'Example contact',
		last_name: '',
		email: 'contact@example.com',
		type: 'contact',
		availability_status: 'offline',
		avatar_url: null,
		phone_number: null,
		phone_number_country_code: null,
		country: null,
		custom_attributes: {},
		enabled: true,
		last_active_at: null,
		last_login_at: null,
		external_user_id: null,
		...overrides,
	};
}

function createConversation(overrides: Record<string, unknown> = {}) {
	return {
		id: 42,
		created_at: '2026-10-15T09:12:30.154871Z',
		updated_at: '2026-10-15T09:12:30.154871Z',
		uuid: CONVERSATION_UUID,
		contact_id: 17,
		inbox_id: 1,
		closed_at: null,
		resolved_at: null,
		reference_number: '1042',
		priority: null,
		priority_id: null,
		status: 'Open',
		status_category: 'open',
		status_id: 1,
		first_reply_at: null,
		last_reply_at: null,
		assigned_user_id: null,
		assigned_team_id: null,
		waiting_since: '2026-10-15T09:12:30.201377Z',
		snoozed_until: null,
		subject: 'Cannot sign in',
		inbox_mail: 'support@example.com',
		inbox_reply_to: '',
		inbox_name: 'Support',
		inbox_channel: 'email',
		tags: [],
		meta: {},
		custom_attributes: {},
		last_message_at: '2026-10-15T09:12:30.101552Z',
		last_message: 'Hello, I cannot sign in to my account since this morning.',
		last_message_sender: 'contact',
		last_interaction: 'Hello, I cannot sign in to my account since this morning.',
		last_interaction_at: '2026-10-15T09:12:30.101552Z',
		last_interaction_sender: 'contact',
		contact: createContact(),
		sla_policy_id: null,
		sla_policy_name: null,
		applied_sla_id: null,
		next_sla_deadline_at: null,
		first_response_deadline_at: null,
		resolution_deadline_at: null,
		next_response_deadline_at: null,
		next_response_met_at: null,
		csat_rating: null,
		csat_feedback: null,
		csat_responded_at: null,
		previous_conversations: null,
		...overrides,
	};
}

function createAgentAuthor(overrides: Record<string, unknown> = {}) {
	return {
		id: 5,
		first_name: 'Support',
		last_name: 'agent',
		email: 'agent@example.com',
		avatar_url: null,
		availability_status: 'online',
		type: 'agent',
		last_active_at: '2026-10-15T09:20:02.77631Z',
		...overrides,
	};
}

function createMessage(overrides: Record<string, unknown> = {}) {
	return {
		id: 311,
		created_at: '2026-10-15T09:12:30.101552Z',
		updated_at: '2026-10-15T09:12:30.101552Z',
		uuid: 'c8a1f2e3-4b5d-4c6e-8f7a-9b0c1d2e3f40',
		type: 'incoming',
		status: 'received',
		conversation_id: 42,
		conversation_uuid: CONVERSATION_UUID,
		content:
			'<div dir="ltr"><p>Hello, I cannot sign in to my account since this morning.</p><p>The page says my password is wrong.</p></div>',
		text_content: 'Hello, I cannot sign in to my account since this morning.\n\nThe page says my password is wrong.',
		content_type: 'html',
		private: false,
		sender_id: 17,
		sender_type: 'contact',
		meta: {bcc: [], cc: [], from: ['contact@example.com'], subject: 'Cannot sign in', to: ['support@example.com']},
		attachments: [
			{
				name: 'screenshot.png',
				size: 48213,
				content: null,
				content_id: '',
				content_type: 'image/png',
				disposition: 'attachment',
				uuid: '7b2e9d14-3c5a-4f8e-a1b6-0d9c8e7f6a52',
				url: 'https://desk.example.com/uploads/7b2e9d14-3c5a-4f8e-a1b6-0d9c8e7f6a52?sig=example&exp=1792143150',
				thumbnail_url: 'https://desk.example.com/uploads/thumb_7b2e9d14-3c5a-4f8e-a1b6-0d9c8e7f6a52?sig=example',
			},
		],
		author: {
			id: 17,
			first_name: 'Example contact',
			last_name: '',
			email: 'contact@example.com',
			avatar_url: null,
			availability_status: 'offline',
			type: 'contact',
			last_active_at: null,
		},
		...overrides,
	};
}

function createOutgoingMessage(overrides: Record<string, unknown> = {}) {
	return createMessage({
		id: 318,
		uuid: '0e4d7c2b-9a1f-4b3e-8d6c-5f2a1b0e9d87',
		type: 'outgoing',
		status: 'pending',
		content: '<p>Thanks for reaching out. Please try again.</p>',
		text_content: 'Thanks for reaching out. Please try again.',
		sender_id: 5,
		sender_type: 'agent',
		meta: {to: ['contact@example.com']},
		attachments: [],
		author: createAgentAuthor(),
		...overrides,
	});
}

function render(event: string, payload: unknown, timestamp = '2026-10-15T09:12:30Z'): RichEmbedRequest | null {
	return transformLibredeskWebhook(LibredeskWebhook.parse({event, payload, timestamp}));
}

function renderedText(embed: RichEmbedRequest): Array<string> {
	return [
		embed.title,
		embed.description,
		embed.author?.name,
		embed.footer?.text,
		...(embed.fields ?? []).flatMap((field) => [field.name, field.value]),
	].filter((value): value is string => typeof value === 'string');
}

function embedLength(embed: RichEmbedRequest): number {
	return renderedText(embed).reduce((total, value) => total + value.length, 0);
}

function markdownSlots(embed: RichEmbedRequest): Array<string> {
	return [embed.title, embed.description, ...(embed.fields ?? []).map((field) => field.value)].filter(
		(value): value is string => typeof value === 'string',
	);
}

function unescapedTrigger(value: string): RegExpMatchArray | null {
	return value.match(/(?<!\\)(?:\\\\)*[<@[\]]/);
}

describe('Libredesk transformer', () => {
	describe('conversation events', () => {
		it('renders conversation.created with the inbox, reference, subject and contact name', () => {
			expect(render('conversation.created', createConversation())).toEqual({
				title: '\\[Support\\] New conversation: #1042 Cannot sign in',
				color: 0x3b82f6,
				author: {name: 'Example contact'},
				footer: {text: `Conversation ${CONVERSATION_UUID}`},
			});
		});

		it('never renders the contact email or phone number', () => {
			const embed = render(
				'conversation.created',
				createConversation({contact: createContact({phone_number: '5550100', country: 'Example country'})}),
			);
			const text = renderedText(embed!).join('\n');
			expect(text).not.toContain('contact@example.com');
			expect(text).not.toContain('5550100');
			expect(text).not.toContain('support@example.com');
		});

		it('renders conversation.status_changed with both statuses', () => {
			const embed = render('conversation.status_changed', {
				actor_id: 5,
				conversation: createConversation({status: 'Resolved', status_category: 'resolved'}),
				conversation_uuid: CONVERSATION_UUID,
				new_status: 'Resolved',
				previous_status: 'Open',
				snooze_until: '',
			});
			expect(embed).toEqual({
				title: '\\[Support\\] Status changed: #1042 Cannot sign in',
				color: 0x3ecf8e,
				description: 'Open to Resolved',
				footer: {text: `Conversation ${CONVERSATION_UUID}`},
			});
		});

		it('renders the snooze end as a timestamp', () => {
			const embed = render('conversation.status_changed', {
				actor_id: 5,
				conversation: createConversation({status: 'Snoozed', status_category: 'waiting'}),
				conversation_uuid: CONVERSATION_UUID,
				new_status: 'Snoozed',
				previous_status: 'Open',
				snooze_until: '2026-10-16T09:40:05Z',
			});
			expect(embed?.description).toBe(
				`Open to Snoozed until <t:${Math.floor(Date.parse('2026-10-16T09:40:05Z') / 1000)}:f>`,
			);
			expect(embed?.color).toBe(0xf5a623);
		});

		it('drops a snooze end outside the timestamp range', () => {
			for (const snooze_until of ['1970-01-01T00:00:00Z', '1969-12-31T00:00:00Z', '-271821-04-20T00:00:00Z', 'soon']) {
				const embed = render('conversation.status_changed', {
					conversation: createConversation({status: 'Snoozed', status_category: 'waiting'}),
					conversation_uuid: CONVERSATION_UUID,
					new_status: 'Snoozed',
					previous_status: 'Open',
					snooze_until,
				});
				expect(embed?.description).toBe('Open to Snoozed');
			}
		});

		it('skips a status change without a new status', () => {
			expect(render('conversation.status_changed', {conversation_uuid: CONVERSATION_UUID, new_status: ''})).toBeNull();
		});

		it('renders conversation.tags_changed as the added and removed tags', () => {
			const embed = render('conversation.tags_changed', {
				actor_id: 5,
				conversation: createConversation({tags: ['account', 'login']}),
				conversation_uuid: CONVERSATION_UUID,
				new_tags: ['account', 'login'],
				previous_tags: ['account', 'billing'],
			});
			expect(embed?.title).toBe('\\[Support\\] Tags changed: #1042 Cannot sign in');
			expect(embed?.fields).toEqual([
				{name: 'Added', value: 'login', inline: true},
				{name: 'Removed', value: 'billing', inline: true},
			]);
		});

		it('skips a tags change that changed nothing', () => {
			expect(
				render('conversation.tags_changed', {
					conversation: createConversation(),
					conversation_uuid: CONVERSATION_UUID,
					new_tags: ['account'],
					previous_tags: ['account'],
				}),
			).toBeNull();
		});

		it('renders conversation.assigned with the agent ID', () => {
			const embed = render('conversation.assigned', {
				actor_id: 6,
				assigned_to: 5,
				conversation: createConversation({assigned_user_id: 5}),
				conversation_uuid: CONVERSATION_UUID,
			});
			expect(embed?.title).toBe('\\[Support\\] Conversation assigned: #1042 Cannot sign in');
			expect(embed?.fields).toEqual([{name: 'Agent ID', value: '5', inline: true}]);
		});

		it('renders conversation.unassigned', () => {
			const embed = render('conversation.unassigned', {
				actor_id: 5,
				conversation: createConversation({assigned_team_id: 2}),
				conversation_uuid: CONVERSATION_UUID,
			});
			expect(embed?.title).toBe('\\[Support\\] Conversation unassigned: #1042 Cannot sign in');
			expect(embed?.footer).toEqual({text: `Conversation ${CONVERSATION_UUID}`});
		});

		it('falls back to the top-level uuid when Libredesk could not load the conversation', () => {
			const embed = render('conversation.unassigned', {
				actor_id: 5,
				conversation: createConversation({
					id: 0,
					uuid: '',
					reference_number: '',
					subject: null,
					inbox_name: '',
					inbox_channel: '',
					status: null,
					status_category: null,
					tags: null,
					meta: null,
					custom_attributes: null,
					contact: createContact({id: 0, first_name: '', email: null, custom_attributes: null}),
				}),
				conversation_uuid: CONVERSATION_UUID,
			});
			expect(embed).toEqual({
				title: 'Conversation unassigned',
				color: 0x5c6bc0,
				footer: {text: `Conversation ${CONVERSATION_UUID}`},
			});
		});

		it('renders a live chat conversation without a subject', () => {
			const embed = render(
				'conversation.created',
				createConversation({subject: null, inbox_name: 'Live chat', inbox_channel: 'livechat'}),
			);
			expect(embed?.title).toBe('\\[Live chat\\] New conversation: #1042');
		});
	});

	describe('message events', () => {
		it('renders an incoming message with the sender name, excerpt and attachment names', () => {
			expect(render('message.created', createMessage())).toEqual({
				title: 'New message',
				color: 0xf97316,
				description: 'Hello, I cannot sign in to my account since this morning. The page says my password is wrong.',
				author: {name: 'Example contact'},
				fields: [{name: 'Attachments', value: 'screenshot.png', inline: false}],
				footer: {text: `Conversation ${CONVERSATION_UUID}`},
			});
		});

		it('renders an agent reply', () => {
			const embed = render('message.created', createOutgoingMessage());
			expect(embed?.title).toBe('New reply');
			expect(embed?.author).toEqual({name: 'Support agent'});
			expect(embed?.description).toBe('Thanks for reaching out. Please try again.');
			expect(embed?.fields).toBeUndefined();
		});

		it('labels a private note and keeps the mention text inert', () => {
			const embed = render(
				'message.created',
				createOutgoingMessage({
					status: 'sent',
					private: true,
					meta: {},
					content:
						'<p>The account was locked. <span data-type="agent" data-id="6" data-label="Second agent">@Second agent</span> can you confirm?</p>',
					text_content: 'The account was locked. @Second agent can you confirm?',
				}),
			);
			expect(embed?.title).toBe('New private note');
			expect(embed?.color).toBe(0xfcbd1f);
			expect(embed?.description).toBe('The account was locked. \\@Second agent can you confirm?');
		});

		it('labels an automated reply from the system user', () => {
			const embed = render(
				'message.created',
				createOutgoingMessage({
					sender_id: 1,
					meta: {is_automated: true},
					text_content: 'We have received your request.',
					author: createAgentAuthor({id: 1, first_name: 'System', last_name: '', email: 'System', type: 'agent'}),
				}),
			);
			expect(embed?.title).toBe('Automated reply');
			expect(embed?.author).toEqual({name: 'System'});
		});

		it('skips activity lines, satisfaction surveys and continuity digests', () => {
			expect(
				render(
					'message.created',
					createOutgoingMessage({
						type: 'activity',
						status: 'sent',
						private: true,
						content_type: 'text',
						text_content: 'Support agent self-assigned this conversation',
						meta: {activity_type: 'self_assign'},
					}),
				),
			).toBeNull();
			expect(
				render(
					'message.created',
					createOutgoingMessage({
						meta: {is_csat: true, is_automated: true, csat_uuid: 'b0f5c2d4-0000-4000-8000-000000000000'},
						text_content: 'Rate us at https://desk.example.com/csat/b0f5c2d4-0000-4000-8000-000000000000',
					}),
				),
			).toBeNull();
			expect(render('message.created', createOutgoingMessage({meta: {continuity_email: true}}))).toBeNull();
		});

		it('names the sender generically when Libredesk sends an empty author', () => {
			const embed = render(
				'message.created',
				createMessage({author: {id: 0, first_name: '', last_name: '', email: null, type: ''}, attachments: null}),
			);
			expect(embed?.author).toEqual({name: 'Contact'});
			expect(embed?.fields).toBeUndefined();
		});

		it('renders an image-only message without a description', () => {
			const embed = render(
				'message.created',
				createMessage({content: '<p><img src="cid:ldsk-1"></p>', text_content: ''}),
			);
			expect(embed?.description).toBeUndefined();
			expect(embed?.fields).toEqual([{name: 'Attachments', value: 'screenshot.png', inline: false}]);
		});

		it('renders message.updated only for a failed send', () => {
			const failed = render('message.updated', createOutgoingMessage({status: 'failed'}));
			expect(failed).toEqual({
				title: 'Reply failed to send',
				color: 0xe23c39,
				description: 'Thanks for reaching out. Please try again.',
				author: {name: 'Support agent'},
				footer: {text: `Conversation ${CONVERSATION_UUID}`},
			});
			expect(render('message.updated', createOutgoingMessage({status: 'sent'}))).toBeNull();
			expect(render('message.updated', createOutgoingMessage({status: 'pending'}))).toBeNull();
		});
	});

	describe('test and custom events', () => {
		it('renders webhook.test', () => {
			expect(render('webhook.test', {id: 3, name: 'Support feed'})).toEqual({
				title: 'Test delivery received',
				color: 0x5c6bc0,
				description: 'Support feed',
			});
		});

		it('renders an automation event that carries a conversation', () => {
			const embed = render('priority_escalated', {
				actor_id: 1,
				conversation: createConversation({priority: 'High'}),
			});
			expect(embed?.title).toBe('\\[Support\\] priority_escalated: #1042 Cannot sign in');
			expect(embed?.footer).toEqual({text: `Conversation ${CONVERSATION_UUID}`});
		});

		it('ignores an unknown event without a conversation', () => {
			expect(render('inbox.renamed', {id: 1, name: 'Support'})).toBeNull();
		});

		it('renders an automation event named like a built-in event as an automation event', () => {
			const payload = {actor_id: 1, conversation: createConversation()};
			for (const event of [
				'conversation.created',
				'conversation.status_changed',
				'conversation.tags_changed',
				'conversation.assigned',
				'conversation.unassigned',
				'message.created',
				'message.updated',
				'webhook.test',
			]) {
				expect(render(event, payload)).toEqual({
					title: `\\[Support\\] ${event}: #1042 Cannot sign in`,
					color: 0x5c6bc0,
					footer: {text: `Conversation ${CONVERSATION_UUID}`},
				});
			}
		});

		it('accepts an automation event name of any length and truncates it', () => {
			const embed = render('x'.repeat(300), {conversation: createConversation()});
			expect(embed?.title).toBe(`\\[Support\\] ${'x'.repeat(29)}...: #1042 Cannot sign in`);
		});
	});

	describe('validation', () => {
		it('accepts null and missing optional fields', () => {
			expect(render('conversation.created', {uuid: CONVERSATION_UUID})).toEqual({
				title: 'New conversation',
				color: 0x3b82f6,
				footer: {text: `Conversation ${CONVERSATION_UUID}`},
			});
			expect(render('message.created', {conversation_uuid: CONVERSATION_UUID})).toEqual({
				title: 'New reply',
				color: 0x3b82f6,
				author: {name: 'Agent'},
				footer: {text: `Conversation ${CONVERSATION_UUID}`},
			});
			expect(render('conversation.created', {})).toBeNull();
			expect(render('message.created', {})).toBeNull();
			expect(render('conversation.tags_changed', {conversation: null, new_tags: null, previous_tags: null})).toBeNull();
		});

		it('rejects a body without an event or a payload object', () => {
			expect(LibredeskWebhook.safeParse({payload: {}}).success).toBe(false);
			expect(LibredeskWebhook.safeParse({event: 'webhook.test'}).success).toBe(false);
			expect(LibredeskWebhook.safeParse({event: 'webhook.test', payload: 'text'}).success).toBe(false);
			expect(LibredeskWebhook.safeParse({event: 'message.created', payload: {text_content: 42}}).success).toBe(false);
		});

		it('never reads the raw HTML content', () => {
			const embed = render(
				'message.created',
				createMessage({content: '<script>alert(1)</script><p>Visible</p>', text_content: 'Visible'}),
			);
			expect(embed?.description).toBe('Visible');
		});
	});

	describe('sanitisation', () => {
		it('neutralises mass mentions and mention syntax in every markdown slot', () => {
			const hostile = '@everyone @here <@123456789012345678> <@&123456789012345678> <#123456789012345678> <t:0:R>';
			const embed = render('message.created', createMessage({text_content: hostile, attachments: [{name: hostile}]}));
			const conversation = render(
				'conversation.created',
				createConversation({subject: hostile, inbox_name: '@here', reference_number: '<@1>'}),
			);
			for (const value of [...markdownSlots(embed!), ...markdownSlots(conversation!)]) {
				expect(unescapedTrigger(value)).toBeNull();
			}
			expect(embed?.description).toBe(
				'\\@everyone \\@here \\<\\@123456789012345678> \\<\\@&123456789012345678> \\<#123456789012345678> \\<t:0:R>',
			);
		});

		it('keeps an escaped backslash from re-enabling a mention', () => {
			const embed = render('message.created', createMessage({text_content: '\\@everyone \\<@1>'}));
			expect(embed?.description).toBe('\\\\\\@everyone \\\\\\<\\@1>');
			expect(unescapedTrigger(embed!.description!)).toBeNull();
		});

		it('disarms masked links but keeps plain urls readable', () => {
			const embed = render(
				'message.created',
				createMessage({
					text_content: '[Reset password](https://phishing.example.com) or visit https://example.com/help',
				}),
			);
			expect(embed?.description).toBe(
				'\\[Reset password\\](https://phishing.example.com) or visit https://example.com/help',
			);
		});

		it('percent-encodes markdown triggers inside urls so links keep their target', () => {
			const embed = render(
				'message.created',
				createMessage({
					text_content:
						'See https://social.example/@user/1 and https://docs.example/a[1]?q=a\\b or https://x.example/<@123> then @here',
				}),
			);
			expect(embed?.description).toBe(
				'See https://social.example/%40user/1 and https://docs.example/a%5B1%5D?q=a%5Cb or https://x.example/%3C%40123> then \\@here',
			);
			expect(unescapedTrigger(embed!.description!)).toBeNull();
		});

		it('keeps a url that the parser would reject inert', () => {
			const embed = render(
				'message.created',
				createMessage({text_content: 'https://**x.example/@everyone[click](https://phishing.example.com)'}),
			);
			expect(embed?.description).toBe('https://**x.example/%40everyone%5Bclick%5D(https://phishing.example.com)');
		});

		it('renders HTML in the plain text as literal text', () => {
			const embed = render(
				'message.created',
				createMessage({text_content: '<script>alert(1)</script> <img src=x onerror=alert(1)>'}),
			);
			expect(embed?.description).toBe('\\<script>alert(1)\\</script> \\<img src=x onerror=alert(1)>');
		});

		it('collapses whitespace and strips control and bidi characters', () => {
			const embed = render(
				'message.created',
				createMessage({
					text_content: `Line one\n\n\tLine${String.fromCodePoint(0)} two${RIGHT_TO_LEFT_OVERRIDE} reversed${LINE_SEPARATOR}end   `,
				}),
			);
			expect(embed?.description).toBe('Line one Line two reversed end');
		});

		it('leaves plain-text slots unescaped', () => {
			const embed = render('message.created', createMessage({author: {first_name: '@everyone', last_name: '<@1>'}}));
			expect(embed?.author).toEqual({name: '@everyone <@1>'});
		});

		it('clamps a very long subject and body within the embed limits', () => {
			const long = 'a'.repeat(100_000);
			const created = render(
				'conversation.created',
				createConversation({subject: long, inbox_name: long, reference_number: long}),
			);
			expect(created!.title!.length).toBeLessThanOrEqual(256);
			expect(created!.title!.endsWith('...')).toBe(true);
			const message = render('message.created', createMessage({text_content: long, attachments: [{name: long}]}));
			expect(message!.description!.length).toBeLessThanOrEqual(1000);
			expect(message!.description!.endsWith('...')).toBe(true);
			expect(message!.fields![0]!.value.length).toBeLessThanOrEqual(256);
		});

		it('keeps the worst case title within 256 units', () => {
			const triggers = '@'.repeat(10_000);
			const embed = render('priority_escalated', {
				conversation: createConversation({subject: triggers, inbox_name: triggers, reference_number: triggers}),
			});
			expect(embed!.title!.length).toBeLessThanOrEqual(256);
			expect(unescapedTrigger(embed!.title!)).toBeNull();
		});

		it('never splits an escape when truncating', () => {
			for (let offset = 0; offset < 4; offset++) {
				const embed = render(
					'message.created',
					createMessage({text_content: `${'x'.repeat(offset)}${'@'.repeat(2000)}`}),
				);
				const description = embed!.description!;
				expect(description.length).toBeLessThanOrEqual(1000);
				expect(description.endsWith('...')).toBe(true);
				expect(unescapedTrigger(description)).toBeNull();
				expect(description.slice(0, -3).endsWith('\\')).toBe(false);
			}
		});

		it('bounds a single grapheme built from thousands of combining marks', () => {
			const bomb = `a${COMBINING_ACUTE.repeat(20_000)}`;
			const embed = render(
				'message.created',
				createMessage({
					text_content: bomb,
					author: {first_name: bomb, last_name: bomb},
					attachments: [{name: bomb}],
					conversation_uuid: bomb,
				}),
			);
			expect(embed!.description).toBe('...');
			expect(embed!.author!.name).toBe('...');
			expect(embedLength(embed!)).toBeLessThanOrEqual(EMBED_TOTAL_MAX_LENGTH);
		});

		it('never emits a lone surrogate', () => {
			const lone = render('message.created', createMessage({text_content: 'Hello \uD800 there'}));
			expect(lone!.description).toBe('Hello \uFFFD there');
			const split = render('message.created', createMessage({text_content: `x${'\u{1F600}'.repeat(3000)}`}));
			expect(split!.description!.isWellFormed()).toBe(true);
			expect(split!.description!.length).toBeLessThanOrEqual(1000);
		});

		it('stays within the total embed budget for every event when every string is hostile', () => {
			const hostile = `${'<@&1> @everyone [x](https://example.com) '.repeat(500)}${COMBINING_ACUTE.repeat(500)}`;
			const tags = Array.from({length: 200}, (_, index) => `${hostile}${index}`);
			const conversation = createConversation({
				subject: hostile,
				inbox_name: hostile,
				reference_number: hostile,
				contact: createContact({first_name: hostile, last_name: hostile}),
			});
			const payloads: Array<[string, unknown]> = [
				['conversation.created', conversation],
				[
					'conversation.status_changed',
					{
						conversation,
						conversation_uuid: hostile,
						previous_status: hostile,
						new_status: hostile,
						snooze_until: '2026-10-16T09:40:05Z',
					},
				],
				['conversation.tags_changed', {conversation, conversation_uuid: hostile, previous_tags: [], new_tags: tags}],
				['conversation.assigned', {conversation, conversation_uuid: hostile, assigned_to: Number.MAX_SAFE_INTEGER}],
				['conversation.unassigned', {conversation, conversation_uuid: hostile}],
				[
					'message.created',
					createMessage({
						text_content: hostile,
						author: {first_name: hostile, last_name: hostile},
						attachments: tags.map((name) => ({name})),
						conversation_uuid: hostile,
					}),
				],
				[
					'message.updated',
					createOutgoingMessage({status: 'failed', text_content: hostile, conversation_uuid: hostile}),
				],
				['webhook.test', {id: 3, name: hostile}],
				[hostile.slice(0, 256).trim(), {conversation}],
			];
			for (const [event, payload] of payloads) {
				const embed = render(event, payload);
				expect(embed).not.toBeNull();
				expect(embedLength(embed!)).toBeLessThanOrEqual(EMBED_TOTAL_MAX_LENGTH);
				expect(embed!.title!.length).toBeLessThanOrEqual(256);
				expect((embed!.description ?? '').length).toBeLessThanOrEqual(4096);
				for (const field of embed!.fields ?? []) expect(field.value.length).toBeLessThanOrEqual(1024);
				for (const value of markdownSlots(embed!)) {
					expect(unescapedTrigger(value.replace(/ until <t:\d+:f>$/, ''))).toBeNull();
				}
			}
		});
	});
});
