// SPDX-License-Identifier: AGPL-3.0-or-later

import {transformSlackWebhookRequest} from '@app/api/webhook/transformers/SlackTransformer';
import type {SlackWebhookRequest} from '@fluxer/schema/src/domains/webhook/WebhookRequestSchemas';
import {describe, expect, it} from 'vitest';

describe('Slack Transformer', () => {
	describe('transformSlackWebhookRequest', () => {
		it('transforms a simple text message', () => {
			const payload: SlackWebhookRequest = {
				text: 'Hello from Slack!',
			};
			const result = transformSlackWebhookRequest(payload);
			expect(result.content).toBe('Hello from Slack!');
			expect(result.embeds).toBeUndefined();
		});
		it('transforms a message with username and icon', () => {
			const payload: SlackWebhookRequest = {
				text: 'Notification',
				username: 'Bot',
				icon_url: 'https://example.com/icon.png',
			};
			const result = transformSlackWebhookRequest(payload);
			expect(result.content).toBe('Notification');
			expect(result.username).toBe('Bot');
			expect(result.avatar_url).toBe('https://example.com/icon.png');
		});
		it('transforms a message with a simple attachment', () => {
			const payload: SlackWebhookRequest = {
				text: 'Check this out:',
				attachments: [
					{
						title: 'Important Update',
						text: 'This is the attachment text.',
						color: '#ff5733',
					},
				],
			};
			const result = transformSlackWebhookRequest(payload);
			expect(result.content).toBe('Check this out:');
			expect(result.embeds).toHaveLength(1);
			expect(result.embeds?.[0].title).toBe('Important Update');
			expect(result.embeds?.[0].description).toBe('This is the attachment text.');
			expect(result.embeds?.[0].color).toBe(0xff5733);
		});
		it('transforms attachment with pretext and text', () => {
			const payload: SlackWebhookRequest = {
				attachments: [
					{
						pretext: 'Before the main content',
						text: 'The main content',
					},
				],
			};
			const result = transformSlackWebhookRequest(payload);
			expect(result.content).toBe('');
			expect(result.embeds?.[0].description).toBe('Before the main content\nThe main content');
		});
		it('uses fallback when no text or pretext available', () => {
			const payload: SlackWebhookRequest = {
				attachments: [
					{
						fallback: 'Fallback text for unsupported clients',
					},
				],
			};
			const result = transformSlackWebhookRequest(payload);
			expect(result.embeds?.[0].description).toBe('Fallback text for unsupported clients');
		});
		it('transforms attachment with author information', () => {
			const payload: SlackWebhookRequest = {
				attachments: [
					{
						text: 'Message content',
						author_name: 'John Doe',
						author_link: 'https://example.com/johndoe',
						author_icon: 'https://example.com/johndoe/avatar.png',
					},
				],
			};
			const result = transformSlackWebhookRequest(payload);
			expect(result.embeds?.[0].author?.name).toBe('John Doe');
			expect(result.embeds?.[0].author?.url).toBe('https://example.com/johndoe');
			expect(result.embeds?.[0].author?.icon_url).toBe('https://example.com/johndoe/avatar.png');
		});
		it('transforms attachment with title and title_link', () => {
			const payload: SlackWebhookRequest = {
				attachments: [
					{
						title: 'Click here',
						title_link: 'https://example.com/page',
						text: 'Description',
					},
				],
			};
			const result = transformSlackWebhookRequest(payload);
			expect(result.embeds?.[0].title).toBe('Click here');
			expect(result.embeds?.[0].url).toBe('https://example.com/page');
		});
		it('transforms attachment with fields', () => {
			const payload: SlackWebhookRequest = {
				attachments: [
					{
						text: 'Status update',
						fields: [
							{
								title: 'Status',
								value: 'Deployed',
								short: true,
							},
							{
								title: 'Environment',
								value: 'Production',
								short: true,
							},
							{
								title: 'Details',
								value: 'Full deployment completed successfully',
								short: false,
							},
						],
					},
				],
			};
			const result = transformSlackWebhookRequest(payload);
			expect(result.embeds?.[0].fields).toHaveLength(3);
			expect(result.embeds?.[0].fields?.[0].name).toBe('Status');
			expect(result.embeds?.[0].fields?.[0].value).toBe('Deployed');
			expect(result.embeds?.[0].fields?.[0].inline).toBe(true);
			expect(result.embeds?.[0].fields?.[2].inline).toBe(false);
		});
		it('ignores fields without title or value', () => {
			const payload: SlackWebhookRequest = {
				attachments: [
					{
						text: 'Message',
						fields: [{title: 'Only Title'}, {value: 'Only Value'}, {title: 'Both', value: 'Present'}],
					},
				],
			};
			const result = transformSlackWebhookRequest(payload);
			expect(result.embeds?.[0].fields).toHaveLength(1);
			expect(result.embeds?.[0].fields?.[0].name).toBe('Both');
		});
		it('transforms attachment with footer and timestamp', () => {
			const payload: SlackWebhookRequest = {
				attachments: [
					{
						text: 'Alert',
						footer: 'Monitoring System',
						ts: 1609459200,
					},
				],
			};
			const result = transformSlackWebhookRequest(payload);
			expect(result.embeds?.[0].footer?.text).toBe('Monitoring System');
			expect(result.embeds?.[0].timestamp).toEqual(new Date(1609459200 * 1000));
		});
		it('transforms attachment with image and thumbnail', () => {
			const payload: SlackWebhookRequest = {
				attachments: [
					{
						text: 'Check out this image',
						image_url: 'https://example.com/image.png',
						thumb_url: 'https://example.com/thumb.png',
					},
				],
			};
			const result = transformSlackWebhookRequest(payload);
			expect(result.embeds?.[0].image?.url).toBe('https://example.com/image.png');
			expect(result.embeds?.[0].thumbnail?.url).toBe('https://example.com/thumb.png');
		});
		it('handles multiple attachments', () => {
			const payload: SlackWebhookRequest = {
				text: 'Multiple attachments:',
				attachments: [
					{title: 'First', text: 'First attachment'},
					{title: 'Second', text: 'Second attachment'},
					{title: 'Third', text: 'Third attachment'},
				],
			};
			const result = transformSlackWebhookRequest(payload);
			expect(result.content).toBe('Multiple attachments:');
			expect(result.embeds).toHaveLength(3);
			expect(result.embeds?.[0].title).toBe('First');
			expect(result.embeds?.[1].title).toBe('Second');
			expect(result.embeds?.[2].title).toBe('Third');
		});
		it('handles color without hash prefix', () => {
			const payload: SlackWebhookRequest = {
				attachments: [
					{
						text: 'Colored',
						color: 'aabbcc',
					},
				],
			};
			const result = transformSlackWebhookRequest(payload);
			expect(result.embeds?.[0].color).toBe(0xaabbcc);
		});
		it('handles color with hash prefix', () => {
			const payload: SlackWebhookRequest = {
				attachments: [
					{
						text: 'Colored',
						color: '#123456',
					},
				],
			};
			const result = transformSlackWebhookRequest(payload);
			expect(result.embeds?.[0].color).toBe(0x123456);
		});
		it('ignores invalid color values', () => {
			const payload: SlackWebhookRequest = {
				attachments: [
					{
						text: 'Invalid color',
						color: 'not-a-color',
					},
				],
			};
			const result = transformSlackWebhookRequest(payload);
			expect(result.embeds?.[0].color).toBeUndefined();
		});
		it('ignores invalid URLs for icon_url', () => {
			const payload: SlackWebhookRequest = {
				text: 'Message',
				icon_url: 'not-a-valid-url',
			};
			const result = transformSlackWebhookRequest(payload);
			expect(result.avatar_url).toBeUndefined();
		});
		it('ignores invalid URLs for title_link', () => {
			const payload: SlackWebhookRequest = {
				attachments: [
					{
						title: 'Link',
						title_link: 'invalid-link',
						text: 'Text',
					},
				],
			};
			const result = transformSlackWebhookRequest(payload);
			expect(result.embeds?.[0].url).toBeUndefined();
		});
		it('ignores invalid URLs for author_link', () => {
			const payload: SlackWebhookRequest = {
				attachments: [
					{
						text: 'Text',
						author_name: 'Author',
						author_link: 'ftp://invalid',
					},
				],
			};
			const result = transformSlackWebhookRequest(payload);
			expect(result.embeds?.[0].author?.url).toBeUndefined();
		});
		it('handles empty payload', () => {
			const payload: SlackWebhookRequest = {};
			const result = transformSlackWebhookRequest(payload);
			expect(result.content).toBeUndefined();
			expect(result.embeds).toBeUndefined();
		});
		it('handles empty attachments array', () => {
			const payload: SlackWebhookRequest = {
				text: 'Message',
				attachments: [],
			};
			const result = transformSlackWebhookRequest(payload);
			expect(result.content).toBe('Message');
			expect(result.embeds).toBeUndefined();
		});
		it('handles empty attachment objects', () => {
			const payload: SlackWebhookRequest = {
				text: 'Message',
				attachments: [{}],
			};
			const result = transformSlackWebhookRequest(payload);
			expect(result.content).toBe('Message');
			expect(result.embeds).toBeUndefined();
		});
		it('sets empty content when no text but has embeds', () => {
			const payload: SlackWebhookRequest = {
				attachments: [
					{
						title: 'Embed only',
						text: 'Embed content',
					},
				],
			};
			const result = transformSlackWebhookRequest(payload);
			expect(result.content).toBe('');
			expect(result.embeds).toHaveLength(1);
		});
		it('preserves short field default to false when not specified', () => {
			const payload: SlackWebhookRequest = {
				attachments: [
					{
						fields: [
							{
								title: 'Field',
								value: 'Value',
							},
						],
					},
				],
			};
			const result = transformSlackWebhookRequest(payload);
			expect(result.embeds?.[0].fields?.[0].inline).toBe(false);
		});
		it('handles complex real-world Slack webhook payload', () => {
			const payload: SlackWebhookRequest = {
				username: 'GitHub',
				icon_url: 'https://github.githubassets.com/images/modules/logos_page/GitHub-Mark.png',
				attachments: [
					{
						fallback: '[repo] New commit pushed to main',
						color: '#24292e',
						pretext: 'New activity on your repository',
						author_name: 'developer',
						author_link: 'https://github.com/developer',
						author_icon: 'https://avatars.githubusercontent.com/u/123',
						title: 'Add new feature',
						title_link: 'https://github.com/org/repo/commit/abc123',
						text: 'This commit adds a new feature that improves user experience.',
						fields: [
							{title: 'Repository', value: 'org/repo', short: true},
							{title: 'Branch', value: 'main', short: true},
						],
						image_url: 'https://example.com/screenshot.png',
						thumb_url: 'https://example.com/thumb.png',
						footer: 'GitHub',
						ts: 1609459200,
					},
				],
			};
			const result = transformSlackWebhookRequest(payload);
			expect(result.username).toBe('GitHub');
			expect(result.avatar_url).toBe('https://github.githubassets.com/images/modules/logos_page/GitHub-Mark.png');
			expect(result.content).toBe('');
			expect(result.embeds).toHaveLength(1);
			const embed = result.embeds?.[0];
			expect(embed?.title).toBe('Add new feature');
			expect(embed?.url).toBe('https://github.com/org/repo/commit/abc123');
			expect(embed?.color).toBe(0x24292e);
			expect(embed?.description).toContain('New activity on your repository');
			expect(embed?.description).toContain('This commit adds');
			expect(embed?.author?.name).toBe('developer');
			expect(embed?.fields).toHaveLength(2);
			expect(embed?.image?.url).toBe('https://example.com/screenshot.png');
			expect(embed?.thumbnail?.url).toBe('https://example.com/thumb.png');
			expect(embed?.footer?.text).toBe('GitHub');
			expect(embed?.timestamp).toEqual(new Date(1609459200 * 1000));
		});
	});
});
