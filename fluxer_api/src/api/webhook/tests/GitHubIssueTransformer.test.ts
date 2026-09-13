// SPDX-License-Identifier: AGPL-3.0-or-later

import {transformIssue, transformIssueComment} from '@app/api/webhook/transformers/GitHubIssueTransformer';
import type {GitHubWebhook} from '@fluxer/schema/src/domains/webhook/GitHubWebhookSchemas';
import {describe, expect, it} from 'vitest';

function createBaseSender() {
	return {
		id: 12345,
		login: 'testuser',
		html_url: 'https://github.com/testuser',
		avatar_url: 'https://avatars.githubusercontent.com/u/12345',
	};
}

function createBaseRepository() {
	return {
		id: 67890,
		html_url: 'https://github.com/org/repo',
		name: 'repo',
		full_name: 'org/repo',
	};
}

function createBaseIssue() {
	return {
		id: 111222333n,
		number: 99,
		html_url: 'https://github.com/org/repo/issues/99',
		title: 'Bug: Something is broken',
		body: 'When I try to do X, Y happens instead of Z.',
		user: {
			id: 12345,
			login: 'reporter',
			html_url: 'https://github.com/reporter',
			avatar_url: 'https://avatars.githubusercontent.com/u/12345',
		},
	};
}

describe('GitHub Issue Transformer', () => {
	describe('transformIssue', () => {
		it('transforms an opened issue', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'opened',
				issue: createBaseIssue(),
			};
			const result = await transformIssue(payload);
			expect(result).not.toBeNull();
			expect(result?.title).toContain('[org/repo]');
			expect(result?.title).toContain('Issue opened');
			expect(result?.title).toContain('#99');
			expect(result?.title).toContain('Bug: Something is broken');
			expect(result?.url).toBe('https://github.com/org/repo/issues/99');
			expect(result?.color).toBe(0xeb4841);
			expect(result?.description).toContain('When I try to do X');
			expect(result?.author?.name).toBe('testuser');
			expect(result?.author?.url).toBe('https://github.com/testuser');
		});
		it('transforms a closed issue', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'closed',
				issue: createBaseIssue(),
			};
			const result = await transformIssue(payload);
			expect(result).not.toBeNull();
			expect(result?.title).toContain('Issue closed');
			expect(result?.color).toBe(0x000000);
			expect(result?.description).toBeUndefined();
			expect(result?.author?.name).toBe('testuser');
		});
		it('transforms a reopened issue', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'reopened',
				issue: createBaseIssue(),
			};
			const result = await transformIssue(payload);
			expect(result).not.toBeNull();
			expect(result?.title).toContain('Issue reopened');
			expect(result?.color).toBe(0xfcbd1f);
			expect(result?.author?.name).toBe('testuser');
		});
		it('returns null for unsupported action types', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'labeled',
				issue: createBaseIssue(),
			};
			const result = await transformIssue(payload);
			expect(result).toBeNull();
		});
		it('returns null when issue is missing', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'opened',
			};
			const result = await transformIssue(payload);
			expect(result).toBeNull();
		});
		it('returns null when action is missing', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				issue: createBaseIssue(),
			};
			const result = await transformIssue(payload);
			expect(result).toBeNull();
		});
		it('returns null when repository is missing', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				action: 'opened',
				issue: createBaseIssue(),
			};
			const result = await transformIssue(payload);
			expect(result).toBeNull();
		});
		it('handles issue with empty body', async () => {
			const issue = {...createBaseIssue(), body: null as string | null};
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'opened',
				issue,
			};
			const result = await transformIssue(payload);
			expect(result).not.toBeNull();
			expect(result?.description).toBe('');
		});
	});
	describe('transformIssueComment', () => {
		it('transforms a created issue comment', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'created',
				issue: createBaseIssue(),
				comment: {
					id: 999888777n,
					html_url: 'https://github.com/org/repo/issues/99#issuecomment-123',
					user: {
						id: 54321,
						login: 'commenter',
						html_url: 'https://github.com/commenter',
						avatar_url: 'https://avatars.githubusercontent.com/u/54321',
					},
					body: 'I can reproduce this issue on my machine.',
				},
			};
			const result = await transformIssueComment(payload);
			expect(result).not.toBeNull();
			expect(result?.title).toContain('[org/repo]');
			expect(result?.title).toContain('New comment on issue #99');
			expect(result?.url).toBe('https://github.com/org/repo/issues/99#issuecomment-123');
			expect(result?.color).toBe(0xc00a7f);
			expect(result?.description).toContain('reproduce this issue');
			expect(result?.author?.name).toBe('commenter');
		});
		it('identifies comment on pull request vs issue', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'created',
				issue: createBaseIssue(),
				pull_request: {
					id: 111n,
					number: 99,
					html_url: 'https://github.com/org/repo/pull/99',
					title: 'Feature PR',
					body: 'PR body',
					user: createBaseSender(),
				},
				comment: {
					id: 999888777n,
					html_url: 'https://github.com/org/repo/pull/99#issuecomment-123',
					user: createBaseSender(),
					body: 'Comment on a PR',
				},
			};
			const result = await transformIssueComment(payload);
			expect(result).not.toBeNull();
			expect(result?.title).toContain('New comment on pull request #99');
		});
		it('returns null for non-created comment actions', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'edited',
				issue: createBaseIssue(),
				comment: {
					id: 999888777n,
					html_url: 'https://github.com/org/repo/issues/99#issuecomment-123',
					user: createBaseSender(),
					body: 'Edited comment',
				},
			};
			const result = await transformIssueComment(payload);
			expect(result).toBeNull();
		});
		it('returns null when comment is missing', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'created',
				issue: createBaseIssue(),
			};
			const result = await transformIssueComment(payload);
			expect(result).toBeNull();
		});
		it('returns null when issue is missing', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'created',
				comment: {
					id: 999888777n,
					html_url: 'https://github.com/org/repo/issues/99#issuecomment-123',
					user: createBaseSender(),
					body: 'Some comment',
				},
			};
			const result = await transformIssueComment(payload);
			expect(result).toBeNull();
		});
		it('returns null when repository is missing', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				action: 'created',
				issue: createBaseIssue(),
				comment: {
					id: 999888777n,
					html_url: 'https://github.com/org/repo/issues/99#issuecomment-123',
					user: createBaseSender(),
					body: 'Some comment',
				},
			};
			const result = await transformIssueComment(payload);
			expect(result).toBeNull();
		});
	});
});
