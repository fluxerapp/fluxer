// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GitHubWebhook} from '@fluxer/schema/src/domains/webhook/GitHubWebhookSchemas';
import {describe, expect, it} from 'vitest';
import {
	transformPullRequest,
	transformPullRequestReview,
	transformPullRequestReviewComment,
} from '../transformers/GitHubPullRequestTransformer';

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

function createBasePullRequest() {
	return {
		id: 111222333n,
		number: 42,
		html_url: 'https://github.com/org/repo/pull/42',
		title: 'Add amazing feature',
		body: 'This PR adds an amazing feature that everyone will love.',
		user: {
			id: 12345,
			login: 'prauthor',
			html_url: 'https://github.com/prauthor',
			avatar_url: 'https://avatars.githubusercontent.com/u/12345',
		},
	};
}

describe('GitHub Pull Request Transformer', () => {
	describe('transformPullRequest', () => {
		it('transforms an opened pull request', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'opened',
				pull_request: createBasePullRequest(),
			};
			const result = await transformPullRequest(payload);
			expect(result).not.toBeNull();
			expect(result?.title).toContain('[org/repo]');
			expect(result?.title).toContain('Pull request opened');
			expect(result?.title).toContain('#42');
			expect(result?.title).toContain('Add amazing feature');
			expect(result?.url).toBe('https://github.com/org/repo/pull/42');
			expect(result?.color).toBe(0x098efc);
			expect(result?.description).toContain('amazing feature');
			expect(result?.author?.name).toBe('testuser');
			expect(result?.author?.url).toBe('https://github.com/testuser');
		});
		it('transforms a closed pull request', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'closed',
				pull_request: createBasePullRequest(),
			};
			const result = await transformPullRequest(payload);
			expect(result).not.toBeNull();
			expect(result?.title).toContain('Pull request closed');
			expect(result?.color).toBe(0x000000);
			expect(result?.description).toBeUndefined();
			expect(result?.author?.name).toBe('testuser');
		});
		it('transforms a reopened pull request', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'reopened',
				pull_request: createBasePullRequest(),
			};
			const result = await transformPullRequest(payload);
			expect(result).not.toBeNull();
			expect(result?.title).toContain('Pull request reopened');
			expect(result?.color).toBe(0xfcbd1f);
			expect(result?.author?.name).toBe('testuser');
		});
		it('returns null for unsupported action types', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'labeled',
				pull_request: createBasePullRequest(),
			};
			const result = await transformPullRequest(payload);
			expect(result).toBeNull();
		});
		it('returns null when pull_request is missing', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'opened',
			};
			const result = await transformPullRequest(payload);
			expect(result).toBeNull();
		});
		it('returns null when action is missing', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				pull_request: createBasePullRequest(),
			};
			const result = await transformPullRequest(payload);
			expect(result).toBeNull();
		});
		it('returns null when repository is missing', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				action: 'opened',
				pull_request: createBasePullRequest(),
			};
			const result = await transformPullRequest(payload);
			expect(result).toBeNull();
		});
		it('handles pull request with empty body', async () => {
			const pr = {...createBasePullRequest(), body: null as string | null};
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'opened',
				pull_request: pr,
			};
			const result = await transformPullRequest(payload);
			expect(result).not.toBeNull();
			expect(result?.description).toBeUndefined();
		});
	});
	describe('transformPullRequestReview', () => {
		it('transforms a submitted pull request review', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'submitted',
				pull_request: createBasePullRequest(),
				review: {
					user: {
						id: 54321,
						login: 'reviewer',
						html_url: 'https://github.com/reviewer',
						avatar_url: 'https://avatars.githubusercontent.com/u/54321',
					},
					body: 'LGTM! Great work!',
					html_url: 'https://github.com/org/repo/pull/42#pullrequestreview-123',
					state: 'approved',
				},
			};
			const result = await transformPullRequestReview(payload);
			expect(result).not.toBeNull();
			expect(result?.title).toContain('[org/repo]');
			expect(result?.title).toContain('Pull request review submitted');
			expect(result?.title).toContain('#42');
			expect(result?.url).toBe('https://github.com/org/repo/pull/42#pullrequestreview-123');
			expect(result?.description).toContain('LGTM');
			expect(result?.author?.name).toBe('reviewer');
		});
		it('returns null for non-submitted review actions', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'edited',
				pull_request: createBasePullRequest(),
				review: {
					user: createBaseSender(),
					body: 'Updated review',
					html_url: 'https://github.com/org/repo/pull/42#pullrequestreview-123',
					state: 'approved',
				},
			};
			const result = await transformPullRequestReview(payload);
			expect(result).toBeNull();
		});
		it('returns null when review is missing', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'submitted',
				pull_request: createBasePullRequest(),
			};
			const result = await transformPullRequestReview(payload);
			expect(result).toBeNull();
		});
		it('handles review with empty body', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'submitted',
				pull_request: createBasePullRequest(),
				review: {
					user: createBaseSender(),
					body: null,
					html_url: 'https://github.com/org/repo/pull/42#pullrequestreview-123',
					state: 'approved',
				},
			};
			const result = await transformPullRequestReview(payload);
			expect(result).not.toBeNull();
			expect(result?.description).toBeUndefined();
		});
	});
	describe('transformPullRequestReviewComment', () => {
		it('transforms a created review comment', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'created',
				pull_request: createBasePullRequest(),
				comment: {
					id: 999888777n,
					html_url: 'https://github.com/org/repo/pull/42#discussion_r123',
					user: {
						id: 54321,
						login: 'commenter',
						html_url: 'https://github.com/commenter',
						avatar_url: 'https://avatars.githubusercontent.com/u/54321',
					},
					body: 'Consider using a different approach here.',
				},
			};
			const result = await transformPullRequestReviewComment(payload);
			expect(result).not.toBeNull();
			expect(result?.title).toContain('[org/repo]');
			expect(result?.title).toContain('New review comment on pull request #42');
			expect(result?.url).toBe('https://github.com/org/repo/pull/42#discussion_r123');
			expect(result?.color).toBe(0xc00a7f);
			expect(result?.description).toContain('Consider using');
			expect(result?.author?.name).toBe('commenter');
		});
		it('returns null for non-created comment actions', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'deleted',
				pull_request: createBasePullRequest(),
				comment: {
					id: 999888777n,
					html_url: 'https://github.com/org/repo/pull/42#discussion_r123',
					user: createBaseSender(),
					body: 'Deleted comment',
				},
			};
			const result = await transformPullRequestReviewComment(payload);
			expect(result).toBeNull();
		});
		it('returns null when comment is missing', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'created',
				pull_request: createBasePullRequest(),
			};
			const result = await transformPullRequestReviewComment(payload);
			expect(result).toBeNull();
		});
		it('handles comment with empty body', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'created',
				pull_request: createBasePullRequest(),
				comment: {
					id: 999888777n,
					html_url: 'https://github.com/org/repo/pull/42#discussion_r123',
					user: createBaseSender(),
					body: '',
				},
			};
			const result = await transformPullRequestReviewComment(payload);
			expect(result).not.toBeNull();
			expect(result?.description).toBeUndefined();
		});
	});
});
