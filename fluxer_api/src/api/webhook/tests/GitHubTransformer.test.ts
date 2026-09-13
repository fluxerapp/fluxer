// SPDX-License-Identifier: AGPL-3.0-or-later

import {transform} from '@app/api/webhook/transformers/GitHubTransformer';
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

describe('GitHub Transformer - Main Router', () => {
	it('routes commit_comment events correctly', async () => {
		const payload: GitHubWebhook = {
			sender: createBaseSender(),
			repository: createBaseRepository(),
			action: 'created',
			comment: {
				id: 123n,
				html_url: 'https://github.com/org/repo/commit/abc#comment-123',
				user: createBaseSender(),
				body: 'Great commit!',
				commit_id: 'abc123def456789012345678901234567890abcd',
			},
		};
		const result = await transform('commit_comment', payload);
		expect(result).not.toBeNull();
		expect(result?.title).toContain('New comment on commit');
	});
	it('routes create events correctly', async () => {
		const payload: GitHubWebhook = {
			sender: createBaseSender(),
			repository: createBaseRepository(),
			ref: 'feature/test',
			ref_type: 'branch',
		};
		const result = await transform('create', payload);
		expect(result).not.toBeNull();
		expect(result?.title).toContain('New branch created');
	});
	it('routes delete events correctly', async () => {
		const payload: GitHubWebhook = {
			sender: createBaseSender(),
			repository: createBaseRepository(),
			ref: 'feature/old',
			ref_type: 'branch',
		};
		const result = await transform('delete', payload);
		expect(result).not.toBeNull();
		expect(result?.title).toContain('Branch deleted');
	});
	it('routes fork events correctly', async () => {
		const payload: GitHubWebhook = {
			sender: createBaseSender(),
			repository: createBaseRepository(),
			forkee: {
				id: 11111,
				html_url: 'https://github.com/user/fork',
				name: 'fork',
				full_name: 'user/fork',
			},
		};
		const result = await transform('fork', payload);
		expect(result).not.toBeNull();
		expect(result?.title).toContain('Fork created');
	});
	it('routes issue_comment events correctly', async () => {
		const payload: GitHubWebhook = {
			sender: createBaseSender(),
			repository: createBaseRepository(),
			action: 'created',
			issue: {
				id: 123n,
				number: 42,
				html_url: 'https://github.com/org/repo/issues/42',
				user: createBaseSender(),
				title: 'Test Issue',
				body: 'Issue body',
			},
			comment: {
				id: 456n,
				html_url: 'https://github.com/org/repo/issues/42#comment-456',
				user: createBaseSender(),
				body: 'Comment body',
			},
		};
		const result = await transform('issue_comment', payload);
		expect(result).not.toBeNull();
		expect(result?.title).toContain('New comment on issue');
	});
	it('routes issues events correctly', async () => {
		const payload: GitHubWebhook = {
			sender: createBaseSender(),
			repository: createBaseRepository(),
			action: 'opened',
			issue: {
				id: 123n,
				number: 42,
				html_url: 'https://github.com/org/repo/issues/42',
				user: createBaseSender(),
				title: 'Test Issue',
				body: 'Issue body',
			},
		};
		const result = await transform('issues', payload);
		expect(result).not.toBeNull();
		expect(result?.title).toContain('Issue opened');
	});
	it('routes member events correctly', async () => {
		const payload: GitHubWebhook = {
			sender: createBaseSender(),
			repository: createBaseRepository(),
			action: 'added',
			member: {
				id: 99999,
				login: 'newmember',
				html_url: 'https://github.com/newmember',
				avatar_url: 'https://avatars.githubusercontent.com/u/99999',
			},
		};
		const result = await transform('member', payload);
		expect(result).not.toBeNull();
		expect(result?.title).toContain('New collaborator added');
	});
	it('routes public events correctly', async () => {
		const payload: GitHubWebhook = {
			sender: createBaseSender(),
			repository: createBaseRepository(),
		};
		const result = await transform('public', payload);
		expect(result).not.toBeNull();
		expect(result?.title).toContain('Now open sourced');
	});
	it('routes pull_request events correctly', async () => {
		const payload: GitHubWebhook = {
			sender: createBaseSender(),
			repository: createBaseRepository(),
			action: 'opened',
			pull_request: {
				id: 123n,
				number: 10,
				html_url: 'https://github.com/org/repo/pull/10',
				user: createBaseSender(),
				title: 'Add feature',
				body: 'PR body',
			},
		};
		const result = await transform('pull_request', payload);
		expect(result).not.toBeNull();
		expect(result?.title).toContain('Pull request opened');
	});
	it('routes pull_request_review events correctly', async () => {
		const payload: GitHubWebhook = {
			sender: createBaseSender(),
			repository: createBaseRepository(),
			action: 'submitted',
			pull_request: {
				id: 123n,
				number: 10,
				html_url: 'https://github.com/org/repo/pull/10',
				user: createBaseSender(),
				title: 'Add feature',
				body: 'PR body',
			},
			review: {
				user: createBaseSender(),
				body: 'LGTM',
				html_url: 'https://github.com/org/repo/pull/10#review-123',
				state: 'approved',
			},
		};
		const result = await transform('pull_request_review', payload);
		expect(result).not.toBeNull();
		expect(result?.title).toContain('Pull request review submitted');
	});
	it('routes pull_request_review_comment events correctly', async () => {
		const payload: GitHubWebhook = {
			sender: createBaseSender(),
			repository: createBaseRepository(),
			action: 'created',
			pull_request: {
				id: 123n,
				number: 10,
				html_url: 'https://github.com/org/repo/pull/10',
				user: createBaseSender(),
				title: 'Add feature',
				body: 'PR body',
			},
			comment: {
				id: 456n,
				html_url: 'https://github.com/org/repo/pull/10#discussion-456',
				user: createBaseSender(),
				body: 'Review comment',
			},
		};
		const result = await transform('pull_request_review_comment', payload);
		expect(result).not.toBeNull();
		expect(result?.title).toContain('New review comment on pull request');
	});
	it('routes push events correctly', async () => {
		const payload: GitHubWebhook = {
			sender: createBaseSender(),
			repository: createBaseRepository(),
			ref: 'refs/heads/main',
			compare: 'https://github.com/org/repo/compare/abc...def',
			commits: [
				{
					id: 'def456789012345678901234567890abcdef0123',
					url: 'https://github.com/org/repo/commit/def456',
					message: 'Add something',
					author: {name: 'Dev'},
				},
			],
		};
		const result = await transform('push', payload);
		expect(result).not.toBeNull();
		expect(result?.title).toContain('new commit');
	});
	it('routes release events correctly', async () => {
		const payload: GitHubWebhook = {
			sender: createBaseSender(),
			repository: createBaseRepository(),
			action: 'published',
			release: {
				id: 55555,
				tag_name: 'v1.0.0',
				html_url: 'https://github.com/org/repo/releases/tag/v1.0.0',
				body: 'Release notes',
			},
		};
		const result = await transform('release', payload);
		expect(result).not.toBeNull();
		expect(result?.title).toContain('New release published');
	});
	it('routes watch events correctly', async () => {
		const payload: GitHubWebhook = {
			sender: createBaseSender(),
			repository: createBaseRepository(),
			action: 'started',
		};
		const result = await transform('watch', payload);
		expect(result).not.toBeNull();
		expect(result?.title).toContain('New star added');
	});
	it('routes check_run events correctly', async () => {
		const payload: GitHubWebhook = {
			sender: createBaseSender(),
			repository: createBaseRepository(),
			action: 'completed',
			check_run: {
				conclusion: 'success',
				name: 'build',
				html_url: 'https://github.com/org/repo/runs/123',
				check_suite: {
					conclusion: 'success',
					head_branch: 'main',
					head_sha: 'abc123',
					app: {name: 'GitHub Actions'},
				},
			},
		};
		const result = await transform('check_run', payload);
		expect(result).not.toBeNull();
		expect(result?.title).toContain('build');
	});
	it('routes check_suite events correctly', async () => {
		const payload: GitHubWebhook = {
			sender: createBaseSender(),
			repository: createBaseRepository(),
			action: 'completed',
			check_suite: {
				conclusion: 'success',
				head_branch: 'main',
				head_sha: 'abc123',
				app: {name: 'GitHub Actions'},
			},
		};
		const result = await transform('check_suite', payload);
		expect(result).not.toBeNull();
		expect(result?.title).toContain('GitHub Actions');
	});
	it('routes discussion events correctly', async () => {
		const payload: GitHubWebhook = {
			sender: createBaseSender(),
			repository: createBaseRepository(),
			action: 'created',
			discussion: {
				title: 'Discussion title',
				number: 5,
				html_url: 'https://github.com/org/repo/discussions/5',
				body: 'Discussion body',
				user: createBaseSender(),
			},
		};
		const result = await transform('discussion', payload);
		expect(result).not.toBeNull();
		expect(result?.title).toContain('New discussion');
	});
	it('routes discussion_comment events correctly', async () => {
		const payload: GitHubWebhook = {
			sender: createBaseSender(),
			repository: createBaseRepository(),
			action: 'created',
			discussion: {
				title: 'Discussion title',
				number: 5,
				html_url: 'https://github.com/org/repo/discussions/5',
				body: 'Discussion body',
				user: createBaseSender(),
			},
			comment: {
				id: 789n,
				html_url: 'https://github.com/org/repo/discussions/5#comment-789',
				user: createBaseSender(),
				body: 'Comment body',
			},
		};
		const result = await transform('discussion_comment', payload);
		expect(result).not.toBeNull();
		expect(result?.title).toContain('New comment on discussion');
	});
	it('routes repository events correctly', async () => {
		const payload: GitHubWebhook = {
			sender: createBaseSender(),
			repository: createBaseRepository(),
			action: 'created',
		};
		const result = await transform('repository', payload);
		expect(result).not.toBeNull();
		expect(result?.title).toContain('Repository created');
	});
	it('returns null for unknown event types', async () => {
		const payload: GitHubWebhook = {
			sender: createBaseSender(),
			repository: createBaseRepository(),
		};
		const result = await transform('unknown_event_type', payload);
		expect(result).toBeNull();
	});
	it('returns null for ping events', async () => {
		const payload: GitHubWebhook = {
			sender: createBaseSender(),
			repository: createBaseRepository(),
		};
		const result = await transform('ping', payload);
		expect(result).toBeNull();
	});
});
