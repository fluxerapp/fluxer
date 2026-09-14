// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	transformCheckRun,
	transformCheckSuite,
	transformDiscussion,
	transformDiscussionComment,
} from '@app/api/webhook/transformers/GitHubCheckTransformer';
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

describe('GitHub Check Transformer', () => {
	describe('transformCheckRun', () => {
		it('transforms a completed successful check run', async () => {
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
						head_sha: 'abc123def456789012345678901234567890abcd',
						app: {name: 'GitHub Actions'},
					},
				},
			};
			const result = await transformCheckRun(payload);
			expect(result).not.toBeNull();
			expect(result?.title).toContain('[repo]');
			expect(result?.title).toContain('build');
			expect(result?.title).toContain('success');
			expect(result?.title).toContain('main');
			expect(result?.url).toContain('/commit/abc123def456789012345678901234567890abcd');
			expect(result?.color).toBe(0x009800);
		});
		it('transforms a completed failed check run', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'completed',
				check_run: {
					conclusion: 'failure',
					name: 'test',
					html_url: 'https://github.com/org/repo/runs/456',
					check_suite: {
						conclusion: 'failure',
						head_branch: 'feature',
						head_sha: 'def456789012345678901234567890abcdef1234',
						app: {name: 'GitHub Actions'},
					},
				},
			};
			const result = await transformCheckRun(payload);
			expect(result).not.toBeNull();
			expect(result?.title).toContain('failure');
			expect(result?.color).toBe(0xfc2929);
		});
		it('returns null for skipped check runs', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'completed',
				check_run: {
					conclusion: 'success',
					name: 'skipped-check',
					html_url: 'https://github.com/org/repo/runs/789',
					check_suite: {
						conclusion: 'skipped',
						head_branch: 'main',
						head_sha: 'abc123',
						app: {name: 'GitHub Actions'},
					},
				},
			};
			const result = await transformCheckRun(payload);
			expect(result).toBeNull();
		});
		it('returns null for non-completed actions', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'created',
				check_run: {
					conclusion: null,
					name: 'build',
					html_url: 'https://github.com/org/repo/runs/123',
					check_suite: {
						conclusion: null,
						head_branch: 'main',
						head_sha: 'abc123',
						app: {name: 'GitHub Actions'},
					},
				},
			};
			const result = await transformCheckRun(payload);
			expect(result).toBeNull();
		});
		it('returns null when check_run is missing', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'completed',
			};
			const result = await transformCheckRun(payload);
			expect(result).toBeNull();
		});
		it('returns null when repository is missing', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
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
			const result = await transformCheckRun(payload);
			expect(result).toBeNull();
		});
	});
	describe('transformCheckSuite', () => {
		it('transforms a completed successful check suite', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'completed',
				check_suite: {
					conclusion: 'success',
					head_branch: 'main',
					head_sha: 'abc123def456789012345678901234567890abcd',
					app: {name: 'GitHub Actions'},
				},
			};
			const result = await transformCheckSuite(payload);
			expect(result).not.toBeNull();
			expect(result?.title).toContain('[repo]');
			expect(result?.title).toContain('GitHub Actions');
			expect(result?.title).toContain('checks');
			expect(result?.title).toContain('success');
			expect(result?.title).toContain('main');
			expect(result?.color).toBe(0x009800);
		});
		it('transforms a completed failed check suite', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'completed',
				check_suite: {
					conclusion: 'failure',
					head_branch: 'develop',
					head_sha: 'def456',
					app: {name: 'CircleCI'},
				},
			};
			const result = await transformCheckSuite(payload);
			expect(result).not.toBeNull();
			expect(result?.title).toContain('CircleCI');
			expect(result?.title).toContain('failure');
			expect(result?.color).toBe(0xfc2929);
		});
		it('returns null for skipped check suites', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'completed',
				check_suite: {
					conclusion: 'skipped',
					head_branch: 'main',
					head_sha: 'abc123',
					app: {name: 'GitHub Actions'},
				},
			};
			const result = await transformCheckSuite(payload);
			expect(result).toBeNull();
		});
		it('returns null for non-completed actions', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'requested',
				check_suite: {
					conclusion: null,
					head_branch: 'main',
					head_sha: 'abc123',
					app: {name: 'GitHub Actions'},
				},
			};
			const result = await transformCheckSuite(payload);
			expect(result).toBeNull();
		});
		it('returns null when check_suite is missing', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'completed',
			};
			const result = await transformCheckSuite(payload);
			expect(result).toBeNull();
		});
		it('returns null when repository is missing', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				action: 'completed',
				check_suite: {
					conclusion: 'success',
					head_branch: 'main',
					head_sha: 'abc123',
					app: {name: 'GitHub Actions'},
				},
			};
			const result = await transformCheckSuite(payload);
			expect(result).toBeNull();
		});
	});
	describe('transformDiscussion', () => {
		it('transforms a created discussion', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'created',
				discussion: {
					title: 'How to use this library?',
					number: 5,
					html_url: 'https://github.com/org/repo/discussions/5',
					body: 'I am trying to understand how to use this library for my project.',
					user: {
						id: 54321,
						login: 'questionasker',
						html_url: 'https://github.com/questionasker',
						avatar_url: 'https://avatars.githubusercontent.com/u/54321',
					},
				},
			};
			const result = await transformDiscussion(payload);
			expect(result).not.toBeNull();
			expect(result?.title).toContain('[org/repo]');
			expect(result?.title).toContain('New discussion #5');
			expect(result?.title).toContain('How to use this library?');
			expect(result?.url).toBe('https://github.com/org/repo/discussions/5');
			expect(result?.color).toBe(0xe6c2b0);
			expect(result?.description).toContain('trying to understand');
			expect(result?.author?.name).toBe('questionasker');
		});
		it('returns null for non-created actions', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'edited',
				discussion: {
					title: 'Updated discussion',
					number: 5,
					html_url: 'https://github.com/org/repo/discussions/5',
					body: 'Updated body',
					user: createBaseSender(),
				},
			};
			const result = await transformDiscussion(payload);
			expect(result).toBeNull();
		});
		it('returns null when discussion is missing', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'created',
			};
			const result = await transformDiscussion(payload);
			expect(result).toBeNull();
		});
		it('returns null when repository is missing', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				action: 'created',
				discussion: {
					title: 'Discussion',
					number: 5,
					html_url: 'https://github.com/org/repo/discussions/5',
					body: 'Body',
					user: createBaseSender(),
				},
			};
			const result = await transformDiscussion(payload);
			expect(result).toBeNull();
		});
		it('handles discussion with null body', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'created',
				discussion: {
					title: 'Discussion without body',
					number: 10,
					html_url: 'https://github.com/org/repo/discussions/10',
					body: null,
					user: createBaseSender(),
				},
			};
			const result = await transformDiscussion(payload);
			expect(result).not.toBeNull();
			expect(result?.description).toBe('');
		});
	});
	describe('transformDiscussionComment', () => {
		it('transforms a created discussion comment', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'created',
				discussion: {
					title: 'How to use this library?',
					number: 5,
					html_url: 'https://github.com/org/repo/discussions/5',
					body: 'Original question',
					user: createBaseSender(),
				},
				comment: {
					id: 888777666n,
					html_url: 'https://github.com/org/repo/discussions/5#discussioncomment-123',
					user: {
						id: 11111,
						login: 'helper',
						html_url: 'https://github.com/helper',
						avatar_url: 'https://avatars.githubusercontent.com/u/11111',
					},
					body: 'You can use it like this...',
				},
			};
			const result = await transformDiscussionComment(payload);
			expect(result).not.toBeNull();
			expect(result?.title).toContain('[org/repo]');
			expect(result?.title).toContain('New comment on discussion #5');
			expect(result?.url).toBe('https://github.com/org/repo/discussions/5#discussioncomment-123');
			expect(result?.color).toBe(0xe6c2b0);
			expect(result?.description).toContain('You can use it like this');
			expect(result?.author?.name).toBe('helper');
		});
		it('returns null for non-created actions', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'edited',
				discussion: {
					title: 'Discussion',
					number: 5,
					html_url: 'https://github.com/org/repo/discussions/5',
					body: 'Body',
					user: createBaseSender(),
				},
				comment: {
					id: 888777666n,
					html_url: 'https://github.com/org/repo/discussions/5#discussioncomment-123',
					user: createBaseSender(),
					body: 'Edited comment',
				},
			};
			const result = await transformDiscussionComment(payload);
			expect(result).toBeNull();
		});
		it('returns null when comment is missing', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'created',
				discussion: {
					title: 'Discussion',
					number: 5,
					html_url: 'https://github.com/org/repo/discussions/5',
					body: 'Body',
					user: createBaseSender(),
				},
			};
			const result = await transformDiscussionComment(payload);
			expect(result).toBeNull();
		});
		it('returns null when discussion is missing', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'created',
				comment: {
					id: 888777666n,
					html_url: 'https://github.com/org/repo/discussions/5#discussioncomment-123',
					user: createBaseSender(),
					body: 'Comment without discussion',
				},
			};
			const result = await transformDiscussionComment(payload);
			expect(result).toBeNull();
		});
		it('returns null when repository is missing', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				action: 'created',
				discussion: {
					title: 'Discussion',
					number: 5,
					html_url: 'https://github.com/org/repo/discussions/5',
					body: 'Body',
					user: createBaseSender(),
				},
				comment: {
					id: 888777666n,
					html_url: 'https://github.com/org/repo/discussions/5#discussioncomment-123',
					user: createBaseSender(),
					body: 'Comment',
				},
			};
			const result = await transformDiscussionComment(payload);
			expect(result).toBeNull();
		});
	});
});
