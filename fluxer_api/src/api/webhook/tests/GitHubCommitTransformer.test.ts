// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	transformCommitComment,
	transformCreate,
	transformDelete,
} from '@app/api/webhook/transformers/GitHubCommitTransformer';
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

describe('GitHub Commit Transformer', () => {
	describe('transformCommitComment', () => {
		it('transforms a created commit comment', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'created',
				comment: {
					id: 999888777n,
					html_url: 'https://github.com/org/repo/commit/abc123#commitcomment-456',
					user: {
						id: 54321,
						login: 'commenter',
						html_url: 'https://github.com/commenter',
						avatar_url: 'https://avatars.githubusercontent.com/u/54321',
					},
					body: 'This line looks problematic.',
					commit_id: 'abc123def456789012345678901234567890abcd',
				},
			};
			const result = await transformCommitComment(payload);
			expect(result).not.toBeNull();
			expect(result?.title).toContain('[org/repo]');
			expect(result?.title).toContain('New comment on commit');
			expect(result?.title).toContain('abc123d');
			expect(result?.url).toBe('https://github.com/org/repo/commit/abc123#commitcomment-456');
			expect(result?.description).toContain('This line looks problematic');
			expect(result?.author?.name).toBe('commenter');
		});
		it('returns null for non-created actions', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'edited',
				comment: {
					id: 999888777n,
					html_url: 'https://github.com/org/repo/commit/abc123#commitcomment-456',
					user: createBaseSender(),
					body: 'Edited comment',
					commit_id: 'abc123def456789012345678901234567890abcd',
				},
			};
			const result = await transformCommitComment(payload);
			expect(result).toBeNull();
		});
		it('returns null when repository is missing', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				action: 'created',
				comment: {
					id: 999888777n,
					html_url: 'https://github.com/org/repo/commit/abc123#commitcomment-456',
					user: createBaseSender(),
					body: 'Comment text',
					commit_id: 'abc123def456789012345678901234567890abcd',
				},
			};
			const result = await transformCommitComment(payload);
			expect(result).toBeNull();
		});
		it('returns null when comment is missing', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'created',
			};
			const result = await transformCommitComment(payload);
			expect(result).toBeNull();
		});
		it('returns null when commit_id is missing', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'created',
				comment: {
					id: 999888777n,
					html_url: 'https://github.com/org/repo/commit/abc123#commitcomment-456',
					user: createBaseSender(),
					body: 'Comment without commit_id',
				},
			};
			const result = await transformCommitComment(payload);
			expect(result).toBeNull();
		});
	});
	describe('transformCreate', () => {
		it('transforms a branch creation event', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				ref: 'feature/new-feature',
				ref_type: 'branch',
			};
			const result = await transformCreate(payload);
			expect(result).not.toBeNull();
			expect(result?.title).toContain('[org/repo]');
			expect(result?.title).toContain('New branch created');
			expect(result?.title).toContain('feature/new-feature');
			expect(result?.author?.name).toBe('testuser');
			expect(result?.author?.url).toBe('https://github.com/testuser');
		});
		it('transforms a tag creation event', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				ref: 'v2.0.0',
				ref_type: 'tag',
			};
			const result = await transformCreate(payload);
			expect(result).not.toBeNull();
			expect(result?.title).toContain('New tag created');
			expect(result?.title).toContain('v2.0.0');
		});
		it('returns null for unsupported ref_type', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				ref: 'something',
				ref_type: 'repository',
			};
			const result = await transformCreate(payload);
			expect(result).toBeNull();
		});
		it('returns null when ref is missing', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				ref_type: 'branch',
			};
			const result = await transformCreate(payload);
			expect(result).toBeNull();
		});
		it('returns null when ref_type is missing', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				ref: 'feature/branch',
			};
			const result = await transformCreate(payload);
			expect(result).toBeNull();
		});
		it('returns null when repository is missing', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				ref: 'feature/branch',
				ref_type: 'branch',
			};
			const result = await transformCreate(payload);
			expect(result).toBeNull();
		});
	});
	describe('transformDelete', () => {
		it('transforms a branch deletion event', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				ref: 'feature/old-feature',
				ref_type: 'branch',
			};
			const result = await transformDelete(payload);
			expect(result).not.toBeNull();
			expect(result?.title).toContain('[org/repo]');
			expect(result?.title).toContain('Branch deleted');
			expect(result?.title).toContain('feature/old-feature');
			expect(result?.author?.name).toBe('testuser');
		});
		it('transforms a tag deletion event', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				ref: 'v1.0.0-beta',
				ref_type: 'tag',
			};
			const result = await transformDelete(payload);
			expect(result).not.toBeNull();
			expect(result?.title).toContain('Tag deleted');
			expect(result?.title).toContain('v1.0.0-beta');
		});
		it('returns null for unsupported ref_type', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				ref: 'something',
				ref_type: 'repository',
			};
			const result = await transformDelete(payload);
			expect(result).toBeNull();
		});
		it('returns null when ref is missing', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				ref_type: 'branch',
			};
			const result = await transformDelete(payload);
			expect(result).toBeNull();
		});
		it('returns null when ref_type is missing', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				ref: 'feature/branch',
			};
			const result = await transformDelete(payload);
			expect(result).toBeNull();
		});
		it('returns null when repository is missing', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				ref: 'feature/branch',
				ref_type: 'branch',
			};
			const result = await transformDelete(payload);
			expect(result).toBeNull();
		});
	});
});
