// SPDX-License-Identifier: AGPL-3.0-or-later

import {transformPush} from '@app/api/webhook/transformers/GitHubCommitTransformer';
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

describe('GitHub Push Transformer', () => {
	it('transforms a single commit push to main branch', async () => {
		const payload: GitHubWebhook = {
			sender: createBaseSender(),
			repository: createBaseRepository(),
			ref: 'refs/heads/main',
			compare: 'https://github.com/org/repo/compare/abc123...def456',
			commits: [
				{
					id: 'def456789abcdef0123456789abcdef01234567',
					url: 'https://github.com/org/repo/commit/def456',
					message: 'Add new feature',
					author: {name: 'Test User'},
				},
			],
		};
		const result = await transformPush(payload);
		expect(result).not.toBeNull();
		expect(result?.title).toContain('[repo:main]');
		expect(result?.title).toContain('1 new commit');
		expect(result?.url).toBe('https://github.com/org/repo/compare/abc123...def456');
		expect(result?.color).toBe(0x7289da);
		expect(result?.description).toContain('def4567');
		expect(result?.description).toContain('Add new feature');
		expect(result?.author?.name).toBe('testuser');
	});
	it('transforms multiple commits push', async () => {
		const payload: GitHubWebhook = {
			sender: createBaseSender(),
			repository: createBaseRepository(),
			ref: 'refs/heads/develop',
			compare: 'https://github.com/org/repo/compare/abc123...xyz789',
			commits: [
				{
					id: 'abc123456789abcdef0123456789abcdef0123456',
					url: 'https://github.com/org/repo/commit/abc123',
					message: 'First commit',
					author: {name: 'Dev One'},
				},
				{
					id: 'def456789abcdef0123456789abcdef01234567',
					url: 'https://github.com/org/repo/commit/def456',
					message: 'Second commit',
					author: {name: 'Dev Two'},
				},
				{
					id: 'xyz789012345abcdef0123456789abcdef012345',
					url: 'https://github.com/org/repo/commit/xyz789',
					message: 'Third commit',
					author: {name: 'Dev Three'},
				},
			],
		};
		const result = await transformPush(payload);
		expect(result).not.toBeNull();
		expect(result?.title).toContain('[repo:develop]');
		expect(result?.title).toContain('3 new commits');
		expect(result?.description).toContain('First commit');
		expect(result?.description).toContain('Second commit');
		expect(result?.description).toContain('Third commit');
	});
	it('transforms a force push event', async () => {
		const payload: GitHubWebhook = {
			sender: createBaseSender(),
			repository: createBaseRepository(),
			ref: 'refs/heads/feature',
			forced: true,
			compare: 'https://github.com/org/repo/compare/oldsha...newsha',
			head_commit: {
				id: 'newsha123456789abcdef0123456789abcdef01234',
				url: 'https://github.com/org/repo/commit/newsha',
				message: 'Latest commit',
				author: {name: 'Test User'},
			},
		};
		const result = await transformPush(payload);
		expect(result).not.toBeNull();
		expect(result?.title).toContain('Branch feature was force-pushed');
		expect(result?.title).toContain('newsha1');
		expect(result?.color).toBe(0xfcbd1f);
		expect(result?.description).toContain('Compare changes');
	});
	it('transforms a tag push event', async () => {
		const payload: GitHubWebhook = {
			sender: createBaseSender(),
			repository: createBaseRepository(),
			ref: 'refs/tags/v1.0.0',
			compare: 'https://github.com/org/repo/compare/abc...def',
			commits: [
				{
					id: 'abc123456789abcdef0123456789abcdef0123456',
					url: 'https://github.com/org/repo/commit/abc123',
					message: 'Release v1.0.0',
					author: {name: 'Release Manager'},
				},
			],
		};
		const result = await transformPush(payload);
		expect(result).not.toBeNull();
		expect(result?.title).toContain('[repo:v1.0.0]');
	});
	it('returns null when repository is missing', async () => {
		const payload: GitHubWebhook = {
			sender: createBaseSender(),
			ref: 'refs/heads/main',
		};
		const result = await transformPush(payload);
		expect(result).toBeNull();
	});
	it('returns null when ref is missing', async () => {
		const payload: GitHubWebhook = {
			sender: createBaseSender(),
			repository: createBaseRepository(),
		};
		const result = await transformPush(payload);
		expect(result).toBeNull();
	});
	it('returns null when commits array is empty and not a force push', async () => {
		const payload: GitHubWebhook = {
			sender: createBaseSender(),
			repository: createBaseRepository(),
			ref: 'refs/heads/main',
			compare: 'https://github.com/org/repo/compare/abc...def',
			commits: [],
		};
		const result = await transformPush(payload);
		expect(result).toBeNull();
	});
	it('returns null for force push without head_commit', async () => {
		const payload: GitHubWebhook = {
			sender: createBaseSender(),
			repository: createBaseRepository(),
			ref: 'refs/heads/main',
			forced: true,
			compare: 'https://github.com/org/repo/compare/abc...def',
		};
		const result = await transformPush(payload);
		expect(result).toBeNull();
	});
	it('returns null for force push without compare URL', async () => {
		const payload: GitHubWebhook = {
			sender: createBaseSender(),
			repository: createBaseRepository(),
			ref: 'refs/heads/main',
			forced: true,
			head_commit: {
				id: 'abc123456789abcdef0123456789abcdef0123456',
				url: 'https://github.com/org/repo/commit/abc123',
				message: 'Latest commit',
				author: {name: 'Test User'},
			},
		};
		const result = await transformPush(payload);
		expect(result).toBeNull();
	});
	it('returns null when commits exist but compare URL is missing', async () => {
		const payload: GitHubWebhook = {
			sender: createBaseSender(),
			repository: createBaseRepository(),
			ref: 'refs/heads/main',
			commits: [
				{
					id: 'abc123456789abcdef0123456789abcdef0123456',
					url: 'https://github.com/org/repo/commit/abc123',
					message: 'A commit',
					author: {name: 'Test User'},
				},
			],
		};
		const result = await transformPush(payload);
		expect(result).toBeNull();
	});
	it('handles commit messages with revert references', async () => {
		const payload: GitHubWebhook = {
			sender: createBaseSender(),
			repository: createBaseRepository(),
			ref: 'refs/heads/main',
			compare: 'https://github.com/org/repo/compare/abc...def',
			commits: [
				{
					id: 'abc123456789abcdef0123456789abcdef0123456',
					url: 'https://github.com/org/repo/commit/abc123',
					message: 'Revert "Bad change"\n\nThis reverts commit 1234567890abcdef1234567890abcdef12345678.',
					author: {name: 'Test User'},
				},
			],
		};
		const result = await transformPush(payload);
		expect(result).not.toBeNull();
		expect(result?.description).toContain('1234567');
		expect(result?.description).toContain(
			'https://github.com/org/repo/commit/1234567890abcdef1234567890abcdef12345678',
		);
	});
	it('handles non-standard ref formats', async () => {
		const payload: GitHubWebhook = {
			sender: createBaseSender(),
			repository: createBaseRepository(),
			ref: 'refs/pull/123/head',
			compare: 'https://github.com/org/repo/compare/abc...def',
			commits: [
				{
					id: 'abc123456789abcdef0123456789abcdef0123456',
					url: 'https://github.com/org/repo/commit/abc123',
					message: 'PR commit',
					author: {name: 'Test User'},
				},
			],
		};
		const result = await transformPush(payload);
		expect(result).not.toBeNull();
		expect(result?.title).toContain('refs/pull/123/head');
	});
});
