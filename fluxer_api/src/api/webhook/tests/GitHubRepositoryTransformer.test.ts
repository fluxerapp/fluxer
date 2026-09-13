// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	transformFork,
	transformMember,
	transformPublic,
	transformRelease,
	transformRepository,
	transformWatch,
} from '@app/api/webhook/transformers/GitHubRepositoryTransformer';
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

describe('GitHub Repository Transformer', () => {
	describe('transformFork', () => {
		it('transforms a fork event', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				forkee: {
					id: 11111,
					html_url: 'https://github.com/testuser/repo',
					name: 'repo',
					full_name: 'testuser/repo',
				},
			};
			const result = await transformFork(payload);
			expect(result).not.toBeNull();
			expect(result?.title).toContain('[org/repo]');
			expect(result?.title).toContain('Fork created');
			expect(result?.title).toContain('testuser/repo');
			expect(result?.url).toBe('https://github.com/testuser/repo');
			expect(result?.author?.name).toBe('testuser');
		});
		it('returns null when repository is missing', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				forkee: {
					id: 11111,
					html_url: 'https://github.com/testuser/repo',
					name: 'repo',
					full_name: 'testuser/repo',
				},
			};
			const result = await transformFork(payload);
			expect(result).toBeNull();
		});
		it('returns null when forkee is missing', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
			};
			const result = await transformFork(payload);
			expect(result).toBeNull();
		});
	});
	describe('transformMember', () => {
		it('transforms a member added event', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'added',
				member: {
					id: 99999,
					login: 'newcollab',
					html_url: 'https://github.com/newcollab',
					avatar_url: 'https://avatars.githubusercontent.com/u/99999',
				},
			};
			const result = await transformMember(payload);
			expect(result).not.toBeNull();
			expect(result?.title).toContain('[org/repo]');
			expect(result?.title).toContain('New collaborator added');
			expect(result?.title).toContain('newcollab');
			expect(result?.url).toBe('https://github.com/newcollab');
			expect(result?.author?.name).toBe('testuser');
		});
		it('returns null for non-added actions', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'removed',
				member: {
					id: 99999,
					login: 'removedcollab',
					html_url: 'https://github.com/removedcollab',
					avatar_url: 'https://avatars.githubusercontent.com/u/99999',
				},
			};
			const result = await transformMember(payload);
			expect(result).toBeNull();
		});
		it('returns null when member is missing', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'added',
			};
			const result = await transformMember(payload);
			expect(result).toBeNull();
		});
		it('returns null when repository is missing', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				action: 'added',
				member: {
					id: 99999,
					login: 'newcollab',
					html_url: 'https://github.com/newcollab',
					avatar_url: 'https://avatars.githubusercontent.com/u/99999',
				},
			};
			const result = await transformMember(payload);
			expect(result).toBeNull();
		});
	});
	describe('transformPublic', () => {
		it('transforms a public event', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
			};
			const result = await transformPublic(payload);
			expect(result).not.toBeNull();
			expect(result?.title).toContain('[org/repo]');
			expect(result?.title).toContain('Now open sourced!');
			expect(result?.author?.name).toBe('testuser');
		});
		it('returns null when repository is missing', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
			};
			const result = await transformPublic(payload);
			expect(result).toBeNull();
		});
	});
	describe('transformRelease', () => {
		it('transforms a published release event', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'published',
				release: {
					id: 55555,
					tag_name: 'v3.0.0',
					html_url: 'https://github.com/org/repo/releases/tag/v3.0.0',
					body: 'Major release with breaking changes.',
				},
			};
			const result = await transformRelease(payload);
			expect(result).not.toBeNull();
			expect(result?.title).toContain('[org/repo]');
			expect(result?.title).toContain('New release published');
			expect(result?.title).toContain('v3.0.0');
			expect(result?.url).toBe('https://github.com/org/repo/releases/tag/v3.0.0');
			expect(result?.author?.name).toBe('testuser');
		});
		it('returns null for non-published actions', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'created',
				release: {
					id: 55555,
					tag_name: 'v3.0.0',
					html_url: 'https://github.com/org/repo/releases/tag/v3.0.0',
					body: 'Draft release.',
				},
			};
			const result = await transformRelease(payload);
			expect(result).toBeNull();
		});
		it('returns null when release is missing', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'published',
			};
			const result = await transformRelease(payload);
			expect(result).toBeNull();
		});
		it('returns null when repository is missing', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				action: 'published',
				release: {
					id: 55555,
					tag_name: 'v3.0.0',
					html_url: 'https://github.com/org/repo/releases/tag/v3.0.0',
					body: 'Release notes.',
				},
			};
			const result = await transformRelease(payload);
			expect(result).toBeNull();
		});
	});
	describe('transformWatch', () => {
		it('transforms a star added event', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'started',
			};
			const result = await transformWatch(payload);
			expect(result).not.toBeNull();
			expect(result?.title).toContain('[org/repo]');
			expect(result?.title).toContain('New star added');
			expect(result?.url).toBe('https://github.com/org/repo');
			expect(result?.author?.name).toBe('testuser');
		});
		it('returns null for non-started actions', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'stopped',
			};
			const result = await transformWatch(payload);
			expect(result).toBeNull();
		});
		it('returns null when repository is missing', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				action: 'started',
			};
			const result = await transformWatch(payload);
			expect(result).toBeNull();
		});
	});
	describe('transformRepository', () => {
		it('transforms a repository created event', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'created',
			};
			const result = await transformRepository(payload);
			expect(result).not.toBeNull();
			expect(result?.title).toContain('[org/repo]');
			expect(result?.title).toContain('Repository created');
			expect(result?.url).toBe('https://github.com/org/repo');
			expect(result?.color).toBe(0x2cbe4e);
			expect(result?.author?.name).toBe('testuser');
		});
		it('returns null for non-created actions', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				repository: createBaseRepository(),
				action: 'deleted',
			};
			const result = await transformRepository(payload);
			expect(result).toBeNull();
		});
		it('returns null when repository is missing', async () => {
			const payload: GitHubWebhook = {
				sender: createBaseSender(),
				action: 'created',
			};
			const result = await transformRepository(payload);
			expect(result).toBeNull();
		});
	});
});
