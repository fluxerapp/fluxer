/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {describe, expect, it, vi} from 'vitest';
import {findInvite, findInvites} from './InviteUtils';

vi.mock('~/Config', () => ({
	Config: {
		hosts: {
			invite: 'fluxer.gg',
			gift: 'fluxer.gift',
			marketing: 'marketing.fluxer.app',
			unfurlIgnored: [],
		},
		endpoints: {
			webApp: 'https://web.fluxer.app',
		},
	},
}));

describe('InviteUtils', () => {
	describe('findInvites', () => {
		it('should return empty array for null or empty content', () => {
			expect(findInvites(null)).toEqual([]);
			expect(findInvites('')).toEqual([]);
			expect(findInvites('   ')).toEqual([]);
		});

		it('should find invite codes from fluxer.gg URLs (direct, no /invite/)', () => {
			const content = 'Check out this guild: https://fluxer.gg/abc123';
			const result = findInvites(content);

			expect(result).toEqual(['abc123']);
		});

		it('should find invite codes from web.fluxer.app/invite/ URLs', () => {
			const content = 'Join us: https://web.fluxer.app/invite/test123';
			const result = findInvites(content);

			expect(result).toHaveLength(1);
			expect(result[0]).toBe('test123');
		});

		it('should NOT match fluxer.gg/invite/ URLs', () => {
			const content = 'Invalid: https://fluxer.gg/invite/shouldnotwork';
			const result = findInvites(content);

			expect(result).toEqual([]);
		});

		it('should handle URLs without protocol', () => {
			const content = 'Join us: fluxer.gg/test123';
			const result = findInvites(content);

			expect(result).toHaveLength(1);
			expect(result[0]).toBe('test123');
		});

		it('should handle URLs with hash fragment', () => {
			const content = 'Come join: https://web.fluxer.app/#/invite/hash456';
			const result = findInvites(content);

			expect(result).toHaveLength(1);
			expect(result[0]).toBe('hash456');
		});

		it('should find multiple unique invite codes from different hosts', () => {
			const content = `
				First: https://fluxer.gg/invite1
				Second: https://web.fluxer.app/invite/invite2
				Third: https://web.fluxer.app/#/invite/invite3
			`;
			const result = findInvites(content);

			expect(result).toHaveLength(3);
			expect(result).toEqual(['invite1', 'invite2', 'invite3']);
		});

		it('should deduplicate identical invite codes', () => {
			const content = `
				https://fluxer.gg/duplicate
				fluxer.gg/duplicate
				Another mention: https://fluxer.gg/duplicate
			`;
			const result = findInvites(content);

			expect(result).toHaveLength(1);
			expect(result[0]).toBe('duplicate');
		});

		it('should deduplicate codes across different hosts', () => {
			const content = `
				https://fluxer.gg/samecode
				https://web.fluxer.app/invite/samecode
			`;
			const result = findInvites(content);

			expect(result).toHaveLength(1);
			expect(result[0]).toBe('samecode');
		});

		it('should limit to maximum 10 invites', () => {
			let content = '';
			for (let i = 1; i <= 15; i++) {
				content += `https://fluxer.gg/code${i.toString().padStart(2, '0')} `;
			}

			const result = findInvites(content);
			expect(result).toHaveLength(10);
		});

		it('should handle invite codes with valid characters', () => {
			const validCodes = ['abc123', 'TEST-CODE', 'mix3d-Ch4rs', 'AB', 'a'.repeat(32)];

			validCodes.forEach((code) => {
				const content = `https://fluxer.gg/${code}`;
				const result = findInvites(content);

				expect(result).toHaveLength(1);
				expect(result[0]).toBe(code);
			});
		});

		it('should ignore invite codes that are too short', () => {
			const code = 'a';
			const content = `https://fluxer.gg/${code}`;
			const result = findInvites(content);

			expect(result).toHaveLength(0);
		});

		it('should ignore invite codes that are too long', () => {
			const code = 'a'.repeat(33);
			const content = `https://fluxer.gg/${code}`;
			const result = findInvites(content);

			expect(result).toHaveLength(0);
		});

		it('should handle mixed case URLs', () => {
			const content = 'Join: https://fluxer.gg/MixedCase123';
			const result = findInvites(content);

			expect(result).toHaveLength(1);
			expect(result[0]).toBe('MixedCase123');
		});

		it('should handle URLs with extra text around them', () => {
			const content = 'Before text https://fluxer.gg/surrounded123 after text';
			const result = findInvites(content);

			expect(result).toHaveLength(1);
			expect(result[0]).toBe('surrounded123');
		});

		it('should handle web.fluxer.app URLs with and without protocol', () => {
			const content = `
				https://web.fluxer.app/invite/withprotocol
				web.fluxer.app/invite/withoutprotocol
			`;
			const result = findInvites(content);

			expect(result).toHaveLength(2);
			expect(result).toEqual(['withprotocol', 'withoutprotocol']);
		});

		it('should handle mixed fluxer.gg and web.fluxer.app URLs', () => {
			const content = `
				Direct: fluxer.gg/direct123
				Web app: web.fluxer.app/invite/local456
				Another direct: https://fluxer.gg/direct789
			`;
			const result = findInvites(content);

			expect(result).toHaveLength(3);
			expect(result).toEqual(['direct123', 'local456', 'direct789']);
		});

		it('should handle canary domain', () => {
			const content = `
				Canary: https://web.canary.fluxer.app/invite/canary123
				Stable: https://web.fluxer.app/invite/stable456
			`;
			const result = findInvites(content);

			expect(result).toHaveLength(1);
			expect(result).toEqual(['stable456']);
		});

		it('should NOT match marketing site invite URLs', () => {
			const content = 'Invalid: https://fluxer.app/invite/shouldnotwork';
			const result = findInvites(content);

			expect(result).toEqual([]);
		});
	});

	describe('findInvite', () => {
		it('should return null for null or empty content', () => {
			expect(findInvite(null)).toBeNull();
			expect(findInvite('')).toBeNull();
			expect(findInvite('   ')).toBeNull();
		});

		it('should find first invite code from fluxer.gg', () => {
			const content = 'Check out: https://fluxer.gg/first123';
			const result = findInvite(content);

			expect(result).toBe('first123');
		});

		it('should find first invite code from web.fluxer.app', () => {
			const content = 'Check out: https://web.fluxer.app/invite/first123';
			const result = findInvite(content);

			expect(result).toBe('first123');
		});

		it('should return first invite when multiple exist', () => {
			const content = `
				First: https://fluxer.gg/first456
				Second: web.fluxer.app/invite/second789
			`;
			const result = findInvite(content);

			expect(result).toBe('first456');
		});

		it('should handle URLs without protocol', () => {
			const content = 'Join: fluxer.gg/noprotocol';
			const result = findInvite(content);

			expect(result).toBe('noprotocol');
		});

		it('should handle URLs with hash fragment', () => {
			const content = 'Visit: https://web.fluxer.app/#/invite/hashcode';
			const result = findInvite(content);

			expect(result).toBe('hashcode');
		});

		it('should return null when no valid invite found', () => {
			const invalidContents = [
				'No invites here',
				'https://other-site.com/invite/code123',
				'https://fluxer.gg/invite/shouldnotmatch',
				'https://fluxer.gg/a',
				'https://fluxer.app/invite/marketing',
			];

			invalidContents.forEach((content) => {
				expect(findInvite(content)).toBeNull();
			});
		});

		it('should handle case insensitive matching', () => {
			const content = 'Visit: HTTPS://FLUXER.GG/CaseTest';
			const result = findInvite(content);

			expect(result).toBe('CaseTest');
		});

		it('should handle complex content with multiple URLs', () => {
			const content = `
				Visit our website at https://fluxer.app
				Join our guild: https://fluxer.gg/complex123
				Learn more at https://fluxer.app/about
			`;
			const result = findInvite(content);

			expect(result).toBe('complex123');
		});
	});

	describe('edge cases', () => {
		it('should handle content with special regex characters', () => {
			const content = 'Check this (important): https://fluxer.gg/special123 [link]';
			const result = findInvites(content);

			expect(result).toHaveLength(1);
			expect(result[0]).toBe('special123');
		});

		it('should handle very long content without crashing', () => {
			const longContent = `${'a'.repeat(10000)}https://fluxer.gg/buried123${'b'.repeat(10000)}`;
			const result = findInvites(longContent);

			expect(result).toEqual([]);
		});

		it('should handle malformed URLs gracefully', () => {
			const content = `
				https://fluxer.gg/good123
				https://fluxer.gg/
				https://fluxer.gg
				fluxer.gg/another456
				web.fluxer.app/invite/valid789
			`;
			const result = findInvites(content);

			expect(result).toHaveLength(3);
			expect(result).toEqual(['good123', 'another456', 'valid789']);
		});

		it('should reset regex state between calls', () => {
			const content1 = 'https://fluxer.gg/first123';
			const content2 = 'https://web.fluxer.app/invite/second456';

			const result1 = findInvite(content1);
			const result2 = findInvite(content2);

			expect(result1).toBe('first123');
			expect(result2).toBe('second456');
		});

		it('should handle codes at exact length boundaries', () => {
			const minCode = 'ab';
			const maxCode = 'a'.repeat(32);

			const contentMin = `https://fluxer.gg/${minCode}`;
			const contentMax = `https://fluxer.gg/${maxCode}`;

			expect(findInvite(contentMin)).toBe(minCode);
			expect(findInvite(contentMax)).toBe(maxCode);
		});

		it('should distinguish between marketing and web app domains', () => {
			const content = `
				Marketing: https://fluxer.app/invite/marketing123
				Web app: https://web.fluxer.app/invite/webapp456
				Shortlink: https://fluxer.gg/shortlink789
			`;
			const result = findInvites(content);

			expect(result).toHaveLength(2);
			expect(result).toEqual(['webapp456', 'shortlink789']);
		});
	});
});
