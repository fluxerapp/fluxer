// SPDX-License-Identifier: AGPL-3.0-or-later

import {resolveExpressionSourceGuild} from '@app/api/guild/ExpressionSourceGuild';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {describe, expect, it, vi} from 'vitest';

function guild(features: Array<string>, icon: string | null = null): GuildResponse {
	return {id: '20', name: 'Blob Club', icon, features} as unknown as GuildResponse;
}

describe('resolveExpressionSourceGuild', () => {
	it('hides a community the gateway reports as unavailable without checking membership', async () => {
		const isMember = vi.fn(() => Promise.resolve(true));
		await expect(
			resolveExpressionSourceGuild({loadGuild: () => Promise.reject(new UnknownGuildError()), isMember}),
		).rejects.toBeInstanceOf(UnknownGuildError);
		expect(isMember).not.toHaveBeenCalled();
	});

	it('propagates other gateway failures instead of reporting the community as unavailable', async () => {
		const failure = new Error('gateway timeout');
		const isMember = vi.fn(() => Promise.resolve(true));
		await expect(resolveExpressionSourceGuild({loadGuild: () => Promise.reject(failure), isMember})).rejects.toBe(
			failure,
		);
		expect(isMember).not.toHaveBeenCalled();
	});

	it('reveals a discoverable community without checking membership', async () => {
		const isMember = vi.fn(() => Promise.resolve(false));
		const community = await resolveExpressionSourceGuild({
			loadGuild: () => Promise.resolve(guild([GuildFeatures.DISCOVERABLE])),
			isMember,
		});
		expect(community).toEqual({id: '20', name: 'Blob Club', icon: null, features: [GuildFeatures.DISCOVERABLE]});
		expect(isMember).not.toHaveBeenCalled();
	});

	it('reveals a private community to a member', async () => {
		const community = await resolveExpressionSourceGuild({
			loadGuild: () => Promise.resolve(guild([])),
			isMember: () => Promise.resolve(true),
		});
		expect(community.id).toBe('20');
		expect(community.features).toEqual([]);
	});

	it('hides a private community from a non-member', async () => {
		await expect(
			resolveExpressionSourceGuild({
				loadGuild: () => Promise.resolve(guild([])),
				isMember: () => Promise.resolve(false),
			}),
		).rejects.toBeInstanceOf(UnknownGuildError);
	});

	it('limits features to the badge flags', async () => {
		const community = await resolveExpressionSourceGuild({
			loadGuild: () =>
				Promise.resolve(
					guild([
						GuildFeatures.DISCOVERABLE,
						GuildFeatures.VERIFIED,
						GuildFeatures.INVITES_DISABLED,
						GuildFeatures.CLONE_EMOJI_ENABLED,
					]),
				),
			isMember: () => Promise.resolve(false),
		});
		expect(community.features).toEqual(expect.arrayContaining([GuildFeatures.DISCOVERABLE, GuildFeatures.VERIFIED]));
		expect(community.features).not.toContain(GuildFeatures.INVITES_DISABLED);
		expect(community.features).not.toContain(GuildFeatures.CLONE_EMOJI_ENABLED);
	});

	it('strips the animated icon prefix unless the community is entitled to it', async () => {
		const plain = await resolveExpressionSourceGuild({
			loadGuild: () => Promise.resolve(guild([GuildFeatures.DISCOVERABLE], 'a_icon')),
			isMember: () => Promise.resolve(false),
		});
		const animated = await resolveExpressionSourceGuild({
			loadGuild: () => Promise.resolve(guild([GuildFeatures.DISCOVERABLE, GuildFeatures.ANIMATED_ICON], 'a_icon')),
			isMember: () => Promise.resolve(false),
		});
		expect(plain.icon).toBe('icon');
		expect(animated.icon).toBe('a_icon');
	});
});
