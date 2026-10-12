// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildID, createUserID} from '@app/api/BrandedTypes';
import {clearChannelThreadsTaintCacheForTesting, isTainted} from '@app/api/experiment/ChannelThreadsGate';
import {createGuildMfaEnforcer} from '@app/api/guild/services/GuildMfaEnforcement';
import {
	maskGuildResponseThreadBits,
	maskUserGuildsThreadBits,
	resolveProtectedBitActor,
	shouldMaskThreadPermissionBits,
} from '@app/api/guild/services/ThreadPermissionBits';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {
	applyProtectedOverwriteBits,
	applyProtectedRolePermissions,
	type ProtectedBitActor,
	permissionWriteMask,
	protectedThreadBits,
} from '@app/api/utils/featureUtils';
import {computePermissionsDiff} from '@app/api/utils/PermissionUtils';
import {seedThreadOverwriteBits, seedThreadOverwriteValue} from '@app/api/worker/tasks/SeedThreadPermissions';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildMFALevel} from '@fluxer/constants/src/GuildConstants';
import {
	DEFAULT_THREAD_PERMISSIONS,
	THREAD_AWARE_ALL_PERMISSIONS,
	THREAD_PERMISSIONS,
	ThreadPermissionFlags,
} from '@fluxer/constants/src/ThreadPermissionUtils';
import {MfaNotEnabledError} from '@fluxer/errors/src/domains/auth/MfaNotEnabledError';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {describe, expect, it} from 'vitest';

const NONE: ReadonlySet<string> = new Set();
const CAPABLE: ReadonlySet<string> = new Set(['channel_threads']);
const MANAGE_THREADS = ThreadPermissionFlags.MANAGE_THREADS;
const VCM = Permissions.VIEW_CHANNEL_MEMBERS;

function actor(opts: {bot?: boolean; features?: ReadonlySet<string>} = {}): ProtectedBitActor {
	return {
		clientFeatures: opts.features ?? NONE,
		threadBits: {mode: 'active', writer: opts.bot === true || (opts.features ?? NONE).has('channel_threads')},
	};
}

describe('thread bits in the protected permission helper', () => {
	it('lets bots and capable active users write, and restores for everyone else', () => {
		const writers = [actor({bot: true}), actor({features: CAPABLE})];
		for (const writer of writers) {
			expect(permissionWriteMask(writer)).toBe(THREAD_AWARE_ALL_PERMISSIONS);
			expect(protectedThreadBits(writer)).toBe(0n);
			expect(applyProtectedRolePermissions(DEFAULT_THREAD_PERMISSIONS, MANAGE_THREADS, writer)).toBe(
				DEFAULT_THREAD_PERMISSIONS,
			);
		}
		const readers = [actor({})];
		for (const reader of readers) {
			expect(protectedThreadBits(reader)).toBe(THREAD_PERMISSIONS);
			expect(applyProtectedRolePermissions(DEFAULT_THREAD_PERMISSIONS, MANAGE_THREADS, reader)).toBe(MANAGE_THREADS);
			expect(
				applyProtectedOverwriteBits(
					{allow: DEFAULT_THREAD_PERMISSIONS, deny: 0n},
					{allow: 0n, deny: MANAGE_THREADS},
					reader,
				),
			).toEqual({allow: 0n, deny: MANAGE_THREADS});
		}
	});

	it('keeps VIEW_CHANNEL_MEMBERS header-gated for bots', () => {
		expect(applyProtectedRolePermissions(VCM | MANAGE_THREADS, 0n, actor({bot: true}))).toBe(MANAGE_THREADS);
	});
});

describe('thread bit writers', () => {
	it('allows bots and capable viewers while refusing incapable viewers', async () => {
		const guildId = createGuildID(10n);
		const writer = async (userId: bigint, bot: boolean, capable: boolean) =>
			(
				await resolveProtectedBitActor({
					guildId,
					userId: createUserID(userId),
					clientFeatures: CAPABLE,
					viewer: {kind: 'user', userId: createUserID(userId), bot, capable},
					isBot: async () => bot,
				})
			).threadBits?.writer;
		expect(await writer(2n, false, true)).toBe(true);
		expect(await writer(2n, false, false)).toBe(false);
		expect(await writer(2n, true, true)).toBe(true);
	});

	it('taints a guild on its first protected-bit write', async () => {
		clearChannelThreadsTaintCacheForTesting();
		const guildId = createGuildID(11n);

		expect(await isTainted(guildId)).toBe(false);
		await resolveProtectedBitActor({
			guildId,
			userId: createUserID(2n),
			clientFeatures: NONE,
			isBot: async () => true,
		});
		clearChannelThreadsTaintCacheForTesting();
		expect(await isTainted(guildId)).toBe(true);
	});

	it('masks thread bits for non-viewers of an active guild while a clean taint read is still cached', async () => {
		clearChannelThreadsTaintCacheForTesting();
		const guildId = createGuildID(13n);

		expect(await isTainted(guildId)).toBe(false);
		const bits = [Permissions.SEND_MESSAGES | DEFAULT_THREAD_PERMISSIONS];
		const control = {kind: 'user', userId: createUserID(3n), bot: false, capable: false} as const;
		const member = {kind: 'user', userId: createUserID(2n), bot: false, capable: true} as const;
		expect(await shouldMaskThreadPermissionBits(guildId, control, bits)).toBe(true);
		expect(await shouldMaskThreadPermissionBits(guildId, member, bits)).toBe(false);
		expect(await shouldMaskThreadPermissionBits(createGuildID(14n), control, bits)).toBe(true);
		const guild = {id: '13', roles: [{id: '13', permissions: bits[0]!.toString()}]} as unknown as GuildResponse;
		expect((await maskGuildResponseThreadBits(guildId, control, guild)).roles?.[0]?.permissions).toBe(
			Permissions.SEND_MESSAGES.toString(),
		);
		expect(await maskGuildResponseThreadBits(guildId, member, guild)).toBe(guild);
	});

	it('masks the computed guild list permissions for non-viewers only', async () => {
		clearChannelThreadsTaintCacheForTesting();

		const bits = (Permissions.SEND_MESSAGES | DEFAULT_THREAD_PERMISSIONS).toString();
		const guilds = [
			{id: '15', permissions: bits},
			{id: '16', permissions: bits},
			{id: '17'},
		] as unknown as Array<GuildResponse>;
		const control = {kind: 'user', userId: createUserID(3n), bot: false, capable: false} as const;
		const member = {kind: 'user', userId: createUserID(2n), bot: false, capable: true} as const;
		expect((await maskUserGuildsThreadBits(control, guilds)).map((guild) => guild.permissions)).toEqual([
			Permissions.SEND_MESSAGES.toString(),
			Permissions.SEND_MESSAGES.toString(),
			undefined,
		]);
		expect(await maskUserGuildsThreadBits(member, guilds)).toEqual(guilds);
	});
});

describe('thread permission audit naming', () => {
	it('names thread bits only outside control guilds', () => {
		const before = Permissions.SEND_MESSAGES;
		const after = Permissions.SEND_MESSAGES | MANAGE_THREADS;
		expect(computePermissionsDiff(before, after)).toEqual({added: [], removed: []});
		expect(computePermissionsDiff(before, after, {threads: true})).toEqual({added: ['MANAGE_THREADS'], removed: []});
	});
});

describe('guild MFA enforcement', () => {
	it('treats MANAGE_THREADS as elevated', async () => {
		const userRepository = {
			findUnique: async () => ({authenticatorTypes: new Set()}),
		} as unknown as UserRepository;
		const enforce = await createGuildMfaEnforcer({
			userRepository,
			guildData: {mfa_level: GuildMFALevel.ELEVATED, owner_id: '1'},
			userId: createUserID(2n),
		});
		expect(() => enforce(MANAGE_THREADS)).toThrow(MfaNotEnabledError);
		expect(() => enforce(ThreadPermissionFlags.CREATE_PUBLIC_THREADS)).not.toThrow();
	});
});

describe('thread permission seeding rules', () => {
	it('clears raw thread bits on overwrites without mirroring SEND_MESSAGES', () => {
		expect(seedThreadOverwriteBits(MANAGE_THREADS | Permissions.VIEW_CHANNEL)).toBe(Permissions.VIEW_CHANNEL);
		expect(seedThreadOverwriteBits(Permissions.SEND_MESSAGES | MANAGE_THREADS)).toBe(Permissions.SEND_MESSAGES);
		expect(seedThreadOverwriteBits(Permissions.SEND_MESSAGES) & DEFAULT_THREAD_PERMISSIONS).toBe(0n);
		expect(seedThreadOverwriteBits(seedThreadOverwriteBits(Permissions.SEND_MESSAGES))).toBe(Permissions.SEND_MESSAGES);
	});

	it('mirrors SEND_MESSAGES into the thread bits on either overwrite side', () => {
		expect(seedThreadOverwriteValue(Permissions.SEND_MESSAGES | MANAGE_THREADS)).toBe(
			Permissions.SEND_MESSAGES | DEFAULT_THREAD_PERMISSIONS,
		);
		expect(seedThreadOverwriteValue(MANAGE_THREADS | Permissions.VIEW_CHANNEL)).toBe(Permissions.VIEW_CHANNEL);
		expect(seedThreadOverwriteValue(seedThreadOverwriteValue(Permissions.SEND_MESSAGES))).toBe(
			Permissions.SEND_MESSAGES | DEFAULT_THREAD_PERMISSIONS,
		);
	});
});
