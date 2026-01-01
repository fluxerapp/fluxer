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

import type {EmojiID, GuildID, RoleID, StickerID, UserID} from '~/BrandedTypes';
import type {AuditLogActionType} from '~/constants/AuditLogActionType';
import type {
	GuildAuditLogRow,
	GuildBanRow,
	GuildEmojiRow,
	GuildMemberRow,
	GuildRoleRow,
	GuildRow,
	GuildStickerRow,
} from '~/database/CassandraTypes';
import type {Guild, GuildAuditLog, GuildBan, GuildEmoji, GuildMember, GuildRole, GuildSticker} from '~/Models';
import {GuildContentRepository} from './GuildContentRepository';
import {GuildDataRepository} from './GuildDataRepository';
import {GuildMemberRepository} from './GuildMemberRepository';
import {GuildModerationRepository} from './GuildModerationRepository';
import {GuildRoleRepository} from './GuildRoleRepository';
import type {IGuildRepositoryAggregate} from './IGuildRepositoryAggregate';

export class GuildRepository implements IGuildRepositoryAggregate {
	private dataRepo: GuildDataRepository;
	private memberRepo: GuildMemberRepository;
	private roleRepo: GuildRoleRepository;
	private moderationRepo: GuildModerationRepository;
	private contentRepo: GuildContentRepository;

	constructor() {
		this.dataRepo = new GuildDataRepository();
		this.memberRepo = new GuildMemberRepository();
		this.roleRepo = new GuildRoleRepository();
		this.moderationRepo = new GuildModerationRepository();
		this.contentRepo = new GuildContentRepository();
	}

	async findUnique(guildId: GuildID): Promise<Guild | null> {
		return await this.dataRepo.findUnique(guildId);
	}

	async listGuilds(guildIds: Array<GuildID>): Promise<Array<Guild>> {
		return await this.dataRepo.listGuilds(guildIds);
	}

	async listAllGuildsPaginated(limit: number, lastGuildId?: GuildID): Promise<Array<Guild>> {
		return await this.dataRepo.listAllGuildsPaginated(limit, lastGuildId);
	}

	async listUserGuilds(userId: UserID): Promise<Array<Guild>> {
		return await this.dataRepo.listUserGuilds(userId);
	}

	async countUserGuilds(userId: UserID): Promise<number> {
		return await this.dataRepo.countUserGuilds(userId);
	}

	async listOwnedGuildIds(userId: UserID): Promise<Array<GuildID>> {
		return await this.dataRepo.listOwnedGuildIds(userId);
	}

	async upsert(data: GuildRow): Promise<Guild> {
		return await this.dataRepo.upsert(data);
	}

	async delete(guildId: GuildID): Promise<void> {
		const guild = await this.findUnique(guildId);
		if (!guild) {
			return;
		}

		const [members, roles, emojis] = await Promise.all([
			this.memberRepo.listMembers(guildId),
			this.roleRepo.listRoles(guildId),
			this.contentRepo.listEmojis(guildId),
		]);

		const BATCH_SIZE = 50;
		for (let i = 0; i < members.length; i += BATCH_SIZE) {
			const memberBatch = members.slice(i, i + BATCH_SIZE);
			await Promise.all(memberBatch.map((member) => this.memberRepo.deleteMember(guildId, member.userId)));
		}

		for (let i = 0; i < roles.length; i += BATCH_SIZE) {
			const roleBatch = roles.slice(i, i + BATCH_SIZE);
			await Promise.all(roleBatch.map((role) => this.roleRepo.deleteRole(guildId, role.id)));
		}

		for (let i = 0; i < emojis.length; i += BATCH_SIZE) {
			const emojiBatch = emojis.slice(i, i + BATCH_SIZE);
			await Promise.all(emojiBatch.map((emoji) => this.contentRepo.deleteEmoji(guildId, emoji.id)));
		}

		await this.dataRepo.delete(guildId);
	}

	async getMember(guildId: GuildID, userId: UserID): Promise<GuildMember | null> {
		return await this.memberRepo.getMember(guildId, userId);
	}

	async listMembers(guildId: GuildID): Promise<Array<GuildMember>> {
		return await this.memberRepo.listMembers(guildId);
	}

	async upsertMember(data: GuildMemberRow): Promise<GuildMember> {
		return await this.memberRepo.upsertMember(data);
	}

	async deleteMember(guildId: GuildID, userId: UserID): Promise<void> {
		return await this.memberRepo.deleteMember(guildId, userId);
	}

	async getRole(roleId: RoleID, guildId: GuildID): Promise<GuildRole | null> {
		return await this.roleRepo.getRole(roleId, guildId);
	}

	async listRoles(guildId: GuildID): Promise<Array<GuildRole>> {
		return await this.roleRepo.listRoles(guildId);
	}

	async listRolesByIds(roleIds: Array<RoleID>, guildId: GuildID): Promise<Array<GuildRole>> {
		return await this.roleRepo.listRolesByIds(roleIds, guildId);
	}

	async countRoles(guildId: GuildID): Promise<number> {
		return await this.roleRepo.countRoles(guildId);
	}

	async upsertRole(data: GuildRoleRow): Promise<GuildRole> {
		return await this.roleRepo.upsertRole(data);
	}

	async deleteRole(guildId: GuildID, roleId: RoleID): Promise<void> {
		return await this.roleRepo.deleteRole(guildId, roleId);
	}

	async getBan(guildId: GuildID, userId: UserID): Promise<GuildBan | null> {
		return await this.moderationRepo.getBan(guildId, userId);
	}

	async listBans(guildId: GuildID): Promise<Array<GuildBan>> {
		return await this.moderationRepo.listBans(guildId);
	}

	async upsertBan(data: GuildBanRow): Promise<GuildBan> {
		return await this.moderationRepo.upsertBan(data);
	}

	async deleteBan(guildId: GuildID, userId: UserID): Promise<void> {
		return await this.moderationRepo.deleteBan(guildId, userId);
	}

	async createAuditLog(data: GuildAuditLogRow): Promise<GuildAuditLog> {
		return await this.moderationRepo.createAuditLog(data);
	}

	async getAuditLog(guildId: GuildID, logId: bigint): Promise<GuildAuditLog | null> {
		return await this.moderationRepo.getAuditLog(guildId, logId);
	}

	async listAuditLogs(params: {
		guildId: GuildID;
		limit: number;
		afterLogId?: bigint;
		beforeLogId?: bigint;
		userId?: UserID;
		actionType?: AuditLogActionType;
	}): Promise<Array<GuildAuditLog>> {
		return await this.moderationRepo.listAuditLogs(params);
	}

	async listAuditLogsByIds(guildId: GuildID, logIds: Array<bigint>): Promise<Array<GuildAuditLog>> {
		return await this.moderationRepo.listAuditLogsByIds(guildId, logIds);
	}

	async deleteAuditLogs(guildId: GuildID, logs: Array<GuildAuditLog>): Promise<void> {
		return await this.moderationRepo.deleteAuditLogs(guildId, logs);
	}

	async updateAuditLogsIndexedAt(guildId: GuildID, indexedAt: Date | null): Promise<void> {
		return await this.moderationRepo.updateAuditLogsIndexedAt(guildId, indexedAt);
	}

	async getEmoji(emojiId: EmojiID, guildId: GuildID): Promise<GuildEmoji | null> {
		return await this.contentRepo.getEmoji(emojiId, guildId);
	}

	async getEmojiById(emojiId: EmojiID): Promise<GuildEmoji | null> {
		return await this.contentRepo.getEmojiById(emojiId);
	}

	async listEmojis(guildId: GuildID): Promise<Array<GuildEmoji>> {
		return await this.contentRepo.listEmojis(guildId);
	}

	async countEmojis(guildId: GuildID): Promise<number> {
		return await this.contentRepo.countEmojis(guildId);
	}

	async upsertEmoji(data: GuildEmojiRow): Promise<GuildEmoji> {
		return await this.contentRepo.upsertEmoji(data);
	}

	async deleteEmoji(guildId: GuildID, emojiId: EmojiID): Promise<void> {
		return await this.contentRepo.deleteEmoji(guildId, emojiId);
	}

	async getSticker(stickerId: StickerID, guildId: GuildID): Promise<GuildSticker | null> {
		return await this.contentRepo.getSticker(stickerId, guildId);
	}

	async getStickerById(stickerId: StickerID): Promise<GuildSticker | null> {
		return await this.contentRepo.getStickerById(stickerId);
	}

	async listStickers(guildId: GuildID): Promise<Array<GuildSticker>> {
		return await this.contentRepo.listStickers(guildId);
	}

	async countStickers(guildId: GuildID): Promise<number> {
		return await this.contentRepo.countStickers(guildId);
	}

	async upsertSticker(data: GuildStickerRow): Promise<GuildSticker> {
		return await this.contentRepo.upsertSticker(data);
	}

	async deleteSticker(guildId: GuildID, stickerId: StickerID): Promise<void> {
		return await this.contentRepo.deleteSticker(guildId, stickerId);
	}
}
