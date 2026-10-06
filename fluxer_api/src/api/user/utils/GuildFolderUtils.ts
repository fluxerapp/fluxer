// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, UserID} from '@app/api/BrandedTypes';
import type {GuildFolder} from '@app/api/database/types/UserTypes';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import {mapUserSettingsToResponse} from '@app/api/user/UserMappers';
import {type GuildFolderIcon, UNCATEGORIZED_FOLDER_ID} from '@fluxer/constants/src/UserConstants';

export function dedupeGuildFolders(folders: ReadonlyArray<GuildFolder>): {
	folders: Array<GuildFolder>;
	removed: number;
} {
	if (folders.length === 0) return {folders: [], removed: 0};
	const occurrences = new Map<
		GuildID,
		Array<{
			fi: number;
			pi: number;
		}>
	>();
	folders.forEach((folder, fi) => {
		(folder.guild_ids ?? []).forEach((gid, pi) => {
			let arr = occurrences.get(gid);
			if (!arr) {
				arr = [];
				occurrences.set(gid, arr);
			}
			arr.push({fi, pi});
		});
	});
	const winner = new Map<
		GuildID,
		{
			fi: number;
			pi: number;
		}
	>();
	for (const [gid, occs] of occurrences) {
		if (occs.length === 1) {
			winner.set(gid, occs[0]);
			continue;
		}
		let best = occs[0];
		let bestRank = rankOccurrence(folders, best);
		for (let i = 1; i < occs.length; i++) {
			const cand = occs[i];
			const r = rankOccurrence(folders, cand);
			if (r > bestRank) {
				best = cand;
				bestRank = r;
			}
		}
		winner.set(gid, best);
	}
	let removed = 0;
	const out = folders.map((folder, fi) => {
		const gids = folder.guild_ids ?? [];
		const kept: Array<GuildID> = [];
		gids.forEach((gid, pi) => {
			const w = winner.get(gid);
			if (w && w.fi === fi && w.pi === pi) {
				kept.push(gid);
			} else {
				removed += 1;
			}
		});
		if (kept.length === gids.length) return folder;
		return {...folder, guild_ids: kept.length > 0 ? kept : null};
	});
	return {folders: out, removed};
}

function rankOccurrence(
	folders: ReadonlyArray<GuildFolder>,
	o: {
		fi: number;
		pi: number;
	},
): number {
	const f = folders[o.fi];
	const named = f.name && f.name.trim().length > 0 ? 1 : 0;
	const nonUncat = f.folder_id !== UNCATEGORIZED_FOLDER_ID ? 1 : 0;
	return named * 4 + nonUncat * 2;
}

export async function removeGuildFromUserFolders(params: {
	userId: UserID;
	guildId: GuildID;
	userRepository: IUserRepository;
	gatewayService: IGatewayService;
}): Promise<void> {
	const {userId, guildId, userRepository, gatewayService} = params;
	const userSettings = await userRepository.findSettings(userId);
	if (!userSettings) return;
	const settingsRow = userSettings.toRow();
	const existingFolders = settingsRow.guild_folders ?? [];
	let modified = false;
	const updatedFolders = existingFolders
		.map((folder) => {
			const currentGuildIds = folder.guild_ids ?? [];
			const filteredGuildIds = currentGuildIds.filter((id) => id !== guildId);
			if (filteredGuildIds.length !== currentGuildIds.length) {
				modified = true;
			}
			return {
				...folder,
				guild_ids: filteredGuildIds.length > 0 ? filteredGuildIds : null,
			};
		})
		.filter((folder) => {
			const guildIds = folder.guild_ids ?? [];
			return folder.folder_id === UNCATEGORIZED_FOLDER_ID || guildIds.length > 0;
		});
	if (modified) {
		settingsRow.guild_folders = updatedFolders;
		const updatedSettings = await userRepository.upsertSettings(settingsRow);
		await gatewayService.dispatchPresence({
			userId,
			event: 'USER_SETTINGS_UPDATE',
			data: mapUserSettingsToResponse({settings: updatedSettings}),
		});
	}
}

export function addGuildToUncategorizedFolder(params: {
	folders: ReadonlyArray<GuildFolder>;
	guildId: GuildID;
	defaultIcon: GuildFolderIcon;
}): {
	folders: Array<GuildFolder>;
	modified: boolean;
} {
	const {folders, guildId, defaultIcon} = params;
	const alreadyPresent = folders.some((f) => (f.guild_ids ?? []).includes(guildId));
	const next: Array<GuildFolder> = folders.map((f) => ({...f}));
	if (!alreadyPresent) {
		const uncategorizedIndex = next.findIndex((f) => f.folder_id === UNCATEGORIZED_FOLDER_ID);
		if (uncategorizedIndex !== -1) {
			const cur = next[uncategorizedIndex];
			next[uncategorizedIndex] = {
				...cur,
				guild_ids: [guildId, ...(cur.guild_ids ?? []).filter((id) => id !== guildId)],
			};
		} else {
			next.push({
				folder_id: UNCATEGORIZED_FOLDER_ID,
				name: null,
				color: null,
				flags: 0,
				icon: defaultIcon,
				guild_ids: [guildId],
			});
		}
	}
	const {folders: deduped, removed} = dedupeGuildFolders(next);
	const modified = !alreadyPresent || removed > 0;
	return {folders: deduped, modified};
}
