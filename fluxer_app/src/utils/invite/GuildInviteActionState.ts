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

import {GuildFeatures} from '~/Constants';
import type {Guild, GuildRecord} from '~/records/GuildRecord';
import type {Invite} from '~/records/MessageRecord';
import AuthenticationStore from '~/stores/AuthenticationStore';
import GuildMemberStore from '~/stores/GuildMemberStore';
import PresenceStore from '~/stores/PresenceStore';
import {isGuildInvite} from '~/types/InviteTypes';

const normalizeFeatures = (features?: Iterable<string> | null): Array<string> => {
	if (!features) return [];
	if (Array.isArray(features)) return features;
	return Array.from(features);
};

export interface GuildInviteActionState {
	guildId: string | null;
	isMember: boolean;
	presenceCount: number;
	memberCount: number;
	isInvitesDisabled: boolean;
	features: Array<string>;
}

export enum GuildInvitePrimaryAction {
	JoinCommunity = 'join',
	GoToCommunity = 'go',
	InvitesDisabled = 'disabled',
}

export const getGuildInviteActionState = (params: {
	invite?: Invite | null;
	guild?: GuildRecord | Guild | null;
}): GuildInviteActionState => {
	const inviteGuild = params.invite && isGuildInvite(params.invite) ? params.invite.guild : null;
	const guildRecord = params.guild ?? inviteGuild ?? null;
	const guildId = guildRecord?.id ?? null;
	const currentUserId = AuthenticationStore.currentUserId;
	const isMember = Boolean(guildId && currentUserId && GuildMemberStore.getMember(guildId, currentUserId));
	const presenceCount =
		guildId && isMember
			? PresenceStore.getPresenceCount(guildId)
			: params.invite && isGuildInvite(params.invite)
				? params.invite.presence_count
				: 0;
	const memberCount =
		guildId && isMember
			? GuildMemberStore.getMemberCount(guildId)
			: params.invite && isGuildInvite(params.invite)
				? params.invite.member_count
				: 0;
	const features = normalizeFeatures(guildRecord?.features ?? inviteGuild?.features);

	return {
		guildId,
		isMember,
		presenceCount,
		memberCount,
		features,
		isInvitesDisabled: features.includes(GuildFeatures.INVITES_DISABLED),
	};
};

export const getGuildInvitePrimaryAction = (state: GuildInviteActionState): GuildInvitePrimaryAction => {
	if (state.isInvitesDisabled && !state.isMember) {
		return GuildInvitePrimaryAction.InvitesDisabled;
	}
	if (state.isMember) {
		return GuildInvitePrimaryAction.GoToCommunity;
	}
	return GuildInvitePrimaryAction.JoinCommunity;
};

export const isGuildInviteActionDisabled = (state: GuildInviteActionState): boolean =>
	state.isInvitesDisabled && !state.isMember;
