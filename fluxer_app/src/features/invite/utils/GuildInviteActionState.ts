// SPDX-License-Identifier: AGPL-3.0-or-later

import Authentication from '@app/features/auth/state/Authentication';
import type {Guild} from '@app/features/guild/models/Guild';
import Guilds from '@app/features/guild/state/Guilds';
import {isGuildInvite} from '@app/features/invite/types/InviteTypes';
import GuildMembers from '@app/features/member/state/GuildMembers';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import type {Guild as WireGuild} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {Invite} from '@fluxer/schema/src/domains/invite/InviteSchemas';

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
	isRaidDetected: boolean;
	visitsEnabled: boolean;
	features: Array<string>;
}

export enum GuildInvitePrimaryAction {
	JoinCommunity = 'join',
	OpenVisit = 'visit',
	GoToCommunity = 'go',
	InvitesDisabled = 'disabled',
}

export function getGuildInviteActionState(params: {
	invite?: Invite | null;
	guild?: Guild | WireGuild | null;
}): GuildInviteActionState {
	const inviteGuild = params['invite'] && isGuildInvite(params['invite']) ? params['invite'].guild : null;
	const guildRecord = params['guild'] ?? inviteGuild ?? null;
	const guildId = guildRecord?.id ?? null;
	const currentUserId = Authentication.currentUserId;
	const isMember = Boolean(guildId && currentUserId && GuildMembers.getMember(guildId, currentUserId));
	const presenceCount =
		params['invite'] && isGuildInvite(params['invite']) ? (params['invite'].presence_count ?? 0) : 0;
	const memberCount = params['invite'] && isGuildInvite(params['invite']) ? (params['invite'].member_count ?? 0) : 0;
	const features = normalizeFeatures(guildRecord?.features ?? inviteGuild?.features);
	return {
		guildId,
		isMember,
		presenceCount,
		memberCount,
		features,
		isInvitesDisabled: features.includes(GuildFeatures.INVITES_DISABLED),
		isRaidDetected: features.includes(GuildFeatures.RAID_DETECTED),
		visitsEnabled:
			inviteGuild?.visits_enabled === true || (guildId !== null && Guilds.getGuild(guildId)?.visiting === true),
	};
}

export function getGuildInvitePrimaryAction(state: GuildInviteActionState): GuildInvitePrimaryAction {
	if (state.isInvitesDisabled && !state.isMember) {
		return GuildInvitePrimaryAction.InvitesDisabled;
	}
	if (state.isMember) {
		return GuildInvitePrimaryAction.GoToCommunity;
	}
	if (state.visitsEnabled) {
		return GuildInvitePrimaryAction.OpenVisit;
	}
	return GuildInvitePrimaryAction.JoinCommunity;
}

export function isGuildInviteActionDisabled(state: GuildInviteActionState): boolean {
	return state.isInvitesDisabled && !state.isMember;
}
