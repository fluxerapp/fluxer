// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	buildForwardSearchWeights,
	type ForwardChannelCandidate,
	type ForwardFrequentItem,
	type ForwardGroupDMCandidate,
	type ForwardGuildCandidate,
	type ForwardSearchWeights,
	type ForwardUserCandidate,
} from '@app/features/app/components/dialogs/shared/ForwardDestinationSearch';
import ChannelFrecency from '@app/features/channel/state/ChannelFrecency';
import Channels from '@app/features/channel/state/Channels';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import Guilds from '@app/features/guild/state/Guilds';
import {PERSONAL_NOTES_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import GuildMembers from '@app/features/member/state/GuildMembers';
import Permission from '@app/features/permissions/state/Permission';
import Relationships from '@app/features/relationship/state/Relationships';
import Users from '@app/features/user/state/Users';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import type {I18n} from '@lingui/core';
import {compareStructural, computed, type IComputedValue} from 'mobx';

const NO_STRINGS: ReadonlyArray<string> = Object.freeze([]);

export interface ForwardSearchCandidates {
	readonly weights: ForwardSearchWeights;
	readonly channels: ReadonlyArray<ForwardChannelCandidate>;
	readonly groupDMs: ReadonlyArray<ForwardGroupDMCandidate>;
	readonly guilds: ReadonlyArray<ForwardGuildCandidate>;
	readonly users: ReadonlyArray<ForwardUserCandidate>;
}

function collectGuildNicknames(): ReadonlyMap<string, Array<string>> {
	const nicknames = new Map<string, Array<string>>();
	for (const guild of Guilds.getGuilds()) {
		for (const member of GuildMembers.getMembers(guild.id)) {
			if (member.nick == null) continue;
			const userNicknames = nicknames.get(member.user.id);
			if (userNicknames === undefined) {
				nicknames.set(member.user.id, [member.nick]);
			} else {
				userNicknames.push(member.nick);
			}
		}
	}
	return nicknames;
}

function buildForwardUserCandidates(): ReadonlyArray<ForwardUserCandidate> {
	const nicknames = collectGuildNicknames();
	const candidates: Array<ForwardUserCandidate> = [];
	for (const user of Users.getUsers()) {
		const relationship = Relationships.getRelationship(user.id);
		if (relationship?.type === RelationshipTypes.BLOCKED) continue;
		let friendAlias: string | null = null;
		if (relationship?.type === RelationshipTypes.FRIEND) friendAlias = relationship.nickname;
		candidates.push(
			Object.freeze({
				friendAlias,
				globalName: user.globalName,
				id: user.id,
				nicknames: nicknames.get(user.id) ?? NO_STRINGS,
				username: user.discriminator === '0' ? user.username : `${user.username}#${user.discriminator}`,
			}),
		);
	}
	return Object.freeze(candidates);
}

function collectRecipientSearchFields(recipientIds: ReadonlyArray<string>): ReadonlyArray<string> {
	const fields: Array<string> = [];
	for (const recipientId of recipientIds) {
		const recipient = Users.getUser(recipientId);
		if (recipient == null) continue;
		fields.push(recipient.username);
		if (recipient.globalName != null) fields.push(recipient.globalName);
		const relationshipNickname = Relationships.getRelationship(recipientId)?.nickname;
		if (relationshipNickname != null) fields.push(relationshipNickname);
	}
	return Object.freeze(fields);
}

function buildForwardGroupDMCandidates(i18n: I18n): ReadonlyArray<ForwardGroupDMCandidate> {
	const candidates: Array<ForwardGroupDMCandidate> = [];
	for (const channel of Channels.getPrivateChannels()) {
		if (channel.type !== ChannelTypes.GROUP_DM) continue;
		candidates.push(
			Object.freeze({
				id: channel.id,
				memberFields: collectRecipientSearchFields(channel.recipientIds),
				name: ChannelUtils.getDMDisplayName(channel),
			}),
		);
	}
	const currentUserId = Users.currentUserId;
	const personalNotes = currentUserId == null ? undefined : Channels.getChannel(currentUserId);
	if (personalNotes?.type === ChannelTypes.DM_PERSONAL_NOTES) {
		candidates.push(
			Object.freeze({id: personalNotes.id, memberFields: NO_STRINGS, name: i18n._(PERSONAL_NOTES_DESCRIPTOR)}),
		);
	}
	return Object.freeze(candidates);
}

function buildForwardChannelCandidates(): ReadonlyArray<ForwardChannelCandidate> {
	const candidates: Array<ForwardChannelCandidate> = [];
	for (const channel of Channels.allChannels) {
		if (channel.type !== ChannelTypes.GUILD_TEXT && channel.type !== ChannelTypes.GUILD_VOICE) continue;
		const isVoice = channel.type === ChannelTypes.GUILD_VOICE;
		const requiredPermissions = isVoice ? Permissions.VIEW_CHANNEL | Permissions.CONNECT : Permissions.VIEW_CHANNEL;
		candidates.push(
			Object.freeze({
				canAccess: Permission.can(requiredPermissions, channel),
				guildName: channel.guildId == null ? null : (Guilds.getGuild(channel.guildId)?.name ?? null),
				hasFrecency: ChannelFrecency.scoreFor(channel.id) > 0,
				id: channel.id,
				kind: isVoice ? 'voice' : 'text',
				name: channel.name ?? '',
				parentName: channel.parentId == null ? null : (Channels.getChannel(channel.parentId)?.name ?? null),
			}),
		);
	}
	return Object.freeze(candidates);
}

function resolveForwardFrequentItem(id: string): ForwardFrequentItem {
	const score = ChannelFrecency.scoreFor(id);
	if (Guilds.getGuild(id) != null) return {id, kind: 'guild', score};
	const channel = Channels.getChannel(id);
	switch (channel?.type) {
		case ChannelTypes.DM:
			return {id, kind: 'dm', recipientId: channel.recipientIds.length > 0 ? channel.recipientIds[0] : null, score};
		case ChannelTypes.GROUP_DM:
		case ChannelTypes.DM_PERSONAL_NOTES:
			return {id, kind: 'group_dm', score};
		case ChannelTypes.GUILD_TEXT:
			return {id, kind: 'text', score};
		case ChannelTypes.GUILD_VOICE:
			return {id, kind: 'voice', score};
		default:
			return {id, kind: 'other', score};
	}
}

function buildForwardSearchWeightsFromStores(): ForwardSearchWeights {
	const friendIds: Array<string> = [];
	for (const relationship of Relationships.getRelationships()) {
		if (relationship.type === RelationshipTypes.FRIEND) friendIds.push(relationship.userId);
	}
	const dmUserIds: Array<string> = [];
	for (const channel of Channels.getPrivateChannels()) {
		if (channel.type === ChannelTypes.DM && channel.recipientIds.length > 0) dmUserIds.push(channel.recipientIds[0]);
	}
	return buildForwardSearchWeights({
		dmUserIds,
		frequent: ChannelFrecency.frequentIds.map(resolveForwardFrequentItem),
		friendIds,
	});
}

function buildForwardGuildCandidates(): ReadonlyArray<ForwardGuildCandidate> {
	return Object.freeze(Guilds.getGuilds().map((guild) => Object.freeze({id: guild.id, name: guild.name})));
}

export function createForwardSearchCandidates(i18n: I18n): IComputedValue<ForwardSearchCandidates> {
	const options = {equals: compareStructural};
	const users = computed(buildForwardUserCandidates, options);
	const groupDMs = computed(() => buildForwardGroupDMCandidates(i18n), options);
	const channels = computed(buildForwardChannelCandidates, options);
	const guilds = computed(buildForwardGuildCandidates, options);
	const weights = computed(buildForwardSearchWeightsFromStores, options);
	return computed(() =>
		Object.freeze({
			weights: weights.get(),
			channels: channels.get(),
			groupDMs: groupDMs.get(),
			guilds: guilds.get(),
			users: users.get(),
		}),
	);
}
