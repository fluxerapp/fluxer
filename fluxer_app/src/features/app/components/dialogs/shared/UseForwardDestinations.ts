// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	buildForwardDefaultDestinations,
	type ForwardDestination,
	type ForwardRowIdentity,
	filterForwardSearchRows,
	resolveForwardOrigin,
} from '@app/features/app/components/dialogs/shared/ForwardDefaultDestinations';
import {parseForwardDestinationQuery} from '@app/features/app/components/dialogs/shared/ForwardDestinationQuery';
import {
	buildForwardSearchBoosters,
	type ForwardChannelCandidate,
	type ForwardFrequentItem,
	type ForwardGroupDMCandidate,
	type ForwardSearchBoosters,
	type ForwardSearchResult,
	type ForwardUserCandidate,
	searchForwardDestinations,
} from '@app/features/app/components/dialogs/shared/ForwardDestinationSearch';
import {
	type ForwardPinnedDestinations,
	forwardDestinationKey,
	pinForwardDestinations,
	toggleForwardDestination,
} from '@app/features/app/components/dialogs/shared/ForwardDestinationSelection';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {formatSlowmodeTime} from '@app/features/channel/components/SlowmodeIndicator';
import type {Channel} from '@app/features/channel/models/Channel';
import ChannelFrecency from '@app/features/channel/state/ChannelFrecency';
import Channels from '@app/features/channel/state/Channels';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import type {Guild} from '@app/features/guild/models/Guild';
import Guilds from '@app/features/guild/state/Guilds';
import {PERSONAL_NOTES_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import GuildMembers from '@app/features/member/state/GuildMembers';
import type {ForwardMediaSelection} from '@app/features/messaging/commands/MessageCommands';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import Permission from '@app/features/permissions/state/Permission';
import {formatPermissionLabel} from '@app/features/permissions/utils/PermissionUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import Relationships from '@app/features/relationship/state/Relationships';
import {getLoadedUnicodeConfusables, loadUnicodeConfusables} from '@app/features/search/utils/SearchTextMatching';
import Slowmode from '@app/features/slowmode/state/Slowmode';
import {useNow} from '@app/features/ui/state/Tick';
import type {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildNSFWLevel, GuildOperations} from '@fluxer/constants/src/GuildConstants';
import {CHANNEL_RATE_LIMIT_PER_USER_MAX} from '@fluxer/constants/src/LimitConstants';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import type {MessageEmbed} from '@fluxer/schema/src/domains/message/EmbedSchemas';
import type {MessageAttachment} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {comparer, computed, type IComputedValue} from 'mobx';
import {useEffect, useMemo, useState} from 'react';

const GUILD_MESSAGES_DISABLED_DESCRIPTOR = msg({
	message: 'Sending messages is disabled in this community',
	comment: 'Short label under a destination row in the forward modal, shown when the community has messaging disabled.',
});
const EMBED_LINKS_PERMISSION_REQUIRED_DESCRIPTOR = msg({
	message: 'You need the "{embedLinksPermissionLabel}" permission to embed links in this channel',
	comment:
		'Forward dialog error shown when the forwarded message contains embeds and the user lacks Embed Links in the target channel.',
});
const ATTACH_FILES_PERMISSION_REQUIRED_DESCRIPTOR = msg({
	message: 'You need the "{attachFilesPermissionLabel}" permission to attach files in this channel',
	comment:
		'Forward dialog error shown when the forwarded message has attachments and the user lacks Attach Files in the target channel.',
});
const AGE_RESTRICTED_DESTINATION_DESCRIPTOR = msg({
	message: 'This message can only be forwarded to age-restricted channels',
	comment:
		'Forward dialog error shown when the forwarded message contains age-restricted media and the target channel is not age-restricted.',
});
const SLOWMODE_WAIT_DESCRIPTOR = msg({
	message: 'Slowmode · wait {remaining}',
	comment:
		'Short label under a destination row in the forward modal while slowmode blocks sending. Preserve {remaining}; it is inserted by code.',
});
const CHANNEL_DESCRIPTOR = msg({
	message: 'Channel {id}',
	comment:
		'Fallback name for an unnamed channel in the forward modal destination list. Preserve {id}; it is inserted by code.',
});

const SEARCH_LIMIT = 20;
const SINGLE_TYPE_SEARCH_LIMIT = 50;
const NO_DESTINATIONS: ReadonlyArray<ForwardDestination> = Object.freeze([]);
const NO_STRINGS: ReadonlyArray<string> = Object.freeze([]);
const NO_ATTACHMENTS: ReadonlyArray<MessageAttachment> = Object.freeze([]);
const NO_EMBEDS: ReadonlyArray<MessageEmbed> = Object.freeze([]);
const NO_SEARCH_RESULTS: ReadonlyArray<ForwardSearchResult> = Object.freeze([]);
const NO_CONFUSABLES: ReadonlyMap<string, string> = new Map();
const INITIAL_PINNED_DESTINATIONS: ForwardPinnedDestinations = Object.freeze({
	engineQuery: null,
	pinned: NO_DESTINATIONS,
});
const logger = new Logger('ForwardDestinations');

interface ForwardMediaNeeds {
	readonly hasAgeRestrictedMedia: boolean;
	readonly hasAttachments: boolean;
	readonly hasEmbeds: boolean;
}

interface ForwardSearchCandidates {
	readonly boosters: ForwardSearchBoosters;
	readonly channels: ReadonlyArray<ForwardChannelCandidate>;
	readonly groupDMs: ReadonlyArray<ForwardGroupDMCandidate>;
	readonly users: ReadonlyArray<ForwardUserCandidate>;
}

export interface ForwardDestinationOption {
	readonly channel: Channel | null;
	readonly destination: ForwardDestination;
	readonly detail: string | null;
	readonly disableReason: string | null;
	readonly displayName: string;
	readonly key: string;
	readonly slowmodeEnabled: boolean;
	readonly slowmodeRemainingMs: number;
	readonly user: User | null;
}

interface UseForwardDestinationsOptions {
	readonly mediaSelection: ForwardMediaSelection | undefined;
	readonly message: Message;
}

interface ForwardDestinationsState {
	readonly composerChannel: Channel | null;
	readonly options: ReadonlyArray<ForwardDestinationOption>;
	readonly searchQuery: string;
	readonly selected: ReadonlyArray<ForwardDestination>;
	readonly selectedKeys: ReadonlySet<string>;
	readonly setSearchQuery: (query: string) => void;
	readonly slowmodeActiveSelectedOptions: ReadonlyArray<ForwardDestinationOption>;
	readonly slowmodeEnabledSelectedOptions: ReadonlyArray<ForwardDestinationOption>;
	readonly toggleDestination: (destination: ForwardDestination) => void;
}

function findDirectMessageChannel(userId: string): Channel | null {
	for (const channel of Channels.getPrivateChannels()) {
		if (channel.type === ChannelTypes.DM && channel.recipientIds.includes(userId)) return channel;
	}
	return null;
}

function resolveForwardChannelRow(channelId: string): ForwardRowIdentity | null {
	const channel = Channels.getChannel(channelId);
	if (channel == null) return null;
	switch (channel.type) {
		case ChannelTypes.DM: {
			if (channel.recipientIds.length === 0) return null;
			const recipientId = channel.recipientIds[0];
			if (Users.getUser(recipientId) == null) return null;
			return {id: recipientId, type: 'user'};
		}
		case ChannelTypes.GROUP_DM:
		case ChannelTypes.DM_PERSONAL_NOTES:
			return {id: channel.id, type: 'group_dm'};
		case ChannelTypes.GUILD_VOICE:
			return {id: channel.id, type: 'voice_channel'};
		case ChannelTypes.GUILD_TEXT:
			return {id: channel.id, type: 'text_channel'};
		default:
			return null;
	}
}

function resolveForwardDestinationRow(destination: ForwardDestination): ForwardRowIdentity | null {
	if (destination.type === 'channel') return resolveForwardChannelRow(destination.id);
	if (Users.getUser(destination.id) == null) return null;
	return {id: destination.id, type: 'user'};
}

function isForwardRowValid(row: ForwardRowIdentity): boolean {
	switch (row.type) {
		case 'user':
			return !RuntimeConfig.directMessagesDisabled || findDirectMessageChannel(row.id) != null;
		case 'group_dm':
			return true;
		case 'text_channel':
		case 'voice_channel': {
			const channel = Channels.getChannel(row.id);
			if (channel == null) return false;
			if (!Permission.can(Permissions.VIEW_CHANNEL | Permissions.SEND_MESSAGES, channel)) return false;
			return !GuildMembers.isUserTimedOut(channel.guildId ?? null, Users.currentUserId);
		}
	}
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
		let friendNickname: string | null = null;
		if (relationship?.type === RelationshipTypes.FRIEND) friendNickname = relationship.nickname;
		candidates.push(
			Object.freeze({
				friendNickname,
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
		const accessPermissions = isVoice ? Permissions.VIEW_CHANNEL | Permissions.CONNECT : Permissions.VIEW_CHANNEL;
		candidates.push(
			Object.freeze({
				canAccess: Permission.can(accessPermissions, channel),
				guildName: channel.guildId == null ? null : (Guilds.getGuild(channel.guildId)?.name ?? null),
				hasFrecency: ChannelFrecency.getScore(channel.id) > 0,
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
	const score = ChannelFrecency.getScore(id);
	const channel = Guilds.getGuild(id) == null ? Channels.getChannel(id) : undefined;
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

function buildForwardSearchBoostersFromStores(): ForwardSearchBoosters {
	const friendIds: Array<string> = [];
	for (const relationship of Relationships.getRelationships()) {
		if (relationship.type === RelationshipTypes.FRIEND) friendIds.push(relationship.userId);
	}
	const dmUserIds: Array<string> = [];
	for (const channel of Channels.getPrivateChannels()) {
		if (channel.type === ChannelTypes.DM && channel.recipientIds.length > 0) dmUserIds.push(channel.recipientIds[0]);
	}
	return buildForwardSearchBoosters({
		dmUserIds,
		frequent: ChannelFrecency.frequentIds.map(resolveForwardFrequentItem),
		friendIds,
	});
}

function createForwardSearchCandidates(i18n: I18n): IComputedValue<ForwardSearchCandidates> {
	const options = {equals: comparer.structural};
	const users = computed(buildForwardUserCandidates, options);
	const groupDMs = computed(() => buildForwardGroupDMCandidates(i18n), options);
	const channels = computed(buildForwardChannelCandidates, options);
	const boosters = computed(buildForwardSearchBoostersFromStores, options);
	return computed(() =>
		Object.freeze({boosters: boosters.get(), channels: channels.get(), groupDMs: groupDMs.get(), users: users.get()}),
	);
}

function selectForwardedAttachments(
	message: Message,
	mediaSelection: ForwardMediaSelection | undefined,
): ReadonlyArray<MessageAttachment> {
	const snapshot = message.messageSnapshots?.[0];
	const attachments = snapshot == null ? message.attachments : (snapshot.attachments ?? NO_ATTACHMENTS);
	const attachmentIds = mediaSelection?.attachmentIds;
	if (attachmentIds !== undefined) return attachments.filter((attachment) => attachmentIds.includes(attachment.id));
	if (mediaSelection?.embedIndices !== undefined) return NO_ATTACHMENTS;
	return attachments;
}

function selectForwardedEmbeds(
	message: Message,
	mediaSelection: ForwardMediaSelection | undefined,
): ReadonlyArray<MessageEmbed> {
	const snapshot = message.messageSnapshots?.[0];
	const embeds = snapshot == null ? message.embeds : (snapshot.embeds ?? NO_EMBEDS);
	const embedIndices = mediaSelection?.embedIndices;
	if (embedIndices !== undefined) return embeds.filter((_embed, index) => embedIndices.includes(index));
	if (mediaSelection?.attachmentIds !== undefined) return NO_EMBEDS;
	return embeds;
}

function isAgeRestrictedEmbed(embed: MessageEmbed): boolean {
	if (embed.nsfw === true) return true;
	return (embed.children ?? []).some((child) => child.nsfw === true);
}

function resolveForwardMediaNeeds(
	message: Message,
	mediaSelection: ForwardMediaSelection | undefined,
): ForwardMediaNeeds {
	const attachments = selectForwardedAttachments(message, mediaSelection);
	const embeds = selectForwardedEmbeds(message, mediaSelection);
	return Object.freeze({
		hasAgeRestrictedMedia:
			attachments.some((attachment) => attachment.nsfw === true) || embeds.some(isAgeRestrictedEmbed),
		hasAttachments: attachments.length > 0,
		hasEmbeds: embeds.length > 0,
	});
}

function canReceiveAgeRestrictedMedia(channel: Channel): boolean {
	if (channel.type === ChannelTypes.DM_PERSONAL_NOTES) return true;
	if (channel.nsfw) return true;
	if (channel.guildId == null) return false;
	return Guilds.getGuild(channel.guildId)?.nsfwLevel === GuildNSFWLevel.AGE_RESTRICTED;
}

function resolveAgeRestrictedDisableReason(
	channel: Channel | null,
	mediaNeeds: ForwardMediaNeeds,
	i18n: I18n,
): string | null {
	if (!mediaNeeds.hasAgeRestrictedMedia) return null;
	if (channel != null && canReceiveAgeRestrictedMedia(channel)) return null;
	return i18n._(AGE_RESTRICTED_DESTINATION_DESCRIPTOR);
}

function isSlowmodeEnforced(channel: Channel): boolean {
	const rateLimitPerUser = channel.rateLimitPerUser;
	if (!Number.isSafeInteger(rateLimitPerUser) || rateLimitPerUser <= 0) return false;
	if (rateLimitPerUser > CHANNEL_RATE_LIMIT_PER_USER_MAX) return false;
	return !Permission.can(Permissions.BYPASS_SLOWMODE, channel);
}

function resolveSlowmodeRemainingMs(channel: Channel): number {
	if (DeveloperOptions.mockSlowmodeActive) return DeveloperOptions.mockSlowmodeRemaining;
	return Slowmode.getSlowmodeRemaining(channel.id, channel.rateLimitPerUser);
}

function isSlowmodeActive(option: ForwardDestinationOption): boolean {
	return option.slowmodeRemainingMs > 0;
}

function formatGuildChannelDetail(guild: Guild | undefined, channel: Channel): string | null {
	let categoryName: string | undefined;
	if (channel.parentId != null) categoryName = Channels.getChannel(channel.parentId)?.name;
	const detail = [guild?.name, categoryName].filter(Boolean).join(' • ');
	return detail === '' ? null : detail;
}

function resolveGuildChannelDisableReason(
	channel: Channel,
	guild: Guild | undefined,
	slowmodeRemainingMs: number,
	mediaNeeds: ForwardMediaNeeds,
	i18n: I18n,
): string | null {
	const ageRestrictedReason = resolveAgeRestrictedDisableReason(channel, mediaNeeds, i18n);
	if (ageRestrictedReason != null) return ageRestrictedReason;
	if (guild != null && (guild.disabledOperations & GuildOperations.SEND_MESSAGE) !== 0) {
		return i18n._(GUILD_MESSAGES_DISABLED_DESCRIPTOR);
	}
	if (mediaNeeds.hasAttachments && !Permission.can(Permissions.ATTACH_FILES, channel)) {
		const attachFilesPermissionLabel = formatPermissionLabel(i18n, Permissions.ATTACH_FILES);
		return i18n._(ATTACH_FILES_PERMISSION_REQUIRED_DESCRIPTOR, {attachFilesPermissionLabel});
	}
	if (mediaNeeds.hasEmbeds && !Permission.can(Permissions.EMBED_LINKS, channel)) {
		const embedLinksPermissionLabel = formatPermissionLabel(i18n, Permissions.EMBED_LINKS);
		return i18n._(EMBED_LINKS_PERMISSION_REQUIRED_DESCRIPTOR, {embedLinksPermissionLabel});
	}
	if (slowmodeRemainingMs <= 0) return null;
	return i18n._(SLOWMODE_WAIT_DESCRIPTOR, {remaining: formatSlowmodeTime(slowmodeRemainingMs, i18n.locale)});
}

function resolveForwardDestinationOption(
	destination: ForwardDestination,
	mediaNeeds: ForwardMediaNeeds,
	i18n: I18n,
): ForwardDestinationOption | null {
	const key = forwardDestinationKey(destination);
	if (destination.type === 'user') {
		const user = Users.getUser(destination.id);
		if (user == null) return null;
		return Object.freeze({
			channel: null,
			destination,
			detail: NicknameUtils.formatUserTagForStreamerMode(user),
			disableReason: resolveAgeRestrictedDisableReason(findDirectMessageChannel(destination.id), mediaNeeds, i18n),
			displayName: NicknameUtils.getNickname(user, null),
			key,
			slowmodeEnabled: false,
			slowmodeRemainingMs: 0,
			user,
		});
	}
	const channel = Channels.getChannel(destination.id);
	if (channel == null) return null;
	if (channel.guildId == null) {
		return Object.freeze({
			channel,
			destination,
			detail: null,
			disableReason: resolveAgeRestrictedDisableReason(channel, mediaNeeds, i18n),
			displayName: ChannelUtils.getDMDisplayName(channel),
			key,
			slowmodeEnabled: false,
			slowmodeRemainingMs: 0,
			user: null,
		});
	}
	const guild = Guilds.getGuild(channel.guildId);
	const slowmodeEnabled = isSlowmodeEnforced(channel);
	const slowmodeRemainingMs = slowmodeEnabled ? resolveSlowmodeRemainingMs(channel) : 0;
	return Object.freeze({
		channel,
		destination,
		detail: formatGuildChannelDetail(guild, channel),
		disableReason: resolveGuildChannelDisableReason(channel, guild, slowmodeRemainingMs, mediaNeeds, i18n),
		displayName: channel.name || i18n._(CHANNEL_DESCRIPTOR, {id: channel.id}),
		key,
		slowmodeEnabled,
		slowmodeRemainingMs,
		user: null,
	});
}

function resolveForwardDestinationOptions(
	destinations: ReadonlyArray<ForwardDestination>,
	mediaNeeds: ForwardMediaNeeds,
	i18n: I18n,
): ReadonlyArray<ForwardDestinationOption> {
	const options: Array<ForwardDestinationOption> = [];
	for (const destination of destinations) {
		const option = resolveForwardDestinationOption(destination, mediaNeeds, i18n);
		if (option != null) options.push(option);
	}
	return options;
}

function resolveForwardComposerChannel(selected: ReadonlyArray<ForwardDestination>): Channel | null {
	for (const destination of selected) {
		const channel =
			destination.type === 'user' ? findDirectMessageChannel(destination.id) : Channels.getChannel(destination.id);
		if (channel != null) return channel;
	}
	return null;
}

export function useForwardDestinations({
	mediaSelection,
	message,
}: UseForwardDestinationsOptions): ForwardDestinationsState {
	const {i18n} = useLingui();
	const [searchQuery, setSearchQuery] = useState('');
	const [selected, setSelected] = useState(NO_DESTINATIONS);
	const [pinnedDestinations, setPinnedDestinations] = useState(INITIAL_PINNED_DESTINATIONS);
	const [confusables, setConfusables] = useState(() => getLoadedUnicodeConfusables() ?? NO_CONFUSABLES);
	const searchCandidates = useMemo(() => createForwardSearchCandidates(i18n), [i18n, i18n.locale]);
	const mediaNeeds = useMemo(() => resolveForwardMediaNeeds(message, mediaSelection), [message, mediaSelection]);
	const parsedQuery = useMemo(() => parseForwardDestinationQuery(searchQuery), [searchQuery]);
	const engineQuery = parsedQuery.query.trim() === '' ? '' : parsedQuery.query;
	const currentPinned = pinForwardDestinations(pinnedDestinations, engineQuery, selected);
	if (currentPinned !== pinnedDestinations) setPinnedDestinations(currentPinned);
	useEffect(() => {
		let isMounted = true;
		loadUnicodeConfusables().then(
			(loaded) => {
				if (isMounted) setConfusables(loaded);
			},
			(error: unknown) => logger.warn('Failed to load Unicode confusables', error),
		);
		return () => {
			isMounted = false;
		};
	}, []);
	const candidates = engineQuery === '' ? null : searchCandidates.get();
	const currentUserId = Users.currentUserId;
	const searchResults = useMemo(() => {
		if (candidates == null) return NO_SEARCH_RESULTS;
		return searchForwardDestinations({
			...candidates,
			blacklist: new Set(currentUserId == null ? NO_STRINGS : [currentUserId]),
			confusables,
			limit: parsedQuery.resultTypes.length === 1 ? SINGLE_TYPE_SEARCH_LIMIT : SEARCH_LIMIT,
			query: engineQuery,
			resultTypes: parsedQuery.resultTypes,
		});
	}, [candidates, confusables, currentUserId, engineQuery, parsedQuery]);
	const rows =
		candidates == null
			? buildForwardDefaultDestinations({
					frequentIds: ChannelFrecency.frequentIds,
					history: [...new Set(SelectedChannel.sortedRecentVisits.map((visit) => visit.channelId))],
					isValid: isForwardRowValid,
					mode: parsedQuery.mode,
					origin: resolveForwardOrigin(message.channelId, Channels.getChannel(message.channelId)),
					pinned: currentPinned.pinned,
					resolveChannel: resolveForwardChannelRow,
					resolveDestination: resolveForwardDestinationRow,
					selected,
				})
			: filterForwardSearchRows(searchResults, isForwardRowValid);
	const options = resolveForwardDestinationOptions(
		rows.map((row) => row.destination),
		mediaNeeds,
		i18n,
	);
	const selectedOptions = resolveForwardDestinationOptions(selected, mediaNeeds, i18n);
	useNow(options.some(isSlowmodeActive) || selectedOptions.some(isSlowmodeActive));
	const toggleDestination = (destination: ForwardDestination) => {
		const next = toggleForwardDestination(selected, destination);
		if (next === selected) return;
		if (next.length > selected.length) setSearchQuery('');
		setSelected(next);
	};
	return {
		composerChannel: resolveForwardComposerChannel(selected),
		options,
		searchQuery,
		selected,
		selectedKeys: new Set(selected.map(forwardDestinationKey)),
		setSearchQuery,
		slowmodeActiveSelectedOptions: selectedOptions.filter(isSlowmodeActive),
		slowmodeEnabledSelectedOptions: selectedOptions.filter((option) => option.slowmodeEnabled),
		toggleDestination,
	};
}
