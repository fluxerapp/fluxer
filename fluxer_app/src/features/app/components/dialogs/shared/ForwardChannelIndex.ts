// SPDX-License-Identifier: AGPL-3.0-or-later

import {formatSlowmodeTime} from '@app/features/channel/components/SlowmodeIndicator';
import type {Channel} from '@app/features/channel/models/Channel';
import {PERSONAL_NOTES_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {formatPermissionLabel} from '@app/features/permissions/utils/PermissionUtils';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {isForwardableChannelType} from './ForwardChannelEligibility';

const GUILD_MESSAGES_DISABLED_DESCRIPTOR = msg({
	message: 'Sending messages is disabled in this community',
	comment: 'Short label in the settings dialog forward channel selection.',
});
const MEMBER_TIMED_OUT_DESCRIPTOR = msg({
	message: "You're on timeout in this community",
	comment: 'Short label in the settings dialog forward channel selection. Keep the tone plain and specific.',
});
const SEND_MESSAGES_PERMISSION_REQUIRED_DESCRIPTOR = msg({
	message: 'You need the "{sendMessagesPermissionLabel}" permission to send messages in this channel',
	comment:
		'Forward dialog error shown when the user lacks the Send Messages permission in the target channel. Permission name is interpolated.',
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
const SLOWMODE_WAIT_DESCRIPTOR = msg({
	message: 'Slowmode · wait {remaining}',
	comment:
		'Short label in the settings dialog forward channel selection. Preserve {remaining}; it is inserted by code.',
});

const FORWARD_CHANNEL_RESULT_LIMIT = 100;

export interface ForwardMessageMediaSelection {
	readonly hasAttachments: boolean;
	readonly hasEmbeds: boolean;
}

interface ResolveForwardMessageMediaSelectionArgs {
	readonly message: Message;
	readonly override: ForwardMessageMediaSelection | undefined;
}

function hasSnapshotAttachments(message: Message): boolean {
	if (message.attachments.length > 0) return true;
	const snapshots = message.messageSnapshots;
	if (snapshots === undefined) return false;
	return snapshots.some((snapshot) => {
		const attachments = snapshot.attachments;
		return attachments != null && attachments.length > 0;
	});
}

function hasSnapshotEmbeds(message: Message): boolean {
	if (message.embeds.length > 0) return true;
	const snapshots = message.messageSnapshots;
	if (snapshots === undefined) return false;
	return snapshots.some((snapshot) => {
		const embeds = snapshot.embeds;
		return embeds != null && embeds.length > 0;
	});
}

export function resolveForwardMessageMediaSelection({
	message,
	override,
}: ResolveForwardMessageMediaSelectionArgs): ForwardMessageMediaSelection {
	if (override !== undefined) return override;
	return Object.freeze({
		hasAttachments: hasSnapshotAttachments(message),
		hasEmbeds: hasSnapshotEmbeds(message),
	});
}

export interface ForwardChannelObservation {
	readonly canAttachFiles: boolean;
	readonly canEmbedLinks: boolean;
	readonly canSendMessages: boolean;
	readonly categoryName: string | null;
	readonly channel: Channel;
	readonly displayName: string;
	readonly guildMessagesDisabled: boolean;
	readonly guildName: string | null;
	readonly memberTimedOut: boolean;
	readonly slowmodeEnabled: boolean;
	readonly slowmodeRemainingMs: number;
}

export interface ForwardChannelOption {
	readonly categoryName: string | null;
	readonly channel: Channel;
	readonly disableReason: string | null;
	readonly displayName: string;
	readonly guildName: string | null;
	readonly slowmodeEnabled: boolean;
	readonly slowmodeRemainingMs: number;
}

interface IndexedForwardChannelOption extends ForwardChannelOption {
	readonly channelNameSearchValue: string;
	readonly displayNameSearchValue: string;
	readonly guildNameSearchValue: string;
	readonly isPersonalNotes: boolean;
	readonly isSource: boolean;
	readonly recentRank: number | null;
}

interface BuildForwardChannelIndexRequest {
	readonly excludedChannelId: string;
	readonly i18n: I18n;
	readonly mediaSelection: ForwardMessageMediaSelection;
	readonly observations: ReadonlyArray<ForwardChannelObservation>;
	readonly recentChannelIds: ReadonlyArray<string>;
}

interface ForwardChannelSelectionDisabledRequest {
	readonly maxSelections: number;
	readonly option: ForwardChannelOption;
	readonly selectedChannelIds: ReadonlySet<string>;
}

export class ForwardChannelIndex {
	private readonly i18n: I18n;
	private readonly options: ReadonlyArray<IndexedForwardChannelOption>;
	private readonly optionsByChannelId: ReadonlyMap<string, IndexedForwardChannelOption>;

	constructor({
		excludedChannelId,
		i18n,
		mediaSelection,
		observations,
		recentChannelIds,
	}: BuildForwardChannelIndexRequest) {
		this.i18n = i18n;
		const recentRanks = ForwardChannelIndex.buildRecentRanks(recentChannelIds);
		const options = observations
			.filter((observation) => isForwardableChannelType(observation.channel.type))
			.map((observation) => this.buildOption({excludedChannelId, mediaSelection, observation, recentRanks}));
		options.sort((left, right) => this.compareOptions(left, right));
		this.options = Object.freeze(options);
		this.optionsByChannelId = new Map(options.map((option) => [option.channel.id, option]));
	}

	filter(searchQuery: string): ReadonlyArray<ForwardChannelOption> {
		if (searchQuery.trim().length === 0) {
			return this.options.slice(0, FORWARD_CHANNEL_RESULT_LIMIT);
		}
		const normalizedQuery = searchQuery.toLowerCase();
		const personalNotesSearchValue = this.i18n._(PERSONAL_NOTES_DESCRIPTOR).toLowerCase();
		const matches: Array<IndexedForwardChannelOption> = [];
		for (const option of this.options) {
			if (!ForwardChannelIndex.matchesSearch({normalizedQuery, personalNotesSearchValue, option})) continue;
			matches.push(option);
			if (matches.length === FORWARD_CHANNEL_RESULT_LIMIT) break;
		}
		return matches;
	}

	isSelectionDisabled({maxSelections, option, selectedChannelIds}: ForwardChannelSelectionDisabledRequest): boolean {
		if (option.disableReason != null) return true;
		if (selectedChannelIds.has(option.channel.id)) return false;
		return selectedChannelIds.size >= maxSelections;
	}

	select(selectedChannelIds: ReadonlySet<string>): ReadonlyArray<ForwardChannelOption> {
		const selected: Array<IndexedForwardChannelOption> = [];
		for (const channelId of selectedChannelIds) {
			const option = this.optionsByChannelId.get(channelId);
			if (option != null) selected.push(option);
		}
		return selected;
	}

	private static buildRecentRanks(recentChannelIds: ReadonlyArray<string>): ReadonlyMap<string, number> {
		return new Map(recentChannelIds.map((channelId, index) => [channelId, index]));
	}

	private static matchesSearch({
		normalizedQuery,
		personalNotesSearchValue,
		option,
	}: {
		readonly normalizedQuery: string;
		readonly personalNotesSearchValue: string;
		readonly option: IndexedForwardChannelOption;
	}): boolean {
		if (option.isPersonalNotes && personalNotesSearchValue.includes(normalizedQuery)) return true;
		if (option.displayNameSearchValue.includes(normalizedQuery)) return true;
		if (option.channelNameSearchValue.includes(normalizedQuery)) return true;
		return option.guildNameSearchValue.includes(normalizedQuery);
	}

	private buildOption({
		excludedChannelId,
		mediaSelection,
		observation,
		recentRanks,
	}: {
		readonly excludedChannelId: string;
		readonly mediaSelection: ForwardMessageMediaSelection;
		readonly observation: ForwardChannelObservation;
		readonly recentRanks: ReadonlyMap<string, number>;
	}): IndexedForwardChannelOption {
		const permissionIssue = this.resolvePermissionIssue(observation, mediaSelection);
		const disableReason = this.resolveDisableReason(observation, permissionIssue);
		const recentRankValue = recentRanks.get(observation.channel.id);
		let recentRank: number | null = null;
		if (recentRankValue !== undefined) recentRank = recentRankValue;
		let channelNameSearchValue = '';
		if (observation.channel.name) channelNameSearchValue = observation.channel.name.toLowerCase();
		let guildNameSearchValue = '';
		if (observation.guildName != null) guildNameSearchValue = observation.guildName.toLowerCase();
		return Object.freeze({
			categoryName: observation.categoryName,
			channel: observation.channel,
			channelNameSearchValue,
			disableReason,
			displayName: observation.displayName,
			displayNameSearchValue: observation.displayName.toLowerCase(),
			guildName: observation.guildName,
			guildNameSearchValue,
			isPersonalNotes: observation.channel.type === ChannelTypes.DM_PERSONAL_NOTES,
			isSource: observation.channel.id === excludedChannelId,
			recentRank,
			slowmodeEnabled: observation.slowmodeEnabled,
			slowmodeRemainingMs: observation.slowmodeRemainingMs,
		});
	}

	private compareOptions(left: IndexedForwardChannelOption, right: IndexedForwardChannelOption): number {
		if (left.isSource !== right.isSource) return left.isSource ? 1 : -1;
		const leftUnavailable = left.disableReason != null;
		const rightUnavailable = right.disableReason != null;
		if (leftUnavailable !== rightUnavailable) return leftUnavailable ? 1 : -1;
		if (left.recentRank != null && right.recentRank != null) return left.recentRank - right.recentRank;
		if (left.recentRank != null) return -1;
		if (right.recentRank != null) return 1;
		return left.displayNameSearchValue.localeCompare(right.displayNameSearchValue);
	}

	private resolveDisableReason(observation: ForwardChannelObservation, permissionIssue: string | null): string | null {
		if (permissionIssue != null) return permissionIssue;
		if (observation.slowmodeRemainingMs <= 0) return null;
		const remaining = formatSlowmodeTime(observation.slowmodeRemainingMs, this.i18n.locale);
		return this.i18n._(SLOWMODE_WAIT_DESCRIPTOR, {remaining});
	}

	private resolvePermissionIssue(
		observation: ForwardChannelObservation,
		mediaSelection: ForwardMessageMediaSelection,
	): string | null {
		if (observation.guildMessagesDisabled) return this.i18n._(GUILD_MESSAGES_DISABLED_DESCRIPTOR);
		if (observation.memberTimedOut) return this.i18n._(MEMBER_TIMED_OUT_DESCRIPTOR);
		if (!observation.canSendMessages) {
			const sendMessagesPermissionLabel = formatPermissionLabel(this.i18n, Permissions.SEND_MESSAGES);
			return this.i18n._(SEND_MESSAGES_PERMISSION_REQUIRED_DESCRIPTOR, {sendMessagesPermissionLabel});
		}
		if (mediaSelection.hasEmbeds && !observation.canEmbedLinks) {
			const embedLinksPermissionLabel = formatPermissionLabel(this.i18n, Permissions.EMBED_LINKS);
			return this.i18n._(EMBED_LINKS_PERMISSION_REQUIRED_DESCRIPTOR, {embedLinksPermissionLabel});
		}
		if (mediaSelection.hasAttachments && !observation.canAttachFiles) {
			const attachFilesPermissionLabel = formatPermissionLabel(this.i18n, Permissions.ATTACH_FILES);
			return this.i18n._(ATTACH_FILES_PERMISSION_REQUIRED_DESCRIPTOR, {attachFilesPermissionLabel});
		}
		return null;
	}
}
