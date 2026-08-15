// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ForwardChannelObservation} from '@app/features/app/components/dialogs/shared/ForwardChannelIndex';
import {useShallowStableArray} from '@app/features/app/hooks/useShallowStableArray';
import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import Guilds from '@app/features/guild/state/Guilds';
import GuildMembers from '@app/features/member/state/GuildMembers';
import Permission from '@app/features/permissions/state/Permission';
import Slowmode from '@app/features/slowmode/state/Slowmode';
import {useNow} from '@app/features/ui/state/Tick';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildOperations} from '@fluxer/constants/src/GuildConstants';
import {CHANNEL_RATE_LIMIT_PER_USER_MAX} from '@fluxer/constants/src/LimitConstants';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {computed, type IComputedValue} from 'mobx';
import {useMemo, useRef} from 'react';

const CHANNEL_DESCRIPTOR = msg({
	message: 'Channel {id}',
	comment: 'Short label in the settings dialog forward channel selection. Preserve {id}; it is inserted by code.',
});

interface UseForwardChannelObservationsOptions {
	readonly channels: ReadonlyArray<Channel>;
	readonly currentUserId: string | null;
	readonly i18n: I18n;
}

interface ForwardChannelObservationBase {
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
}

interface ForwardSlowmodeChannel {
	readonly channelId: string;
	readonly index: number;
	readonly rateLimitPerUser: number;
}

interface ForwardChannelObservationSelection {
	readonly bases: ReadonlyArray<ForwardChannelObservationBase>;
	readonly slowmodeChannels: ReadonlyArray<ForwardSlowmodeChannel>;
}

function resolveChannelDisplayName(channel: Channel, i18n: I18n): string {
	if (
		channel.type === ChannelTypes.DM_PERSONAL_NOTES ||
		channel.type === ChannelTypes.DM ||
		channel.type === ChannelTypes.GROUP_DM
	) {
		return ChannelUtils.getDMDisplayName(channel);
	}
	if (channel.name) return channel.name;
	return i18n._(CHANNEL_DESCRIPTOR, {id: channel.id});
}

function resolveChannelCategoryName(channel: Channel): string | null {
	if (!channel.parentId) return null;
	const category = Channels.getChannel(channel.parentId);
	if (!category || !category.name) return null;
	return category.name;
}

function isCurrentMemberTimedOut(channel: Channel, currentUserId: string | null): boolean {
	if (!channel.guildId || currentUserId === null) return false;
	const member = GuildMembers.getMember(channel.guildId, currentUserId);
	if (!member) return false;
	return member.isTimedOut();
}

function resolveSlowmodeRemainingMs(
	entry: ForwardSlowmodeChannel,
	mockSlowmodeActive: boolean,
	mockSlowmodeRemaining: number,
): number {
	if (mockSlowmodeActive) return mockSlowmodeRemaining;
	return Slowmode.getSlowmodeRemaining(entry.channelId, entry.rateLimitPerUser);
}

function areForwardChannelObservationBasesEqual(
	left: ForwardChannelObservationBase,
	right: ForwardChannelObservationBase,
): boolean {
	if (left.channel !== right.channel || left.displayName !== right.displayName) return false;
	if (left.guildName !== right.guildName || left.categoryName !== right.categoryName) return false;
	if (left.guildMessagesDisabled !== right.guildMessagesDisabled || left.memberTimedOut !== right.memberTimedOut) {
		return false;
	}
	if (left.canSendMessages !== right.canSendMessages || left.canEmbedLinks !== right.canEmbedLinks) return false;
	return left.canAttachFiles === right.canAttachFiles && left.slowmodeEnabled === right.slowmodeEnabled;
}

function areForwardSlowmodeChannelsEqual(left: ForwardSlowmodeChannel, right: ForwardSlowmodeChannel): boolean {
	return (
		left.channelId === right.channelId && left.index === right.index && left.rateLimitPerUser === right.rateLimitPerUser
	);
}

function areForwardChannelObservationSelectionsEqual(
	left: ForwardChannelObservationSelection,
	right: ForwardChannelObservationSelection,
): boolean {
	if (left.bases.length !== right.bases.length || left.slowmodeChannels.length !== right.slowmodeChannels.length) {
		return false;
	}
	for (let index = 0; index < left.bases.length; index += 1) {
		const leftBase = left.bases[index];
		const rightBase = right.bases[index];
		if (leftBase === undefined || rightBase === undefined) {
			throw new Error(`Forward channel observation ${index.toString()} is missing`);
		}
		if (!areForwardChannelObservationBasesEqual(leftBase, rightBase)) return false;
	}
	for (let index = 0; index < left.slowmodeChannels.length; index += 1) {
		const leftEntry = left.slowmodeChannels[index];
		const rightEntry = right.slowmodeChannels[index];
		if (leftEntry === undefined || rightEntry === undefined) {
			throw new Error(`Forward slowmode channel ${index.toString()} is missing`);
		}
		if (!areForwardSlowmodeChannelsEqual(leftEntry, rightEntry)) return false;
	}
	return true;
}

class ForwardChannelObservationsOwner {
	private readonly selection: IComputedValue<ForwardChannelObservationSelection>;
	private readonly locale: string;

	constructor(private readonly request: UseForwardChannelObservationsOptions) {
		this.locale = request.i18n.locale;
		this.selection = computed(() => this.buildSelection(), {equals: areForwardChannelObservationSelectionsEqual});
	}

	matches(request: UseForwardChannelObservationsOptions): boolean {
		return (
			this.request.channels === request.channels &&
			this.request.currentUserId === request.currentUserId &&
			this.request.i18n === request.i18n &&
			this.locale === request.i18n.locale
		);
	}

	read(): ForwardChannelObservationSelection {
		return this.selection.get();
	}

	private buildSelection(): ForwardChannelObservationSelection {
		const bases: Array<ForwardChannelObservationBase> = [];
		const slowmodeChannels: Array<ForwardSlowmodeChannel> = [];
		for (const channel of this.request.channels) {
			const index = bases.length;
			const base = this.buildBase(channel);
			bases.push(base);
			if (base.slowmodeEnabled) {
				slowmodeChannels.push(
					Object.freeze({channelId: channel.id, index, rateLimitPerUser: channel.rateLimitPerUser}),
				);
			}
		}
		return Object.freeze({bases: Object.freeze(bases), slowmodeChannels: Object.freeze(slowmodeChannels)});
	}

	private buildBase(channel: Channel): ForwardChannelObservationBase {
		const guildId = channel.guildId;
		if (!guildId) {
			return Object.freeze({
				canAttachFiles: true,
				canEmbedLinks: true,
				canSendMessages: true,
				categoryName: resolveChannelCategoryName(channel),
				channel,
				displayName: resolveChannelDisplayName(channel, this.request.i18n),
				guildMessagesDisabled: false,
				guildName: null,
				memberTimedOut: false,
				slowmodeEnabled: false,
			});
		}
		const guild = Guilds.getGuild(guildId);
		let guildName: string | null = null;
		let guildMessagesDisabled = false;
		if (guild) {
			guildName = guild.name;
			guildMessagesDisabled = (guild.disabledOperations & GuildOperations.SEND_MESSAGE) !== 0;
		}
		let slowmodeEnabled = false;
		if (
			Number.isSafeInteger(channel.rateLimitPerUser) &&
			channel.rateLimitPerUser > 0 &&
			channel.rateLimitPerUser <= CHANNEL_RATE_LIMIT_PER_USER_MAX
		) {
			slowmodeEnabled = !Permission.can(Permissions.BYPASS_SLOWMODE, channel);
		}
		return Object.freeze({
			canAttachFiles: Permission.can(Permissions.ATTACH_FILES, channel),
			canEmbedLinks: Permission.can(Permissions.EMBED_LINKS, channel),
			canSendMessages: Permission.can(Permissions.SEND_MESSAGES, channel),
			categoryName: resolveChannelCategoryName(channel),
			channel,
			displayName: resolveChannelDisplayName(channel, this.request.i18n),
			guildMessagesDisabled,
			guildName,
			memberTimedOut: isCurrentMemberTimedOut(channel, this.request.currentUserId),
			slowmodeEnabled,
		});
	}
}

function buildForwardChannelObservations(
	selection: ForwardChannelObservationSelection,
	slowmodeRemainingValues: ReadonlyArray<number>,
): ReadonlyArray<ForwardChannelObservation> {
	const remainingByIndex = new Map<number, number>();
	for (let entryIndex = 0; entryIndex < selection.slowmodeChannels.length; entryIndex += 1) {
		const entry = selection.slowmodeChannels[entryIndex];
		const remainingMs = slowmodeRemainingValues[entryIndex];
		if (entry === undefined || remainingMs === undefined) {
			throw new Error(`Forward slowmode remaining value ${entryIndex.toString()} is missing`);
		}
		remainingByIndex.set(entry.index, remainingMs);
	}
	return Object.freeze(
		selection.bases.map((base, index) => {
			const remainingMs = remainingByIndex.get(index);
			return Object.freeze({
				canAttachFiles: base.canAttachFiles,
				canEmbedLinks: base.canEmbedLinks,
				canSendMessages: base.canSendMessages,
				categoryName: base.categoryName,
				channel: base.channel,
				displayName: base.displayName,
				guildMessagesDisabled: base.guildMessagesDisabled,
				guildName: base.guildName,
				memberTimedOut: base.memberTimedOut,
				slowmodeEnabled: base.slowmodeEnabled,
				slowmodeRemainingMs: remainingMs === undefined ? 0 : remainingMs,
			});
		}),
	);
}

export function useForwardChannelObservations({
	channels,
	currentUserId,
	i18n,
}: UseForwardChannelObservationsOptions): ReadonlyArray<ForwardChannelObservation> {
	const request: UseForwardChannelObservationsOptions = {channels, currentUserId, i18n};
	const ownerRef = useRef<ForwardChannelObservationsOwner | null>(null);
	let owner = ownerRef.current;
	if (owner === null || !owner.matches(request)) {
		owner = new ForwardChannelObservationsOwner(request);
		ownerRef.current = owner;
	}
	const selection = owner.read();
	const mockSlowmodeActive = DeveloperOptions.mockSlowmodeActive;
	const mockSlowmodeRemaining = DeveloperOptions.mockSlowmodeRemaining;
	const slowmodeRemainingValues = useShallowStableArray(
		selection.slowmodeChannels.map((entry) =>
			resolveSlowmodeRemainingMs(entry, mockSlowmodeActive, mockSlowmodeRemaining),
		),
	);
	let hasActiveSlowmodeChannel = false;
	for (const remainingMs of slowmodeRemainingValues) {
		if (remainingMs > 0) {
			hasActiveSlowmodeChannel = true;
			break;
		}
	}
	useNow(hasActiveSlowmodeChannel);
	return useMemo(
		() => buildForwardChannelObservations(selection, slowmodeRemainingValues),
		[selection, slowmodeRemainingValues],
	);
}
