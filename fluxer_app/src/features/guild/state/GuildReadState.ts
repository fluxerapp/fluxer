// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import {
	type GuildReadStateContribution,
	resolveGuildReadStateContribution,
} from '@app/features/guild/state/GuildReadStateContributionMachine';
import Guilds from '@app/features/guild/state/Guilds';
import {deferUntilModulesLoaded} from '@app/features/platform/utils/DeferUntilModulesLoaded';
import ReadStates from '@app/features/read_state/state/ReadStates';
import AdvancedSettings from '@app/features/user/state/AdvancedSettings';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import {ME} from '@fluxer/constants/src/AppConstants';
import {GUILD_TEXT_BASED_CHANNEL_TYPES} from '@fluxer/constants/src/ChannelConstants';
import {MessageNotifications} from '@fluxer/constants/src/NotificationConstants';
import type {ChannelId, GuildId} from '@fluxer/schema/src/branded/WireIds';
import {makeAutoObservable, observable, reaction, runInAction} from 'mobx';

const PRIVATE_CHANNEL_SENTINEL = ME;

class GuildReadStateEntry {
	unread = observable.box(false);
	unreadChannelId = observable.box<ChannelId | null>(null);
	mentionCount = observable.box(0);
	mentionChannels = observable.set(new Set<ChannelId>());
	sentinel = observable.box(0);

	incrementSentinel(): void {
		this.sentinel.set(this.sentinel.get() + 1);
	}

	reset(): void {
		this.unread.set(false);
		this.unreadChannelId.set(null);
		this.mentionCount.set(0);
		this.mentionChannels.clear();
	}
}

type ContributeChannel = {
	id: string;
	type: number;
	guildId?: string | null;
	parentId?: string | null;
	isPrivate(): boolean;
	isGuildVocal?(): boolean;
};

function isChannelMutedForUnread(channel: ContributeChannel): boolean {
	if (channel.isPrivate()) return false;
	return UserGuildSettings.isGuildOrCategoryOrChannelMuted(channel.guildId ?? null, channel.id);
}

function resolveUnreadBadgesLevel(channel: ContributeChannel): number | null {
	if (channel.isPrivate()) return null;
	if (isChannelMutedForUnread(channel)) return MessageNotifications.NO_MESSAGES;
	return UserGuildSettings.resolvedGuildUnreadBadgesLevel({
		id: channel.id,
		guildId: channel.guildId ?? undefined,
		parentId: channel.parentId ?? undefined,
		type: channel.type,
	});
}

function getChannelContribution(channel: ContributeChannel, channelId: string): GuildReadStateContribution {
	const mentionCount = ReadStates.getMentionCount(channelId);
	const hasUnread = ReadStates.hasUnread(channelId);
	return resolveGuildReadStateContribution({
		isEligibleTextChannel: GUILD_TEXT_BASED_CHANNEL_TYPES.has(channel.type),
		isPrivate: channel.isPrivate(),
		unreadBadgesLevel: resolveUnreadBadgesLevel(channel),
		isMutedForUnread: isChannelMutedForUnread(channel),
		hasUnread,
		mentionCount,
	});
}

class GuildReadState {
	private readonly guildStates = observable.map(new Map<GuildId, GuildReadStateEntry>());
	private readonly unreadGuilds = observable.set(new Set<GuildId>());
	updateCounter = 0;
	private readStateReactionInstalled = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		this.installReadStateReaction();
		deferUntilModulesLoaded(() => {
			reaction(
				() => UserGuildSettings.version,
				() => {
					this.processUserGuildSettingsUpdates();
				},
			);
			reaction(
				() => AdvancedSettings.unreadBadgeCustomizationEnabled,
				() => {
					this.recomputeAllReadStates();
				},
			);
		});
	}

	private installReadStateReaction(): void {
		if (this.readStateReactionInstalled) return;
		if (ReadStates == null) {
			setTimeout(() => this.installReadStateReaction(), 0);
			return;
		}
		this.readStateReactionInstalled = true;
		reaction(
			() => ReadStates.version,
			() => {
				const {all, changes} = ReadStates.consumePendingChanges();
				if (all) {
					this.handleConnectionOpen();
					return;
				}
				if (changes.length === 0) {
					return;
				}
				const byGuild = new Map<GuildId | null, Array<ChannelId>>();
				for (const {channelId, guildId} of changes) {
					let list = byGuild.get(guildId as GuildId | null);
					if (list == null) {
						list = [];
						byGuild.set(guildId as GuildId | null, list);
					}
					list.push(channelId as ChannelId);
				}
				for (const [guildId, ids] of byGuild.entries()) {
					if (guildId == null) {
						this.recomputeAll(null);
					} else {
						this.recomputeChannels(guildId, ids);
					}
				}
			},
		);
	}

	get version(): number {
		return this.updateCounter;
	}

	private getOrCreate(guildId: string | null): GuildReadStateEntry {
		const id = guildId ?? PRIVATE_CHANNEL_SENTINEL;
		let state = this.guildStates.get(id as GuildId);
		if (state == null) {
			state = new GuildReadStateEntry();
			this.guildStates.set(id as GuildId, state);
		}
		return state;
	}

	private notifyChange(): void {
		this.updateCounter++;
	}

	private incrementSentinel(guildId: string | null): void {
		const state = this.getOrCreate(guildId);
		state.incrementSentinel();
		this.notifyChange();
	}

	private recomputeChannels(guildId: string | null, channelIds: Array<ChannelId>): boolean {
		const id = guildId ?? PRIVATE_CHANNEL_SENTINEL;
		const state = this.getOrCreate(id);
		const previousUnread = state.unread.get();
		const previousUnreadChannelId = state.unreadChannelId.get();
		const previousMentionCount = state.mentionCount.get();
		const mentionChannelUpdates = new Map<ChannelId, boolean>();
		let foundUnread = false;
		let shouldClearUnreadChannelId = false;
		let nextUnreadChannelId = previousUnreadChannelId;
		for (const channelId of channelIds) {
			const channel = Channels.getChannel(channelId);
			if (channel == null) {
				mentionChannelUpdates.set(channelId, false);
				if (nextUnreadChannelId === channelId) {
					shouldClearUnreadChannelId = true;
				}
				continue;
			}
			const channelGuildId = channel.guildId ?? null;
			if (channelGuildId !== guildId) {
				if (channelGuildId != null) {
					this.recomputeChannels(channelGuildId, [channelId]);
				} else if (guildId != null) {
					this.recomputeChannels(null, [channelId]);
				}
				continue;
			}
			const contribution = getChannelContribution(channel, channelId);
			mentionChannelUpdates.set(channelId, contribution.mentionAllowed);
			if (guildId != null && !foundUnread && contribution.unreadAllowed) {
				foundUnread = true;
				nextUnreadChannelId = channelId;
			} else if (!contribution.unreadAllowed && nextUnreadChannelId === channelId) {
				shouldClearUnreadChannelId = true;
			}
		}
		const nextUnread = foundUnread;
		if (!nextUnread && shouldClearUnreadChannelId) {
			nextUnreadChannelId = null;
		}
		if (previousUnread && !nextUnread) {
			return this.recomputeAll(guildId);
		}
		let mentionTotal = 0;
		for (const channelId of state.mentionChannels) {
			if (mentionChannelUpdates.get(channelId) === false) {
				continue;
			}
			mentionTotal += ReadStates.getMentionCount(channelId);
		}
		for (const [channelId, mentionAllowed] of mentionChannelUpdates) {
			if (mentionAllowed && !state.mentionChannels.has(channelId)) {
				mentionTotal += ReadStates.getMentionCount(channelId);
			}
		}
		const changed =
			nextUnread !== previousUnread ||
			nextUnreadChannelId !== previousUnreadChannelId ||
			mentionTotal !== previousMentionCount;
		if (!changed && mentionChannelUpdates.size === 0) {
			return false;
		}
		runInAction(() => {
			for (const [channelId, mentionAllowed] of mentionChannelUpdates) {
				if (mentionAllowed) {
					state.mentionChannels.add(channelId);
				} else {
					state.mentionChannels.delete(channelId);
				}
			}
			if (!changed) {
				return;
			}
			state.unread.set(nextUnread);
			state.unreadChannelId.set(nextUnreadChannelId);
			state.mentionCount.set(mentionTotal);
			if (id !== PRIVATE_CHANNEL_SENTINEL) {
				if (nextUnread) {
					this.unreadGuilds.add(id as GuildId);
				} else {
					this.unreadGuilds.delete(id as GuildId);
				}
			}
			this.incrementSentinel(guildId);
		});
		return changed;
	}

	private recomputeAll(guildId: string | null, skipIfMuted = false): boolean {
		const id = guildId ?? PRIVATE_CHANNEL_SENTINEL;
		const newState = new GuildReadStateEntry();
		if (guildId == null) {
			const privateChannels = Channels.getPrivateChannels();
			for (const channel of privateChannels) {
				const contribution = getChannelContribution(channel, channel.id);
				if (contribution.mentionAllowed) {
					newState.mentionCount.set(newState.mentionCount.get() + contribution.mentionCount);
					newState.mentionChannels.add(channel.id as ChannelId);
				}
				if (!newState.unread.get() && contribution.unreadAllowed) {
					newState.unread.set(true);
					newState.unreadChannelId.set(channel.id as ChannelId);
				}
			}
		} else {
			const isGuildMuted = UserGuildSettings.isMuted(guildId);
			if (isGuildMuted && skipIfMuted) {
				return false;
			}
			const channels = Channels.getGuildChannels(guildId);
			for (const channel of channels) {
				const contribution = getChannelContribution(channel, channel.id);
				if (contribution.mentionAllowed) {
					newState.mentionCount.set(newState.mentionCount.get() + contribution.mentionCount);
					newState.mentionChannels.add(channel.id as ChannelId);
				}
				if (!newState.unread.get() && contribution.unreadAllowed) {
					newState.unread.set(true);
					newState.unreadChannelId.set(channel.id as ChannelId);
				}
			}
		}
		const prevState = this.getOrCreate(id);
		return this.commitState(id, newState, prevState);
	}

	private commitState(guildId: string, newState: GuildReadStateEntry, prevState: GuildReadStateEntry): boolean {
		if (
			newState.unread.get() === prevState.unread.get() &&
			newState.unreadChannelId.get() === prevState.unreadChannelId.get() &&
			newState.mentionCount.get() === prevState.mentionCount.get()
		) {
			return false;
		}
		runInAction(() => {
			this.guildStates.set(guildId as GuildId, newState);
			if (guildId !== PRIVATE_CHANNEL_SENTINEL) {
				if (newState.unread.get()) {
					this.unreadGuilds.add(guildId as GuildId);
				} else {
					this.unreadGuilds.delete(guildId as GuildId);
				}
			}
			this.incrementSentinel(guildId === PRIVATE_CHANNEL_SENTINEL ? null : guildId);
		});
		return true;
	}

	private processUserGuildSettingsUpdates(): void {
		const updatedGuilds = UserGuildSettings.consumePendingGuildUpdates();
		if (updatedGuilds.length === 0) {
			return;
		}
		const processed = new Set<GuildId | null>();
		for (const guildId of updatedGuilds) {
			if (processed.has(guildId)) continue;
			processed.add(guildId);
			if (guildId == null) {
				this.recomputeAll(null);
			} else {
				this.recomputeAll(guildId);
			}
		}
	}

	private recomputeAllReadStates(): void {
		this.recomputeAll(null);
		for (const guildId of Guilds.getGuildIds()) {
			this.recomputeAll(guildId);
		}
	}

	get hasAnyUnread(): boolean {
		return this.unreadGuilds.size > 0;
	}

	hasUnread(guildId: string): boolean {
		return this.unreadGuilds.has(guildId as GuildId);
	}

	getMentionCount(guildId: string | null): number {
		const id = guildId ?? PRIVATE_CHANNEL_SENTINEL;
		const state = this.guildStates.get(id as GuildId);
		return state?.mentionCount.get() ?? 0;
	}

	getTotalMentionCount(excludePrivate = false): number {
		let total = 0;
		for (const [guildId, state] of this.guildStates.entries()) {
			if (excludePrivate && guildId === PRIVATE_CHANNEL_SENTINEL) continue;
			total += state.mentionCount.get();
		}
		return total;
	}

	getPrivateChannelMentionCount(): number {
		const state = this.guildStates.get(PRIVATE_CHANNEL_SENTINEL as GuildId);
		return state?.mentionCount.get() ?? 0;
	}

	getMentionCountForPrivateChannel(channelId: string): number {
		return ReadStates.getMentionCount(channelId);
	}

	getGuildChangeSentinel(guildId: string | null): number {
		const id = guildId ?? PRIVATE_CHANNEL_SENTINEL;
		const state = this.guildStates.get(id as GuildId);
		return state?.sentinel.get() ?? 0;
	}

	getGuildHasUnreadIgnoreMuted(guildId: string): boolean {
		const channels = Channels.getGuildChannels(guildId);
		for (const channel of channels) {
			if (!GUILD_TEXT_BASED_CHANNEL_TYPES.has(channel.type)) {
				continue;
			}
			if (ReadStates.hasUnreadOrMentions(channel.id)) {
				return true;
			}
		}
		return false;
	}

	handleConnectionOpen(): void {
		this.guildStates.clear();
		this.unreadGuilds.clear();
		this.updateCounter = 0;
		this.recomputeAll(null);
		for (const guildId of Guilds.getGuildIds()) {
			this.recomputeAll(guildId);
		}
		this.notifyChange();
	}

	handleGuildCreate(action: {
		guild: {
			id: string;
		};
	}): void {
		this.recomputeAll(action.guild.id);
	}

	handleGuildDelete(action: {
		guild: {
			id: string;
		};
	}): void {
		this.guildStates.delete(action.guild.id as GuildId);
		this.unreadGuilds.delete(action.guild.id as GuildId);
		this.notifyChange();
	}

	handleChannelUpdate(action: {
		channel: {
			id: string;
			guildId?: string;
		};
	}): void {
		this.recomputeChannels(action.channel.guildId ?? null, [action.channel.id as ChannelId]);
	}

	handleGenericUpdate(channelId: string): void {
		const channel = Channels.getChannel(channelId);
		if (channel == null) return;
		this.recomputeChannels(channel.guildId ?? null, [channelId as ChannelId]);
	}

	handleBulkChannelUpdate(action: {
		channels: Array<{
			id: string;
			guildId?: string;
		}>;
	}): void {
		const byGuild = new Map<GuildId | null, Array<ChannelId>>();
		for (const channel of action.channels) {
			const guildId = channel.guildId ?? null;
			let channels = byGuild.get(guildId as GuildId | null);
			if (channels == null) {
				channels = [];
				byGuild.set(guildId as GuildId | null, channels);
			}
			channels.push(channel.id as ChannelId);
		}
		for (const [guildId, channelIds] of byGuild.entries()) {
			this.recomputeChannels(guildId, channelIds);
		}
	}

	handleGuildSettingsUpdate(action: {guildId: string}): void {
		this.recomputeAll(action.guildId);
	}

	handleRecomputeAll(): void {
		this.handleConnectionOpen();
	}

	handleWindowFocus(): void {
		this.notifyChange();
	}

	handleGuildUpdate(guildId: string): void {
		this.recomputeAll(guildId);
	}

	handleGuildMemberUpdate(_userId: string, guildId: string): void {
		this.recomputeAll(guildId);
	}

	handleChannelDelete(channelId: string): void {
		const channel = Channels.getChannel(channelId);
		if (channel == null) return;
		this.recomputeChannels(channel.guildId ?? null, [channelId as ChannelId]);
	}

	handleUserGuildSettingsUpdate(): void {
		this.processUserGuildSettingsUpdates();
	}

	subscribe(callback: () => void): () => void {
		return reaction(
			() => this.version,
			() => callback(),
			{fireImmediately: true},
		);
	}
}

export default new GuildReadState();
