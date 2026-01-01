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

import {makeAutoObservable, observable, reaction, runInAction} from 'mobx';
import {ChannelTypes, ME, Permissions} from '~/Constants';
import {Logger} from '~/lib/Logger';
import ChannelStore from './ChannelStore';
import GuildStore from './GuildStore';
import PermissionStore from './PermissionStore';
import ReadStateStore from './ReadStateStore';
import UserGuildSettingsStore from './UserGuildSettingsStore';

type GuildId = string;
type ChannelId = string;

const PRIVATE_CHANNEL_SENTINEL = ME;
const CAN_READ_PERMISSIONS = Permissions.VIEW_CHANNEL | Permissions.READ_MESSAGE_HISTORY;

const _logger = new Logger('GuildReadStateStore');

class GuildReadState {
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

	clone(): GuildReadState {
		const state = new GuildReadState();
		state.unread.set(this.unread.get());
		state.unreadChannelId.set(this.unreadChannelId.get());
		state.mentionCount.set(this.mentionCount.get());
		state.mentionChannels = observable.set(new Set(this.mentionChannels));
		state.sentinel.set(this.sentinel.get());
		return state;
	}
}

function canContributeToGuildUnread(
	channel: {
		id: string;
		type: number;
		guildId?: string | null;
		parentId?: string | null;
		isPrivate(): boolean;
		isGuildVocal?(): boolean;
	},
	mentionCount: number,
): boolean {
	if (channel.type === ChannelTypes.GUILD_VOICE && mentionCount === 0) {
		return false;
	}

	if (channel.isPrivate()) {
		return true;
	}

	if (
		!PermissionStore.can(CAN_READ_PERMISSIONS, {
			channelId: channel.id,
			guildId: channel.guildId ?? undefined,
		})
	) {
		return false;
	}

	const isMuted = UserGuildSettingsStore.isGuildOrCategoryOrChannelMuted(channel.guildId ?? null, channel.id);
	if (isMuted && mentionCount === 0) {
		return false;
	}

	return true;
}

class GuildReadStateStore {
	private readonly guildStates = observable.map(new Map<GuildId, GuildReadState>());
	private readonly unreadGuilds = observable.set(new Set<GuildId>());
	updateCounter = 0;
	private readStateReactionInstalled = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});

		this.installReadStateReaction();
		reaction(
			() => UserGuildSettingsStore.version,
			() => {
				this.processUserGuildSettingsUpdates();
			},
		);
	}

	private installReadStateReaction(): void {
		if (this.readStateReactionInstalled) return;
		if (ReadStateStore == null) {
			setTimeout(() => this.installReadStateReaction(), 0);
			return;
		}
		this.readStateReactionInstalled = true;

		reaction(
			() => ReadStateStore.version,
			() => {
				const {all, changes} = ReadStateStore.consumePendingChanges();

				if (all) {
					this.handleConnectionOpen();
					return;
				}

				if (changes.length === 0) {
					return;
				}

				const byGuild = new Map<GuildId | null, Array<ChannelId>>();
				for (const {channelId, guildId} of changes) {
					let list = byGuild.get(guildId);
					if (list == null) {
						list = [];
						byGuild.set(guildId, list);
					}
					list.push(channelId);
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

	private getOrCreate(guildId: GuildId | null): GuildReadState {
		const id = guildId ?? PRIVATE_CHANNEL_SENTINEL;
		let state = this.guildStates.get(id);
		if (state == null) {
			state = new GuildReadState();
			this.guildStates.set(id, state);
		}
		return state;
	}

	private notifyChange(): void {
		this.updateCounter++;
	}

	private incrementSentinel(guildId: GuildId | null): void {
		const state = this.getOrCreate(guildId);
		state.incrementSentinel();
		this.notifyChange();
	}

	private recomputeChannels(guildId: GuildId | null, channelIds: Array<ChannelId>): boolean {
		const id = guildId ?? PRIVATE_CHANNEL_SENTINEL;
		const prevState = this.getOrCreate(id);
		const newState = prevState.clone();

		let foundUnread = false;
		let shouldClearUnreadChannelId = false;

		for (const channelId of channelIds) {
			const channel = ChannelStore.getChannel(channelId);
			if (channel == null) {
				newState.mentionChannels.delete(channelId);
				if (newState.unreadChannelId.get() === channelId) {
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

			const mentionCount = ReadStateStore.getMentionCount(channelId);
			const hasUnread = ReadStateStore.hasUnread(channelId);
			const canContribute = canContributeToGuildUnread(channel, mentionCount);

			if (mentionCount > 0 && canContribute) {
				newState.mentionChannels.add(channelId);
			} else {
				newState.mentionChannels.delete(channelId);
			}

			if (guildId != null && !foundUnread && hasUnread && canContribute) {
				foundUnread = true;
				newState.unreadChannelId.set(channelId);
			} else if (!hasUnread && newState.unreadChannelId.get() === channelId) {
				shouldClearUnreadChannelId = true;
			}
		}

		newState.unread.set(foundUnread);

		if (!foundUnread && shouldClearUnreadChannelId) {
			newState.unreadChannelId.set(null);
		}

		let mentionTotal = 0;
		for (const channelId of newState.mentionChannels) {
			mentionTotal += ReadStateStore.getMentionCount(channelId);
		}
		newState.mentionCount.set(mentionTotal);

		if (newState.unread.get() !== prevState.unread.get() && !newState.unread.get()) {
			const oldUnreadChannelId = prevState.unreadChannelId.get();
			const oldUnreadChannel = oldUnreadChannelId != null ? ChannelStore.getChannel(oldUnreadChannelId) : null;

			if (
				oldUnreadChannel != null &&
				!channelIds.includes(oldUnreadChannel.id) &&
				ReadStateStore.hasUnread(oldUnreadChannel.id) &&
				canContributeToGuildUnread(oldUnreadChannel, 0)
			) {
				return this.recomputeAll(guildId);
			}
		}

		return this.commitState(id, newState, prevState);
	}

	private recomputeAll(guildId: GuildId | null, skipIfMuted = false): boolean {
		const id = guildId ?? PRIVATE_CHANNEL_SENTINEL;
		const newState = new GuildReadState();

		if (guildId == null) {
			const privateChannels = ChannelStore.getPrivateChannels();
			for (const channel of privateChannels) {
				const mentionCount = ReadStateStore.getMentionCount(channel.id);
				const canContribute = canContributeToGuildUnread(channel, mentionCount);

				if (mentionCount > 0 && canContribute) {
					newState.mentionCount.set(newState.mentionCount.get() + mentionCount);
					newState.mentionChannels.add(channel.id);
				}

				if (!newState.unread.get() && ReadStateStore.hasUnread(channel.id) && canContributeToGuildUnread(channel, 0)) {
					newState.unread.set(true);
					newState.unreadChannelId.set(channel.id);
				}
			}
		} else {
			const isGuildMuted = UserGuildSettingsStore.isMuted(guildId);

			if (isGuildMuted && skipIfMuted) {
				return false;
			}

			const mutedChannels = UserGuildSettingsStore.getMutedChannels(guildId);

			const channels = ChannelStore.getGuildChannels(guildId);
			for (const channel of channels) {
				const isChannelMuted =
					isGuildMuted ||
					mutedChannels.has(channel.id) ||
					(channel.parentId != null && mutedChannels.has(channel.parentId));

				const mentionCount = ReadStateStore.getMentionCount(channel.id);
				const hasUnread = ReadStateStore.hasUnread(channel.id);

				const hasMention = mentionCount > 0;

				if (!hasMention && isChannelMuted) {
					continue;
				}

				const shouldShowUnread = !newState.unread.get() && (!isChannelMuted || hasMention) && hasUnread;

				if ((shouldShowUnread || hasMention) && canContributeToGuildUnread(channel, mentionCount)) {
					if (shouldShowUnread) {
						newState.unread.set(true);
						newState.unreadChannelId.set(channel.id);
					}

					if (hasMention) {
						newState.mentionCount.set(newState.mentionCount.get() + mentionCount);
						newState.mentionChannels.add(channel.id);
					}
				}
			}
		}

		const prevState = this.getOrCreate(id);
		return this.commitState(id, newState, prevState);
	}

	private commitState(guildId: string, newState: GuildReadState, prevState: GuildReadState): boolean {
		if (
			newState.unread.get() === prevState.unread.get() &&
			newState.unreadChannelId.get() === prevState.unreadChannelId.get() &&
			newState.mentionCount.get() === prevState.mentionCount.get()
		) {
			return false;
		}

		runInAction(() => {
			this.guildStates.set(guildId, newState);

			if (guildId !== PRIVATE_CHANNEL_SENTINEL) {
				if (newState.unread.get()) {
					this.unreadGuilds.add(guildId);
				} else {
					this.unreadGuilds.delete(guildId);
				}
			}

			this.incrementSentinel(guildId === PRIVATE_CHANNEL_SENTINEL ? null : guildId);
		});
		return true;
	}

	private processUserGuildSettingsUpdates(): void {
		const updatedGuilds = UserGuildSettingsStore.consumePendingGuildUpdates();
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

	get hasAnyUnread(): boolean {
		return this.unreadGuilds.size > 0;
	}

	hasUnread(guildId: GuildId): boolean {
		return this.unreadGuilds.has(guildId);
	}

	getMentionCount(guildId: GuildId | null): number {
		const id = guildId ?? PRIVATE_CHANNEL_SENTINEL;
		const state = this.guildStates.get(id);
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
		const state = this.guildStates.get(PRIVATE_CHANNEL_SENTINEL);
		return state?.mentionCount.get() ?? 0;
	}

	getMentionCountForPrivateChannel(channelId: ChannelId): number {
		return ReadStateStore.getMentionCount(channelId);
	}

	getGuildChangeSentinel(guildId: GuildId | null): number {
		const id = guildId ?? PRIVATE_CHANNEL_SENTINEL;
		const state = this.guildStates.get(id);
		return state?.sentinel.get() ?? 0;
	}

	getGuildHasUnreadIgnoreMuted(guildId: GuildId): boolean {
		const channels = ChannelStore.getGuildChannels(guildId);

		for (const channel of channels) {
			if (channel.type === ChannelTypes.GUILD_VOICE && ReadStateStore.getMentionCount(channel.id) === 0) {
				continue;
			}

			if (PermissionStore.can(CAN_READ_PERMISSIONS, channel) && ReadStateStore.hasUnreadOrMentions(channel.id)) {
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

		for (const guildId of GuildStore.getGuildIds()) {
			this.recomputeAll(guildId);
		}

		this.notifyChange();
	}

	handleGuildCreate(action: {guild: {id: GuildId}}): void {
		this.recomputeAll(action.guild.id);
	}

	handleGuildDelete(action: {guild: {id: GuildId}}): void {
		this.guildStates.delete(action.guild.id);
		this.unreadGuilds.delete(action.guild.id);
		this.notifyChange();
	}

	handleChannelUpdate(action: {channel: {id: ChannelId; guildId?: GuildId}}): void {
		this.recomputeChannels(action.channel.guildId ?? null, [action.channel.id]);
	}

	handleGenericUpdate(channelId: ChannelId): void {
		const channel = ChannelStore.getChannel(channelId);
		if (channel == null) return;
		this.recomputeChannels(channel.guildId ?? null, [channelId]);
	}

	handleBulkChannelUpdate(action: {channels: Array<{id: ChannelId; guildId?: GuildId}>}): void {
		const byGuild = new Map<GuildId | null, Array<ChannelId>>();

		for (const channel of action.channels) {
			const guildId = channel.guildId ?? null;
			let channels = byGuild.get(guildId);
			if (channels == null) {
				channels = [];
				byGuild.set(guildId, channels);
			}
			channels.push(channel.id);
		}

		for (const [guildId, channelIds] of byGuild.entries()) {
			this.recomputeChannels(guildId, channelIds);
		}
	}

	handleGuildSettingsUpdate(action: {guildId: GuildId}): void {
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
		const channel = ChannelStore.getChannel(channelId);
		if (channel == null) return;
		this.recomputeChannels(channel.guildId ?? null, [channelId]);
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

export default new GuildReadStateStore();
