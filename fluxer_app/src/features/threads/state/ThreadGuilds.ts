// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {cleanupChannelLocalState} from '@app/features/channel/events/ChannelDelete';
import Channels from '@app/features/channel/state/Channels';
import ForumPosts from '@app/features/forum/state/ForumPosts';
import ForumReadState from '@app/features/forum/state/ForumReadState';
import type {GuildReadyData} from '@app/features/gateway/types/GatewayGuildTypes';
import Messages from '@app/features/messaging/state/MessagingMessages';
import Navigation from '@app/features/navigation/state/Navigation';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import ChannelThreads from '@app/features/threads/state/ChannelThreads';
import ThreadPanel from '@app/features/threads/state/ThreadPanel';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as PopoutCommands from '@app/features/ui/commands/PopoutCommands';
import {THREAD_ONLY_CHANNEL_TYPES} from '@fluxer/constants/src/ThreadConstants';
import {makeAutoObservable, observable} from 'mobx';

class ThreadGuilds {
	private readonly activeGuildIds = observable.set<string>();
	readonly purgedThreadIds = new Set<string>();
	private readonly purgedByGuild = new Map<string, ReadonlyArray<string>>();

	constructor() {
		makeAutoObservable<this, 'activeGuildIds' | 'purgedByGuild'>(
			this,
			{activeGuildIds: false, purgedThreadIds: false, purgedByGuild: false},
			{autoBind: true},
		);
	}

	get anyActive(): boolean {
		return this.activeGuildIds.size > 0;
	}

	isActive(guildId: string | null | undefined): boolean {
		return guildId != null && this.activeGuildIds.has(guildId);
	}

	get guildIds(): ReadonlyArray<string> {
		return Array.from(this.activeGuildIds);
	}

	handleGatewayReady(guilds: ReadonlyArray<GuildReadyData>): void {
		const present = new Set(guilds.map((guild) => guild.id));
		for (const guildId of Array.from(this.activeGuildIds)) {
			if (!present.has(guildId)) this.deactivate(guildId);
		}
		for (const guild of guilds) {
			if (!guild.unavailable) this.handleGuild(guild);
		}
		const {guildId, channelId, threadId} = Navigation;
		if (threadId == null || guildId == null || channelId == null || this.activeGuildIds.has(guildId)) return;
		const readyGuild = guilds.find((guild) => guild.id === guildId && !guild.unavailable);
		if (!readyGuild) return;
		const channel = readyGuild.channels.find((candidate) => candidate.id === channelId);
		const keepChannel = channel !== undefined && !THREAD_ONLY_CHANNEL_TYPES.has(channel.type);
		RouterUtils.replaceWith(Routes.guildChannel(guildId, keepChannel ? channelId : undefined));
	}

	handleGuild(guild: GuildReadyData): void {
		if ('threads' in guild && guild.threads !== undefined) {
			this.activate(guild.id);
		} else if (this.activeGuildIds.has(guild.id)) {
			this.deactivate(guild.id);
		}
	}

	handleGuildDelete(guildId: string, unavailable: boolean): void {
		ChannelThreads.handleGuildRemoved(guildId);
		if (!unavailable) this.activeGuildIds.delete(guildId);
	}

	private activate(guildId: string): void {
		const purged = this.purgedByGuild.get(guildId);
		if (purged) {
			for (const threadId of purged) this.purgedThreadIds.delete(threadId);
			this.purgedByGuild.delete(guildId);
		}
		this.activeGuildIds.add(guildId);
	}

	private deactivate(guildId: string): void {
		this.activeGuildIds.delete(guildId);
		this.purgeGuild(guildId);
	}

	purgeGuild(guildId: string): void {
		const openThreadId = Navigation.guildId === guildId ? (Navigation.threadId ?? Navigation.channelId) : null;
		const fallbackChannelId =
			Navigation.threadId != null
				? Navigation.channelId
				: openThreadId != null
					? (Channels.getChannel(openThreadId)?.parentId ?? null)
					: null;
		const createTarget = ThreadPanel.createTarget;
		if (createTarget && Channels.getChannel(createTarget.parentId)?.guildId === guildId) ThreadPanel.closeCreate();
		const threadIds = ChannelThreads.purgeGuild(guildId);
		for (const threadId of threadIds) this.purgedThreadIds.add(threadId);
		if (threadIds.length > 0) this.purgedByGuild.set(guildId, threadIds);
		for (const channel of [...Channels.getGuildChannels(guildId)]) {
			if (THREAD_ONLY_CHANNEL_TYPES.has(channel.type)) {
				cleanupChannelLocalState(channel.toJSON());
				ForumPosts.purgeForum(channel.id, threadIds);
			}
		}
		ForumReadState.purgeGuild(guildId);
		Messages.handleGuildThreadsPurged(guildId);
		Messages.handleCleanup();
		ContextMenuCommands.close();
		PopoutCommands.closeAll();
		if (openThreadId != null && threadIds.includes(openThreadId)) {
			const keepChannel = fallbackChannelId != null && Channels.getChannel(fallbackChannelId) !== undefined;
			RouterUtils.replaceWith(Routes.guildChannel(guildId, keepChannel ? fallbackChannelId : undefined));
		}
	}

	isPurgedEvent(data: unknown): boolean {
		if (this.purgedThreadIds.size === 0 || data == null || typeof data !== 'object') return false;
		const channelId = (data as {channel_id?: unknown}).channel_id;
		return typeof channelId === 'string' && this.purgedThreadIds.has(channelId);
	}

	reset(): void {
		this.activeGuildIds.clear();
		this.purgedThreadIds.clear();
		this.purgedByGuild.clear();
	}
}

export default new ThreadGuilds();
