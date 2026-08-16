// SPDX-License-Identifier: AGPL-3.0-or-later

import {action, makeObservable, observable} from 'mobx';

const BOTTOM_TOLERANCE = 2;

interface ChannelDimensions {
	channelId: string;
	scrollTop: number;
	scrollHeight: number;
	offsetHeight: number;
}

interface GuildDimensions {
	guildId: string;
	scrollTop: number | null;
	scrollTo: number | null;
}

interface GuildListDimensions {
	scrollTop: number;
}

function createDefaultGuildDimensions(guildId: string): GuildDimensions {
	return {
		guildId,
		scrollTop: null,
		scrollTo: null,
	};
}

class Dimension {
	channelDimensions = observable.map(new Map<string, ChannelDimensions>());
	guildDimensions = observable.map(new Map<string, GuildDimensions>());
	guildListDimensions = observable.box<GuildListDimensions>({scrollTop: 0}, {deep: false});

	constructor() {
		makeObservable(this, {
			updateChannelDimensions: action,
			updateGuildDimensions: action,
			updateGuildListDimensions: action,
			clearChannelDimensions: action,
		});
	}

	percentageScrolled(channelId: string): number {
		const dimensions = this.channelDimensions.get(channelId);
		if (dimensions != null) {
			const {scrollTop, scrollHeight} = dimensions;
			return scrollTop / scrollHeight;
		}
		return 1;
	}

	getChannelDimensions(channelId: string): ChannelDimensions | undefined {
		return this.channelDimensions.get(channelId);
	}

	getGuildDimensions(guildId: string): GuildDimensions {
		const existing = this.guildDimensions.get(guildId);
		if (existing != null) {
			return existing;
		}
		return createDefaultGuildDimensions(guildId);
	}

	getGuildListDimensions(): GuildListDimensions {
		return this.guildListDimensions.get();
	}

	isAtBottom(channelId: string): boolean {
		const dimensions = this.channelDimensions.get(channelId);
		if (dimensions == null) {
			return true;
		}
		const {scrollTop, scrollHeight, offsetHeight} = dimensions;
		return scrollTop >= scrollHeight - offsetHeight - BOTTOM_TOLERANCE;
	}

	updateChannelDimensions(
		channelId: string,
		scrollTop: number | null,
		scrollHeight: number | null,
		offsetHeight: number | null,
		callback?: () => void,
	): void {
		const existing = this.channelDimensions.get(channelId);
		if (scrollTop == null || scrollHeight == null || offsetHeight == null) {
			if (existing != null) {
				this.channelDimensions.delete(channelId);
			}
			callback?.();
			return;
		}
		const newDimensions: ChannelDimensions = {
			channelId,
			scrollTop,
			scrollHeight,
			offsetHeight,
		};
		if (
			existing == null ||
			existing.scrollTop !== scrollTop ||
			existing.scrollHeight !== scrollHeight ||
			existing.offsetHeight !== offsetHeight
		) {
			this.channelDimensions.set(channelId, newDimensions);
		}
		callback?.();
	}

	updateGuildDimensions(guildId: string, scrollTop?: number | null, scrollTo?: number | null): void {
		let dimensions = this.guildDimensions.get(guildId);
		if (dimensions == null) {
			dimensions = createDefaultGuildDimensions(guildId);
		}
		const updated: GuildDimensions = {
			...dimensions,
			scrollTop: scrollTop !== undefined ? scrollTop : dimensions.scrollTop,
			scrollTo: scrollTo !== undefined ? scrollTo : dimensions.scrollTo,
		};
		this.guildDimensions.set(guildId, updated);
	}

	updateGuildListDimensions(scrollTop: number): void {
		if (this.guildListDimensions.get().scrollTop === scrollTop) {
			return;
		}
		this.guildListDimensions.set({scrollTop});
	}

	clearChannelDimensions(channelId: string): void {
		this.channelDimensions.delete(channelId);
	}

	scrollGuildListTo(guildId: string, scrollTo: number): void {
		this.updateGuildDimensions(guildId, undefined, scrollTo);
	}

	clearGuildListScrollTo(guildId: string): void {
		this.updateGuildDimensions(guildId, undefined, null);
	}

	handleCallCreate(channelId: string): void {
		this.clearChannelDimensions(channelId);
	}
}

export default new Dimension();
