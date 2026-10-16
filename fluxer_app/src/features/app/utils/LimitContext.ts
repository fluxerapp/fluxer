// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import Guilds from '@app/features/guild/state/Guilds';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import SelectedGuild from '@app/features/navigation/state/SelectedGuild';
import Users from '@app/features/user/state/Users';

export interface LimitContextInput {
	traits?: Iterable<string> | null;
	guildFeatures?: Iterable<string> | null;
	guildId?: string | null;
}

interface LimitMatchContext {
	traits: Set<string>;
	guildFeatures: Set<string>;
}

class LimitContextClass {
	current(): LimitMatchContext {
		return this.build({});
	}

	build(options: LimitContextInput = {}): LimitMatchContext {
		const currentUser = Users.getCurrentUser();
		const traitSet = new Set<string>();
		const traitsSource = options.traits ?? currentUser?.traits ?? [];
		for (const trait of traitsSource) {
			if (trait) traitSet.add(trait);
		}
		const guildFeatureSet = new Set<string>();
		if (options.guildFeatures !== undefined) {
			for (const feature of options.guildFeatures ?? []) {
				if (feature) guildFeatureSet.add(feature);
			}
		} else {
			const guildFeatures = this._getGuildFeatures(options.guildId);
			if (guildFeatures) {
				for (const feature of guildFeatures) {
					if (feature) guildFeatureSet.add(feature);
				}
			}
		}
		return {
			traits: traitSet,
			guildFeatures: guildFeatureSet,
		};
	}

	private _getGuildFeatures(guildIdOverride?: string | null): Iterable<string> | null {
		if (guildIdOverride !== undefined) {
			if (!guildIdOverride) return null;
			const guild = Guilds.getGuild(guildIdOverride);
			return guild?.features ?? null;
		}
		const channelId = SelectedChannel.currentChannelId;
		const channel = channelId ? Channels.getChannel(channelId) : null;
		const guildId = channel?.guildId ?? SelectedGuild.selectedGuildId;
		if (!guildId) return null;
		const guild = Guilds.getGuild(guildId);
		return guild?.features ?? null;
	}

	premium(): LimitMatchContext {
		return this.build({traits: ['premium']});
	}

	stock(): LimitMatchContext {
		return this.premium();
	}

	free(): LimitMatchContext {
		const currentUser = Users.getCurrentUser();
		const traits = currentUser?.traits ? Array.from(currentUser.traits).filter((t) => t !== 'premium') : [];
		return this.build({traits});
	}

	restricted(): LimitMatchContext {
		return this.free();
	}
}

export const LimitContext = new LimitContextClass();
