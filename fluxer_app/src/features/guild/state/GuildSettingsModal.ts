// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildSettingsTabType} from '@app/features/user/components/settings_utils/GuildSettingsConstants';
import {makeAutoObservable} from 'mobx';

interface NavigationHandler {
	guildId: string;
	navigate: (tab: GuildSettingsTabType) => void;
}

class GuildSettingsModal {
	private activeHandler: NavigationHandler | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	register(handler: NavigationHandler): void {
		this.activeHandler = handler;
	}

	unregister(guildId: string): void {
		if (this.activeHandler?.guildId === guildId) {
			this.activeHandler = null;
		}
	}

	navigateToTab(guildId: string, tab: GuildSettingsTabType): boolean {
		if (!this.activeHandler) return false;
		if (this.activeHandler.guildId !== guildId) return false;
		this.activeHandler.navigate(tab);
		return true;
	}
}

export default new GuildSettingsModal();
