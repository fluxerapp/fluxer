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

import {makeAutoObservable} from 'mobx';
import type {GuildSettingsTabType} from '~/components/modals/utils/guildSettingsConstants';

interface NavigationHandler {
	guildId: string;
	navigate: (tab: GuildSettingsTabType) => void;
}

class GuildSettingsModalStore {
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

	isOpen(guildId?: string): boolean {
		if (!this.activeHandler) return false;
		if (guildId) return this.activeHandler.guildId === guildId;
		return true;
	}

	navigateToTab(guildId: string, tab: GuildSettingsTabType): boolean {
		if (!this.activeHandler) return false;
		if (this.activeHandler.guildId !== guildId) return false;
		this.activeHandler.navigate(tab);
		return true;
	}
}

export default new GuildSettingsModalStore();
