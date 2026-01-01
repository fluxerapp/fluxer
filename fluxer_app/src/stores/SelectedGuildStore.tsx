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

import {action, makeAutoObservable, reaction} from 'mobx';
import {ME} from '~/Constants';
import {makePersistent} from '~/lib/MobXPersistence';
import NavigationStore from '~/stores/NavigationStore';

const FAVORITES_ROUTE_ID = '@favorites';

class SelectedGuildStore {
	lastSelectedGuildId: string | null = null;
	selectedGuildId: string | null = null;

	selectionNonce: number = 0;

	private navigationDisposer: (() => void) | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		void this.initPersistence();
	}

	@action
	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'SelectedGuildStore', ['lastSelectedGuildId']);
		this.setupNavigationReaction();
	}

	private setupNavigationReaction(): void {
		this.navigationDisposer?.();
		this.navigationDisposer = reaction(
			() => NavigationStore.guildId,
			(guildId) => {
				const normalized = this.normalizeGuildFromNavigation(guildId);
				if (normalized) {
					this.applyNavigationGuild(normalized);
				} else {
					this.clearSelection();
				}
			},
			{
				fireImmediately: true,
			},
		);
	}

	private normalizeGuildFromNavigation(guildId: string | null): string | null {
		if (!guildId || guildId === ME || guildId === FAVORITES_ROUTE_ID) {
			return null;
		}
		return guildId;
	}

	@action
	selectGuild(guildId: string, _forceSync = false): void {
		if (!guildId) {
			return;
		}

		this.setGuild(guildId, {forceNonce: true});
	}

	@action
	syncCurrentGuild(): void {
		this.bumpNonce();
	}

	@action
	deselectGuild(): void {
		this.clearSelection();
	}

	private applyNavigationGuild(guildId: string): void {
		this.setGuild(guildId);
	}

	private setGuild(guildId: string, options?: {forceNonce?: boolean}): void {
		const hasChanged = guildId !== this.selectedGuildId;
		if (hasChanged) {
			this.lastSelectedGuildId = this.selectedGuildId;
			this.selectedGuildId = guildId;
			this.bumpNonce();
			return;
		}

		if (options?.forceNonce) {
			this.bumpNonce();
		}
	}

	private clearSelection(): void {
		if (this.selectedGuildId == null) {
			return;
		}

		this.lastSelectedGuildId = this.selectedGuildId;
		this.selectedGuildId = null;
		this.bumpNonce();
	}

	private bumpNonce(): void {
		this.selectionNonce++;
	}
}

export default new SelectedGuildStore();
