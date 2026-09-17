// SPDX-License-Identifier: AGPL-3.0-or-later

import Navigation from '@app/features/navigation/state/Navigation';
import {makePersistent} from '@app/features/platform/utils/MobXPersistence';
import {ME} from '@fluxer/constants/src/AppConstants';
import {makeAutoObservable, reaction} from 'mobx';

const FAVORITES_ROUTE_ID = '@favorites';

class SelectedGuild {
	lastSelectedGuildId: string | null = null;
	selectedGuildId: string | null = null;
	selectionNonce: number = 0;
	private navigationDisposer: (() => void) | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		void this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'SelectedGuild', ['lastSelectedGuildId']);
		this.setupNavigationReaction();
	}

	private setupNavigationReaction(): void {
		this.navigationDisposer?.();
		this.navigationDisposer = reaction(
			() => Navigation.guildId,
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

	selectGuild(guildId: string, _forceSync = false): void {
		if (!guildId) {
			return;
		}
		this.setGuild(guildId, {forceNonce: true});
	}

	syncCurrentGuild(): void {
		this.bumpNonce();
	}

	deselectGuild(): void {
		this.clearSelection();
	}

	private applyNavigationGuild(guildId: string): void {
		this.setGuild(guildId);
	}

	private setGuild(
		guildId: string,
		options?: {
			forceNonce?: boolean;
		},
	): void {
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

export default new SelectedGuild();
