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

import {makeAutoObservable, runInAction} from 'mobx';
import * as PackActionCreators from '~/actions/PackActionCreators';
import type {PackDashboardResponse} from '~/types/PackTypes';

type FetchStatus = 'idle' | 'pending' | 'success' | 'error';

class PackStore {
	dashboard: PackDashboardResponse | null = null;
	fetchStatus: FetchStatus = 'idle';
	error: Error | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	async fetch(): Promise<PackDashboardResponse> {
		if (this.fetchStatus === 'pending') {
			throw new Error('Pack fetch already in progress');
		}
		this.fetchStatus = 'pending';
		this.error = null;

		try {
			const dashboard = await PackActionCreators.list();
			runInAction(() => {
				this.dashboard = dashboard;
				this.fetchStatus = 'success';
			});
			return dashboard;
		} catch (err) {
			runInAction(() => {
				this.fetchStatus = 'error';
				this.error = err instanceof Error ? err : new Error('Failed to load packs');
			});
			throw err;
		}
	}

	async refresh(): Promise<void> {
		await this.fetch();
	}

	async createPack(type: 'emoji' | 'sticker', name: string, description?: string | null): Promise<void> {
		await PackActionCreators.create(type, name, description);
		await this.refresh();
	}

	async updatePack(packId: string, data: {name?: string; description?: string | null}): Promise<void> {
		await PackActionCreators.update(packId, data);
		await this.refresh();
	}

	async deletePack(packId: string): Promise<void> {
		await PackActionCreators.remove(packId);
		await this.refresh();
	}

	async installPack(packId: string): Promise<void> {
		await PackActionCreators.install(packId);
		await this.refresh();
	}

	async uninstallPack(packId: string): Promise<void> {
		await PackActionCreators.uninstall(packId);
		await this.refresh();
	}
}

export default new PackStore();
