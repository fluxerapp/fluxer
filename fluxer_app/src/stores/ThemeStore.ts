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

import {action, makeAutoObservable, runInAction} from 'mobx';
import RuntimeConfigStore from '~/stores/RuntimeConfigStore';
import {buildMediaProxyURL} from '~/utils/MediaProxyUtils';

export interface ThemeData {
	id: string;
	css: string;
}

export interface ThemeState {
	loading: boolean;
	error: Error | null;
	data: ThemeData | null;
}

function buildThemeUrl(endpoint: string, themeId: string): string {
	const base = endpoint.replace(/\/$/, '');
	return `${base}/themes/${themeId}.css`;
}

class ThemeStore {
	themes: Map<string, ThemeState> = new Map();
	pendingRequests: Map<string, Promise<ThemeData>> = new Map();

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	getTheme(themeId: string): ThemeState | null {
		return this.themes.get(themeId) || null;
	}

	fetchTheme = action(async (themeId: string): Promise<ThemeData> => {
		const existingRequest = this.pendingRequests.get(themeId);
		if (existingRequest) {
			return existingRequest;
		}

		const existingTheme = this.themes.get(themeId);
		if (existingTheme?.data) {
			return existingTheme.data;
		}

		runInAction(() => {
			this.themes = new Map(this.themes).set(themeId, {loading: true, error: null, data: null});
		});

		const mediaEndpoint = RuntimeConfigStore.mediaEndpoint;
		if (!mediaEndpoint) {
			const error = new Error('Media endpoint not configured');
			runInAction(() => {
				this.themes = new Map(this.themes).set(themeId, {loading: false, error, data: null});
			});
			throw error;
		}

		const promise = this.doFetch(themeId, mediaEndpoint);

		runInAction(() => {
			this.pendingRequests = new Map(this.pendingRequests).set(themeId, promise);
		});

		try {
			const themeData = await promise;
			runInAction(() => {
				const newPendingRequests = new Map(this.pendingRequests);
				newPendingRequests.delete(themeId);
				this.themes = new Map(this.themes).set(themeId, {loading: false, error: null, data: themeData});
				this.pendingRequests = newPendingRequests;
			});
			return themeData;
		} catch (error) {
			runInAction(() => {
				const newPendingRequests = new Map(this.pendingRequests);
				newPendingRequests.delete(themeId);
				this.themes = new Map(this.themes).set(themeId, {loading: false, error: error as Error, data: null});
				this.pendingRequests = newPendingRequests;
			});
			throw error;
		}
	});

	private async doFetch(themeId: string, mediaEndpoint: string): Promise<ThemeData> {
		const response = await fetch(buildMediaProxyURL(buildThemeUrl(mediaEndpoint, themeId)));
		if (!response.ok) {
			throw new Error('Theme not found');
		}
		const css = await response.text();
		return {id: themeId, css};
	}
}

export default new ThemeStore();
