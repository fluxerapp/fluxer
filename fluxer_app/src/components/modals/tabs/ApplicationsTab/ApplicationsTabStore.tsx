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
import {Endpoints} from '~/Endpoints';
import HttpClient from '~/lib/HttpClient';
import type {DeveloperApplication} from '~/records/DeveloperApplicationRecord';
import {DeveloperApplicationRecord} from '~/records/DeveloperApplicationRecord';

enum NavigationState {
	LOADING_LIST = 'LOADING_LIST',
	LIST = 'LIST',
	LOADING_DETAIL = 'LOADING_DETAIL',
	DETAIL = 'DETAIL',
	ERROR = 'ERROR',
}

class ApplicationsTabStore {
	navigationState: NavigationState = NavigationState.LOADING_LIST;
	applicationOrder: Array<string> = [];
	applicationsById: Record<string, DeveloperApplicationRecord> = {};
	selectedAppId: string | null = null;
	error: string | null = null;
	isLoading: boolean = false;

	private listAbortController: AbortController | null = null;
	private detailAbortController: AbortController | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	get contentKey(): string {
		if (this.navigationState === NavigationState.DETAIL && this.selectedAppId) {
			return `applications-detail-${this.selectedAppId}`;
		}
		return 'applications-main';
	}

	get isDetailView(): boolean {
		return this.navigationState === NavigationState.DETAIL || this.navigationState === NavigationState.LOADING_DETAIL;
	}

	get isListView(): boolean {
		return this.navigationState === NavigationState.LIST || this.navigationState === NavigationState.LOADING_LIST;
	}

	get applications(): ReadonlyArray<DeveloperApplicationRecord> {
		const records: Array<DeveloperApplicationRecord> = [];
		for (const id of this.applicationOrder) {
			const record = this.applicationsById[id];
			if (record) {
				records.push(record);
			}
		}
		return records;
	}

	get selectedApplication(): DeveloperApplicationRecord | null {
		if (!this.selectedAppId) {
			return null;
		}
		return this.applicationsById[this.selectedAppId] ?? null;
	}

	get hasApplications(): boolean {
		return this.applicationOrder.length > 0;
	}

	async fetchApplications(options?: {showLoading?: boolean}): Promise<void> {
		if (this.listAbortController) {
			this.listAbortController.abort();
		}

		this.listAbortController = new AbortController();

		const shouldShowLoading = options?.showLoading ?? (!this.hasApplications && !this.isDetailView);

		runInAction(() => {
			if (shouldShowLoading) {
				this.navigationState = NavigationState.LOADING_LIST;
			}
			this.isLoading = shouldShowLoading;
			this.error = null;
		});

		try {
			const response = await HttpClient.get<Array<DeveloperApplication>>({
				url: Endpoints.OAUTH_APPLICATIONS_LIST,
				signal: this.listAbortController.signal,
			});

			runInAction(() => {
				this.mergeApplications(response.body);
				if (!this.isDetailView) {
					this.navigationState = NavigationState.LIST;
				}
			});
		} catch (err) {
			if ((err as DOMException).name === 'AbortError') {
				return;
			}

			console.error('[ApplicationsTabStore] Failed to fetch applications:', err);

			runInAction(() => {
				this.error = 'Failed to load applications';
				if (!this.isDetailView) {
					this.navigationState = NavigationState.ERROR;
				}
			});
		} finally {
			runInAction(() => {
				this.isLoading = false;
				this.listAbortController = null;
			});
		}
	}

	async fetchApplication(appId: string, options?: {showLoading?: boolean}): Promise<void> {
		if (this.detailAbortController) {
			this.detailAbortController.abort();
		}

		this.detailAbortController = new AbortController();

		const shouldShowLoading = Boolean(options?.showLoading);

		runInAction(() => {
			this.isLoading = shouldShowLoading;
			if (shouldShowLoading) {
				this.navigationState = NavigationState.LOADING_DETAIL;
			}
			this.error = null;
		});

		try {
			const response = await HttpClient.get<DeveloperApplication>({
				url: Endpoints.OAUTH_APPLICATION(appId),
				signal: this.detailAbortController.signal,
			});

			runInAction(() => {
				this.cacheApplication(response.body);
				this.navigationState = NavigationState.DETAIL;
			});
		} catch (err) {
			if ((err as DOMException).name === 'AbortError') {
				return;
			}

			console.error('[ApplicationsTabStore] Failed to fetch application:', err);
			runInAction(() => {
				this.error = 'Failed to load application details';
				this.navigationState = NavigationState.ERROR;
			});
		} finally {
			runInAction(() => {
				this.isLoading = false;
				this.detailAbortController = null;
			});
		}
	}

	async navigateToDetail(appId: string, initialApplication?: DeveloperApplication | null): Promise<void> {
		if (
			this.selectedAppId === appId &&
			(this.navigationState === NavigationState.DETAIL || this.navigationState === NavigationState.LOADING_DETAIL)
		) {
			return;
		}

		let cacheHit = Boolean(this.applicationsById[appId]);
		if (initialApplication) {
			this.cacheApplication(initialApplication);
			cacheHit = true;
		}

		runInAction(() => {
			this.selectedAppId = appId;
			this.error = null;
			this.navigationState = cacheHit ? NavigationState.DETAIL : NavigationState.LOADING_DETAIL;
		});

		await this.fetchApplication(appId, {showLoading: !cacheHit});
	}

	async navigateToList(): Promise<void> {
		if (this.detailAbortController) {
			this.detailAbortController.abort();
			this.detailAbortController = null;
		}

		runInAction(() => {
			this.selectedAppId = null;
			this.error = null;
			if (this.hasApplications) {
				this.navigationState = NavigationState.LIST;
			} else {
				this.navigationState = NavigationState.LOADING_LIST;
				this.isLoading = true;
			}
		});

		if (!this.hasApplications) {
			await this.fetchApplications({showLoading: true});
		}
	}

	@action
	clearError(): void {
		this.error = null;
		if (this.navigationState === NavigationState.ERROR) {
			if (this.isDetailView) {
				this.navigationState = NavigationState.LOADING_DETAIL;
			} else if (this.hasApplications) {
				this.navigationState = NavigationState.LIST;
			} else {
				this.navigationState = NavigationState.LOADING_LIST;
			}
		}
	}

	private mergeApplications(applications: Array<DeveloperApplication>): void {
		const nextById: Record<string, DeveloperApplicationRecord> = {...this.applicationsById};
		const nextOrder: Array<string> = [];

		for (const application of applications) {
			const record = DeveloperApplicationRecord.from(application);
			nextById[record.id] = record;
			nextOrder.push(record.id);
		}

		this.applicationOrder = nextOrder;
		this.applicationsById = nextById;
	}

	private cacheApplication(application: DeveloperApplication): DeveloperApplicationRecord {
		const record = DeveloperApplicationRecord.from(application);
		const nextById = {...this.applicationsById, [record.id]: record};
		let nextOrder: Array<string> = this.applicationOrder;

		if (!nextOrder.includes(record.id)) {
			nextOrder = [...nextOrder, record.id];
		}

		this.applicationsById = nextById;
		this.applicationOrder = nextOrder;

		return record;
	}
}

export default new ApplicationsTabStore();
