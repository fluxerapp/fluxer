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

import HttpClient from '~/lib/HttpClient';
import SudoPromptStore from '~/stores/SudoPromptStore';

class SudoStore {
	private token: string | null = null;
	private expiresAt: number | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	init(): void {
		HttpClient.setSudoTokenProvider(this.getValidToken);

		HttpClient.setSudoTokenListener((token) => {
			if (token) {
				this.setToken(token);
			}
			SudoPromptStore.handleTokenReceived(token);
		});

		HttpClient.setSudoTokenInvalidator(this.clearToken);
	}

	get hasValidTokenFlag(): boolean {
		return Boolean(this.token && this.expiresAt && Date.now() < this.expiresAt);
	}

	private getValidToken = (): string | null => {
		if (this.token && this.expiresAt && Date.now() < this.expiresAt) {
			return this.token;
		}
		return null;
	};

	setToken(token: string): void {
		this.token = token;
		this.expiresAt = Date.now() + 4.5 * 60 * 1000;
	}

	clearToken(): void {
		this.token = null;
		this.expiresAt = null;
	}

	hasValidToken(): boolean {
		return this.hasValidTokenFlag;
	}
}

export default new SudoStore();
