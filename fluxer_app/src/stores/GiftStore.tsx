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

import {makeAutoObservable, observable, runInAction} from 'mobx';
import type {Gift} from '~/actions/GiftActionCreators';
import * as GiftActionCreators from '~/actions/GiftActionCreators';

interface GiftState {
	loading: boolean;
	error: Error | null;
	data: Gift | null;
	invalid?: boolean;
}

class GiftStore {
	gifts: Map<string, GiftState> = observable.map();
	pendingRequests: Map<string, Promise<Gift>> = observable.map();

	constructor() {
		makeAutoObservable(
			this,
			{
				gifts: false,
				pendingRequests: false,
			},
			{autoBind: true},
		);
	}

	markAsRedeemed(code: string): void {
		const existingGift = this.gifts.get(code);
		if (existingGift?.data) {
			const updatedGift: Gift = {
				...existingGift.data,
				redeemed: true,
			};
			this.gifts.set(code, {
				...existingGift,
				data: updatedGift,
			});
		}
	}

	markAsInvalid(code: string): void {
		this.gifts.set(code, {
			loading: false,
			error: new Error('Gift code not found'),
			data: null,
			invalid: true,
		});
	}

	async fetchGift(code: string): Promise<Gift> {
		const existingGift = this.gifts.get(code);
		if (existingGift?.invalid) {
			throw new Error('Gift code not found');
		}

		const existingRequest = this.pendingRequests.get(code);
		if (existingRequest) {
			return existingRequest;
		}

		if (existingGift?.data) {
			return existingGift.data;
		}

		this.gifts.set(code, {loading: true, error: null, data: null});

		const promise = GiftActionCreators.fetch(code);

		this.pendingRequests.set(code, promise);

		try {
			const gift = await promise;
			runInAction(() => {
				this.pendingRequests.delete(code);
				this.gifts.set(code, {loading: false, error: null, data: gift});
			});
			return gift;
		} catch (error) {
			runInAction(() => {
				this.pendingRequests.delete(code);
				this.gifts.set(code, {
					loading: false,
					error: error as Error,
					data: null,
				});
			});
			throw error;
		}
	}
}

export default new GiftStore();
