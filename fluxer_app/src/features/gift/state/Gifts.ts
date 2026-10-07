// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Gift} from '@app/features/gift/commands/GiftCommands';
import * as GiftCommands from '@app/features/gift/commands/GiftCommands';
import {AccountScopedWork} from '@app/features/platform/state/AccountScopedWork';
import {type InstanceHTTPTarget, instanceTargetIdentity} from '@app/features/platform/transport/InstanceHTTP';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {makeAutoObservable, observable, runInAction} from 'mobx';

interface GiftState {
	loading: boolean;
	error: Error | null;
	data: Gift | null;
	invalid?: boolean;
}

function giftResourceKey(code: string, target: InstanceHTTPTarget): string {
	return `${instanceTargetIdentity(target)}\u0000${code}`;
}

class Gifts {
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

	getGift(code: string, target: InstanceHTTPTarget): GiftState | null {
		return this.gifts.get(giftResourceKey(code, target)) ?? null;
	}

	markAsRedeemed(code: string, target: InstanceHTTPTarget): void {
		const resourceKey = giftResourceKey(code, target);
		const existingGift = this.gifts.get(resourceKey);
		if (existingGift?.data) {
			const updatedGift: Gift = {
				...existingGift.data,
				redeemed: true,
			};
			this.gifts.set(resourceKey, {
				...existingGift,
				data: updatedGift,
			});
		}
	}

	markAsInvalid(code: string, target: InstanceHTTPTarget): void {
		this.gifts.set(giftResourceKey(code, target), {
			loading: false,
			error: new Error('Gift code not found'),
			data: null,
			invalid: true,
		});
	}

	async fetchGift(code: string, target: InstanceHTTPTarget): Promise<Gift> {
		const resourceKey = giftResourceKey(code, target);
		const existingGift = this.gifts.get(resourceKey);
		if (existingGift?.invalid) {
			throw new Error('Gift code not found');
		}
		const existingRequest = this.pendingRequests.get(resourceKey);
		if (existingRequest) {
			return existingRequest;
		}
		if (existingGift?.data) {
			return existingGift.data;
		}
		this.gifts.set(resourceKey, {loading: true, error: null, data: null});
		const promise = GiftCommands.fetch(code, target);
		this.pendingRequests.set(resourceKey, promise);
		try {
			const gift = await promise;
			runInAction(() => {
				this.pendingRequests.delete(resourceKey);
				this.gifts.set(resourceKey, {loading: false, error: null, data: gift});
			});
			return gift;
		} catch (error) {
			runInAction(() => {
				this.pendingRequests.delete(resourceKey);
				this.gifts.set(resourceKey, {
					loading: false,
					error: error as Error,
					data: null,
					invalid: error instanceof HttpError && error.status === 404,
				});
			});
			throw error;
		}
	}

	reset(): void {
		this.gifts.clear();
		this.pendingRequests.clear();
	}
}

const gifts = new Gifts();

AccountScopedWork.registerCancellation(() => gifts.reset());

export default gifts;
