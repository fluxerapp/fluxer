// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/api/Logger';
import {RefreshSubscription, type RefreshSubscriptionOptions} from '@app/api/utils/RefreshSubscription';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';

export abstract class RefreshedCache {
	private kvClient: IKVProvider | null = null;
	private readonly refreshSubscription: RefreshSubscription;
	constructor(
		private readonly options: Pick<
			RefreshSubscriptionOptions,
			'name' | 'channels' | 'periodicIntervalMs' | 'onPeriodicRefreshError'
		>,
	) {
		this.refreshSubscription = new RefreshSubscription({
			...options,
			refresh: () => this.refresh(),
			escalateAfterConsecutiveFailures: 5,
		});
	}
	protected abstract load(): Promise<void>;
	async refresh(): Promise<void> {
		await this.load();
		this.refreshSubscription.resetFailures();
	}
	setRefreshSubscriber(kvClient: IKVProvider | null): void {
		this.kvClient = kvClient;
	}
	initialize(): Promise<void> {
		return this.refreshSubscription.start(this.kvClient);
	}
	shutdown(): Promise<void> {
		return this.refreshSubscription.stop();
	}
	protected resetRefreshState(): void {
		void this.shutdown().catch((error) => Logger.error({error}, `Failed to shut down ${this.options.name}`));
		this.kvClient = null;
		this.refreshSubscription.resetFailures();
	}
}
