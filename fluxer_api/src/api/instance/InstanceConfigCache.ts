import {randomUUID} from 'node:crypto';
import {Logger} from '@app/api/Logger';
import {parseJsonRecord} from '@app/api/utils/JsonBoundaryUtils';
import {RefreshSubscription, RefreshSubscriptionUnavailableError} from '@app/api/utils/RefreshSubscription';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';

interface InstanceConfigCacheOptions {
	provider: IKVProvider | null;
	channel: string;
	load: () => Promise<Map<string, string>>;
	onRefresh: (snapshot: ReadonlyMap<string, string>) => void;
}

export class InstanceConfigCache {
	readonly sourceId = randomUUID();
	private active = true;
	private snapshot: Map<string, string> | null = null;
	private writesDuringRefresh: Map<string, string> | null = null;
	private initialization: Promise<boolean> | null = null;
	private shutdownCompletion: Promise<void> | null = null;
	private readonly subscription: RefreshSubscription;

	constructor(
		private readonly options: InstanceConfigCacheOptions,
		private readonly previousShutdown: Promise<void> = Promise.resolve(),
	) {
		this.subscription = new RefreshSubscription({
			name: 'InstanceConfigCache',
			channels: [options.channel],
			shouldRefresh: (_channel, message) => parseJsonRecord(message)?.source_id !== this.sourceId,
			drainInitialNotifications: true,
			refresh: () => this.refresh(),
			onRefreshError: (error) => {
				Logger.error({error}, 'Failed to refresh instance config cache from pubsub');
			},
		});
	}

	assertActive(): void {
		if (!this.active) throw new Error('Instance config cache is shutting down or shut down');
	}

	async getSnapshot(): Promise<ReadonlyMap<string, string> | null> {
		this.assertActive();
		if (!this.subscription.isReady) {
			this.initialization ??= this.initialize().finally(() => {
				this.initialization = null;
			});
			const cached = await this.initialization;
			this.assertActive();
			if (!cached) return null;
		}
		if (this.snapshot === null) throw new Error('Instance config cache is ready without a snapshot');
		return this.snapshot;
	}

	private async initialize(): Promise<boolean> {
		await this.previousShutdown;
		this.assertActive();
		if (this.options.provider === null) return false;
		try {
			await this.subscription.start(this.options.provider);
			return true;
		} catch (error) {
			this.assertActive();
			if (!(error instanceof RefreshSubscriptionUnavailableError)) throw error;
			Logger.error({error}, 'Instance config subscription unavailable, reading directly from the database');
			return false;
		}
	}

	private async refresh(): Promise<void> {
		this.assertActive();
		if (this.writesDuringRefresh !== null) throw new Error('Instance config refresh already in progress');
		const writes = new Map<string, string>();
		this.writesDuringRefresh = writes;
		try {
			const snapshot = await this.options.load();
			if (!this.active) return;
			for (const [key, value] of writes) snapshot.set(key, value);
			this.options.onRefresh(snapshot);
			this.snapshot = snapshot;
		} finally {
			this.writesDuringRefresh = null;
		}
	}

	update(key: string, value: string): void {
		if (!this.active) return;
		this.snapshot?.set(key, value);
		this.writesDuringRefresh?.set(key, value);
	}

	shutdown(): Promise<void> {
		if (this.shutdownCompletion) return this.shutdownCompletion;
		this.active = false;
		this.snapshot = null;
		this.shutdownCompletion = this.close();
		return this.shutdownCompletion;
	}

	private async close(): Promise<void> {
		const [subscription, previous] = await Promise.allSettled([
			this.subscription.stop(),
			this.previousShutdown,
			this.initialization,
		]);
		const errors: Array<unknown> = [subscription, previous].flatMap((result) =>
			result.status === 'rejected' ? [result.reason] : [],
		);
		if (errors.length === 1) throw errors[0];
		if (errors.length > 1) throw new AggregateError(errors, 'Failed to shut down instance config cache');
	}
}
