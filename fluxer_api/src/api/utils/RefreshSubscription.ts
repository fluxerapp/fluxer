import type {IKVProvider, IKVSubscription} from '@pkgs/kv_client/src/IKVProvider';

type RefreshTrigger = 'notification' | 'periodic';

interface RefreshSubscriptionOptions {
	name: string;
	channels: ReadonlyArray<string>;
	refresh: () => Promise<void>;
	onRefreshError: (error: unknown, trigger: RefreshTrigger) => void;
	shouldRefresh?: (channel: string, message: string) => boolean;
	drainInitialNotifications?: boolean;
	periodicIntervalMs?: () => number | null;
	closeMode?: 'disconnect' | 'quit';
}

export class RefreshSubscriptionUnavailableError extends Error {
	constructor(name: string, cause: unknown) {
		super(`Failed to subscribe ${name}`, {cause});
		this.name = 'RefreshSubscriptionUnavailableError';
	}
}

export class RefreshSubscription {
	private session: RefreshSubscriptionSession | null = null;

	constructor(private readonly options: RefreshSubscriptionOptions) {}

	get isActive(): boolean {
		return this.session?.isActive ?? false;
	}

	get isReady(): boolean {
		return this.session?.isReady ?? false;
	}

	async start(provider: IKVProvider | null): Promise<void> {
		const session = (this.session ??= new RefreshSubscriptionSession(this.options, provider));
		try {
			await session.start();
		} catch (error) {
			try {
				await session.stop();
			} catch (cleanupError) {
				throw new AggregateError([error, cleanupError], `Failed to initialize and close ${this.options.name}`);
			} finally {
				if (this.session === session) this.session = null;
			}
			if (error instanceof RefreshSubscriptionUnavailableError) throw error;
			throw new Error(`Failed to initialize ${this.options.name}`, {cause: error});
		}
	}

	async stop(): Promise<void> {
		const session = this.session;
		if (!session) return;
		try {
			await session.stop();
		} finally {
			if (this.session === session) this.session = null;
		}
	}
}

class RefreshSubscriptionSession {
	private readonly subscription: IKVSubscription | null;
	private phase: 'starting' | 'running' | 'stopping' | 'stopped' = 'starting';
	private startCompletion: Promise<void> | null = null;
	private stopCompletion: Promise<void> | null = null;
	private refreshTask: Promise<void> | null = null;
	private pendingRefresh: RefreshTrigger | null = null;
	private timer: NodeJS.Timeout | null = null;
	private readonly messageHandler = (channel: string, message: string): void => {
		if (!this.options.channels.includes(channel)) return;
		if (this.options.shouldRefresh && !this.options.shouldRefresh(channel, message)) return;
		this.requestRefresh('notification');
	};

	constructor(
		private readonly options: RefreshSubscriptionOptions,
		provider: IKVProvider | null,
	) {
		try {
			this.subscription = provider?.duplicate() ?? null;
		} catch (error) {
			throw new RefreshSubscriptionUnavailableError(options.name, error);
		}
	}

	get isReady(): boolean {
		return this.phase === 'running';
	}

	get isActive(): boolean {
		return this.phase === 'starting' || this.phase === 'running';
	}

	start(): Promise<void> {
		this.assertActive();
		this.startCompletion ??= Promise.resolve().then(() => this.initialize());
		return this.startCompletion;
	}

	private async initialize(): Promise<void> {
		this.assertActive();
		if (this.subscription) {
			this.subscription.on('message', this.messageHandler);
			try {
				await this.subscription.connect();
				this.assertActive();
				for (const channel of this.options.channels) {
					await this.subscription.subscribe(channel);
					this.assertActive();
				}
			} catch (error) {
				this.assertActive();
				throw new RefreshSubscriptionUnavailableError(this.options.name, error);
			}
		}
		await this.options.refresh();
		this.assertActive();
		while (this.options.drainInitialNotifications && this.pendingRefresh !== null) {
			this.pendingRefresh = null;
			await this.options.refresh();
			this.assertActive();
		}
		this.phase = 'running';
		const intervalMs = this.options.periodicIntervalMs?.();
		if (intervalMs !== undefined && intervalMs !== null) {
			this.timer = setInterval(() => this.requestRefresh('periodic'), intervalMs);
			this.timer.unref();
		}
		this.startPendingRefresh();
	}

	private assertActive(): void {
		if (!this.isActive) {
			throw new Error(`${this.options.name} is stopping or stopped`);
		}
	}

	private requestRefresh(trigger: RefreshTrigger): void {
		if (this.phase === 'stopping' || this.phase === 'stopped') return;
		if (this.pendingRefresh !== 'notification') this.pendingRefresh = trigger;
		this.startPendingRefresh();
	}

	private startPendingRefresh(): void {
		if (!this.isReady || this.refreshTask || this.pendingRefresh === null) return;
		this.refreshTask = Promise.resolve()
			.then(() => this.refreshPending())
			.finally(() => {
				this.refreshTask = null;
				this.startPendingRefresh();
			});
	}

	private async refreshPending(): Promise<void> {
		while (this.isReady && this.pendingRefresh !== null) {
			const trigger = this.pendingRefresh;
			this.pendingRefresh = null;
			try {
				await this.options.refresh();
			} catch (error) {
				this.options.onRefreshError(error, trigger);
			}
		}
	}

	stop(): Promise<void> {
		if (this.stopCompletion) return this.stopCompletion;
		const wasStarting = this.phase === 'starting';
		this.phase = 'stopping';
		this.pendingRefresh = null;
		if (this.timer) clearInterval(this.timer);
		this.timer = null;
		this.subscription?.off('message', this.messageHandler);
		this.stopCompletion = this.close(wasStarting);
		return this.stopCompletion;
	}

	private async close(wasStarting: boolean): Promise<void> {
		try {
			const [closed] = await Promise.allSettled([
				Promise.resolve().then(() => {
					if (wasStarting || this.options.closeMode !== 'quit') return this.subscription?.disconnect();
					return this.subscription?.quit();
				}),
				this.startCompletion,
				this.refreshTask,
			]);
			if (closed.status === 'rejected') throw closed.reason;
		} finally {
			this.phase = 'stopped';
		}
	}
}
