// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	exceedsDispatchBufferCapacity,
	GATEWAY_DISPATCH_BUFFER_MAX_BYTES,
} from '@app/features/gateway/transport/GatewayDispatchBufferBounds';
import type {GatewayDispatchDelivery, GatewaySocket} from '@app/features/gateway/transport/GatewaySocket';

const BACKGROUND_DISPATCH_IDLE_WAITER_MAX_ENTRIES = 8;

export const BackgroundDispatchHandoverOutcome = Object.freeze({
	CONSUMED: 'consumed',
	RESTORED: 'restored',
	LOST: 'lost',
} as const);

export type BackgroundDispatchHandoverOutcome =
	(typeof BackgroundDispatchHandoverOutcome)[keyof typeof BackgroundDispatchHandoverOutcome];

export type BackgroundDispatchHandoverEnd =
	| {readonly outcome: typeof BackgroundDispatchHandoverOutcome.CONSUMED}
	| {
			readonly outcome: typeof BackgroundDispatchHandoverOutcome.RESTORED;
			readonly deliveries: ReadonlyArray<GatewayDispatchDelivery>;
	  }
	| {readonly outcome: typeof BackgroundDispatchHandoverOutcome.LOST};

interface QueuedBackgroundDispatch {
	readonly socket: GatewaySocket;
	readonly delivery: GatewayDispatchDelivery;
}

export interface BackgroundGatewayDispatchQueueHost {
	isCurrentSocket(socket: GatewaySocket): boolean;
	persistHandoverAuthToken(socket: GatewaySocket, delivery: GatewayDispatchDelivery): Promise<GatewayDispatchDelivery>;
	processDispatch(socket: GatewaySocket, delivery: GatewayDispatchDelivery): Promise<void>;
	handleQueueOverflow(socket: GatewaySocket, delivery: GatewayDispatchDelivery, error: Error): void;
	handleQueueFailure(error: unknown): void;
}

class BackgroundGatewayDispatchQueueOverflowError extends Error {
	constructor(entryCount: number, retainedByteSize: number) {
		super(
			`Background gateway dispatch queue exceeded its bound at ${entryCount} entries and ${retainedByteSize} bytes`,
		);
		this.name = 'BackgroundGatewayDispatchQueueOverflowError';
	}
}

class BackgroundGatewayDispatchIdleWaiterCapacityError extends Error {
	constructor() {
		super(
			`Background gateway dispatch queue reached its ${BACKGROUND_DISPATCH_IDLE_WAITER_MAX_ENTRIES} idle waiter bound`,
		);
		this.name = 'BackgroundGatewayDispatchIdleWaiterCapacityError';
	}
}

export class BackgroundGatewayDispatchQueue {
	private handoverListener: ((delivery: GatewayDispatchDelivery) => void) | null = null;
	private queued: Array<QueuedBackgroundDispatch> = [];
	private queuedRetainedByteSize = 0;
	private inFlightEntryCount = 0;
	private inFlightRetainedByteSize = 0;
	private drainScheduled = false;
	private idleWaiters: Array<() => void> = [];

	constructor(private readonly host: BackgroundGatewayDispatchQueueHost) {}

	get isHandoverActive(): boolean {
		return this.handoverListener !== null;
	}

	beginHandover(listener: (delivery: GatewayDispatchDelivery) => void): void {
		if (this.handoverListener !== null) {
			throw new Error('Background gateway dispatch handover is already active');
		}
		this.handoverListener = listener;
	}

	endHandover(request: BackgroundDispatchHandoverEnd, socket: GatewaySocket | null): boolean {
		if (this.handoverListener === null) {
			throw new Error('Background gateway dispatch handover is not active');
		}
		this.handoverListener = null;
		if (socket === null) {
			return false;
		}
		switch (request.outcome) {
			case BackgroundDispatchHandoverOutcome.CONSUMED:
				return false;
			case BackgroundDispatchHandoverOutcome.RESTORED:
				this.restore(socket, request.deliveries);
				return false;
			case BackgroundDispatchHandoverOutcome.LOST:
				return true;
		}
	}

	waitForIdle(): Promise<void> {
		if (this.isIdle()) {
			return Promise.resolve();
		}
		if (this.idleWaiters.length >= BACKGROUND_DISPATCH_IDLE_WAITER_MAX_ENTRIES) {
			return Promise.reject(new BackgroundGatewayDispatchIdleWaiterCapacityError());
		}
		return new Promise((resolve) => {
			this.idleWaiters.push(resolve);
		});
	}

	assertDetachable(): void {
		if (this.handoverListener !== null) {
			throw new Error('Background gateway socket cannot detach during an active dispatch handover');
		}
		if (!this.isIdle()) {
			throw new Error('Background gateway socket cannot detach with queued dispatches');
		}
	}

	enqueue(socket: GatewaySocket, delivery: GatewayDispatchDelivery): void {
		const nextEntryCount = this.inFlightEntryCount + this.queued.length + 1;
		const nextRetainedByteSize =
			this.inFlightRetainedByteSize + this.queuedRetainedByteSize + delivery.retainedByteSize;
		if (exceedsDispatchBufferCapacity(nextEntryCount, nextRetainedByteSize, delivery.retainedByteSize)) {
			this.clearQueued();
			this.host.handleQueueOverflow(
				socket,
				delivery,
				new BackgroundGatewayDispatchQueueOverflowError(nextEntryCount, nextRetainedByteSize),
			);
			return;
		}
		this.queued.push({socket, delivery});
		this.queuedRetainedByteSize += delivery.retainedByteSize;
		this.scheduleDrain();
	}

	stop(): void {
		this.handoverListener = null;
		this.clearQueued();
	}

	private restore(socket: GatewaySocket, deliveries: ReadonlyArray<GatewayDispatchDelivery>): void {
		const retained = deliveries.filter((delivery) => socket.isDispatchActive(delivery.receipt));
		if (retained.length === 0) {
			return;
		}
		const restoredByteSize = retained.reduce((total, delivery) => total + delivery.retainedByteSize, 0);
		const nextEntryCount = this.inFlightEntryCount + this.queued.length + retained.length;
		const nextRetainedByteSize = this.inFlightRetainedByteSize + this.queuedRetainedByteSize + restoredByteSize;
		const oversizedDelivery = retained.find(
			(delivery) => delivery.retainedByteSize >= GATEWAY_DISPATCH_BUFFER_MAX_BYTES,
		);
		if (exceedsDispatchBufferCapacity(nextEntryCount, nextRetainedByteSize, oversizedDelivery?.retainedByteSize ?? 0)) {
			const firstRetainedDelivery = retained[0];
			if (firstRetainedDelivery === undefined) {
				throw new Error('Background gateway dispatch restoration lost its retained delivery');
			}
			this.clearQueued();
			this.host.handleQueueOverflow(
				socket,
				oversizedDelivery ?? firstRetainedDelivery,
				new BackgroundGatewayDispatchQueueOverflowError(nextEntryCount, nextRetainedByteSize),
			);
			return;
		}
		this.queued = [...retained.map((delivery) => ({socket, delivery})), ...this.queued];
		this.queuedRetainedByteSize += restoredByteSize;
		this.scheduleDrain();
	}

	private scheduleDrain(): void {
		if (this.drainScheduled) {
			return;
		}
		this.drainScheduled = true;
		queueMicrotask(() => void this.drain());
	}

	private async drain(): Promise<void> {
		try {
			while (this.queued.length > 0) {
				const batch: Array<QueuedBackgroundDispatch | null> = this.queued;
				this.inFlightEntryCount = batch.length;
				this.inFlightRetainedByteSize = this.queuedRetainedByteSize;
				this.queued = [];
				this.queuedRetainedByteSize = 0;
				for (let index = 0; index < batch.length; index += 1) {
					const entry = batch[index];
					if (entry === null) {
						throw new Error('Background gateway dispatch queue released an unprocessed entry');
					}
					try {
						if (!this.host.isCurrentSocket(entry.socket) || !entry.socket.isDispatchActive(entry.delivery.receipt)) {
							continue;
						}
						const delivery =
							this.handoverListener === null
								? entry.delivery
								: await this.host.persistHandoverAuthToken(entry.socket, entry.delivery);
						if (!this.host.isCurrentSocket(entry.socket) || !entry.socket.isDispatchActive(delivery.receipt)) {
							continue;
						}
						const handoverListener = this.handoverListener;
						if (handoverListener === null) {
							await this.host.processDispatch(entry.socket, delivery);
						} else {
							handoverListener(delivery);
						}
					} finally {
						batch[index] = null;
						this.inFlightEntryCount -= 1;
						this.inFlightRetainedByteSize -= entry.delivery.retainedByteSize;
					}
				}
			}
			if (this.inFlightEntryCount !== 0 || this.inFlightRetainedByteSize !== 0) {
				throw new Error('Background gateway dispatch queue accounting did not drain to zero');
			}
		} catch (error) {
			this.clearQueued();
			this.inFlightEntryCount = 0;
			this.inFlightRetainedByteSize = 0;
			this.host.handleQueueFailure(error);
		} finally {
			this.drainScheduled = false;
			this.resolveIdleWaiters();
		}
	}

	private isIdle(): boolean {
		return (
			!this.drainScheduled &&
			this.queued.length === 0 &&
			this.inFlightEntryCount === 0 &&
			this.inFlightRetainedByteSize === 0
		);
	}

	private clearQueued(): void {
		this.queued = [];
		this.queuedRetainedByteSize = 0;
		if (!this.drainScheduled) {
			this.resolveIdleWaiters();
		}
	}

	private resolveIdleWaiters(): void {
		const waiters = this.idleWaiters;
		this.idleWaiters = [];
		for (const resolve of waiters) {
			resolve();
		}
	}
}
