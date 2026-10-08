// SPDX-License-Identifier: AGPL-3.0-or-later

import {exceedsDispatchBufferCapacity} from '@app/features/gateway/transport/GatewayDispatchBufferBounds';
import type {GatewayDispatchDelivery, GatewaySocket} from '@app/features/gateway/transport/GatewaySocket';

interface ActiveForegroundPromotionReplay {
	readonly accountKey: string;
	readonly socket: GatewaySocket;
	readonly deliveries: Array<GatewayDispatchDelivery>;
	retainedByteSize: number;
	inFlightEntryCount: number;
	overflowed: boolean;
}

export type ForegroundPromotionDispatch = (delivery: GatewayDispatchDelivery) => void;

class ForegroundPromotionReplayBufferOverflowError extends Error {
	constructor(entryCount: number, retainedByteSize: number) {
		super(
			`Foreground gateway promotion replay buffer exceeded its bound at ${entryCount} entries and ${retainedByteSize} bytes`,
		);
		this.name = 'ForegroundPromotionReplayBufferOverflowError';
	}
}

export class ForegroundPromotionReplayBuffer {
	private active: ActiveForegroundPromotionReplay | null = null;

	begin(accountKey: string, socket: GatewaySocket): void {
		if (this.active !== null) {
			throw new Error(`Foreground gateway promotion replay is already active for ${this.active.accountKey}`);
		}
		this.active = {
			accountKey,
			socket,
			deliveries: [],
			retainedByteSize: 0,
			inFlightEntryCount: 0,
			overflowed: false,
		};
	}

	capture(socket: GatewaySocket, accountKey: string | null, delivery: GatewayDispatchDelivery): boolean {
		const active = this.active;
		if (active === null || active.socket !== socket || active.accountKey !== accountKey) {
			return false;
		}
		const nextEntryCount = active.inFlightEntryCount + active.deliveries.length + 1;
		const nextRetainedByteSize = active.retainedByteSize + delivery.retainedByteSize;
		if (exceedsDispatchBufferCapacity(nextEntryCount, nextRetainedByteSize, delivery.retainedByteSize)) {
			active.overflowed = true;
			active.socket.failDispatchProcessing(
				delivery.receipt,
				new ForegroundPromotionReplayBufferOverflowError(nextEntryCount, nextRetainedByteSize),
			);
			return true;
		}
		active.deliveries.push(delivery);
		active.retainedByteSize = nextRetainedByteSize;
		return true;
	}

	drain(accountKey: string, socket: GatewaySocket, dispatch: ForegroundPromotionDispatch): void {
		const active = this.active;
		if (active === null || active.accountKey !== accountKey || active.socket !== socket || active.overflowed) {
			throw new Error('Cannot finalize a promoted foreground gateway with an invalid replay buffer');
		}
		while (active.deliveries.length > 0) {
			const queued: Array<GatewayDispatchDelivery | null> = active.deliveries.splice(0, active.deliveries.length);
			active.inFlightEntryCount = queued.length;
			for (let index = 0; index < queued.length; index += 1) {
				const delivery = queued[index];
				if (delivery === null) {
					throw new Error('Foreground promotion replay queue released an unprocessed dispatch');
				}
				try {
					dispatch(delivery);
				} finally {
					queued[index] = null;
					active.inFlightEntryCount -= 1;
					active.retainedByteSize -= delivery.retainedByteSize;
				}
			}
		}
		if (active.inFlightEntryCount !== 0 || active.retainedByteSize !== 0) {
			throw new Error('Foreground promotion replay queue accounting did not drain to zero');
		}
		this.active = null;
	}

	clear(): void {
		this.active = null;
	}
}
