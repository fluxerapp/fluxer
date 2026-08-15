// SPDX-License-Identifier: AGPL-3.0-or-later

import Window from '@app/features/window/state/Window';
import {makeAutoObservable, onBecomeObserved, onBecomeUnobserved, reaction} from 'mobx';

class TickRegistryImpl {
	nowSecond: number = Math.floor(Date.now() / 1000);
	nowMinute: number = Math.floor(Date.now() / 60000);
	private intervalId: number | null = null;
	private observed = false;

	constructor() {
		makeAutoObservable<this, 'intervalId' | 'observed'>(this, {intervalId: false, observed: false}, {autoBind: true});
		onBecomeObserved(this, 'nowSecond', () => {
			this.observed = true;
			this.start();
		});
		onBecomeUnobserved(this, 'nowSecond', () => {
			this.observed = false;
			this.stop();
		});
		reaction(
			() => Window.focused,
			(focused) => {
				if (focused) this.start();
				else this.stop();
			},
			{fireImmediately: true},
		);
	}

	private start(): void {
		if (!this.observed || !Window.focused || this.intervalId !== null) return;
		this.tick();
		this.intervalId = window.setInterval(() => this.tick(), 1000);
	}

	private stop(): void {
		if (this.intervalId === null) return;
		window.clearInterval(this.intervalId);
		this.intervalId = null;
	}

	private tick(): void {
		const now = Date.now();
		const second = Math.floor(now / 1000);
		const minute = Math.floor(now / 60000);
		if (second !== this.nowSecond) this.nowSecond = second;
		if (minute !== this.nowMinute) this.nowMinute = minute;
	}
}

const Tick = new TickRegistryImpl();

export function useNow(enabled: boolean): number {
	if (enabled) return Tick.nowSecond;
	return Date.now();
}

export default Tick;
