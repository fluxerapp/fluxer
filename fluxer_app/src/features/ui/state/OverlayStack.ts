// SPDX-License-Identifier: AGPL-3.0-or-later

import {makeAutoObservable} from 'mobx';

export const OVERLAY_STACK_BASE_Z_INDEX = 10000;
const BASE_Z_INDEX = OVERLAY_STACK_BASE_Z_INDEX;
const ABOVE_OVERLAY_BASE_Z_INDEX = 41000;
const Z_INDEX_INCREMENT = 10;

class OverlayStack {
	private counter = 0;
	private sequence = 0;
	private aboveOverlayBaseDepth = 0;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	acquire(): number {
		const zIndex = this.getBaseZIndex() + this.sequence * Z_INDEX_INCREMENT;
		this.sequence++;
		this.counter++;
		return zIndex;
	}

	release(): void {
		if (this.counter === 0) return;
		this.counter--;
		if (this.counter === 0) {
			this.sequence = 0;
		}
	}

	peek(): number {
		return this.getBaseZIndex() + this.sequence * Z_INDEX_INCREMENT;
	}

	enableAboveOverlayBase(): () => void {
		this.aboveOverlayBaseDepth++;
		return () => this.disableAboveOverlayBase();
	}

	private disableAboveOverlayBase(): void {
		this.aboveOverlayBaseDepth = Math.max(0, this.aboveOverlayBaseDepth - 1);
		if (this.counter === 0) {
			this.sequence = 0;
		}
	}

	private getBaseZIndex(): number {
		return this.aboveOverlayBaseDepth > 0 ? ABOVE_OVERLAY_BASE_Z_INDEX : BASE_Z_INDEX;
	}
}

export default new OverlayStack();
