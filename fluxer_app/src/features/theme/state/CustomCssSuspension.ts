// SPDX-License-Identifier: AGPL-3.0-or-later

import {makeAutoObservable} from 'mobx';
import {useLayoutEffect} from 'react';

class CustomCssSuspension {
	count = 0;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	get active(): boolean {
		return this.count > 0;
	}

	acquire(): void {
		this.count += 1;
	}

	release(): void {
		this.count -= 1;
	}
}

const customCssSuspension = new CustomCssSuspension();

export function useCustomCssSuspension(enabled = true): void {
	useLayoutEffect(() => {
		if (!enabled) return;
		customCssSuspension.acquire();
		return customCssSuspension.release;
	}, [enabled]);
}

export default customCssSuspension;
