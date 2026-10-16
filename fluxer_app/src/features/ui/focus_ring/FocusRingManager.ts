// SPDX-License-Identifier: AGPL-3.0-or-later

import {makeAutoObservable} from 'mobx';

class FocusRingManagerClass {
	ringsEnabled = true;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	setRingsEnabled(enabled: boolean) {
		if (this.ringsEnabled === enabled) return;
		this.ringsEnabled = enabled;
	}
}

const FocusRingManager = new FocusRingManagerClass();

export default FocusRingManager;
