// SPDX-License-Identifier: AGPL-3.0-or-later

import {makeAutoObservable, observable} from 'mobx';

class CallAvailability {
	unavailableCalls: Set<string> = observable.set();

	constructor() {
		makeAutoObservable(
			this,
			{
				unavailableCalls: false,
			},
			{autoBind: true},
		);
	}

	setCallAvailable(channelId: string): void {
		if (this.unavailableCalls.has(channelId)) {
			this.unavailableCalls.delete(channelId);
		}
	}
}

export default new CallAvailability();
