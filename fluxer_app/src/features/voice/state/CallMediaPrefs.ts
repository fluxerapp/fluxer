// SPDX-License-Identifier: AGPL-3.0-or-later

import {makeAutoObservable} from 'mobx';

interface CallScopedPrefs {
	disabledVideoByIdentity: Record<string, boolean>;
}

class CallMediaPrefs {
	private byCall: Record<string, CallScopedPrefs> = {};

	constructor() {
		makeAutoObservable(this, {isVideoDisabled: false}, {autoBind: true});
	}

	private ensure(callId: string): CallScopedPrefs {
		return (this.byCall[callId] ||= {disabledVideoByIdentity: {}});
	}

	isVideoDisabled(callId: string, identity: string): boolean {
		return !!this.byCall[callId]?.disabledVideoByIdentity[identity];
	}

	setVideoDisabled(callId: string, identity: string, disabled: boolean): void {
		const scope = this.ensure(callId);
		scope.disabledVideoByIdentity = {
			...scope.disabledVideoByIdentity,
			[identity]: disabled,
		};
	}

	clearForCall(callId: string): void {
		delete this.byCall[callId];
	}
}

export default new CallMediaPrefs();
