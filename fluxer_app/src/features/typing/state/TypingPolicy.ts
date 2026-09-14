// SPDX-License-Identifier: AGPL-3.0-or-later

import LegacyTypingIndicator from '@app/features/typing/legacy/LegacyTypingIndicator';
import RollingTypingSender from '@app/features/typing/rolling/RollingTypingSender';
import RollingTypingStore from '@app/features/typing/rolling/RollingTypingStore';
import TypingIndicatorReworkRollout from '@app/features/typing/state/TypingIndicatorReworkRollout';
import {action, type IReactionDisposer, makeObservable, observable, reaction} from 'mobx';

type TypingPolicyName = 'legacy' | 'rolling';

class TypingPolicy {
	active: TypingPolicyName = 'legacy';
	private disposer: IReactionDisposer | null = null;

	constructor() {
		makeObservable(this, {active: observable, applyPolicy: action});
	}

	start(): void {
		if (this.disposer !== null) {
			return;
		}
		this.disposer = reaction(
			() => TypingIndicatorReworkRollout.usesRollingTyping,
			(usesRollingTyping) => this.applyPolicy(usesRollingTyping ? 'rolling' : 'legacy'),
			{fireImmediately: true},
		);
	}

	stop(): void {
		const disposer = this.disposer;
		this.disposer = null;
		disposer?.();
		this.applyPolicy('legacy');
	}

	applyPolicy(next: TypingPolicyName): void {
		if (this.active === next) {
			return;
		}
		LegacyTypingIndicator.reset();
		RollingTypingSender.reset();
		RollingTypingStore.reset();
		this.active = next;
	}
}

export default new TypingPolicy();
