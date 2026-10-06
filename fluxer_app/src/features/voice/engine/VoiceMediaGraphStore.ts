// SPDX-License-Identifier: AGPL-3.0-or-later

import {IS_DEV} from '@app/features/platform/types/Env';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {Store} from '@app/features/voice/engine/Store';
import {
	createVoiceMediaGraphSnapshot,
	selectVoiceMediaGraphSubscriptionCommands,
	transitionVoiceMediaGraph,
	type VoiceMediaGraphEvent,
	type VoiceMediaGraphFailure,
	type VoiceMediaGraphSnapshot,
	type VoiceMediaGraphSubscriptionCommand,
	type VoiceMediaGraphSubscriptionEvent,
} from '@app/features/voice/engine/VoiceMediaGraph';
import {
	systemVoiceMediaGraphClock,
	type VoiceMediaGraphClockPort,
} from '@app/features/voice/engine/VoiceMediaGraphClock';
import {checkVoiceMediaGraphInvariants} from '@app/features/voice/engine/VoiceMediaGraphInvariants';
import {makeObservable, observableRef} from 'mobx';

const logger = new Logger('VoiceMediaGraphStore');
const LOGGED_VIOLATION_LIMIT = 256;

export class VoiceMediaGraphStore extends Store {
	graph: VoiceMediaGraphSnapshot = createVoiceMediaGraphSnapshot();

	private readonly clock: VoiceMediaGraphClockPort;
	private readonly loggedViolations = new Set<string>();

	constructor(clock: VoiceMediaGraphClockPort = systemVoiceMediaGraphClock) {
		super();
		this.clock = clock;
		makeObservable(this, {
			graph: observableRef,
		});
	}

	nowMs(): number {
		return this.clock.now();
	}

	getGraphSnapshot(): VoiceMediaGraphSnapshot {
		return this.graph;
	}

	transition(event: VoiceMediaGraphEvent): VoiceMediaGraphSnapshot {
		return this.update(() => {
			this.graph = transitionVoiceMediaGraph(this.graph, event);
			this.checkInvariants();
			return this.graph;
		});
	}

	transitionTypedFailure<TFailure extends VoiceMediaGraphFailure>(
		event: VoiceMediaGraphEvent<TFailure>,
	): VoiceMediaGraphSnapshot<TFailure> {
		return this.update(() => {
			this.graph = transitionVoiceMediaGraph(this.graph as VoiceMediaGraphSnapshot<TFailure>, event);
			this.checkInvariants();
			return this.graph as VoiceMediaGraphSnapshot<TFailure>;
		});
	}

	takeSubscriptionCommands(event: VoiceMediaGraphSubscriptionEvent): Array<VoiceMediaGraphSubscriptionCommand> {
		return this.update(() => {
			this.graph = transitionVoiceMediaGraph(this.graph, event);
			const commands = [...selectVoiceMediaGraphSubscriptionCommands(this.graph)];
			this.graph = transitionVoiceMediaGraph(this.graph, {type: 'subscription.clearCommands'});
			this.checkInvariants();
			return commands;
		});
	}

	reset(): void {
		this.update(() => {
			this.graph = createVoiceMediaGraphSnapshot();
		});
	}

	private checkInvariants(): void {
		if (!IS_DEV) return;
		const violations = checkVoiceMediaGraphInvariants(this.graph);
		for (const violation of violations) {
			if (this.loggedViolations.has(violation)) continue;
			if (this.loggedViolations.size >= LOGGED_VIOLATION_LIMIT) return;
			this.loggedViolations.add(violation);
			logger.warn('Voice media graph invariant violated', {violation});
		}
	}
}

export const voiceMediaGraphStore = new VoiceMediaGraphStore();

export default voiceMediaGraphStore;
