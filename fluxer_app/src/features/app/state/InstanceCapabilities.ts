// SPDX-License-Identifier: AGPL-3.0-or-later

import {makeAutoObservable} from 'mobx';

export const InstanceResponseCapability = Object.freeze({
	HANDOFF_POLL_SECRET: 'handoffPollSecret',
} as const);

export type InstanceResponseCapability = (typeof InstanceResponseCapability)[keyof typeof InstanceResponseCapability];

type InstanceResponseObservations = Readonly<Partial<Record<InstanceResponseCapability, boolean>>>;

interface ObserveInstanceResponseRequest {
	readonly instanceKey: string;
	readonly capability: InstanceResponseCapability;
	readonly supported: boolean;
}

class InstanceCapabilitiesStore {
	private records: Map<string, InstanceResponseObservations> = new Map();

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	observedSupport(instanceKey: string, capability: InstanceResponseCapability): boolean | null {
		return this.records.get(instanceKey)?.[capability] ?? null;
	}

	observeResponse({instanceKey, capability, supported}: ObserveInstanceResponseRequest): void {
		const responses = this.records.get(instanceKey);
		if (responses != null && responses[capability] === supported) {
			return;
		}
		this.records.set(instanceKey, {...responses, [capability]: supported});
	}

	forget(instanceKey: string): void {
		this.records.delete(instanceKey);
	}

	reset(): void {
		this.records.clear();
	}
}

export default new InstanceCapabilitiesStore();
