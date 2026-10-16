// SPDX-License-Identifier: AGPL-3.0-or-later

import {hostname} from 'node:os';
import {readOptionalEnv} from '@app/api/utils/IntegerOptions';
import type {RpcSessionTimings, RpcTimingNode} from '@fluxer/schema/src/domains/rpc/RpcSchemas';

export type RpcTimingSteps = Record<string, RpcTimingNode>;

export function startRpcTiming(): bigint {
	return process.hrtime.bigint();
}

function elapsedRpcTimingUs(startedAtNs: bigint): number {
	return Number((process.hrtime.bigint() - startedAtNs) / 1000n);
}

export function createRpcTimingNode(startedAtNs: bigint, steps?: RpcTimingSteps): RpcTimingNode {
	return {
		duration_us: elapsedRpcTimingUs(startedAtNs),
		...(steps && Object.keys(steps).length > 0 ? {steps} : {}),
	};
}

function apiPodName(): string {
	return readOptionalEnv('POD_NAME') ?? readOptionalEnv('HOSTNAME') ?? hostname();
}

export async function timeRpcStep<T>(steps: RpcTimingSteps, name: string, operation: () => Promise<T>): Promise<T> {
	const startedAtNs = startRpcTiming();
	try {
		return await operation();
	} finally {
		steps[name] = createRpcTimingNode(startedAtNs);
	}
}

export function timeRpcStepSync<T>(steps: RpcTimingSteps, name: string, operation: () => T): T {
	const startedAtNs = startRpcTiming();
	try {
		return operation();
	} finally {
		steps[name] = createRpcTimingNode(startedAtNs);
	}
}

export class RpcTimingRecorder {
	private readonly startedAtNs = startRpcTiming();
	private readonly steps: RpcTimingSteps = {};

	async time<T>(name: string, operation: () => Promise<T>): Promise<T> {
		return await timeRpcStep(this.steps, name, operation);
	}

	timeSync<T>(name: string, operation: () => T): T {
		return timeRpcStepSync(this.steps, name, operation);
	}

	record(name: string, startedAtNs: bigint, steps?: RpcTimingSteps): void {
		this.steps[name] = createRpcTimingNode(startedAtNs, steps);
	}

	finalize(): RpcSessionTimings {
		const podName = apiPodName();
		return {
			unit: 'microseconds',
			total_us: elapsedRpcTimingUs(this.startedAtNs),
			pod_name: podName,
			steps: this.steps,
		};
	}
}
