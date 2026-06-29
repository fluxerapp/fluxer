// SPDX-License-Identifier: AGPL-3.0-or-later

import type {APIWorkerLaneName, APIWorkerMode} from '../config/APIConfig';

interface LaneSettings {
	readonly consumerName: string;
	readonly tasks: ReadonlyArray<string>;
	readonly concurrency: number;
	readonly maxAckPending: number;
	readonly ackWaitMs: number;
	readonly maxDeliver: number;
}

const LANE_CONFIG = {
	realtime: {
		consumerName: 'workers_realtime',
		tasks: ['handleMentions', 'handleMentionChunk'] as const,
		concurrency: 10,
		maxAckPending: 50,
		ackWaitMs: 15000,
		maxDeliver: 3,
	},
	unfurl: {
		consumerName: 'workers_unfurl',
		tasks: ['extractEmbeds'] as const,
		concurrency: 20,
		maxAckPending: 200,
		ackWaitMs: 30000,
		maxDeliver: 3,
	},
	lifecycle: {
		consumerName: 'workers_lifecycle',
		tasks: [
			'processStripeWebhook',
			'sendScheduledMessage',
			'sendSystemDm',
			'userProcessPendingDeletion',
			'userProcessPendingDeletions',
			'applicationProcessDeletion',
			'bulkDeleteSelfMessagesImmediate',
			'bulkDeleteUserMessages',
			'bulkDeleteUserMessagesScoped',
			'deleteUserMessagesInGuildByTime',
			'messageShred',
			'finalizeNcmecAttachmentReport',
			'harvestGuildData',
			'harvestUserData',
			'batchGuildAuditLogMessageDeletes',
			'reconcileUserPayments',
			'revalidateUserConnections',
			'bulkUpdateUserFlags',
			'bulkUpdateSuspiciousActivityFlags',
			'bulkScheduleUserDeletion',
			'bulkUpdateGuildFeatures',
			'bulkAddGuildMembers',
			'bulkBanFileShas',
		] as const,
		concurrency: 8,
		maxAckPending: 50,
		ackWaitMs: 60000,
		maxDeliver: 25,
	},
	batch: {
		consumerName: 'workers_batch',
		tasks: [
			'expireAttachments',
			'indexChannelMessages',
			'indexGuildMembers',
			'processAssetDeletionQueue',
			'processBunnyPurgeQueue',
			'processExpiredPremiumSweep',
			'processInactivityDeletions',
			'processPendingBulkMessageDeletions',
			'processPremiumStateReconciliationQueue',
			'prunePostgresKvTtl',
			'refreshSearchIndex',
			'syncDiscoveryIndex',
			'syncDisposableEmailDomains',
			'syncUrlBlocklists',
			'syncFileShaBlocklists',
			'flushUserActivityBuffer',
		] as const,
		concurrency: 12,
		maxAckPending: 100,
		ackWaitMs: 120000,
		maxDeliver: 25,
	},
} satisfies Record<APIWorkerLaneName, LaneSettings>;

export type WorkerTaskName = (typeof LANE_CONFIG)[keyof typeof LANE_CONFIG]['tasks'][number];

interface WorkerLaneDefinition {
	name: APIWorkerLaneName;
	consumerName: string;
	taskTypes: ReadonlyArray<WorkerTaskName>;
	concurrency: number;
	maxAckPending: number;
	ackWaitMs: number;
	maxDeliver: number;
}

interface WorkerLaneRuntimeConfig {
	mode: APIWorkerMode;
	laneName?: APIWorkerLaneName;
	taskName?: WorkerTaskName;
	laneConcurrencyOverrides: Partial<Record<APIWorkerLaneName, number>>;
}

function makeLane(name: APIWorkerLaneName): WorkerLaneDefinition {
	const config = LANE_CONFIG[name];
	return {
		name,
		consumerName: config.consumerName,
		taskTypes: config.tasks,
		concurrency: config.concurrency,
		maxAckPending: config.maxAckPending,
		ackWaitMs: config.ackWaitMs,
		maxDeliver: config.maxDeliver,
	};
}

const WORKER_LANES: ReadonlyArray<WorkerLaneDefinition> = [
	makeLane('realtime'),
	makeLane('unfurl'),
	makeLane('lifecycle'),
	makeLane('batch'),
];
const WORKER_LANE_MAP = buildWorkerLaneMap();
const WORKER_LANE_NAMES = Object.freeze(WORKER_LANES.map((lane) => lane.name));

function buildWorkerLaneMap(): Record<APIWorkerLaneName, WorkerLaneDefinition> {
	const laneMap = {} as Record<APIWorkerLaneName, WorkerLaneDefinition>;
	for (const lane of WORKER_LANES) {
		laneMap[lane.name] = lane;
	}
	return laneMap;
}

function resolveLaneName(laneName: APIWorkerLaneName | undefined): APIWorkerLaneName {
	if (!laneName) {
		throw new Error(
			`Worker lane name is required when worker mode is "single_lane". Available lanes: ${WORKER_LANE_NAMES.join(', ')}`,
		);
	}
	if (!Object.hasOwn(WORKER_LANE_MAP, laneName)) {
		throw new Error(`Unknown worker lane "${laneName}". Available lanes: ${WORKER_LANE_NAMES.join(', ')}`);
	}
	return laneName;
}

function resolveSingleTaskLane(taskName: WorkerTaskName | undefined): WorkerLaneDefinition {
	if (!taskName) {
		throw new Error('Worker task name is required when worker mode is "single_task"');
	}
	const parentLane = WORKER_LANES.find((lane) => lane.taskTypes.includes(taskName));
	if (!parentLane) {
		const allTaskNames = WORKER_LANES.flatMap<string>((lane) => [...lane.taskTypes]);
		throw new Error(`Task "${taskName}" is not assigned to any lane. Known tasks: ${allTaskNames.join(', ')}`);
	}
	return {
		name: parentLane.name,
		consumerName: `worker_${taskName}`,
		taskTypes: [taskName],
		concurrency: parentLane.concurrency,
		maxAckPending: parentLane.maxAckPending,
		ackWaitMs: parentLane.ackWaitMs,
		maxDeliver: parentLane.maxDeliver,
	};
}

function resolveLaneNames(config: WorkerLaneRuntimeConfig): Array<APIWorkerLaneName> {
	if (config.mode === 'single_lane') {
		return [resolveLaneName(config.laneName)];
	}
	return [...WORKER_LANE_NAMES];
}

function resolveLaneByName(laneName: APIWorkerLaneName): WorkerLaneDefinition {
	const lane = WORKER_LANE_MAP[laneName];
	if (!lane) {
		throw new Error(`Unknown worker lane "${laneName}". Available lanes: ${WORKER_LANE_NAMES.join(', ')}`);
	}
	return lane;
}

function applyLaneConcurrencyOverride(
	lane: WorkerLaneDefinition,
	laneConcurrencyOverrides: Partial<Record<APIWorkerLaneName, number>>,
): WorkerLaneDefinition {
	const overrideConcurrency = laneConcurrencyOverrides[lane.name];
	if (overrideConcurrency === undefined) {
		return lane;
	}
	if (!Number.isInteger(overrideConcurrency) || overrideConcurrency < 1) {
		throw new Error(
			`Invalid concurrency override for lane "${lane.name}": ${overrideConcurrency}. Expected an integer >= 1.`,
		);
	}
	return {
		...lane,
		concurrency: overrideConcurrency,
	};
}

function resolveWorkerLanes(config: WorkerLaneRuntimeConfig): Array<WorkerLaneDefinition> {
	if (config.mode === 'single_task') {
		return [resolveSingleTaskLane(config.taskName)];
	}
	const laneNames = resolveLaneNames(config);
	return laneNames.map((laneName) => {
		const lane = resolveLaneByName(laneName);
		return applyLaneConcurrencyOverride(lane, config.laneConcurrencyOverrides);
	});
}

function resolveCronSchedulerEnabled(mode: APIWorkerMode, configuredValue: boolean | undefined): boolean {
	if (configuredValue !== undefined) {
		return configuredValue;
	}
	return mode === 'all_lanes';
}

function validateLaneCompleteness(registeredTasks: Record<string, unknown>): void {
	const registeredTaskNames = new Set(Object.keys(registeredTasks));
	const laneTaskNames = new Set<string>(WORKER_LANES.flatMap<string>((lane) => [...lane.taskTypes]));
	const missingFromLanes = [...registeredTaskNames].filter((task) => !laneTaskNames.has(task));
	const missingFromRegistry = [...laneTaskNames].filter((task) => !registeredTaskNames.has(task));
	const errors: Array<string> = [];
	if (missingFromLanes.length > 0) {
		errors.push(`Registered tasks not assigned to any lane: ${missingFromLanes.join(', ')}`);
	}
	if (missingFromRegistry.length > 0) {
		errors.push(`Lane tasks not found in registry: ${missingFromRegistry.join(', ')}`);
	}
	if (errors.length > 0) {
		throw new Error(`Worker lane configuration mismatch:\n${errors.join('\n')}`);
	}
}

export function findLaneForTask(taskName: string): APIWorkerLaneName | null {
	for (const [laneName, lane] of Object.entries(LANE_CONFIG) as Array<[APIWorkerLaneName, LaneSettings]>) {
		if ((lane.tasks as ReadonlyArray<string>).includes(taskName)) return laneName;
	}
	return null;
}

export type {WorkerLaneDefinition};
export {resolveCronSchedulerEnabled, resolveWorkerLanes, validateLaneCompleteness, WORKER_LANES};
