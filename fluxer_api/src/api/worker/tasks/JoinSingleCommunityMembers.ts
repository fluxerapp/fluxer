// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash, randomUUID} from 'node:crypto';
import {createGuildID} from '@app/api/BrandedTypes';
import {canJoinSingleCommunity} from '@app/api/instance/SingleCommunityService';
import {createRequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskName} from '@app/api/worker/WorkerLaneConfig';
import {JoinSourceTypes} from '@fluxer/constants/src/GuildConstants';
import {FluxerError} from '@fluxer/errors/src/FluxerError';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import type {WorkerTaskHandler, WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import {ms, seconds} from 'itty-time';
import {z} from 'zod';

const PAGE_SIZE = 100;
const RUN_KEY = 'single_community:join_members_run';
const LOCK_KEY = 'single_community:join_members_lock';
const RUN_TTL_SECONDS = seconds('30 days');
const LOCK_TTL_SECONDS = seconds('5 minutes');
const STALL_MS = ms('10 minutes');
const PayloadSchema = z.object({
	run_id: z.string().uuid().optional(),
	guild_id: z.string().regex(/^\d+$/).optional(),
	page_state: z.string().max(4096).optional(),
});
const ProgressSchema = z.object({
	guild_id: z.string().regex(/^\d+$/),
	page_state: z.string().nullable(),
	updated_at: z.number(),
	joining: z.array(z.string().regex(/^\d+$/)).max(PAGE_SIZE).default([]),
});

type JoinRun = z.infer<typeof ProgressSchema> & {run_id: string};

function progressKey(runId: string): string {
	return `single_community:join_members_progress:${runId}`;
}

function pageJobKey(runId: string, pageState: string | null): string {
	return `${runId}:${pageState === null ? 'start' : createHash('sha256').update(pageState).digest('hex')}`;
}

function pagePayload(run: JoinRun): {run_id: string; page_state?: string} {
	return run.page_state === null ? {run_id: run.run_id} : {run_id: run.run_id, page_state: run.page_state};
}

async function saveRun(kvClient: IKVProvider, run: JoinRun): Promise<void> {
	const {run_id: runId, ...progress} = run;
	await kvClient.setex(progressKey(runId), RUN_TTL_SECONDS, JSON.stringify(progress));
}

async function readRun(kvClient: IKVProvider): Promise<JoinRun | null> {
	const runId = await kvClient.get(RUN_KEY);
	if (runId === null) return null;
	const raw = await kvClient.get(progressKey(runId));
	if (raw === null) return null;
	try {
		return {...ProgressSchema.parse(JSON.parse(raw)), run_id: runId};
	} catch {
		return null;
	}
}

export async function singleCommunityJoinPending(kvClient: IKVProvider): Promise<boolean> {
	return (await kvClient.get(RUN_KEY)) !== null;
}

async function finishRun(kvClient: IKVProvider, runId: string): Promise<void> {
	await kvClient.del(progressKey(runId));
	await kvClient.releaseLock(RUN_KEY, runId);
}

export async function startSingleCommunityJoin(
	kvClient: IKVProvider,
	workerService: IWorkerService<WorkerTaskName>,
	guildId: string,
): Promise<void> {
	const run: JoinRun = {run_id: randomUUID(), guild_id: guildId, page_state: null, updated_at: Date.now(), joining: []};
	await saveRun(kvClient, run);
	await kvClient.set(RUN_KEY, run.run_id);
	await workerService.addJob('joinSingleCommunityMembers', pagePayload(run), {jobKey: pageJobKey(run.run_id, null)});
}

async function adoptLegacyPage(
	kvClient: IKVProvider,
	guildId: string,
	pageState: string | null,
	helpers: WorkerTaskHelpers,
): Promise<void> {
	const run: JoinRun = {
		run_id: randomUUID(),
		guild_id: guildId,
		page_state: pageState,
		updated_at: Date.now(),
		joining: [],
	};
	await saveRun(kvClient, run);
	if (!(await kvClient.acquireLock(RUN_KEY, run.run_id, RUN_TTL_SECONDS))) {
		await kvClient.del(progressKey(run.run_id));
		helpers.logger.info({guildId}, 'Dropping an old single-community join page because a run is already going');
		return;
	}
	helpers.logger.info({guildId, runId: run.run_id}, 'Continuing a single-community join queued before the upgrade');
	await helpers.addJob('joinSingleCommunityMembers', pagePayload(run), {
		jobKey: pageJobKey(run.run_id, run.page_state),
	});
}

async function joinPage(
	kvClient: IKVProvider,
	run: JoinRun,
	lockToken: string,
	helpers: WorkerTaskHelpers,
): Promise<void> {
	const {userRepository, guildRepository, guildService, gatewayService, instanceConfigRepository} =
		getWorkerDependencies();
	const policy = await instanceConfigRepository.readStoredInstancePolicyConfig();
	if (!policy.single_community_enabled || policy.single_community_guild_id !== run.guild_id) {
		helpers.logger.warn(
			{
				guildId: run.guild_id,
				currentGuildId: policy.single_community_enabled ? policy.single_community_guild_id : null,
			},
			'Single-community mode changed, stopping the join for existing accounts',
		);
		await finishRun(kvClient, run.run_id);
		return;
	}
	const guildId = createGuildID(BigInt(run.guild_id));
	if ((await guildRepository.findUnique(guildId)) === null) {
		helpers.logger.warn(
			{guildId: run.guild_id},
			'The single community is gone, stopping the join for existing accounts',
		);
		await finishRun(kvClient, run.run_id);
		return;
	}
	const page = await userRepository.scanAllUsersPage(PAGE_SIZE, run.page_state);
	const joining = new Set(run.joining);
	let added = 0;
	let rejoined = 0;
	let refused = 0;
	const failed: Array<string> = [];
	for (const user of page.users) {
		if (!canJoinSingleCommunity(user)) continue;
		const userId = user.id.toString();
		const isMember = (await guildRepository.getMember(guildId, user.id)) !== null;
		if (isMember && !joining.has(userId)) continue;
		if (!(await kvClient.extendLock(LOCK_KEY, lockToken, LOCK_TTL_SECONDS))) {
			throw new Error('Lost the lock while adding existing accounts to the single community');
		}
		if (isMember) {
			try {
				await gatewayService.joinGuild({userId: user.id, guildId});
				joining.delete(userId);
				rejoined++;
			} catch (error) {
				failed.push(userId);
				helpers.logger.warn({userId, error}, 'Could not finish adding an account to the single community');
			}
			continue;
		}
		joining.add(userId);
		await saveRun(kvClient, {...run, joining: [...joining]});
		try {
			await guildService.members.addUserToGuild({
				userId: user.id,
				guildId,
				sendJoinMessage: false,
				skipGuildLimitCheck: true,
				skipAccountLimitCheck: true,
				joinSourceType: JoinSourceTypes.ADMIN_FORCE_ADD,
				requestCache: createRequestCache(),
			});
			joining.delete(userId);
			added++;
		} catch (error) {
			if (error instanceof FluxerError && error.status < 500) {
				refused++;
				helpers.logger.info({userId, error}, 'The single community refused an account');
				continue;
			}
			failed.push(userId);
			helpers.logger.warn({userId, error}, 'Could not add an account to the single community');
		}
	}
	if (failed.length > 0 && helpers.attempt?.isLastAttempt !== true) {
		await saveRun(kvClient, {...run, joining: [...joining]});
		throw new Error(`Could not add ${failed.length} accounts to the single community`);
	}
	helpers.logger.info(
		{guildId: run.guild_id, added, rejoined, refused, skipped: failed},
		'Added a page of existing accounts to the single community',
	);
	if (page.pageState === null) {
		await finishRun(kvClient, run.run_id);
		return;
	}
	const next: JoinRun = {...run, page_state: page.pageState, updated_at: Date.now(), joining: []};
	await saveRun(kvClient, next);
	await helpers.addJob('joinSingleCommunityMembers', pagePayload(next), {
		jobKey: pageJobKey(next.run_id, next.page_state),
	});
}

const joinSingleCommunityMembers: WorkerTaskHandler = async (payload, helpers) => {
	const {run_id: runId, guild_id: legacyGuildId, page_state: pageState = null} = PayloadSchema.parse(payload);
	const {kvClient} = getWorkerDependencies();
	if (runId === undefined && legacyGuildId !== undefined) {
		await adoptLegacyPage(kvClient, legacyGuildId, pageState, helpers);
		return;
	}
	const isDue = (run: JoinRun | null): run is JoinRun =>
		run !== null &&
		(runId === undefined
			? Date.now() - run.updated_at >= STALL_MS
			: run.run_id === runId && run.page_state === pageState);
	if (!isDue(await readRun(kvClient))) return;
	const token = randomUUID();
	if (!(await kvClient.acquireLock(LOCK_KEY, token, LOCK_TTL_SECONDS))) {
		if (runId === undefined) return;
		throw new Error('Another job is adding existing accounts to the single community');
	}
	try {
		const run = await readRun(kvClient);
		if (!isDue(run)) return;
		if (runId === undefined) helpers.logger.warn({runId: run.run_id}, 'Resuming a stalled single-community join');
		await joinPage(kvClient, run, token, helpers);
	} finally {
		await kvClient.releaseLock(LOCK_KEY, token);
	}
};

export default joinSingleCommunityMembers;
