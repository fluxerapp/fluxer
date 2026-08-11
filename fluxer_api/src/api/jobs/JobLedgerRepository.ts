// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {seconds} from 'itty-time';
import {
	BatchBuilder,
	deleteOneOrMany,
	executeQuery,
	fetchMany,
	fetchOne,
	fetchPage,
	upsertOne,
} from '../database/CassandraQueryExecution';
import {type CassandraParams, Db} from '../database/CassandraTypes';
import type {
	JobActiveRow,
	JobActiveV2Row,
	JobByDayBucketRow,
	JobByIdRow,
	JobStatus,
} from '../database/types/JobLedgerTypes';
import {Logger} from '../Logger';
import {JobsActive, JobsActiveLegacy, JobsByDayBucket, JobsById} from '../Tables';
import {
	type CreateJobInput,
	type DeadletterPublicationLease,
	IJobLedgerRepository,
	type ListActiveJobsInput,
	type ListActiveJobsResult,
	type ListJobsCursor,
	type ListJobsFilters,
	type ListJobsResult,
	type ReconcileActiveJobsInput,
	type ReconcileActiveJobsResult,
} from './IJobLedgerRepository';

const FETCH_JOB_BY_ID_QUERY = JobsById.select({
	where: JobsById.where.eq('job_id'),
});
const FETCH_CANCEL_REQUESTED_QUERY = JobsById.select({
	where: JobsById.where.eq('job_id'),
});
const ACTIVE_JOBS_SHARD_COUNT = 64;
const ACTIVE_AUTHORITY_HYDRATION_LIMIT = 200;

export const JOB_HISTORY_RETENTION_SECONDS = seconds('7 days');
const TERMINAL_JOB_STATUSES = new Set<JobStatus>(['succeeded', 'failed', 'cancelled', 'deadletter']);
type JobByIdPatch = Parameters<typeof JobsById.patchByPk>[1];
type JobByIdCondition = Parameters<typeof JobsById.patchByPkIf>[2];

function bucketDayFor(d: Date): string {
	return d.toISOString().slice(0, 10);
}

export class JobLedgerRepository extends IJobLedgerRepository {
	private activeReconcileShard = 0;
	private activeReconcileCursor: bigint | null = null;
	private legacyMigrationPageState: string | null = null;

	private activePk(jobId: bigint): {shard: number; job_id: bigint} {
		return {shard: this.activeShard(jobId), job_id: jobId};
	}

	private activeShard(jobId: bigint): number {
		const positive = jobId < 0n ? -jobId : jobId;
		return Number(positive % BigInt(ACTIVE_JOBS_SHARD_COUNT));
	}

	private legacyActiveRow(row: JobActiveV2Row): JobActiveRow {
		const {shard: _shard, ...legacy} = row;
		return legacy;
	}

	private async upsertActive(row: JobActiveV2Row): Promise<void> {
		await Promise.all([
			upsertOne(JobsActive.upsertAll(row)),
			upsertOne(JobsActiveLegacy.upsertAll(this.legacyActiveRow(row))),
		]);
	}

	private async deleteActive(jobId: bigint): Promise<void> {
		await Promise.all([
			deleteOneOrMany(JobsActive.deleteByPk(this.activePk(jobId))),
			deleteOneOrMany(JobsActiveLegacy.deleteByPk({job_id: jobId})),
		]);
	}

	private async migrateLegacyActivePage(limit: number): Promise<void> {
		if (limit <= 0) return;
		const page = await fetchPage<JobActiveRow>(JobsActiveLegacy.select().bind({}), undefined, {
			pageSize: limit,
			pageState: this.legacyMigrationPageState,
		});
		this.legacyMigrationPageState = page.pageState;
		for (const legacy of page.rows) {
			await upsertOne(
				JobsActive.upsertAll({
					shard: this.activeShard(legacy.job_id),
					...legacy,
				}),
			);
		}
	}

	private async fetchActiveReconciliationPage(limit: number): Promise<Array<JobActiveV2Row>> {
		if (limit <= 0) return [];
		await this.migrateLegacyActivePage(limit);
		const rows: Array<JobActiveV2Row> = [];
		let shardsVisited = 0;
		while (rows.length < limit && shardsVisited < ACTIVE_JOBS_SHARD_COUNT) {
			const remaining = limit - rows.length;
			const where =
				this.activeReconcileCursor === null
					? JobsActive.where.eq('shard')
					: [JobsActive.where.eq('shard'), JobsActive.where.gt('job_id', 'last_job_id')];
			const query = JobsActive.select({where, orderBy: {col: 'job_id', direction: 'ASC'}, limit: remaining + 1});
			const params: CassandraParams = {shard: this.activeReconcileShard};
			if (this.activeReconcileCursor !== null) params['last_job_id'] = this.activeReconcileCursor;
			const scanned = await fetchMany<JobActiveV2Row>(query.bind(params));
			const accepted = scanned.slice(0, remaining);
			rows.push(...accepted);
			if (scanned.length > remaining) {
				this.activeReconcileCursor = accepted.at(-1)?.job_id ?? this.activeReconcileCursor;
				break;
			}
			this.activeReconcileCursor = null;
			this.activeReconcileShard = (this.activeReconcileShard + 1) % ACTIVE_JOBS_SHARD_COUNT;
			shardsVisited += 1;
		}
		return rows;
	}

	async createJob(input: CreateJobInput): Promise<void> {
		const now = new Date();
		const status: JobStatus = 'queued';
		const idRow: JobByIdRow = {
			job_id: input.jobId,
			task_type: input.taskType,
			status,
			progress_current: null,
			progress_total: null,
			progress_message: null,
			payload: JSON.stringify(input.payload),
			result: null,
			error_message: null,
			created_at: now,
			state_changed_at: now,
			started_at: null,
			completed_at: null,
			requested_by_user_id: input.requestedByUserId,
			audit_log_reason: input.auditLogReason,
			jet_stream_seq: input.jetStreamSeq,
			jet_stream_lane: input.jetStreamLane,
			lease_token: null,
			lease_expires_at: null,
			dlq_attempts: 0,
			attempts: 0,
			max_attempts: input.maxAttempts,
			run_at: input.runAt,
			cancel_requested: false,
			context_link: null,
		};
		const bucketRow: JobByDayBucketRow = {
			bucket_day: bucketDayFor(now),
			created_at: now,
			job_id: input.jobId,
			task_type: input.taskType,
			status,
			requested_by_user_id: input.requestedByUserId,
		};
		const activeRow: JobActiveV2Row = {
			shard: this.activeShard(input.jobId),
			job_id: input.jobId,
			task_type: input.taskType,
			status,
			requested_by_user_id: input.requestedByUserId,
			created_at: now,
			started_at: null,
		};
		const batch = new BatchBuilder();
		batch.addPrepared(JobsById.insert(idRow));
		batch.addPrepared(JobsByDayBucket.insert(bucketRow));
		batch.addPrepared(JobsActive.insert(activeRow));
		batch.addPrepared(JobsActiveLegacy.insert(this.legacyActiveRow(activeRow)));
		await batch.executeChunked(10, true);
	}

	async getJob(jobId: bigint): Promise<JobByIdRow | null> {
		return fetchOne<JobByIdRow>(FETCH_JOB_BY_ID_QUERY.bind({job_id: jobId}));
	}

	private async wasApplied(query: ReturnType<typeof JobsById.patchByPkIf>): Promise<boolean> {
		const [result] = await executeQuery<{'[applied]': boolean}>(query);
		return result?.['[applied]'] === true;
	}

	private ownedCondition(leaseToken: string): JobByIdCondition {
		return {col: 'lease_token', expected: leaseToken};
	}

	private async writeNonTerminalBucket(job: JobByIdRow): Promise<void> {
		await upsertOne(
			JobsByDayBucket.upsertAll({
				bucket_day: bucketDayFor(job.created_at),
				created_at: job.created_at,
				job_id: job.job_id,
				task_type: job.task_type,
				status: job.status,
				requested_by_user_id: job.requested_by_user_id,
			}),
		);
	}

	private async synchronizeActiveFromAuthoritative(jobId: bigint): Promise<void> {
		for (let attempt = 0; attempt < 2; attempt += 1) {
			const current = await this.getJob(jobId);
			if (!current) {
				await this.deleteActive(jobId);
				return;
			}
			if (TERMINAL_JOB_STATUSES.has(current.status)) {
				await this.finalizeTerminalIndexes(current);
				return;
			}
			await this.writeNonTerminalBucket(current);
			await this.upsertActive({
				shard: this.activeShard(current.job_id),
				job_id: current.job_id,
				task_type: current.task_type,
				status: current.status,
				requested_by_user_id: current.requested_by_user_id,
				created_at: current.created_at,
				started_at: current.started_at,
			});
			const verified = await this.getJob(jobId);
			if (!verified) {
				await this.deleteActive(jobId);
				return;
			}
			if (TERMINAL_JOB_STATUSES.has(verified.status)) {
				await this.finalizeTerminalIndexes(verified);
				return;
			}
			if (verified.status === current.status && verified.started_at?.getTime() === current.started_at?.getTime()) {
				return;
			}
		}
	}

	private async patchNonTerminal(
		jobId: bigint,
		buildPatch: (current: JobByIdRow) => JobByIdPatch,
		leaseToken: string | null = null,
	): Promise<boolean> {
		for (let attempt = 0; attempt < 2; attempt += 1) {
			const current = await this.getJob(jobId);
			if (!current || TERMINAL_JOB_STATUSES.has(current.status)) return false;
			if (leaseToken !== null && current.lease_token !== leaseToken) return false;
			const applied = await this.wasApplied(
				JobsById.patchByPkIf(
					{job_id: jobId},
					buildPatch(current),
					leaseToken !== null ? this.ownedCondition(leaseToken) : {col: 'status', expected: current.status},
				),
			);
			if (applied) return true;
		}
		return false;
	}

	async claimJob(
		jobId: bigint,
		lane: string,
		leaseToken: string,
		now: Date,
		leaseDurationMs: number,
	): Promise<JobByIdRow | null> {
		const current = await this.getJob(jobId);
		if (!current) return null;
		const leaseExpiresAt = new Date(now.getTime() + leaseDurationMs);
		let condition: JobByIdCondition;
		if (current.status === 'queued') {
			condition = {col: 'status', expected: 'queued'};
		} else if (
			current.status === 'running' &&
			typeof current.lease_token === 'string' &&
			current.lease_expires_at instanceof Date &&
			current.lease_expires_at.getTime() <= now.getTime()
		) {
			condition = [
				{col: 'lease_token', expected: current.lease_token},
				{col: 'lease_expires_at', expected: current.lease_expires_at},
			];
		} else if (
			current.status === 'running' &&
			current.lease_token == null &&
			current.lease_expires_at == null &&
			(current.state_changed_at ?? current.started_at ?? current.created_at).getTime() + leaseDurationMs <=
				now.getTime()
		) {
			condition = [
				{col: 'status', expected: 'running'},
				{col: 'lease_token', expected: null},
				{col: 'lease_expires_at', expected: null},
			];
		} else {
			return null;
		}
		const applied = await this.wasApplied(
			JobsById.patchByPkIf(
				{job_id: jobId},
				{
					status: Db.set('running'),
					state_changed_at: Db.set(now),
					started_at: Db.set(current.started_at ?? now),
					jet_stream_lane: Db.set(lane),
					lease_token: Db.set(leaseToken),
					lease_expires_at: Db.set(leaseExpiresAt),
				},
				condition,
			),
		);
		if (!applied) return null;
		await this.synchronizeActiveFromAuthoritative(jobId);
		const claimed = await this.getJob(jobId);
		return claimed?.lease_token === leaseToken ? claimed : null;
	}

	async renewLease(jobId: bigint, leaseToken: string, now: Date, leaseDurationMs: number): Promise<boolean> {
		return this.extendLeaseIfLater(jobId, leaseToken, new Date(now.getTime() + leaseDurationMs), 'running');
	}

	private async extendLeaseIfLater(
		jobId: bigint,
		leaseToken: string,
		candidateExpiry: Date,
		expectedStatus: Extract<JobStatus, 'running' | 'deadletter_pending'>,
	): Promise<boolean> {
		for (let attempt = 0; attempt < 3; attempt += 1) {
			const current = await this.getJob(jobId);
			if (!current || current.status !== expectedStatus || current.lease_token !== leaseToken) return false;
			if (current.lease_expires_at && current.lease_expires_at.getTime() >= candidateExpiry.getTime()) return true;
			const applied = await this.wasApplied(
				JobsById.patchByPkIf({job_id: jobId}, {lease_expires_at: Db.set(candidateExpiry)}, [
					{col: 'status', expected: expectedStatus},
					{col: 'lease_token', expected: leaseToken},
					{col: 'lease_expires_at', expected: current.lease_expires_at},
				]),
			);
			if (applied) return true;
		}
		return false;
	}

	async releaseForRetry(
		jobId: bigint,
		errorMessage: string,
		incrementAttempt: boolean,
		leaseToken: string,
	): Promise<boolean> {
		const current = await this.getJob(jobId);
		if (!current || current.status !== 'running' || current.lease_token !== leaseToken) return false;
		const status: JobStatus = 'queued';
		const applied = await this.wasApplied(
			JobsById.patchByPkIf(
				{job_id: jobId},
				{
					status: Db.set(status),
					state_changed_at: Db.set(new Date()),
					started_at: Db.clear(),
					lease_token: Db.clear(),
					lease_expires_at: Db.clear(),
					error_message: Db.set(errorMessage),
					attempts: Db.set(current.attempts + (incrementAttempt ? 1 : 0)),
				},
				this.ownedCondition(leaseToken),
			),
		);
		if (!applied) {
			const latest = await this.getJob(jobId);
			if (!latest || TERMINAL_JOB_STATUSES.has(latest.status)) {
				await this.deleteActive(jobId);
			}
			return false;
		}
		await this.upsertActive({
			shard: this.activeShard(current.job_id),
			job_id: current.job_id,
			task_type: current.task_type,
			status,
			requested_by_user_id: current.requested_by_user_id,
			created_at: current.created_at,
			started_at: null,
		});
		await this.synchronizeActiveFromAuthoritative(jobId);
		return true;
	}

	async markEnqueueFailed(jobId: bigint, errorMessage: string): Promise<boolean> {
		return this.markTerminal(jobId, 'failed', {result: null, errorMessage}, 'queued');
	}

	async markSucceeded(jobId: bigint, result: Record<string, unknown> | null, leaseToken: string): Promise<boolean> {
		return this.markTerminal(
			jobId,
			'succeeded',
			{
				result: result === null ? null : JSON.stringify(result),
				errorMessage: null,
			},
			'running',
			leaseToken,
		);
	}

	async markFailed(jobId: bigint, errorMessage: string, leaseToken: string | null = null): Promise<boolean> {
		return this.markTerminal(jobId, 'failed', {result: null, errorMessage}, undefined, leaseToken);
	}

	private async markFailedIfObserved(job: JobByIdRow, errorMessage: string): Promise<boolean> {
		return this.markTerminal(job.job_id, 'failed', {result: null, errorMessage}, undefined, null, [
			{col: 'status', expected: job.status},
			{col: 'state_changed_at', expected: job.state_changed_at},
			{col: 'lease_token', expected: job.lease_token},
			{col: 'lease_expires_at', expected: job.lease_expires_at},
			{col: 'attempts', expected: job.attempts},
		]);
	}

	async markCancelled(jobId: bigint, leaseToken: string): Promise<boolean> {
		return this.markTerminal(jobId, 'cancelled', {result: null, errorMessage: null}, 'running', leaseToken);
	}

	async markDeadletterPending(
		jobId: bigint,
		errorMessage: string,
		leaseToken: string | null = null,
		now = new Date(),
		leaseDurationMs = 60_000,
	): Promise<DeadletterPublicationLease | null> {
		const publicationToken = `deadletter:${randomUUID()}`;
		const leaseExpiresAt = new Date(now.getTime() + leaseDurationMs);
		if (leaseToken !== null) {
			const applied = await this.wasApplied(
				JobsById.patchByPkIf(
					{job_id: jobId},
					{
						status: Db.set('deadletter_pending'),
						state_changed_at: Db.set(now),
						error_message: Db.set(errorMessage),
						lease_token: Db.set(publicationToken),
						lease_expires_at: Db.set(leaseExpiresAt),
						dlq_attempts: Db.set(0),
					},
					this.ownedCondition(leaseToken),
				),
			);
			if (!applied) return null;
			await this.synchronizeActiveFromAuthoritative(jobId);
			return {leaseToken: publicationToken, errorMessage};
		}

		for (let attempt = 0; attempt < 2; attempt += 1) {
			const current = await this.getJob(jobId);
			if (current?.status !== 'deadletter_pending') return null;
			if (current.lease_expires_at && current.lease_expires_at.getTime() > now.getTime()) return null;
			const applied = await this.wasApplied(
				JobsById.patchByPkIf(
					{job_id: jobId},
					{
						state_changed_at: Db.set(now),
						lease_token: Db.set(publicationToken),
						lease_expires_at: Db.set(leaseExpiresAt),
					},
					[
						{col: 'status', expected: current.status},
						{col: 'state_changed_at', expected: current.state_changed_at},
						{col: 'lease_token', expected: current.lease_token},
						{col: 'lease_expires_at', expected: current.lease_expires_at},
					],
				),
			);
			if (applied) {
				await this.synchronizeActiveFromAuthoritative(jobId);
				return {leaseToken: publicationToken, errorMessage: current.error_message ?? errorMessage};
			}
		}
		return null;
	}

	async renewDeadletterPublicationLease(
		jobId: bigint,
		leaseToken: string,
		now: Date,
		leaseDurationMs: number,
	): Promise<boolean> {
		return this.extendLeaseIfLater(jobId, leaseToken, new Date(now.getTime() + leaseDurationMs), 'deadletter_pending');
	}

	async recordDlqPublishFailure(jobId: bigint, leaseToken: string): Promise<number | null> {
		for (let attempt = 0; attempt < 2; attempt += 1) {
			const current = await this.getJob(jobId);
			if (current?.status !== 'deadletter_pending' || current.lease_token !== leaseToken) return null;
			const nextAttempts = (current.dlq_attempts ?? 0) + 1;
			const applied = await this.wasApplied(
				JobsById.patchByPkIf(
					{job_id: jobId},
					{
						dlq_attempts: Db.set(nextAttempts),
						lease_token: Db.set(`deadletter:released:${randomUUID()}`),
						lease_expires_at: Db.clear(),
					},
					[
						{col: 'status', expected: 'deadletter_pending'},
						{col: 'lease_token', expected: leaseToken},
					],
				),
			);
			if (applied) return nextAttempts;
		}
		return null;
	}

	async markDeadletter(jobId: bigint, errorMessage: string, leaseToken: string): Promise<boolean> {
		return this.markTerminal(jobId, 'deadletter', {result: null, errorMessage}, 'deadletter_pending', leaseToken);
	}

	private terminalHistoryTtl(job: JobByIdRow, now = new Date()): number {
		if (!job.completed_at) return JOB_HISTORY_RETENTION_SECONDS;
		const deadlineMs = job.completed_at.getTime() + JOB_HISTORY_RETENTION_SECONDS * 1000;
		return Math.max(0, Math.ceil((deadlineMs - now.getTime()) / 1000));
	}

	private async writeTerminalBucket(job: JobByIdRow, now = new Date()): Promise<void> {
		const bucketRow: JobByDayBucketRow = {
			bucket_day: bucketDayFor(job.created_at),
			created_at: job.created_at,
			job_id: job.job_id,
			task_type: job.task_type,
			status: job.status,
			requested_by_user_id: job.requested_by_user_id,
		};
		const ttl = this.terminalHistoryTtl(job, now);
		if (ttl > 0) {
			await upsertOne(JobsByDayBucket.upsertAllWithTtl(bucketRow, ttl));
		} else {
			await deleteOneOrMany(
				JobsByDayBucket.deleteByPk({
					bucket_day: bucketRow.bucket_day,
					created_at: bucketRow.created_at,
					job_id: bucketRow.job_id,
				}),
			);
		}
	}

	private async finalizeTerminalIndexes(job: JobByIdRow, now = new Date()): Promise<void> {
		await this.writeTerminalBucket(job, now);
		await this.deleteActive(job.job_id);
	}

	private async markTerminal(
		jobId: bigint,
		status: Extract<JobStatus, 'succeeded' | 'failed' | 'cancelled' | 'deadletter'>,
		outcome: {result: string | null; errorMessage: string | null},
		expectedStatus?: JobStatus,
		leaseToken: string | null = null,
		conditionOverride?: JobByIdCondition,
	): Promise<boolean> {
		const current = await this.getJob(jobId);
		if (!current) return false;
		if (TERMINAL_JOB_STATUSES.has(current.status)) {
			await this.finalizeTerminalIndexes(current);
			return current.status === status;
		}
		if (expectedStatus && current.status !== expectedStatus) return false;
		if (leaseToken !== null && current.lease_token !== leaseToken) return false;
		const completedAt = new Date();
		const updated: JobByIdRow = {
			...current,
			status,
			state_changed_at: completedAt,
			completed_at: completedAt,
			result: outcome.result,
			error_message: outcome.errorMessage,
			lease_token: null,
			lease_expires_at: null,
			dlq_attempts: current.dlq_attempts ?? 0,
		};
		await this.writeTerminalBucket(updated, completedAt);
		let applied: boolean;
		try {
			applied = await this.wasApplied(
				JobsById.patchByPkWithTtlIf(
					{job_id: jobId},
					{
						task_type: Db.set(updated.task_type),
						status: Db.set(updated.status),
						progress_current: Db.set(updated.progress_current),
						progress_total: Db.set(updated.progress_total),
						progress_message: Db.set(updated.progress_message),
						payload: Db.set(updated.payload),
						result: Db.set(updated.result),
						error_message: Db.set(updated.error_message),
						created_at: Db.set(updated.created_at),
						state_changed_at: Db.set(updated.state_changed_at),
						started_at: Db.set(updated.started_at),
						completed_at: Db.set(updated.completed_at),
						requested_by_user_id: Db.set(updated.requested_by_user_id),
						audit_log_reason: Db.set(updated.audit_log_reason),
						jet_stream_seq: Db.set(updated.jet_stream_seq),
						jet_stream_lane: Db.set(updated.jet_stream_lane),
						lease_token: Db.set(updated.lease_token),
						lease_expires_at: Db.set(updated.lease_expires_at),
						dlq_attempts: Db.set(updated.dlq_attempts),
						attempts: Db.set(updated.attempts),
						max_attempts: Db.set(updated.max_attempts),
						run_at: Db.set(updated.run_at),
						cancel_requested: Db.set(updated.cancel_requested),
						context_link: Db.set(updated.context_link),
					},
					JOB_HISTORY_RETENTION_SECONDS,
					conditionOverride ??
						(leaseToken !== null
							? {col: 'lease_token', expected: leaseToken}
							: {col: 'status', expected: expectedStatus ?? current.status}),
				),
			);
		} catch (error) {
			try {
				await this.synchronizeActiveFromAuthoritative(jobId);
			} catch (repairError) {
				Logger.error({jobId, err: repairError}, 'Failed to repair job indexes after terminal CAS error');
			}
			throw error;
		}
		if (!applied) {
			await this.synchronizeActiveFromAuthoritative(jobId);
			return false;
		}
		await this.deleteActive(updated.job_id);
		return true;
	}

	async reportProgress(
		jobId: bigint,
		current: number,
		total: number | null,
		message: string | null,
		leaseToken: string,
	): Promise<void> {
		await this.patchNonTerminal(
			jobId,
			() => ({
				progress_current: Db.set(BigInt(current)),
				progress_total: total === null ? Db.clear() : Db.set(BigInt(total)),
				progress_message: message === null ? Db.clear() : Db.set(message),
			}),
			leaseToken,
		);
	}

	async setContextLink(jobId: bigint, link: string, leaseToken: string): Promise<void> {
		await this.patchNonTerminal(jobId, () => ({context_link: Db.set(link)}), leaseToken);
	}

	async requestCancel(jobId: bigint): Promise<boolean> {
		return this.patchNonTerminal(jobId, () => ({cancel_requested: Db.set(true)}));
	}

	async isCancelRequested(jobId: bigint): Promise<boolean> {
		const row = await fetchOne<{
			cancel_requested: boolean | null;
		}>(FETCH_CANCEL_REQUESTED_QUERY.bind({job_id: jobId}));
		return row?.cancel_requested === true;
	}

	async listJobs(opts: {
		limit: number;
		cursor: ListJobsCursor | null;
		filters: ListJobsFilters;
		maxLookbackDays: number;
	}): Promise<ListJobsResult> {
		const {limit, cursor, filters, maxLookbackDays} = opts;
		if (limit <= 0) return {jobs: [], nextCursor: null};
		const startBucket = cursor ? new Date(`${cursor.bucketDay}T00:00:00Z`) : new Date();
		const collected: Array<JobByIdRow> = [];
		let lastReturnedCursor: ListJobsCursor | null = null;
		let nextCursor: ListJobsCursor | null = null;
		const scanChunkSize = Math.max(25, Math.min(250, limit + 1));
		const orderBy = [
			{col: 'created_at' as const, direction: 'DESC' as const},
			{col: 'job_id' as const, direction: 'DESC' as const},
		];
		for (let dayOffset = 0; dayOffset <= maxLookbackDays && nextCursor === null; dayOffset++) {
			const bucketDate = new Date(startBucket);
			bucketDate.setUTCDate(bucketDate.getUTCDate() - dayOffset);
			const bucketDay = bucketDayFor(bucketDate);
			let scanCursor = dayOffset === 0 ? cursor : null;
			while (nextCursor === null) {
				const query = JobsByDayBucket.select({
					where:
						scanCursor === null
							? JobsByDayBucket.where.eq('bucket_day')
							: [
									JobsByDayBucket.where.eq('bucket_day'),
									JobsByDayBucket.where.tupleLt(['created_at', 'job_id'], ['cursor_created_at', 'cursor_job_id']),
								],
					orderBy,
					limit: scanChunkSize,
				});
				const params: CassandraParams = {bucket_day: bucketDay};
				if (scanCursor !== null) {
					params['cursor_created_at'] = scanCursor.createdAt;
					params['cursor_job_id'] = scanCursor.jobId;
				}
				const bucketRows = await fetchMany<JobByDayBucketRow>(query.bind(params));
				for (const bucketRow of bucketRows) {
					const fullRow = await this.getJob(bucketRow.job_id);
					if (!fullRow) continue;
					if (filters.status && fullRow.status !== filters.status) continue;
					if (filters.taskType && fullRow.task_type !== filters.taskType) continue;
					if (
						filters.requestedByUserId !== undefined &&
						filters.requestedByUserId !== null &&
						fullRow.requested_by_user_id !== filters.requestedByUserId
					) {
						continue;
					}
					if (collected.length === limit) {
						nextCursor = lastReturnedCursor;
						break;
					}
					collected.push(fullRow);
					lastReturnedCursor = {bucketDay, createdAt: bucketRow.created_at, jobId: bucketRow.job_id};
				}
				if (nextCursor !== null || bucketRows.length < scanChunkSize) break;
				const lastScanned = bucketRows.at(-1) as JobByDayBucketRow;
				scanCursor = {bucketDay, createdAt: lastScanned.created_at, jobId: lastScanned.job_id};
			}
		}
		return {jobs: collected, nextCursor};
	}

	async reconcileActiveJobs(input: ReconcileActiveJobsInput): Promise<ReconcileActiveJobsResult> {
		const result: ReconcileActiveJobsResult = {
			scanned: 0,
			removedMissing: 0,
			removedTerminal: 0,
			failedStaleQueued: 0,
			failedExpiredRunning: 0,
			failedExpiredDeadletter: 0,
			skippedFresh: 0,
			skippedScheduled: 0,
			skippedRunning: 0,
			skippedStateChanged: 0,
		};
		const activeRows = await this.fetchActiveReconciliationPage(Math.max(0, input.limit));
		for (const active of activeRows) {
			result.scanned += 1;
			const job = await this.getJob(active.job_id);
			if (!job) {
				await deleteOneOrMany(
					JobsByDayBucket.deleteByPk({
						bucket_day: bucketDayFor(active.created_at),
						created_at: active.created_at,
						job_id: active.job_id,
					}),
				);
				await this.deleteActive(active.job_id);
				result.removedMissing += 1;
				continue;
			}
			if (TERMINAL_JOB_STATUSES.has(job.status)) {
				await this.finalizeTerminalIndexes(job, input.now);
				result.removedTerminal += 1;
				continue;
			}
			await this.synchronizeActiveFromAuthoritative(job.job_id);
			if (job.status === 'deadletter_pending') {
				const pendingSince = job.state_changed_at ?? job.started_at ?? job.created_at;
				const publicationLeaseExpired =
					job.lease_expires_at === null || job.lease_expires_at.getTime() <= input.now.getTime();
				if (pendingSince.getTime() < input.staleBefore.getTime() && publicationLeaseExpired) {
					const failed = await this.markFailedIfObserved(
						job,
						'Dead-letter publication outlived the worker queue recovery window',
					);
					if (failed) result.failedExpiredDeadletter += 1;
					else result.skippedStateChanged += 1;
				} else {
					result.skippedFresh += 1;
				}
				continue;
			}
			if (job.status === 'running') {
				const runningSince = job.state_changed_at ?? job.started_at ?? job.created_at;
				const isPastRecoveryWindow = runningSince.getTime() < input.staleBefore.getTime();
				const hasExpiredLease =
					typeof job.lease_token === 'string' &&
					job.lease_expires_at instanceof Date &&
					job.lease_expires_at.getTime() <= input.now.getTime();
				const isStaleLegacyLease = job.lease_token == null && job.lease_expires_at == null;
				if (isPastRecoveryWindow && (hasExpiredLease || isStaleLegacyLease)) {
					const failed = await this.markFailedIfObserved(
						job,
						'Running job lease outlived the worker queue retention window',
					);
					if (failed) result.failedExpiredRunning += 1;
					else result.skippedStateChanged += 1;
				} else {
					result.skippedRunning += 1;
				}
				continue;
			}
			const latestQueuedTransition = job.state_changed_at ?? job.created_at;
			const queuedSince =
				job.run_at && job.run_at.getTime() > latestQueuedTransition.getTime() ? job.run_at : latestQueuedTransition;
			if (queuedSince.getTime() >= input.staleBefore.getTime()) {
				if (job.run_at && job.run_at.getTime() > input.now.getTime()) result.skippedScheduled += 1;
				else result.skippedFresh += 1;
				continue;
			}
			const failed = await this.markFailedIfObserved(job, 'Queued job outlived the worker queue retention window');
			if (failed) result.failedStaleQueued += 1;
			else result.skippedStateChanged += 1;
		}
		return result;
	}

	async listActiveJobs(input: ListActiveJobsInput): Promise<ListActiveJobsResult> {
		const limit = Math.max(0, Math.min(ACTIVE_AUTHORITY_HYDRATION_LIMIT, input.limit));
		if (limit === 0) return {jobs: [], nextPageState: null};
		await this.migrateLegacyActivePage(limit);
		const cursor = this.decodeActiveCursor(input.pageState, input.taskType ?? null);
		let shard = cursor.shard;
		let lastJobId = cursor.lastJobId;
		const pageRows: Array<JobActiveV2Row> = [];
		let nextPageState: string | null = null;
		while (pageRows.length < limit) {
			const remaining = limit - pageRows.length;
			const query = JobsActive.select({
				where:
					lastJobId === null
						? JobsActive.where.eq('shard')
						: [JobsActive.where.eq('shard'), JobsActive.where.gt('job_id', 'last_job_id')],
				orderBy: {col: 'job_id', direction: 'ASC'},
				limit: remaining + 1,
			});
			const params: CassandraParams = {shard};
			if (lastJobId !== null) params['last_job_id'] = lastJobId;
			const scanned = await fetchMany<JobActiveV2Row>(query.bind(params));
			const accepted = scanned.slice(0, remaining);
			pageRows.push(...accepted);
			if (scanned.length > remaining) {
				nextPageState = this.encodeActiveCursor(shard, accepted.at(-1)!.job_id, input.taskType ?? null);
				break;
			}
			if (shard + 1 >= ACTIVE_JOBS_SHARD_COUNT) {
				nextPageState = null;
				break;
			}
			shard += 1;
			lastJobId = null;
			nextPageState = this.encodeActiveCursor(shard, null, input.taskType ?? null);
		}
		const matching = input.taskType ? pageRows.filter((row) => row.task_type === input.taskType) : pageRows;
		const fullRows = await Promise.all(matching.map((row) => this.getJob(row.job_id)));
		return {
			jobs: fullRows.filter((row): row is JobByIdRow => row !== null && !TERMINAL_JOB_STATUSES.has(row.status)),
			nextPageState,
		};
	}

	private decodeActiveCursor(
		pageState: string | null,
		taskType: string | null,
	): {shard: number; lastJobId: bigint | null} {
		if (pageState === null) return {shard: 0, lastJobId: null};
		const decoded = JSON.parse(Buffer.from(pageState, 'base64url').toString('utf8')) as Record<string, unknown>;
		if (
			decoded['version'] !== 1 ||
			!Number.isInteger(decoded['shard']) ||
			(decoded['shard'] as number) < 0 ||
			(decoded['shard'] as number) >= ACTIVE_JOBS_SHARD_COUNT ||
			(decoded['taskType'] ?? null) !== taskType ||
			!(decoded['lastJobId'] === null || typeof decoded['lastJobId'] === 'string')
		) {
			throw new Error('Invalid active-jobs page state');
		}
		return {
			shard: decoded['shard'] as number,
			lastJobId: decoded['lastJobId'] === null ? null : BigInt(decoded['lastJobId'] as string),
		};
	}

	private encodeActiveCursor(shard: number, lastJobId: bigint | null, taskType: string | null): string {
		return Buffer.from(
			JSON.stringify({version: 1, shard, lastJobId: lastJobId?.toString() ?? null, taskType}),
		).toString('base64url');
	}

	async listActiveJobsByTaskType(
		taskType: string,
		input: Omit<ListActiveJobsInput, 'taskType'>,
	): Promise<ListActiveJobsResult> {
		return this.listActiveJobs({...input, taskType});
	}
}
