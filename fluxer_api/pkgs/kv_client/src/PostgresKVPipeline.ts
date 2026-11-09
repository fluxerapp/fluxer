// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IKVPipeline} from '@pkgs/kv_client/src/IKVProvider';
import type pg from 'pg';

type PipelineResult = [Error | null, unknown];

export interface PostgresKVSetexEntry {
	key: string;
	ttlSeconds: number;
	value: string;
}

export interface PostgresKVPipelineCommands {
	connect(): Promise<pg.PoolClient>;
	assertUnderCap(): Promise<void>;
	setexBatch(db: pg.PoolClient, entries: Array<PostgresKVSetexEntry>): Promise<void>;
	del(db: pg.PoolClient, key: string): Promise<number>;
	expire(db: pg.PoolClient, key: string, ttlSeconds: number): Promise<number>;
	sadd(db: pg.PoolClient, key: string, members: Array<string>): Promise<number>;
	hgetall(db: pg.PoolClient, key: string): Promise<Record<string, string>>;
	drainHash(db: pg.PoolClient, key: string): Promise<{hash: Record<string, string>; deleted: number}>;
	normalizeError(command: string, error: unknown): Error;
}

type Operation =
	| {command: 'setex'; key: string; ttlSeconds: number; value: string}
	| {command: 'del'; key: string}
	| {command: 'expire'; key: string; ttlSeconds: number}
	| {command: 'sadd'; key: string; members: Array<string>}
	| {command: 'hgetall'; key: string};

type Step = {kind: 'setex'; entries: Array<PostgresKVSetexEntry>} | {kind: 'single'; operation: Operation};

export class PostgresKVPipeline implements IKVPipeline {
	private operations: Array<Operation> = [];

	constructor(
		private readonly commands: PostgresKVPipelineCommands,
		private readonly mode: 'pipeline' | 'multi',
	) {}

	setex(key: string, ttlSeconds: number, value: string): this {
		this.operations.push({command: 'setex', key, ttlSeconds, value});
		return this;
	}

	del(key: string): this {
		this.operations.push({command: 'del', key});
		return this;
	}

	expire(key: string, ttlSeconds: number): this {
		this.operations.push({command: 'expire', key, ttlSeconds});
		return this;
	}

	sadd(key: string, ...members: Array<string>): this {
		this.operations.push({command: 'sadd', key, members});
		return this;
	}

	hgetall(key: string): this {
		this.operations.push({command: 'hgetall', key});
		return this;
	}

	async exec(): Promise<Array<PipelineResult>> {
		const operations = this.operations;
		this.operations = [];
		const command = `${this.mode}.exec`;
		if (operations.length === 0) return [];
		let client: pg.PoolClient;
		try {
			if (operations.some((operation) => operation.command === 'setex' || operation.command === 'sadd')) {
				await this.commands.assertUnderCap();
			}
			client = await this.commands.connect();
		} catch (error) {
			throw this.commands.normalizeError(command, error);
		}
		let broken = false;
		try {
			if (this.mode === 'pipeline') {
				return await this.runSteps(client, operations, false);
			}
			const drain = drainKey(operations);
			if (drain !== null) {
				const {hash, deleted} = await this.commands.drainHash(client, drain);
				return [
					[null, hash],
					[null, deleted],
				];
			}
			await client.query('BEGIN');
			try {
				const results = await this.runSteps(client, operations, true);
				await client.query('COMMIT');
				return results;
			} catch (error) {
				await client.query('ROLLBACK').catch(() => {
					broken = true;
				});
				throw error;
			}
		} catch (error) {
			throw this.commands.normalizeError(command, error);
		} finally {
			client.release(broken);
		}
	}

	private async runSteps(
		client: pg.PoolClient,
		operations: Array<Operation>,
		savepoints: boolean,
	): Promise<Array<PipelineResult>> {
		const results: Array<PipelineResult> = [];
		for (const step of groupSteps(operations)) {
			const size = step.kind === 'setex' ? step.entries.length : 1;
			const name = step.kind === 'setex' ? 'setex' : step.operation.command;
			if (savepoints) await client.query('SAVEPOINT kvs_step');
			try {
				const value = await this.runStep(client, step);
				if (savepoints) await client.query('RELEASE SAVEPOINT kvs_step');
				for (let index = 0; index < size; index++) results.push([null, value]);
			} catch (error) {
				if (savepoints) await client.query('ROLLBACK TO SAVEPOINT kvs_step');
				const normalized = this.commands.normalizeError(name, error);
				for (let index = 0; index < size; index++) results.push([normalized, null]);
			}
		}
		return results;
	}

	private async runStep(client: pg.PoolClient, step: Step): Promise<unknown> {
		if (step.kind === 'setex') {
			await this.commands.setexBatch(client, step.entries);
			return 'OK';
		}
		const operation = step.operation;
		switch (operation.command) {
			case 'setex':
				await this.commands.setexBatch(client, [operation]);
				return 'OK';
			case 'del':
				return await this.commands.del(client, operation.key);
			case 'expire':
				return await this.commands.expire(client, operation.key, operation.ttlSeconds);
			case 'sadd':
				return operation.members.length === 0 ? 0 : await this.commands.sadd(client, operation.key, operation.members);
			case 'hgetall':
				return await this.commands.hgetall(client, operation.key);
		}
	}
}

function drainKey(operations: Array<Operation>): string | null {
	if (operations.length !== 2) return null;
	const [read, remove] = operations;
	if (read.command !== 'hgetall' || remove.command !== 'del' || read.key !== remove.key) return null;
	return read.key;
}

function groupSteps(operations: Array<Operation>): Array<Step> {
	const steps: Array<Step> = [];
	for (const operation of operations) {
		const last = steps[steps.length - 1];
		if (operation.command === 'setex') {
			if (last?.kind === 'setex') {
				last.entries.push(operation);
			} else {
				steps.push({kind: 'setex', entries: [operation]});
			}
			continue;
		}
		steps.push({kind: 'single', operation});
	}
	return steps;
}
