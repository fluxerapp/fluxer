// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IKVPipeline} from '@pkgs/kv_client/src/IKVProvider';
import {createInvalidResponseError} from '@pkgs/kv_client/src/KVClientError';
import {createStringEntriesFromPairs} from '@pkgs/kv_client/src/KVCommandArguments';
import type {ChainableCommander} from 'ioredis';

type PipelineExecResult = [Error | null, unknown];
type PipelineCommandReply = [Error] | PipelineExecResult;

interface KVPipelineOptions {
	createCommander: () => ChainableCommander;
	normalizeError: (command: string, error: unknown) => Error;
	mode: 'pipeline' | 'multi';
}

export class KVPipeline implements IKVPipeline {
	private readonly createCommander: () => ChainableCommander;
	private readonly normalizeError: (command: string, error: unknown) => Error;
	private readonly mode: 'pipeline' | 'multi';
	private commander: ChainableCommander;

	constructor(options: KVPipelineOptions) {
		this.createCommander = options.createCommander;
		this.normalizeError = options.normalizeError;
		this.mode = options.mode;
		this.commander = options.createCommander();
	}

	get(key: string): this {
		this.commander.get(key);
		return this;
	}

	set(key: string, value: string): this {
		this.commander.set(key, value);
		return this;
	}

	setex(key: string, ttlSeconds: number, value: string): this {
		this.commander.setex(key, ttlSeconds, value);
		return this;
	}

	del(key: string): this {
		this.commander.del(key);
		return this;
	}

	expire(key: string, ttlSeconds: number): this {
		this.commander.expire(key, ttlSeconds);
		return this;
	}

	sadd(key: string, ...members: Array<string>): this {
		this.commander.sadd(key, ...members);
		return this;
	}

	srem(key: string, ...members: Array<string>): this {
		this.commander.srem(key, ...members);
		return this;
	}

	zadd(key: string, score: number, value: string): this {
		this.commander.zadd(key, score, value);
		return this;
	}

	zrem(key: string, ...members: Array<string>): this {
		this.commander.zrem(key, ...members);
		return this;
	}

	hgetall(key: string): this {
		this.commander.hgetall(key);
		return this;
	}

	mset(...args: Array<string>): this {
		const entries = createStringEntriesFromPairs(args);
		if (entries.length === 0) {
			return this;
		}
		const pairs = entries.flatMap((entry) => [entry.key, entry.value]);
		this.commander.mset(...pairs);
		return this;
	}

	async exec(): Promise<Array<PipelineExecResult>> {
		const command = `${this.mode}.exec`;
		try {
			const commander = this.commander;
			const expectedResults = commander.length - (this.mode === 'multi' ? 1 : 0);
			this.commander = this.createCommander();
			const rawResults = await commander.exec();
			if (!Array.isArray(rawResults) || rawResults.length !== expectedResults) {
				throw createInvalidResponseError(command, `${expectedResults} command results`);
			}
			return Array.from(rawResults, (result: unknown): PipelineExecResult => {
				if (!isPipelineCommandReply(result)) {
					throw createInvalidResponseError(command, 'an error/value pair for every command');
				}
				return [result[0], result[1]];
			});
		} catch (error) {
			throw this.normalizeError(command, error);
		}
	}
}

function isPipelineCommandReply(value: unknown): value is PipelineCommandReply {
	if (!Array.isArray(value)) return false;
	if (value[0] instanceof Error) return value.length === 1 || value.length === 2;
	return value.length === 2 && value[0] === null && value[1] !== undefined;
}
