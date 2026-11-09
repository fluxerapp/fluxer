// SPDX-License-Identifier: AGPL-3.0-or-later

import {parseArgs} from 'node:util';
import {type MigrationOutput, MigrationUsageError, positiveInteger} from '@app/api/migrate/MigrationCli';
import {
	type KVTransferKindDigest,
	PostgresTransferSide,
	transferKV,
	ValkeyTransferSide,
} from '@pkgs/kv_client/src/KVTransfer';
import {postgresKVClientConfig} from '@pkgs/kv_client/src/PostgresKVProvider';
import {POSTGRES_KV_RATE_LIMIT_PREFIX} from '@pkgs/kv_client/src/PostgresKVSchema';
import Redis from 'ioredis';

const KV_MIGRATION_DEFAULT_BATCH_SIZE = 1000;
const KV_MIGRATION_TTL_TOLERANCE_MS = 5000;
const KV_MIGRATION_SKIPPED_PREFIXES: ReadonlyArray<string> = [POSTGRES_KV_RATE_LIMIT_PREFIX];

type KVMigrationDirection = 'postgres' | 'valkey';

export interface ValkeyPostgresMigrationOptions {
	to: KVMigrationDirection;
	fromUrl: string | null;
	toUrl: string | null;
	verifyOnly: boolean;
	replace: boolean;
	batchSize: number;
}

export interface ValkeyPostgresMigrationReport {
	to: KVMigrationDirection;
	verified: boolean;
	copied: number;
	kinds: Array<KVTransferKindDigest>;
	targetKinds: Array<KVTransferKindDigest>;
	skipped: number;
	ttlMismatches: Array<string>;
}

export const KV_MIGRATION_USAGE = `usage: MigrateEntrypoint kv [--from-url <valkey url>] [--to postgres|valkey] [--to-url <valkey url>] [--verify-only] [--replace] [--batch-size <n>]

  --from-url     the Valkey to copy from, for --to postgres
  --to           postgres copies Valkey into the configured Postgres, valkey copies it back
  --to-url       the Valkey to copy into, for --to valkey
  --verify-only  compare both sides by key count and sha256 per kind without copying
  --replace      empty the fluxer_kvs_* tables or the target Valkey first
  --batch-size   keys per batch, ${KV_MIGRATION_DEFAULT_BATCH_SIZE} by default`;

export function parseValkeyPostgresArgs(args: ReadonlyArray<string>): ValkeyPostgresMigrationOptions {
	let parsed: ReturnType<typeof parseKVArgs>;
	try {
		parsed = parseKVArgs(args);
	} catch (error) {
		throw new MigrationUsageError(error instanceof Error ? error.message : String(error));
	}
	const {values} = parsed;
	const to = values.to ?? 'postgres';
	if (to !== 'postgres' && to !== 'valkey') {
		throw new MigrationUsageError(`--to must be postgres or valkey, not ${JSON.stringify(to)}`);
	}
	const fromUrl = values['from-url'] ?? null;
	const toUrl = values['to-url'] ?? null;
	if (to === 'postgres' && !fromUrl) {
		throw new MigrationUsageError('--to postgres needs --from-url');
	}
	if (to === 'valkey' && !toUrl) {
		throw new MigrationUsageError('--to valkey needs --to-url');
	}
	return {
		to,
		fromUrl,
		toUrl,
		verifyOnly: values['verify-only'] ?? false,
		replace: values.replace ?? false,
		batchSize: positiveInteger('batch-size', values['batch-size'], KV_MIGRATION_DEFAULT_BATCH_SIZE),
	};
}

function parseKVArgs(args: ReadonlyArray<string>) {
	return parseArgs({
		args: [...args],
		strict: true,
		allowPositionals: false,
		options: {
			'from-url': {type: 'string'},
			to: {type: 'string'},
			'to-url': {type: 'string'},
			'verify-only': {type: 'boolean'},
			replace: {type: 'boolean'},
			'batch-size': {type: 'string'},
		},
	});
}

export async function runValkeyPostgresMigration(
	options: ValkeyPostgresMigrationOptions,
	output: MigrationOutput,
): Promise<ValkeyPostgresMigrationReport> {
	const valkeyUrl = options.to === 'postgres' ? options.fromUrl : options.toUrl;
	if (!valkeyUrl) {
		throw new MigrationUsageError(`--to ${options.to} needs a Valkey URL`);
	}
	const {Config} = await import('@app/api/Config');
	const redis = new Redis(valkeyUrl, {lazyConnect: true, maxRetriesPerRequest: 1});
	const postgres = PostgresTransferSide.connect(
		postgresKVClientConfig(Config.postgres, {maxConnections: 2, synchronousCommitOff: true}),
	);
	try {
		await redis.connect();
		const valkey = new ValkeyTransferSide(redis, KV_MIGRATION_SKIPPED_PREFIXES);
		await postgres.prepare();
		const [source, target] = options.to === 'postgres' ? [valkey, postgres] : [postgres, valkey];
		output.info(`${options.verifyOnly ? 'verifying' : 'copying'} ${source.name} to ${target.name}`);
		const result = await transferKV(source, target, {
			batchSize: options.batchSize,
			verifyOnly: options.verifyOnly,
			replace: options.replace,
			ttlToleranceMs: KV_MIGRATION_TTL_TOLERANCE_MS,
			onProgress: (message) => output.info(message),
		});
		return {
			to: options.to,
			verified: result.verified,
			copied: result.copied,
			kinds: result.source,
			targetKinds: result.target,
			skipped: result.skipped,
			ttlMismatches: result.ttlMismatches,
		};
	} finally {
		redis.disconnect();
		await postgres.close();
	}
}
