// SPDX-License-Identifier: AGPL-3.0-or-later

export const POSTGRES_KV_SCHEMA_VERSION = 1;

export const POSTGRES_KV_TABLES = {
	key: 'fluxer_kvs_key',
	hash: 'fluxer_kvs_hash',
	set: 'fluxer_kvs_set',
	zset: 'fluxer_kvs_zset',
	list: 'fluxer_kvs_list',
	bucket: 'fluxer_kvs_bucket',
	meta: 'fluxer_kvs_meta',
} as const;

export const POSTGRES_KV_FUNCTIONS = {
	purge: 'kvs_purge_v1',
	prepare: 'kvs_prepare_v1',
	set: 'kvs_set_v1',
	expire: 'kvs_expire_v1',
	incr: 'kvs_incr_v1',
	stored: 'kvs_stored_number_v1',
	leakyBucket: 'kvs_leaky_bucket_v1',
	hset: 'kvs_hset_v1',
	hdel: 'kvs_hdel_v1',
	sadd: 'kvs_sadd_v1',
	srem: 'kvs_srem_v1',
	zadd: 'kvs_zadd_v1',
	zrem: 'kvs_zrem_v1',
	rpush: 'kvs_rpush_v1',
	lpop: 'kvs_lpop_v1',
	ltrim: 'kvs_ltrim_v1',
} as const;

export const POSTGRES_KV_META_KEYS = {
	schemaVersion: 'schema_version',
	overCap: 'over_cap',
	sizeBytes: 'size_bytes',
} as const;

export const POSTGRES_KV_NOTIFY_CHANNEL = 'fluxer_kvs';
export const POSTGRES_KV_NOTIFY_MAX_PAYLOAD_BYTES = 7999;
export const POSTGRES_KV_ADVISORY_LOCK_NAMESPACE = 0x6b767331;
const POSTGRES_KV_SCHEMA_LOCK_KEY = 'schema';
export const POSTGRES_KV_RATE_LIMIT_PREFIX = 'ratelimit:';
export const POSTGRES_KV_WRONGTYPE_SQLSTATE = 'KV001';
export const POSTGRES_KV_SCRIPT_SQLSTATE = 'KV002';

export interface PostgresKVQueryResult {
	rows: Array<Record<string, unknown>>;
	rowCount: number | null;
}

export interface PostgresKVQueryable {
	query(text: string, values?: Array<unknown>): Promise<PostgresKVQueryResult>;
}

const T = POSTGRES_KV_TABLES;
const F = POSTGRES_KV_FUNCTIONS;
const LIVE = '(expires_at IS NULL OR expires_at > now())';
const HOT_TABLE_OPTIONS = 'autovacuum_vacuum_scale_factor = 0.02, autovacuum_analyze_scale_factor = 0.02';
const WRONGTYPE = `RAISE EXCEPTION USING ERRCODE = '${POSTGRES_KV_WRONGTYPE_SQLSTATE}', MESSAGE = 'WRONGTYPE Operation against a key holding the wrong kind of value'`;

const TABLES = [
	`CREATE TABLE IF NOT EXISTS ${T.key} (key bytea PRIMARY KEY, kind "char" NOT NULL, value bytea, expires_at timestamptz, list_head bigint, list_tail bigint) WITH (fillfactor = 85, ${HOT_TABLE_OPTIONS})`,
	`CREATE INDEX IF NOT EXISTS ${T.key}_expires_idx ON ${T.key} (expires_at) WHERE expires_at IS NOT NULL`,
	`CREATE TABLE IF NOT EXISTS ${T.hash} (key bytea NOT NULL, field bytea NOT NULL, value bytea NOT NULL, PRIMARY KEY (key, field)) WITH (fillfactor = 85, ${HOT_TABLE_OPTIONS})`,
	`CREATE TABLE IF NOT EXISTS ${T.set} (key bytea NOT NULL, member bytea NOT NULL, PRIMARY KEY (key, member)) WITH (${HOT_TABLE_OPTIONS})`,
	`CREATE TABLE IF NOT EXISTS ${T.zset} (key bytea NOT NULL, member bytea NOT NULL, score float8 NOT NULL, PRIMARY KEY (key, member)) WITH (${HOT_TABLE_OPTIONS})`,
	`CREATE INDEX IF NOT EXISTS ${T.zset}_score_idx ON ${T.zset} (key, score, member)`,
	`CREATE TABLE IF NOT EXISTS ${T.list} (key bytea NOT NULL, seq bigint NOT NULL, value bytea NOT NULL, PRIMARY KEY (key, seq)) WITH (${HOT_TABLE_OPTIONS})`,
	`CREATE UNLOGGED TABLE IF NOT EXISTS ${T.bucket} (key bytea PRIMARY KEY, level float8 NOT NULL, updated_ms bigint NOT NULL, expires_ms bigint NOT NULL) WITH (fillfactor = 70, ${HOT_TABLE_OPTIONS})`,
	`CREATE TABLE IF NOT EXISTS ${T.meta} (name text PRIMARY KEY, value text NOT NULL)`,
];

const FUNCTIONS = [
	`CREATE OR REPLACE FUNCTION ${F.purge}(p_key bytea) RETURNS void LANGUAGE plpgsql AS $$
BEGIN
	DELETE FROM ${T.key} WHERE key = p_key;
	DELETE FROM ${T.hash} WHERE key = p_key;
	DELETE FROM ${T.set} WHERE key = p_key;
	DELETE FROM ${T.zset} WHERE key = p_key;
	DELETE FROM ${T.list} WHERE key = p_key;
END
$$`,
	`CREATE OR REPLACE FUNCTION ${F.prepare}(p_key bytea, p_kind "char", p_overwrite boolean) RETURNS void LANGUAGE plpgsql AS $$
DECLARE
	v_kind "char";
	v_expires timestamptz;
BEGIN
	SELECT kind, expires_at INTO v_kind, v_expires FROM ${T.key} WHERE key = p_key;
	IF FOUND THEN
		IF v_expires IS NOT NULL AND v_expires <= now() THEN
			DELETE FROM ${T.key} WHERE key = p_key AND expires_at <= now() RETURNING kind INTO v_kind;
			IF FOUND THEN
				PERFORM ${F.purge}(p_key);
			END IF;
			RETURN;
		END IF;
		IF v_kind <> p_kind THEN
			IF p_overwrite THEN
				PERFORM ${F.purge}(p_key);
			ELSE
				${WRONGTYPE};
			END IF;
		END IF;
		RETURN;
	END IF;
	IF (p_kind <> 'h' AND EXISTS (SELECT 1 FROM ${T.hash} WHERE key = p_key))
		OR (p_kind <> 'e' AND EXISTS (SELECT 1 FROM ${T.set} WHERE key = p_key))
		OR (p_kind <> 'z' AND EXISTS (SELECT 1 FROM ${T.zset} WHERE key = p_key)) THEN
		IF p_overwrite THEN
			PERFORM ${F.purge}(p_key);
		ELSE
			${WRONGTYPE};
		END IF;
	END IF;
END
$$`,
	`CREATE OR REPLACE FUNCTION ${F.set}(p_key bytea, p_value bytea, p_ttl_ms bigint, p_nx boolean) RETURNS boolean LANGUAGE plpgsql AS $$
DECLARE
	v_expires timestamptz := CASE WHEN p_ttl_ms IS NULL THEN NULL ELSE now() + p_ttl_ms * interval '1 millisecond' END;
BEGIN
	IF p_nx THEN
		INSERT INTO ${T.key} (key, kind, value, expires_at)
			SELECT p_key, 's', p_value, v_expires
			WHERE NOT EXISTS (SELECT 1 FROM ${T.hash} WHERE key = p_key)
				AND NOT EXISTS (SELECT 1 FROM ${T.set} WHERE key = p_key)
				AND NOT EXISTS (SELECT 1 FROM ${T.zset} WHERE key = p_key)
			ON CONFLICT (key) DO NOTHING;
		IF FOUND THEN
			RETURN true;
		END IF;
		DELETE FROM ${T.key} WHERE key = p_key AND expires_at <= now();
		IF NOT FOUND THEN
			RETURN false;
		END IF;
		PERFORM ${F.purge}(p_key);
		INSERT INTO ${T.key} (key, kind, value, expires_at) VALUES (p_key, 's', p_value, v_expires)
			ON CONFLICT (key) DO NOTHING;
		RETURN FOUND;
	END IF;
	UPDATE ${T.key} SET value = p_value, expires_at = v_expires WHERE key = p_key AND kind = 's';
	IF FOUND THEN
		RETURN true;
	END IF;
	PERFORM ${F.purge}(p_key);
	INSERT INTO ${T.key} (key, kind, value, expires_at) VALUES (p_key, 's', p_value, v_expires)
		ON CONFLICT (key) DO UPDATE SET kind = 's', value = EXCLUDED.value, expires_at = EXCLUDED.expires_at, list_head = NULL, list_tail = NULL;
	RETURN true;
END
$$`,
	`CREATE OR REPLACE FUNCTION ${F.expire}(p_key bytea, p_ttl_ms bigint, p_now_ms bigint) RETURNS integer LANGUAGE plpgsql AS $$
DECLARE
	v_kind "char";
	v_expires timestamptz;
BEGIN
	SELECT kind, expires_at INTO v_kind, v_expires FROM ${T.key} WHERE key = p_key FOR UPDATE;
	IF FOUND THEN
		IF v_expires IS NOT NULL AND v_expires <= now() THEN
			PERFORM ${F.purge}(p_key);
			RETURN 0;
		END IF;
		IF v_kind IN ('h', 'e', 'z') AND NOT EXISTS (
			SELECT 1 FROM ${T.hash} WHERE key = p_key
			UNION ALL SELECT 1 FROM ${T.set} WHERE key = p_key
			UNION ALL SELECT 1 FROM ${T.zset} WHERE key = p_key
		) THEN
			DELETE FROM ${T.key} WHERE key = p_key;
			RETURN 0;
		END IF;
		IF p_ttl_ms <= 0 THEN
			PERFORM ${F.purge}(p_key);
		ELSE
			UPDATE ${T.key} SET expires_at = now() + p_ttl_ms * interval '1 millisecond' WHERE key = p_key;
		END IF;
		RETURN 1;
	END IF;
	IF EXISTS (SELECT 1 FROM ${T.hash} WHERE key = p_key) THEN
		v_kind := 'h';
	ELSIF EXISTS (SELECT 1 FROM ${T.set} WHERE key = p_key) THEN
		v_kind := 'e';
	ELSIF EXISTS (SELECT 1 FROM ${T.zset} WHERE key = p_key) THEN
		v_kind := 'z';
	ELSE
		IF p_ttl_ms <= 0 THEN
			DELETE FROM ${T.bucket} WHERE key = p_key AND expires_ms > p_now_ms;
		ELSE
			UPDATE ${T.bucket} SET expires_ms = p_now_ms + p_ttl_ms WHERE key = p_key AND expires_ms > p_now_ms;
		END IF;
		RETURN CASE WHEN FOUND THEN 1 ELSE 0 END;
	END IF;
	IF p_ttl_ms <= 0 THEN
		PERFORM ${F.purge}(p_key);
	ELSE
		INSERT INTO ${T.key} (key, kind, expires_at) VALUES (p_key, v_kind, now() + p_ttl_ms * interval '1 millisecond')
			ON CONFLICT (key) DO UPDATE SET expires_at = EXCLUDED.expires_at;
	END IF;
	RETURN 1;
END
$$`,
	`CREATE OR REPLACE FUNCTION ${F.incr}(p_key bytea) RETURNS bigint LANGUAGE plpgsql AS $$
DECLARE
	v_value bigint;
BEGIN
	PERFORM ${F.prepare}(p_key, 's', false);
	INSERT INTO ${T.key} (key, kind, value) VALUES (p_key, 's', '\\x31')
		ON CONFLICT (key) DO UPDATE SET value = convert_to((convert_from(${T.key}.value, 'UTF8')::bigint + 1)::text, 'UTF8')
		RETURNING convert_from(value, 'UTF8')::bigint INTO v_value;
	RETURN v_value;
END
$$`,
	`CREATE OR REPLACE FUNCTION ${F.hset}(p_key bytea, p_field bytea, p_value bytea) RETURNS integer LANGUAGE plpgsql AS $$
DECLARE
	v_inserted boolean;
BEGIN
	PERFORM ${F.prepare}(p_key, 'h', false);
	INSERT INTO ${T.hash} (key, field, value) VALUES (p_key, p_field, p_value)
		ON CONFLICT (key, field) DO UPDATE SET value = EXCLUDED.value
		RETURNING xmax = 0 INTO v_inserted;
	RETURN CASE WHEN v_inserted THEN 1 ELSE 0 END;
END
$$`,
	`CREATE OR REPLACE FUNCTION ${F.hdel}(p_key bytea, p_fields bytea[]) RETURNS integer LANGUAGE plpgsql AS $$
DECLARE
	v_count integer;
BEGIN
	PERFORM ${F.prepare}(p_key, 'h', false);
	DELETE FROM ${T.hash} WHERE key = p_key AND field = ANY(p_fields);
	GET DIAGNOSTICS v_count = ROW_COUNT;
	IF v_count > 0 AND NOT EXISTS (SELECT 1 FROM ${T.hash} WHERE key = p_key) THEN
		DELETE FROM ${T.key} WHERE key = p_key AND kind = 'h';
	END IF;
	RETURN v_count;
END
$$`,
	`CREATE OR REPLACE FUNCTION ${F.sadd}(p_key bytea, p_members bytea[]) RETURNS integer LANGUAGE plpgsql AS $$
DECLARE
	v_count integer;
BEGIN
	PERFORM ${F.prepare}(p_key, 'e', false);
	INSERT INTO ${T.set} (key, member) SELECT p_key, m FROM unnest(p_members) AS m ON CONFLICT DO NOTHING;
	GET DIAGNOSTICS v_count = ROW_COUNT;
	RETURN v_count;
END
$$`,
	`CREATE OR REPLACE FUNCTION ${F.srem}(p_key bytea, p_members bytea[]) RETURNS integer LANGUAGE plpgsql AS $$
DECLARE
	v_count integer;
BEGIN
	PERFORM ${F.prepare}(p_key, 'e', false);
	DELETE FROM ${T.set} WHERE key = p_key AND member = ANY(p_members);
	GET DIAGNOSTICS v_count = ROW_COUNT;
	IF v_count > 0 AND NOT EXISTS (SELECT 1 FROM ${T.set} WHERE key = p_key) THEN
		DELETE FROM ${T.key} WHERE key = p_key AND kind = 'e';
	END IF;
	RETURN v_count;
END
$$`,
	`CREATE OR REPLACE FUNCTION ${F.zadd}(p_key bytea, p_scores float8[], p_members bytea[]) RETURNS integer LANGUAGE plpgsql AS $$
DECLARE
	v_count integer;
BEGIN
	PERFORM ${F.prepare}(p_key, 'z', false);
	WITH input AS (
		SELECT DISTINCT ON (m) m, s FROM unnest(p_members, p_scores) WITH ORDINALITY AS u(m, s, o) ORDER BY m, o DESC
	), upserted AS (
		INSERT INTO ${T.zset} (key, member, score) SELECT p_key, m, s FROM input
			ON CONFLICT (key, member) DO UPDATE SET score = EXCLUDED.score
			RETURNING xmax = 0 AS inserted
	)
	SELECT count(*) FILTER (WHERE inserted) INTO v_count FROM upserted;
	RETURN v_count;
END
$$`,
	`CREATE OR REPLACE FUNCTION ${F.zrem}(p_key bytea, p_members bytea[]) RETURNS integer LANGUAGE plpgsql AS $$
DECLARE
	v_count integer;
BEGIN
	PERFORM ${F.prepare}(p_key, 'z', false);
	DELETE FROM ${T.zset} WHERE key = p_key AND member = ANY(p_members);
	GET DIAGNOSTICS v_count = ROW_COUNT;
	IF v_count > 0 AND NOT EXISTS (SELECT 1 FROM ${T.zset} WHERE key = p_key) THEN
		DELETE FROM ${T.key} WHERE key = p_key AND kind = 'z';
	END IF;
	RETURN v_count;
END
$$`,
	`CREATE OR REPLACE FUNCTION ${F.rpush}(p_key bytea, p_values bytea[]) RETURNS bigint LANGUAGE plpgsql AS $$
DECLARE
	v_kind "char";
	v_head bigint;
	v_tail bigint;
	v_count bigint := coalesce(array_length(p_values, 1), 0);
BEGIN
	PERFORM ${F.prepare}(p_key, 'l', false);
	LOOP
		SELECT kind, list_head, list_tail INTO v_kind, v_head, v_tail FROM ${T.key} WHERE key = p_key FOR UPDATE;
		EXIT WHEN FOUND;
		INSERT INTO ${T.key} (key, kind, list_head, list_tail) VALUES (p_key, 'l', 0, 0) ON CONFLICT (key) DO NOTHING;
	END LOOP;
	IF v_kind <> 'l' THEN
		${WRONGTYPE};
	END IF;
	INSERT INTO ${T.list} (key, seq, value) SELECT p_key, v_tail + o - 1, v FROM unnest(p_values) WITH ORDINALITY AS u(v, o);
	UPDATE ${T.key} SET list_tail = v_tail + v_count WHERE key = p_key;
	RETURN v_tail + v_count - v_head;
END
$$`,
	`CREATE OR REPLACE FUNCTION ${F.lpop}(p_key bytea, p_count bigint) RETURNS bytea[] LANGUAGE plpgsql AS $$
DECLARE
	v_head bigint;
	v_tail bigint;
	v_taken bigint;
	v_values bytea[];
BEGIN
	PERFORM ${F.prepare}(p_key, 'l', false);
	SELECT list_head, list_tail INTO v_head, v_tail FROM ${T.key} WHERE key = p_key AND kind = 'l' FOR UPDATE;
	IF NOT FOUND THEN
		RETURN ARRAY[]::bytea[];
	END IF;
	v_taken := least(p_count, v_tail - v_head);
	WITH popped AS (
		DELETE FROM ${T.list} WHERE key = p_key AND seq >= v_head AND seq < v_head + v_taken RETURNING seq, value
	)
	SELECT coalesce(array_agg(value ORDER BY seq), ARRAY[]::bytea[]) INTO v_values FROM popped;
	IF v_head + v_taken >= v_tail THEN
		DELETE FROM ${T.key} WHERE key = p_key;
	ELSE
		UPDATE ${T.key} SET list_head = v_head + v_taken WHERE key = p_key;
	END IF;
	RETURN v_values;
END
$$`,
	`CREATE OR REPLACE FUNCTION ${F.ltrim}(p_key bytea, p_start bigint, p_stop bigint) RETURNS void LANGUAGE plpgsql AS $$
DECLARE
	v_head bigint;
	v_tail bigint;
	v_len bigint;
	v_from bigint;
	v_to bigint;
BEGIN
	PERFORM ${F.prepare}(p_key, 'l', false);
	SELECT list_head, list_tail INTO v_head, v_tail FROM ${T.key} WHERE key = p_key AND kind = 'l' FOR UPDATE;
	IF NOT FOUND THEN
		RETURN;
	END IF;
	v_len := v_tail - v_head;
	v_from := CASE WHEN p_start < 0 THEN greatest(v_len + p_start, 0) ELSE p_start END;
	v_to := CASE WHEN p_stop < 0 THEN v_len + p_stop ELSE least(p_stop, v_len - 1) END;
	IF v_from > v_to OR v_from >= v_len THEN
		DELETE FROM ${T.list} WHERE key = p_key;
		DELETE FROM ${T.key} WHERE key = p_key;
		RETURN;
	END IF;
	DELETE FROM ${T.list} WHERE key = p_key AND (seq < v_head + v_from OR seq > v_head + v_to);
	UPDATE ${T.key} SET list_head = v_head + v_from, list_tail = v_head + v_to + 1 WHERE key = p_key;
END
$$`,
	`CREATE OR REPLACE FUNCTION ${F.stored}(p_value float8) RETURNS float8 LANGUAGE plpgsql IMMUTABLE AS $$
DECLARE
	v_bits bigint;
	v_biased integer;
	v_mantissa numeric;
	v_shift integer;
	v_digits numeric;
	v_length integer;
	v_drop integer;
	v_unit numeric;
	v_quotient numeric;
	v_rest numeric;
BEGIN
	IF p_value = 0 OR p_value <> p_value OR p_value IN ('Infinity'::float8, '-Infinity'::float8) THEN
		RETURN p_value;
	END IF;
	v_bits := ('x' || encode(float8send(abs(p_value)), 'hex'))::bit(64)::bigint;
	v_biased := ((v_bits >> 52) & 2047)::integer;
	v_mantissa := (v_bits & 4503599627370495)::numeric;
	IF v_biased = 0 THEN
		v_shift := 1074;
	ELSE
		v_mantissa := v_mantissa + 4503599627370496;
		v_shift := 1075 - v_biased;
	END IF;
	IF v_shift <= 0 THEN
		v_digits := v_mantissa * (2::numeric ^ (-v_shift));
		v_shift := 0;
	ELSE
		v_digits := v_mantissa * (5::numeric ^ v_shift);
	END IF;
	v_digits := trunc(v_digits);
	v_length := length(v_digits::text);
	IF v_length <= 14 THEN
		RETURN p_value;
	END IF;
	v_drop := v_length - 14;
	v_unit := 10::numeric ^ v_drop;
	v_quotient := div(v_digits, v_unit);
	v_rest := v_digits - v_quotient * v_unit;
	IF v_rest * 2 > v_unit OR (v_rest * 2 = v_unit AND mod(v_quotient, 2) = 1) THEN
		v_quotient := v_quotient + 1;
	END IF;
	RETURN sign(p_value) * (v_quotient::text || 'e' || (v_drop - v_shift)::text)::float8;
END
$$`,
	`CREATE OR REPLACE FUNCTION ${F.leakyBucket}(p_key bytea, p_now_ms bigint, p_limit float8, p_window_ms float8, p_cost integer)
RETURNS TABLE (allowed boolean, "limit" bigint, remaining bigint, reset_after_ms bigint, reset_at_ms bigint, retry_after_ms bigint)
LANGUAGE plpgsql AS $$
DECLARE
	v_capacity float8;
	v_window float8;
	v_level float8 := 0;
	v_updated bigint := p_now_ms;
	v_elapsed bigint;
	v_leak float8;
	v_reset float8;
	v_retry float8;
	v_ttl float8;
BEGIN
	IF p_limit IS NULL OR p_limit <= 0 THEN
		RAISE EXCEPTION USING ERRCODE = '${POSTGRES_KV_SCRIPT_SQLSTATE}', MESSAGE = 'limit must be positive';
	END IF;
	IF p_window_ms IS NULL OR p_window_ms <= 0 THEN
		RAISE EXCEPTION USING ERRCODE = '${POSTGRES_KV_SCRIPT_SQLSTATE}', MESSAGE = 'windowMs must be positive';
	END IF;
	IF p_cost IS NULL OR p_cost NOT IN (0, 1) THEN
		RAISE EXCEPTION USING ERRCODE = '${POSTGRES_KV_SCRIPT_SQLSTATE}', MESSAGE = 'cost must be 0 or 1';
	END IF;
	v_capacity := greatest(1, floor(p_limit));
	v_window := greatest(1, floor(p_window_ms));
	SELECT b.level, b.updated_ms INTO v_level, v_updated FROM ${T.bucket} b WHERE b.key = p_key AND b.expires_ms > p_now_ms FOR UPDATE;
	IF NOT FOUND THEN
		v_level := 0;
		v_updated := p_now_ms;
	END IF;
	v_elapsed := p_now_ms - v_updated;
	IF v_elapsed > 0 THEN
		v_level := greatest(0, v_level - (v_elapsed::float8 / v_window) * v_capacity);
		v_updated := p_now_ms;
	END IF;
	v_leak := v_capacity / v_window;
	v_reset := CASE WHEN v_level <= 0 THEN 0 ELSE greatest(0, ceil(v_level / v_leak)) END;
	IF p_cost = 0 THEN
		IF v_level > 0 THEN
			INSERT INTO ${T.bucket} AS b (key, level, updated_ms, expires_ms) VALUES (p_key, ${F.stored}(v_level), v_updated, p_now_ms + greatest(1, v_reset)::bigint)
				ON CONFLICT (key) DO UPDATE SET level = EXCLUDED.level, updated_ms = EXCLUDED.updated_ms, expires_ms = EXCLUDED.expires_ms;
		ELSE
			DELETE FROM ${T.bucket} b WHERE b.key = p_key;
		END IF;
		RETURN QUERY SELECT true, v_capacity::bigint, greatest(0, floor(v_capacity - v_level))::bigint, v_reset::bigint, p_now_ms + v_reset::bigint, 0::bigint;
		RETURN;
	END IF;
	IF v_level + p_cost > v_capacity THEN
		v_retry := greatest(1, ceil((v_level + p_cost - v_capacity) / v_leak));
		v_ttl := greatest(1, v_reset, v_retry);
		INSERT INTO ${T.bucket} AS b (key, level, updated_ms, expires_ms) VALUES (p_key, ${F.stored}(v_level), v_updated, p_now_ms + v_ttl::bigint)
			ON CONFLICT (key) DO UPDATE SET level = EXCLUDED.level, updated_ms = EXCLUDED.updated_ms, expires_ms = EXCLUDED.expires_ms;
		RETURN QUERY SELECT false, v_capacity::bigint, 0::bigint, v_ttl::bigint, p_now_ms + v_ttl::bigint, v_retry::bigint;
		RETURN;
	END IF;
	v_level := v_level + p_cost;
	v_reset := CASE WHEN v_level <= 0 THEN 0 ELSE greatest(0, ceil(v_level / v_leak)) END;
	v_ttl := greatest(1, v_reset);
	INSERT INTO ${T.bucket} AS b (key, level, updated_ms, expires_ms) VALUES (p_key, ${F.stored}(v_level), v_updated, p_now_ms + v_ttl::bigint)
		ON CONFLICT (key) DO UPDATE SET level = EXCLUDED.level, updated_ms = EXCLUDED.updated_ms, expires_ms = EXCLUDED.expires_ms;
	RETURN QUERY SELECT true, v_capacity::bigint, greatest(0, floor(v_capacity - v_level))::bigint, v_reset::bigint, p_now_ms + v_reset::bigint, 0::bigint;
END
$$`,
];

function postgresKVSchemaStatements(): ReadonlyArray<string> {
	return [...TABLES, ...FUNCTIONS];
}

export function postgresKVLiveFilter(alias?: string): string {
	return alias ? LIVE.replaceAll('expires_at', `${alias}.expires_at`) : LIVE;
}

async function readPostgresKVSchemaVersion(db: PostgresKVQueryable): Promise<number | null> {
	const exists = await db.query('SELECT to_regclass($1) IS NOT NULL AS present', [T.meta]);
	if (exists.rows[0]?.present !== true) {
		return null;
	}
	const result = await db.query(`SELECT value FROM ${T.meta} WHERE name = $1`, [POSTGRES_KV_META_KEYS.schemaVersion]);
	const value = result.rows[0]?.value;
	return typeof value === 'string' ? Number(value) : null;
}

export async function ensurePostgresKVStoreSchema(db: PostgresKVQueryable): Promise<number> {
	const current = await readPostgresKVSchemaVersion(db);
	if (current !== null && current >= POSTGRES_KV_SCHEMA_VERSION) {
		return current;
	}
	await db.query('BEGIN');
	try {
		await db.query('SELECT pg_advisory_xact_lock($1, hashtext($2))', [
			POSTGRES_KV_ADVISORY_LOCK_NAMESPACE,
			POSTGRES_KV_SCHEMA_LOCK_KEY,
		]);
		const locked = await readPostgresKVSchemaVersion(db);
		if (locked !== null && locked >= POSTGRES_KV_SCHEMA_VERSION) {
			await db.query('COMMIT');
			return locked;
		}
		for (const statement of postgresKVSchemaStatements()) {
			await db.query(statement);
		}
		await db.query(
			`INSERT INTO ${T.meta} (name, value) VALUES ($1, $2) ON CONFLICT (name) DO UPDATE SET value = EXCLUDED.value`,
			[POSTGRES_KV_META_KEYS.schemaVersion, String(POSTGRES_KV_SCHEMA_VERSION)],
		);
		await db.query('COMMIT');
		return POSTGRES_KV_SCHEMA_VERSION;
	} catch (error) {
		await db.query('ROLLBACK').catch(() => undefined);
		throw error;
	}
}
