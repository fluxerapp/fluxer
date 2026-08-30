// SPDX-License-Identifier: AGPL-3.0-or-later

import cassandra from 'cassandra-driver';
import {describe, expect, it} from 'vitest';
import type {CassandraParams, KvQueryMeta, KvTableSpec, WhereExpr} from './CassandraTypes';
import {buildCandidatePlan, keyFromColumns, matchesWhere} from './PostgresKvQueryExecutor';

type Row = Record<string, unknown>;
type AnyMeta = KvQueryMeta<Row>;

const Spec: KvTableSpec<Row> = {
	name: 'fuzz_two',
	columns: ['k', 'c', 'v'],
	primaryKey: ['k', 'c'],
	partitionKey: ['k', 'c'],
};

const SpecOne: KvTableSpec<Row> = {
	name: 'fuzz_one',
	columns: ['k', 'v'],
	primaryKey: ['k'],
	partitionKey: ['k'],
};

const VALUES: Array<[string, unknown]> = [
	['null', null],
	['undefined', undefined],
	['zero', 0],
	['neg-zero', -0],
	['one-num', 1],
	['one-big', 1n],
	['one-str', '1'],
	['empty-str', ''],
	['true', true],
	['false', false],
	['date-epoch', new Date(0)],
	['date-epoch-2', new Date(0)],
	['buf-a', Buffer.from('a')],
	['buf-a-2', Buffer.from('a')],
	['localdate', cassandra.types.LocalDate.fromString('2020-01-01')],
	['localdate-str', '2020-01-01'],
];

function rowKeyMatchesPlan(plan: ReturnType<typeof buildCandidatePlan>, rowKey: string): boolean {
	const c = plan.candidates;
	const buf = Buffer.from(rowKey, 'utf8');
	switch (c.kind) {
		case 'none':
			return false;
		case 'scan':
			return true;
		case 'rowKeys':
			return c.rowKeys.includes(rowKey);
		case 'range':
			return (
				Buffer.compare(buf, Buffer.from(c.lowerBound, 'utf8')) >= 0 &&
				Buffer.compare(buf, Buffer.from(c.upperBound, 'utf8')) < 0
			);
		case 'ranges':
			return c.lowerBounds.some(
				(lower, i) =>
					Buffer.compare(buf, Buffer.from(lower, 'utf8')) >= 0 &&
					Buffer.compare(buf, Buffer.from(c.upperBounds[i]!, 'utf8')) < 0,
			);
		case 'rangeGroups':
			return c.groups.some((group) =>
				group.lowerBounds.some(
					(lower, i) =>
						Buffer.compare(buf, Buffer.from(lower, 'utf8')) >= 0 &&
						Buffer.compare(buf, Buffer.from(group.upperBounds[i]!, 'utf8')) < 0,
				),
			);
		case 'partitionKeys':
			return c.partitionKeys.includes(rowKey);
	}
}

describe('postgres kv pushdown exactness', () => {
	it('never excludes a row the javascript filter would keep, and is equal when exact', () => {
		const narrowing: Array<string> = [];
		const widening: Array<string> = [];
		const meta = {
			action: 'count',
			table: Spec,
			where: [{kind: 'eq', col: 'k', param: 'k'}] as Array<WhereExpr<Row>>,
		} as AnyMeta;
		for (const [paramName, paramValue] of VALUES) {
			const params = {k: paramValue} as CassandraParams;
			const plan = buildCandidatePlan(meta, params);
			for (const [storedName, storedValue] of VALUES) {
				const row: Row = {k: storedValue, c: 'x', v: 1};
				const rowKey = keyFromColumns(Spec.primaryKey as Array<string>, row);
				const js = matchesWhere(row, meta.where as Array<WhereExpr<Row>>, params);
				const sql = rowKeyMatchesPlan(plan, rowKey);
				const label = `param=${paramName} stored=${storedName} exact=${plan.exact} plan=${plan.candidates.kind}`;
				if (js && !sql) narrowing.push(label);
				if (plan.exact && sql && !js) widening.push(label);
			}
		}
		expect({narrowing, widening}).toEqual({narrowing: [], widening: []});
	});

	it('single-column primary keys behave the same', () => {
		const narrowing: Array<string> = [];
		const widening: Array<string> = [];
		const meta = {
			action: 'count',
			table: SpecOne,
			where: [{kind: 'eq', col: 'k', param: 'k'}] as Array<WhereExpr<Row>>,
		} as AnyMeta;
		for (const [paramName, paramValue] of VALUES) {
			const params = {k: paramValue} as CassandraParams;
			const plan = buildCandidatePlan(meta, params);
			for (const [storedName, storedValue] of VALUES) {
				const row: Row = {k: storedValue, v: 1};
				const rowKey = keyFromColumns(SpecOne.primaryKey as Array<string>, row);
				const js = matchesWhere(row, meta.where as Array<WhereExpr<Row>>, params);
				const sql = rowKeyMatchesPlan(plan, rowKey);
				const label = `param=${paramName} stored=${storedName} exact=${plan.exact} plan=${plan.candidates.kind}`;
				if (js && !sql) narrowing.push(label);
				if (plan.exact && sql && !js) widening.push(label);
			}
		}
		expect({narrowing, widening}).toEqual({narrowing: [], widening: []});
	});

	it('IN lists behave the same', () => {
		const narrowing: Array<string> = [];
		const widening: Array<string> = [];
		const meta = {
			action: 'count',
			table: SpecOne,
			where: [{kind: 'in', col: 'k', param: 'ks'}] as Array<WhereExpr<Row>>,
		} as AnyMeta;
		for (let i = 0; i < VALUES.length; i += 1) {
			for (let j = i; j < VALUES.length; j += 1) {
				const params = {ks: [VALUES[i]![1], VALUES[j]![1]]} as CassandraParams;
				const plan = buildCandidatePlan(meta, params);
				for (const [storedName, storedValue] of VALUES) {
					const row: Row = {k: storedValue, v: 1};
					const rowKey = keyFromColumns(SpecOne.primaryKey as Array<string>, row);
					const js = matchesWhere(row, meta.where as Array<WhereExpr<Row>>, params);
					const sql = rowKeyMatchesPlan(plan, rowKey);
					const label = `in=[${VALUES[i]![0]},${VALUES[j]![0]}] stored=${storedName} exact=${plan.exact} plan=${plan.candidates.kind}`;
					if (js && !sql) narrowing.push(label);
					if (plan.exact && sql && !js) widening.push(label);
				}
			}
		}
		expect({narrowing: narrowing.slice(0, 10), widening: widening.slice(0, 10)}).toEqual({narrowing: [], widening: []});
	});
});
