// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {InMemoryCassandraQueryExecutor} from '../test/InMemoryCassandraQueryExecutor';
import {defineTable} from './CassandraTableDsl';
import {type CassandraParams, normalizeBinaryParams} from './CassandraTypes';

interface BinaryRow {
	bucket: string;
	value: Uint8Array;
	payload: {data: Uint8Array};
}

const BinaryRows = defineTable<BinaryRow, 'bucket' | 'value', 'bucket'>({
	name: 'binary_rows',
	columns: ['bucket', 'value', 'payload'],
	primaryKey: ['bucket', 'value'],
	partitionKey: ['bucket'],
});

describe('normalizeBinaryParams', () => {
	it('copies Buffer and sliced Uint8Array values into canonical Buffers recursively', () => {
		const backing = new Uint8Array([0xaa, 0x00, 0x80, 0xff, 0xbb]);
		const sliced = backing.subarray(1, 4);
		const original = Buffer.from([0x01, 0x02]);
		const normalized = normalizeBinaryParams({
			direct: sliced,
			array: [sliced],
			set: new Set([sliced]),
			map: new Map([[sliced, original]]),
		} as unknown as CassandraParams);

		expect(normalized.direct).toEqual(Buffer.from([0x00, 0x80, 0xff]));
		expect(Buffer.isBuffer(normalized.direct)).toBe(true);
		expect(normalized.direct).not.toBe(sliced);
		expect((normalized.array as Array<Buffer>)[0]).toEqual(Buffer.from([0x00, 0x80, 0xff]));
		expect([...(normalized.set as Set<Buffer>)][0]).toEqual(Buffer.from([0x00, 0x80, 0xff]));
		const [[mapKey, mapValue]] = [...(normalized.map as Map<Buffer, Buffer>)];
		expect(mapKey).toEqual(Buffer.from([0x00, 0x80, 0xff]));
		expect(mapValue).toEqual(original);
		expect(mapValue).not.toBe(original);

		sliced.fill(0x11);
		original.fill(0x22);
		expect(normalized.direct).toEqual(Buffer.from([0x00, 0x80, 0xff]));
		expect(mapValue).toEqual(Buffer.from([0x01, 0x02]));
	});

	it('keeps Buffer and Uint8Array predicate, ordering, and paging semantics aligned in memory', async () => {
		const executor = new InMemoryCassandraQueryExecutor();
		const values = [Buffer.from([0xfb]), Buffer.from([0x00]), Buffer.from([0x7f])];
		for (const value of values) {
			const backing = new Uint8Array([0xaa, value[0]!, 0xbb]);
			await executor.executeQuery(
				BinaryRows.upsertAll({
					bucket: 'a',
					value: backing.subarray(1, 2),
					payload: {data: backing.subarray(1, 2)},
				}),
			);
		}

		const ordered = await executor.executeQuery<BinaryRow>(
			BinaryRows.select({
				where: BinaryRows.where.eq('bucket'),
				orderBy: [{col: 'value', direction: 'ASC'}],
			}).bind({bucket: 'a'}),
		);
		expect(ordered.map((row) => Buffer.from(row.value).toString('hex'))).toEqual(['00', '7f', 'fb']);

		const inRows = await executor.executeQuery<BinaryRow>(
			BinaryRows.select({
				where: [BinaryRows.where.eq('bucket'), BinaryRows.where.in('value', 'values')],
				orderBy: [{col: 'value', direction: 'ASC'}],
			}).bind({bucket: 'a', values: [Buffer.from([0x00]), Buffer.from([0xfb])]}),
		);
		expect(inRows.map((row) => Buffer.from(row.value).toString('hex'))).toEqual(['00', 'fb']);

		for (const [operator, expected] of [
			['lt', ['00']],
			['lte', ['00', '7f']],
			['gt', ['fb']],
			['gte', ['7f', 'fb']],
		] as const) {
			const rows = await executor.executeQuery<BinaryRow>(
				BinaryRows.select({
					where: [BinaryRows.where.eq('bucket'), BinaryRows.where[operator]('value')],
					orderBy: [{col: 'value', direction: 'ASC'}],
				}).bind({bucket: 'a', value: Buffer.from([0x7f])}),
			);
			expect(rows.map((row) => Buffer.from(row.value).toString('hex'))).toEqual(expected);
		}

		const tupleRows = await executor.executeQuery<BinaryRow>(
			BinaryRows.select({
				where: BinaryRows.where.tupleGt(['bucket', 'value'], ['bucket', 'value']),
				orderBy: [
					{col: 'bucket', direction: 'ASC'},
					{col: 'value', direction: 'ASC'},
				],
			}).bind({bucket: 'a', value: Buffer.from([0x7f])}),
		);
		expect(tupleRows.map((row) => Buffer.from(row.value).toString('hex'))).toEqual(['fb']);

		const firstPage = await executor.executePagedQuery<BinaryRow>(
			BinaryRows.select({
				where: BinaryRows.where.eq('bucket'),
				orderBy: [{col: 'value', direction: 'ASC'}],
			}).bind({bucket: 'a'}),
			{pageSize: 2},
		);
		const secondPage = await executor.executePagedQuery<BinaryRow>(
			BinaryRows.select({
				where: BinaryRows.where.eq('bucket'),
				orderBy: [{col: 'value', direction: 'ASC'}],
			}).bind({bucket: 'a'}),
			{pageSize: 2, pageState: firstPage.pageState},
		);
		expect(firstPage.rows.map((row) => Buffer.from(row.value).toString('hex'))).toEqual(['00', '7f']);
		expect(secondPage.rows.map((row) => Buffer.from(row.value).toString('hex'))).toEqual(['fb']);

		ordered[0]!.value[0] = 0x22;
		ordered[0]!.payload.data[0] = 0x33;
		const refetched = await executor.executeQuery<BinaryRow>(
			BinaryRows.select({where: BinaryRows.where.eq('bucket')}).bind({bucket: 'a'}),
		);
		expect(refetched.some((row) => Buffer.from(row.value).equals(Buffer.from([0x00])))).toBe(true);
		expect(refetched.some((row) => Buffer.from(row.payload.data).equals(Buffer.from([0x00])))).toBe(true);
	});
});
