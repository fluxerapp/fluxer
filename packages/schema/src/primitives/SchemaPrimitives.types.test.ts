// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type ReportAdminResponseSchema,
	type ResolveReportResponse,
	SearchReportsRequest,
} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {
	ReportFlowQuery,
	ReportFlowSurface,
	ReportFlowTargetType,
} from '@fluxer/schema/src/domains/report/ReportFlowSchemas';
import {
	createInt32EnumType,
	createNamedLiteralUnion,
	createNamedStringLiteralUnion,
	withFieldDescription,
	withOpenApiType,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {schemaMetadata} from '@fluxer/schema/src/SchemaMetadata';
import {describe, expect, expectTypeOf, it} from 'vitest';
import {z} from 'zod';

describe('named literal union inference', () => {
	it('infers string literals from a bare union', () => {
		const schema = createNamedStringLiteralUnion([
			['asc', 'asc', 'Ascending'],
			['desc', 'desc', 'Descending'],
		]);
		expectTypeOf<z.infer<typeof schema>>().toEqualTypeOf<'asc' | 'desc'>();
		expect(schema.options.map((option) => option.value)).toEqual(['asc', 'desc']);
	});
	it('infers string literals through withOpenApiType', () => {
		const schema = withOpenApiType(
			createNamedStringLiteralUnion([
				['asc', 'asc', 'Ascending'],
				['desc', 'desc', 'Descending'],
			]),
			'TypeTestSortOrder',
		);
		expectTypeOf<z.infer<typeof schema>>().toEqualTypeOf<'asc' | 'desc'>();
		expect(schemaMetadata.get(schema)?.name).toBe('TypeTestSortOrder');
	});
	it('infers string literals through withFieldDescription', () => {
		const schema = withFieldDescription(
			createNamedStringLiteralUnion([
				['asc', 'asc', 'Ascending'],
				['desc', 'desc', 'Descending'],
			]),
			'Sort order',
		);
		expectTypeOf<z.infer<typeof schema>>().toEqualTypeOf<'asc' | 'desc'>();
		expect(schema.description).toBe('Sort order');
	});
	it('infers string literals through both wrappers and object modifiers', () => {
		const schema = z.object({
			order: withFieldDescription(
				withOpenApiType(
					createNamedStringLiteralUnion([
						['asc', 'asc', 'Ascending'],
						['desc', 'desc', 'Descending'],
					]),
					'TypeTestNestedSortOrder',
				),
				'Sort order',
			).optional(),
			fallback: withOpenApiType(
				createNamedStringLiteralUnion([
					['asc', 'asc'],
					['desc', 'desc'],
				]),
				'TypeTestFallbackSortOrder',
			).default('asc'),
		});
		expectTypeOf<z.infer<typeof schema>>().toEqualTypeOf<{order?: 'asc' | 'desc'; fallback: 'asc' | 'desc'}>();
		expect(schema.parse({})).toEqual({fallback: 'asc'});
	});
	it('infers number literals from a bare union', () => {
		const schema = createNamedLiteralUnion([
			[0, 'ROLE', 'A role'],
			[1, 'MEMBER', 'A member'],
		]);
		expectTypeOf<z.infer<typeof schema>>().toEqualTypeOf<0 | 1>();
		expect(schema.options.map((option) => option.value)).toEqual([0, 1]);
	});
	it('infers number literals through withOpenApiType and withFieldDescription', () => {
		const named = withOpenApiType(
			createNamedLiteralUnion([
				[0, 'ROLE', 'A role'],
				[1, 'MEMBER', 'A member'],
			]),
			'TypeTestOverwriteType',
		);
		const described = withFieldDescription(
			createNamedLiteralUnion([
				[0, 'ROLE', 'A role'],
				[1, 'MEMBER', 'A member'],
			]),
			'Overwrite type',
		);
		expectTypeOf<z.infer<typeof named>>().toEqualTypeOf<0 | 1>();
		expectTypeOf<z.infer<typeof described>>().toEqualTypeOf<0 | 1>();
		expect(named.safeParse(2).success).toBe(false);
		expect(described.safeParse(1).success).toBe(true);
	});
	it('infers the widened value type from pairs that are not literals', () => {
		const pairs: ReadonlyArray<readonly [string, string]> = [
			['asc', 'asc'],
			['desc', 'desc'],
		];
		const schema = createNamedStringLiteralUnion(pairs);
		expectTypeOf<z.infer<typeof schema>>().toEqualTypeOf<string>();
		expect(schema.safeParse('asc').success).toBe(true);
	});
	it('infers the report flow target and surface unions', () => {
		expectTypeOf<ReportFlowTargetType>().toEqualTypeOf<'message' | 'user' | 'guild'>();
		expectTypeOf<ReportFlowSurface>().toEqualTypeOf<'in_app' | 'dsa'>();
		expectTypeOf<ReportFlowQuery['surface']>().toEqualTypeOf<'in_app' | 'dsa'>();
		expect(ReportFlowTargetType.options.map((option) => option.value)).toEqual(['message', 'user', 'guild']);
		expect(ReportFlowSurface.options.map((option) => option.value)).toEqual(['in_app', 'dsa']);
		expect(ReportFlowQuery.parse({}).surface).toBe('in_app');
	});
});

describe('int32 enum inference', () => {
	it('infers number literals from a bare enum', () => {
		const schema = createInt32EnumType(
			[
				[0, 'PENDING', 'Pending'],
				[1, 'RESOLVED', 'Resolved'],
			],
			'Status',
			'TypeTestBareStatus',
		);
		expectTypeOf<z.infer<typeof schema>>().toEqualTypeOf<0 | 1>();
		expect(schema.parse(1)).toBe(1);
	});
	it('infers number literals through withOpenApiType and withFieldDescription', () => {
		const named = withOpenApiType(
			createInt32EnumType(
				[
					[0, 'PENDING', 'Pending'],
					[1, 'RESOLVED', 'Resolved'],
				],
				'Status',
				'TypeTestWrappedStatus',
			),
			'TypeTestWrappedStatus',
		);
		const described = withFieldDescription(
			createInt32EnumType([
				[0, 'MESSAGE'],
				[1, 'USER'],
				[2, 'GUILD'],
			]),
			'Report type',
		);
		expectTypeOf<z.infer<typeof named>>().toEqualTypeOf<0 | 1>();
		expectTypeOf<z.infer<typeof described>>().toEqualTypeOf<0 | 1 | 2>();
		expect(named.safeParse(2).success).toBe(false);
		expect(described.safeParse(2).success).toBe(true);
		expect(schemaMetadata.get(named)).toEqual({
			name: 'TypeTestWrappedStatus',
			format: 'int32',
			enumEntries: [
				{value: 0, name: 'PENDING', description: 'Pending'},
				{value: 1, name: 'RESOLVED', description: 'Resolved'},
			],
		});
	});
	it('infers the widened value type from pairs that are not literals', () => {
		const pairs: ReadonlyArray<readonly [number, string]> = [
			[0, 'PENDING'],
			[1, 'RESOLVED'],
		];
		const schema = createInt32EnumType(pairs);
		expectTypeOf<z.infer<typeof schema>>().toEqualTypeOf<number>();
		expect(schema.safeParse(1).success).toBe(true);
	});
	it('infers the report status and type literals on the admin schemas', () => {
		expectTypeOf<z.infer<typeof ReportAdminResponseSchema>['status']>().toEqualTypeOf<0 | 1>();
		expectTypeOf<z.infer<typeof ReportAdminResponseSchema>['report_type']>().toEqualTypeOf<0 | 1 | 2>();
		expectTypeOf<z.infer<typeof ResolveReportResponse>['status']>().toEqualTypeOf<0 | 1>();
		expectTypeOf<SearchReportsRequest['status']>().toEqualTypeOf<0 | 1 | undefined>();
		expectTypeOf<SearchReportsRequest['report_type']>().toEqualTypeOf<0 | 1 | 2 | undefined>();
		expect(SearchReportsRequest.safeParse({status: 2}).success).toBe(false);
		expect(SearchReportsRequest.safeParse({report_type: 2}).success).toBe(true);
	});
});
