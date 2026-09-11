import {ZodOpenAPIConverter} from '@fluxer/openapi/src/converters/ZodToOpenAPI';
import type {OpenAPIDocument} from '@fluxer/openapi/src/OpenAPITypes';
import {SudoVerificationSchema} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {HarvestArchiveResponse} from '@fluxer/schema/src/domains/user/UserHarvestSchemas';
import {SnowflakeType, withOpenApiType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {describe, expect, it} from 'vitest';
import {z} from 'zod';

function documentFor(converter: ZodOpenAPIConverter): OpenAPIDocument {
	return {
		openapi: '3.1.0',
		info: {title: 'Schema conversion', version: '1'},
		paths: {},
		components: {schemas: converter.getAllSchemas(), securitySchemes: {}},
	};
}

describe('native Zod conversion', () => {
	it('keeps defaults and nullability separate from input requiredness', () => {
		const schema = z.object({name: z.string().default('guest'), note: z.string().nullable()});
		const converter = new ZodOpenAPIConverter('draft-2020-12');
		const input = converter.getSchema('Profile', schema, 'input');
		const output = converter.getSchema('Profile', schema, 'output');
		expect(input.required).toEqual(['note']);
		expect(output.required).toEqual(['name', 'note']);
		expect(input.properties?.name).toMatchObject({type: 'string', default: 'guest'});
		expect(input.properties?.note).toEqual({anyOf: [{type: 'string'}, {type: 'null'}]});
		expect(input.additionalProperties).toBeUndefined();
		expect(output.additionalProperties).toBe(false);
		const document = documentFor(converter);
		converter.normalizeComponents(document);
		expect(Object.keys(document.components.schemas)).toEqual(['ProfileInput', 'Profile']);
	});

	it('converts wire input without pretending a bigint transform is a JSON output', () => {
		const converter = new ZodOpenAPIConverter('draft-2020-12');
		expect(converter.getSchema('Snowflake', SnowflakeType, 'input')).toMatchObject({
			anyOf: [{type: 'string'}, {type: 'integer'}],
		});
		expect(() => converter.getSchema('Snowflake', SnowflakeType, 'output')).toThrow(
			'Could not convert OpenAPI schema Snowflake (output)',
		);
	});

	it('preserves the shared sudo schema without adding required fields', () => {
		const converter = new ZodOpenAPIConverter('draft-2020-12');
		const schema = converter.getSchema('SudoVerification', SudoVerificationSchema, 'input');
		expect(schema.type).toBe('object');
		expect(schema.required).toBeUndefined();
		expect(schema.properties).toHaveProperty('password');
	});

	it('renames references without rewriting example payloads or schema property names', () => {
		const example = {$ref: '#/components/schemas/ChildInput'};
		const child = z.string().min(1);
		const schema = z.object({default: child, enum: child}).meta({examples: [example]});
		const converter = new ZodOpenAPIConverter('draft-2020-12');
		converter.register('Child', child);
		converter.getRef('Parent', schema, 'input');
		const document = documentFor(converter);
		converter.normalizeComponents(document);
		expect(document.components.schemas.Parent.properties).toEqual({
			default: {$ref: '#/components/schemas/Child'},
			enum: {$ref: '#/components/schemas/Child'},
		});
		expect(document.components.schemas.Parent.examples).toEqual([example]);
		expect(document.components.schemas.Child).toMatchObject({type: 'string', minLength: 1});
		expect(document.components.schemas.ChildInput).toBeUndefined();
	});

	it('keeps reference descriptions valid for OpenAPI 3.0', () => {
		const child = z.string().describe('Shared value');
		const converter = new ZodOpenAPIConverter('openapi-3.0');
		converter.register('Child', child);
		converter.getRef('Parent', z.object({child: child.describe('Field value')}), 'input');
		const document = documentFor(converter);
		document.openapi = '3.0.3';
		converter.normalizeComponents(document);
		expect(document.components.schemas.Parent.properties?.child).toEqual({
			description: 'Field value',
			allOf: [{$ref: '#/components/schemas/Child'}],
		});
	});

	it.each(['openapi-3.0', 'draft-2020-12'] as const)('converts binary archive responses for %s', (target) => {
		const converter = new ZodOpenAPIConverter(target);
		const schema = converter.getSchema('HarvestArchiveResponse', HarvestArchiveResponse, 'output');
		expect(schema).toMatchObject({type: 'string', format: 'binary'});
		expect(schema.contentEncoding).toBe(target === 'openapi-3.0' ? undefined : 'binary');
	});

	it('rejects different constraints that claim the same component name', () => {
		const schema = z.object({
			short: withOpenApiType(z.string().max(10), 'Text'),
			long: withOpenApiType(z.string().max(100), 'Text'),
		});
		const converter = new ZodOpenAPIConverter('draft-2020-12');
		expect(() => converter.getSchema('ConflictingText', schema, 'input')).toThrow(
			'Conflicting OpenAPI schema definitions: Text (input)',
		);
	});

	it('rejects a component name that collides with another schema input name', () => {
		const converter = new ZodOpenAPIConverter('draft-2020-12');
		converter.getRef('Text', z.string(), 'input');
		expect(() => converter.getRef('TextInput', z.number(), 'output')).toThrow(
			'Conflicting OpenAPI input/output schema names: TextInput',
		);
	});

	it('converts newly discovered components after an earlier conversion pass', () => {
		const converter = new ZodOpenAPIConverter('draft-2020-12');
		converter.getSchema('First', z.string(), 'input');
		converter.getRef('Second', z.number(), 'input');
		expect(converter.getAllSchemas()).toMatchObject({FirstInput: {type: 'string'}, SecondInput: {type: 'number'}});
	});
});
