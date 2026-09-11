// SPDX-License-Identifier: AGPL-3.0-or-later
import {isDeepStrictEqual} from 'node:util';
import {type SchemaIO, ZodOpenAPIConverter} from '@fluxer/openapi/src/converters/ZodToOpenAPI';
import type {OpenAPISchemaTarget} from '@fluxer/openapi/src/OpenAPIGenerationTypes';
import type {OpenAPIDocument, OpenAPIRef, OpenAPISchema} from '@fluxer/openapi/src/OpenAPITypes';
import {core} from 'zod';

export class SchemaRegistry {
	private readonly schemas = new Map<string, OpenAPISchema>();
	private readonly zodSchemas = new Map<string, core.$ZodType>();
	private readonly emptyObjectAcceptance = new WeakMap<core.$ZodType, boolean>();
	private readonly converter: ZodOpenAPIConverter;

	constructor(target: OpenAPISchemaTarget = 'draft-2020-12') {
		this.converter = new ZodOpenAPIConverter(target);
	}

	register(name: string, schema: OpenAPISchema): void {
		if (this.zodSchemas.has(name)) throw new Error(`OpenAPI schema also has a Zod definition: ${name}`);
		const existing = this.schemas.get(name);
		if (existing && !isDeepStrictEqual(existing, schema)) throw new Error(`Conflicting OpenAPI schemas: ${name}`);
		this.schemas.set(name, schema);
	}

	registerZod(name: string, schema: core.$ZodType): void {
		if (this.schemas.has(name)) throw new Error(`Zod schema also has an OpenAPI definition: ${name}`);
		const existing = this.zodSchemas.get(name);
		if (existing && existing !== schema) throw new Error(`Duplicate Zod schema export: ${name}`);
		this.zodSchemas.set(name, schema);
		this.converter.register(name, schema);
	}

	getRef(name: string, io: SchemaIO = 'input'): OpenAPIRef {
		const schema = this.zodSchemas.get(name);
		if (schema) return this.converter.getRef(name, schema, io);
		if (!this.schemas.has(name)) throw new Error(`Unknown OpenAPI schema: ${name}`);
		return {$ref: `#/components/schemas/${name}`};
	}

	get(name: string, io: SchemaIO = 'input'): OpenAPISchema | undefined {
		const schema = this.zodSchemas.get(name);
		return schema ? this.converter.getSchema(name, schema, io) : this.schemas.get(name);
	}

	get size(): number {
		return new Set([...this.schemas.keys(), ...this.zodSchemas.keys()]).size;
	}

	acceptsEmptyObject(name: string): boolean {
		const schema = this.zodSchemas.get(name);
		if (!schema) throw new Error(`Unknown request schema: ${name}`);
		const cached = this.emptyObjectAcceptance.get(schema);
		if (cached !== undefined) return cached;
		try {
			const accepted = core.safeParse(schema, {}).success;
			this.emptyObjectAcceptance.set(schema, accepted);
			return accepted;
		} catch (error) {
			throw new Error(`Could not determine whether request schema ${name} accepts an empty JSON body`, {cause: error});
		}
	}

	getAllSchemas(): Record<string, OpenAPISchema> {
		const converted = this.converter.getAllSchemas();
		for (const name of Object.keys(converted)) {
			if (this.schemas.has(name)) throw new Error(`Duplicate OpenAPI component: ${name}`);
		}
		return {...Object.fromEntries(this.schemas), ...converted};
	}

	normalizeComponents(document: OpenAPIDocument): void {
		this.converter.normalizeComponents(document);
	}
}
