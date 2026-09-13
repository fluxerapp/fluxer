// SPDX-License-Identifier: AGPL-3.0-or-later
import {isDeepStrictEqual} from 'node:util';
import {createOpenAPIComponentRef, validateOpenAPIComponentName} from '@fluxer/openapi/src/OpenAPIComponentRef';
import type {OpenAPISchemaTarget} from '@fluxer/openapi/src/OpenAPIGenerationTypes';
import {visitOpenAPISchemaObjects} from '@fluxer/openapi/src/OpenAPISchemaVisitor';
import type {OpenAPIDocument, OpenAPIRef, OpenAPISchema} from '@fluxer/openapi/src/OpenAPITypes';
import {schemaMetadata} from '@fluxer/schema/src/SchemaMetadata';
import {type core, z} from 'zod';

export type SchemaIO = 'input' | 'output';

interface PendingSchema {
	name: string;
	schema: core.$ZodType;
	io: SchemaIO;
}

function componentName(name: string, io: SchemaIO): string {
	validateOpenAPIComponentName(name);
	return io === 'input' ? `${name}Input` : name;
}

function replaceSchema(target: OpenAPISchema, replacement: OpenAPISchema): void {
	for (const key of Object.keys(target)) delete target[key];
	Object.assign(target, replacement);
}

function schemasHaveSameContract(left: OpenAPISchema, right: OpenAPISchema): boolean {
	const {description: leftDescription, ...leftContract} = left;
	const {description: rightDescription, ...rightContract} = right;
	return isDeepStrictEqual(leftContract, rightContract);
}

function applyMetadata(schema: core.$ZodType, json: OpenAPISchema): void {
	const metadata = schemaMetadata.get(schema);
	if (!metadata) return;
	if (metadata.format) json.format = metadata.format;
	if (metadata.bitflagValues) json['x-bitflagValues'] = metadata.bitflagValues;
	const entries = metadata.enumEntries;
	if (!entries) return;
	json['x-enumNames'] = entries.map((entry) => entry.name);
	const descriptions = entries.map((entry) => entry.description ?? null);
	if (descriptions.some((description) => description !== null)) {
		json['x-enumDescriptions'] = descriptions;
	}
	delete json.anyOf;
	delete json.oneOf;
	if (metadata.openEnum) {
		json.type = 'string';
		const knownValues = entries.map((entry) => String(entry.value)).join(', ');
		json.description = `${json.description ? `${json.description} ` : ''}Known values: ${knownValues} (other values allowed)`;
	} else {
		delete json.const;
		json.enum = entries.map((entry) => entry.value);
		json.type = entries.every((entry) => typeof entry.value === 'string') ? 'string' : 'integer';
	}
}

function relocateLocalRefs(value: unknown, name: string): void {
	visitOpenAPISchemaObjects(value, (schema) => {
		if (schema.$ref === '#') schema.$ref = createOpenAPIComponentRef(name).$ref;
		else if (schema.$ref?.startsWith('#/$defs/') || schema.$ref?.startsWith('#/definitions/')) {
			schema.$ref = `${createOpenAPIComponentRef(name).$ref}${schema.$ref.slice(1)}`;
		}
	});
}

function renameComponentRefs(value: unknown, renames: Map<string, string>): void {
	visitOpenAPISchemaObjects(value, (schema) => {
		const prefix = '#/components/schemas/';
		if (schema.$ref?.startsWith(prefix)) {
			const [name, ...path] = schema.$ref.slice(prefix.length).split('/');
			const replacement = renames.get(name);
			if (replacement) schema.$ref = `${prefix}${[replacement, ...path].join('/')}`;
		}
	});
}

function removeRedundantReferenceProperties(value: unknown, schemas: Record<string, OpenAPISchema>): void {
	visitOpenAPISchemaObjects(value, (schema) => {
		const refs = [schema.$ref, ...(schema.allOf ?? []).map((branch) => branch.$ref)];
		for (const ref of refs) {
			if (typeof ref !== 'string' || !ref.startsWith('#/components/schemas/')) continue;
			const name = ref.slice('#/components/schemas/'.length);
			if (!Object.hasOwn(schemas, name)) continue;
			const referenced = schemas[name];
			if (referenced === schema) continue;
			for (const key of Object.keys(schema)) {
				if (key !== '$ref' && key !== 'allOf' && isDeepStrictEqual(schema[key], referenced[key])) delete schema[key];
			}
		}
	});
}

export class ZodOpenAPIConverter {
	private readonly names = new WeakMap<core.$ZodType, string>();
	private readonly components = new Map<string, OpenAPISchema>();
	private readonly pending = new Map<string, Array<PendingSchema>>();
	private readonly worklist: Array<PendingSchema> = [];
	private nextPendingIndex = 0;

	constructor(private readonly target: OpenAPISchemaTarget) {}

	register(name: string, schema: core.$ZodType): void {
		validateOpenAPIComponentName(name);
		if (!this.names.has(schema)) this.names.set(schema, name);
	}

	getRef(name: string, schema: core.$ZodType, io: SchemaIO): OpenAPIRef {
		const key = componentName(name, io);
		const candidates = this.pending.get(key);
		if (candidates?.some((candidate) => candidate.name !== name || candidate.io !== io)) {
			throw new Error(`Conflicting OpenAPI input/output schema names: ${key}`);
		}
		if (!candidates?.some((candidate) => candidate.schema === schema)) {
			const pending = {name, schema, io};
			if (candidates) candidates.push(pending);
			else this.pending.set(key, [pending]);
			this.worklist.push(pending);
		}
		return createOpenAPIComponentRef(key);
	}

	getSchema(name: string, schema: core.$ZodType, io: SchemaIO): OpenAPISchema {
		this.getRef(name, schema, io);
		this.convertPending();
		const result = this.components.get(componentName(name, io));
		if (!result) throw new Error(`OpenAPI schema was not converted: ${name} (${io})`);
		return result;
	}

	getAllSchemas(): Record<string, OpenAPISchema> {
		this.convertPending();
		return Object.fromEntries(this.components);
	}

	normalizeComponents(document: OpenAPIDocument): void {
		const remaining = new Map<string, string>();
		for (const [key, candidates] of this.pending) {
			if (candidates[0].io === 'input' && Object.hasOwn(document.components.schemas, key)) {
				remaining.set(key, candidates[0].name);
			}
		}
		while (remaining.size > 0) {
			const renames = new Map<string, string>();
			for (const [inputName, outputName] of remaining) {
				const output = Object.hasOwn(document.components.schemas, outputName)
					? document.components.schemas[outputName]
					: undefined;
				if (!output || schemasHaveSameContract(document.components.schemas[inputName], output)) {
					renames.set(inputName, outputName);
				}
			}
			if (renames.size === 0) break;
			renameComponentRefs(document, renames);
			const schemas = new Map<string, OpenAPISchema>();
			for (const [name, schema] of Object.entries(document.components.schemas)) {
				const renamed = renames.get(name) ?? name;
				if (!schemas.has(renamed) || name === renamed) schemas.set(renamed, schema);
			}
			document.components.schemas = Object.fromEntries(schemas);
			for (const name of renames.keys()) remaining.delete(name);
		}
		removeRedundantReferenceProperties(document, document.components.schemas);
	}

	private convertPending(): void {
		while (this.nextPendingIndex < this.worklist.length) {
			const pending = this.worklist[this.nextPendingIndex];
			const {schema, io} = pending;
			const key = componentName(pending.name, io);
			let result: OpenAPISchema;
			try {
				result = z.toJSONSchema(schema, {
					target: this.target,
					io,
					unrepresentable: 'throw',
					override: ({zodSchema, jsonSchema, path}) => {
						applyMetadata(zodSchema, jsonSchema);
						if (
							this.target === 'openapi-3.0' &&
							jsonSchema.format === 'binary' &&
							jsonSchema.contentEncoding === 'binary'
						) {
							delete jsonSchema.contentEncoding;
						}
						if (path.length > 0) {
							const name =
								this.names.get(zodSchema) ??
								(schemaMetadata.has(zodSchema) ? schemaMetadata.get(zodSchema)?.name : undefined);
							if (name !== undefined) {
								const description = jsonSchema.description;
								const ref = this.getRef(name, zodSchema, io);
								replaceSchema(jsonSchema, description ? {...ref, description} : ref);
							}
						}
						if (this.target === 'openapi-3.0' && jsonSchema.$ref && Object.keys(jsonSchema).length > 1) {
							const ref = {$ref: jsonSchema.$ref};
							delete jsonSchema.$ref;
							jsonSchema.allOf = [...(jsonSchema.allOf ?? []), ref];
						}
					},
				});
			} catch (error) {
				throw new Error(`Could not convert OpenAPI schema ${pending.name} (${io})`, {cause: error});
			}
			delete result.$schema;
			delete result.$id;
			relocateLocalRefs(result, key);
			const existing = this.components.get(key);
			if (existing) {
				if (!schemasHaveSameContract(existing, result)) {
					throw new Error(`Conflicting OpenAPI schema definitions: ${pending.name} (${io})`);
				}
			} else this.components.set(key, result);
			this.nextPendingIndex++;
		}
	}
}
