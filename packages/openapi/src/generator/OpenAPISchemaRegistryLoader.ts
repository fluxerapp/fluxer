// SPDX-License-Identifier: AGPL-3.0-or-later
import {
	getRegisteredBitflagSchemas,
	getRegisteredDiscriminatedUnionBranchSchemas,
	getRegisteredInt32EnumSchemas,
	getRegisteredNamedObjectSchemas,
} from '@fluxer/openapi/src/converters/ZodToOpenAPI';
import {OpenAPIGeneratorCatalog} from '@fluxer/openapi/src/generator/OpenAPIGeneratorCatalog';
import {type LoadedSchema, loadSchemas} from '@fluxer/openapi/src/registry/SchemaLoader';
import type {SchemaRegistry} from '@fluxer/openapi/src/registry/SchemaRegistry';
import {CustomSchemaType} from '@fluxer/openapi/src/schemas/CustomSchemaType';

interface OpenAPISchemaRegistryLoadResult {
	readonly loadedSchemas: Map<string, LoadedSchema>;
	readonly totalRegisteredSchemas: number;
}
export async function loadSchemasIntoRegistry(
	basePath: string,
	schemaRegistry: SchemaRegistry,
): Promise<OpenAPISchemaRegistryLoadResult> {
	for (const [name, schema] of OpenAPIGeneratorCatalog.builtInSchemas) {
		schemaRegistry.register(name, schema);
	}
	for (const [name, schema] of Object.entries(CustomSchemaType.getAllSchemas())) {
		if (!schemaRegistry.has(name)) {
			schemaRegistry.register(name, schema);
		}
	}
	let loadedSchemas = new Map<string, LoadedSchema>();
	try {
		const dynamicSchemas = await loadSchemas(basePath);
		loadedSchemas = dynamicSchemas;
		for (const [name, schema] of dynamicSchemas) {
			if (!schemaRegistry.has(name)) {
				schemaRegistry.register(name, schema.openAPISchema);
			}
		}
	} catch (error) {
		console.warn('Warning: Could not load some schemas:', error);
	}
	for (const [name, schema] of Object.entries(getRegisteredBitflagSchemas())) {
		if (!schemaRegistry.has(name)) {
			schemaRegistry.register(name, schema);
		}
	}
	for (const [name, schema] of Object.entries(getRegisteredInt32EnumSchemas())) {
		if (!schemaRegistry.has(name)) {
			schemaRegistry.register(name, schema);
		}
	}
	for (const [name, schema] of Object.entries(getRegisteredDiscriminatedUnionBranchSchemas())) {
		if (!schemaRegistry.has(name)) {
			schemaRegistry.register(name, schema);
		}
	}
	for (const [name, schema] of Object.entries(getRegisteredNamedObjectSchemas())) {
		if (!schemaRegistry.has(name)) {
			schemaRegistry.register(name, schema);
		}
	}
	return {
		loadedSchemas,
		totalRegisteredSchemas: Object.keys(schemaRegistry.getAllSchemas()).length,
	};
}
