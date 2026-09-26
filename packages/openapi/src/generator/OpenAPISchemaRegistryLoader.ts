// SPDX-License-Identifier: AGPL-3.0-or-later
import {OpenAPIGeneratorCatalog} from '@fluxer/openapi/src/generator/OpenAPIGeneratorCatalog';
import {loadSchemas} from '@fluxer/openapi/src/registry/SchemaLoader';
import type {SchemaRegistry} from '@fluxer/openapi/src/registry/SchemaRegistry';

export async function loadSchemasIntoRegistry(basePath: string, schemaRegistry: SchemaRegistry): Promise<void> {
	for (const [name, schema] of OpenAPIGeneratorCatalog.builtInSchemas) {
		schemaRegistry.register(name, schema);
	}
	for (const [name, zodSchema] of await loadSchemas(basePath)) {
		schemaRegistry.registerZod(name, zodSchema);
	}
}
