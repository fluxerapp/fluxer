// SPDX-License-Identifier: AGPL-3.0-or-later

import {visitOpenAPISchemaObjects} from '@fluxer/openapi/src/OpenAPISchemaVisitor';
import type {OpenAPIPathItem, OpenAPISchema} from '@fluxer/openapi/src/OpenAPITypes';

const SCHEMA_REFERENCE_PREFIX = '#/components/schemas/';

export function collectReferencedSchemaNames(
	paths: Record<string, OpenAPIPathItem>,
	allSchemas: Record<string, OpenAPISchema>,
): Set<string> {
	const referenced = new Set<string>();
	const pending: Array<unknown> = [{paths}];
	while (pending.length > 0) {
		const value = pending.pop();
		visitOpenAPISchemaObjects(value, (schema) => {
			if (!schema.$ref?.startsWith(SCHEMA_REFERENCE_PREFIX)) return;
			const schemaName = decodeURIComponent(schema.$ref.slice(SCHEMA_REFERENCE_PREFIX.length).split('/')[0])
				.replaceAll('~1', '/')
				.replaceAll('~0', '~');
			if (!referenced.has(schemaName)) {
				if (!Object.hasOwn(allSchemas, schemaName)) {
					throw new Error(`Reference to an unregistered OpenAPI schema: ${schema.$ref}`);
				}
				referenced.add(schemaName);
				pending.push(allSchemas[schemaName]);
			}
		});
	}
	return referenced;
}
