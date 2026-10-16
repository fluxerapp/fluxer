import type {OpenAPISchema} from '@fluxer/openapi/src/OpenAPITypes';

const DATA_KEYWORDS = new Set(['const', 'default', 'enum', 'example', 'examples']);
const OBJECT_MAP_KEYWORDS = new Set([
	'$defs',
	'callbacks',
	'content',
	'definitions',
	'dependencies',
	'dependentSchemas',
	'encoding',
	'headers',
	'links',
	'parameters',
	'paths',
	'patternProperties',
	'properties',
	'requestBodies',
	'responses',
	'schemas',
	'securitySchemes',
]);

export function visitOpenAPISchemaObjects(value: unknown, visitor: (schema: OpenAPISchema) => void): void {
	const pending: Array<unknown> = [value];
	while (pending.length > 0) {
		const current = pending.pop();
		if (current === null || typeof current !== 'object') {
			continue;
		}
		if (Array.isArray(current)) {
			for (const nested of current) pending.push(nested);
			continue;
		}
		visitor(current as OpenAPISchema);
		for (const [key, nested] of Object.entries(current)) {
			if (DATA_KEYWORDS.has(key) || key.startsWith('x-')) {
				continue;
			}
			if (OBJECT_MAP_KEYWORDS.has(key) && nested !== null && typeof nested === 'object') {
				for (const child of Object.values(nested)) pending.push(child);
			} else {
				pending.push(nested);
			}
		}
	}
}
