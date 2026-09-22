import type {OpenAPIRef} from '@fluxer/openapi/src/OpenAPITypes';

export function validateOpenAPIComponentName(name: string): void {
	if (!/^[A-Za-z0-9._-]+$/.test(name)) {
		throw new Error(`Invalid OpenAPI schema name: ${name}`);
	}
}

export function createOpenAPIComponentRef(name: string): OpenAPIRef {
	validateOpenAPIComponentName(name);
	return {$ref: `#/components/schemas/${name}`};
}
