// SPDX-License-Identifier: AGPL-3.0-or-later
import type {core} from 'zod';
export type HttpMethod = 'get' | 'post' | 'put' | 'patch' | 'delete';
export type ValidatorTarget = 'json' | 'query' | 'param' | 'form' | 'header' | 'cookie';
export interface ExtractedValidator {
	target: ValidatorTarget;
	schemaName: string;
}
export interface OpenAPIExternalDocs {
	url: string;
	description?: string;
}
export interface ExtractedRoute {
	method: HttpMethod;
	path: string;
	controllerFile: string;
	lineNumber: number;
	validators: Array<ExtractedValidator>;
	middlewares: Array<string>;
	hasLoginRequired: boolean;
	hasDefaultUserOnly: boolean;
	hasLoginRequiredAllowSuspicious: boolean;
	rateLimitConfig: string | null;
	responseSchemaName: string | null;
	responseContentType: string;
	hasNoContent: boolean;
	bodylessStatusCodes: Array<number>;
	successStatusCodes: Array<number>;
	explicitRequestSchemaName: string | null;
	explicitRequestFormSchemaName: string | null;
	explicitRequestBodyRequired: boolean | null;
	explicitSummary: string | null;
	explicitOperationId: string | null;
	explicitDescription: string | null;
	explicitStatusCodes: Array<number> | null;
	explicitSecurity: Array<string> | null;
	oauth2RequiredScopes: Array<string> | null;
	oauth2ScopeMode: 'all' | 'any' | null;
	oauth2BearerTokenRequired: boolean;
	explicitTags: Array<string> | null;
	explicitDeprecated: boolean;
	explicitExternalDocs: OpenAPIExternalDocs | null;
}
export interface OpenAPIPathItem {
	[method: string]: OpenAPIOperation;
}
export interface OpenAPIOperation {
	operationId: string;
	tags: Array<string>;
	summary?: string;
	description?: string;
	security?: Array<Record<string, Array<string>>>;
	parameters?: Array<OpenAPIParameter>;
	requestBody?: OpenAPIRequestBody;
	responses: Record<string, OpenAPIResponse>;
	deprecated?: boolean;
	externalDocs?: OpenAPIExternalDocs;
}
export interface OpenAPIParameter {
	name: string;
	in: 'path' | 'query' | 'header' | 'cookie';
	required: boolean;
	schema: OpenAPISchemaOrRef;
	description?: string;
}
interface OpenAPIMediaType {
	schema: OpenAPISchemaOrRef;
}
export interface OpenAPIRequestBody {
	required?: boolean;
	content: Record<string, OpenAPIMediaType>;
}
export interface OpenAPIResponse {
	description: string;
	content?: Record<string, OpenAPIMediaType>;
	headers?: Record<string, OpenAPIHeaderObject>;
}
export interface OpenAPIHeaderObject {
	description?: string;
	schema: OpenAPISchemaOrRef;
}
export interface OpenAPIRef extends core.JSONSchema.JSONSchema {
	$ref: string;
}
export type OpenAPISchemaOrRef = OpenAPISchema | OpenAPIRef;
export type OpenAPISchema = core.JSONSchema.JSONSchema;
export interface OpenAPIDocument {
	openapi: '3.0.3' | '3.1.0';
	security?: Array<Record<string, Array<string>>>;
	info: {
		title: string;
		version: string;
		description?: string;
		contact?: {
			name?: string;
			email?: string;
			url?: string;
		};
		license?: {
			name: string;
			url?: string;
		};
	};
	servers?: Array<{
		url: string;
		description?: string;
	}>;
	paths: Record<string, OpenAPIPathItem>;
	components: {
		schemas: Record<string, OpenAPISchema>;
		securitySchemes: Record<string, OpenAPISecurityScheme>;
	};
	tags?: Array<{
		name: string;
		description?: string;
	}>;
}
export interface OpenAPISecurityScheme {
	type: 'http' | 'apiKey' | 'oauth2' | 'openIdConnect';
	scheme?: string;
	bearerFormat?: string;
	name?: string;
	in?: 'header' | 'query' | 'cookie';
	description?: string;
	flows?: {
		authorizationCode?: {
			authorizationUrl: string;
			tokenUrl: string;
			refreshUrl?: string;
			scopes: Record<string, string>;
		};
		clientCredentials?: {
			tokenUrl: string;
			scopes: Record<string, string>;
		};
	};
}
