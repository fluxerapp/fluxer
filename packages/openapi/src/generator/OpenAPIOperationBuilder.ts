// SPDX-License-Identifier: AGPL-3.0-or-later

import {extractPathParameterNames} from '@fluxer/openapi/src/extractors/PathParameters';
import type {
	ExtractedRoute,
	OpenAPIOperation,
	OpenAPIParameter,
	OpenAPIRequestBody,
	OpenAPIResponse,
	OpenAPISchema,
} from '@fluxer/openapi/src/OpenAPITypes';
import {getErrorResponses} from '@fluxer/openapi/src/registry/ResponseRegistry';
import type {SchemaRegistry} from '@fluxer/openapi/src/registry/SchemaRegistry';

interface OpenAPIOperationBuilderDependencies {
	readonly schemaRegistry: SchemaRegistry;
	readonly usedOperationIds: Set<string>;
}
function successStatusCodes(route: ExtractedRoute): Array<number> {
	if (route.explicitStatusCodes?.length) return route.explicitStatusCodes;
	if (route.successStatusCodes.length) return route.successStatusCodes;
	return [route.hasNoContent ? 204 : 200];
}
export class OpenAPIOperationBuilder {
	private readonly schemaRegistry: SchemaRegistry;
	private readonly usedOperationIds: Set<string>;
	constructor(dependencies: OpenAPIOperationBuilderDependencies) {
		this.schemaRegistry = dependencies.schemaRegistry;
		this.usedOperationIds = dependencies.usedOperationIds;
	}
	public buildOperation(route: ExtractedRoute): OpenAPIOperation {
		if (!route.explicitTags || route.explicitTags.length === 0) {
			throw new Error(
				`Missing explicit tags for ${route.method.toUpperCase()} ${route.path} in ${route.controllerFile}:${route.lineNumber}. All endpoints must use the OpenAPI middleware with explicit tags.`,
			);
		}
		if (!route.explicitSummary) {
			throw new Error(
				`Missing explicit summary for ${route.method.toUpperCase()} ${route.path} in ${route.controllerFile}:${route.lineNumber}. All endpoints must use the OpenAPI middleware with an explicit summary.`,
			);
		}
		if (!route.explicitOperationId) {
			throw new Error(
				`Missing explicit operationId for ${route.method.toUpperCase()} ${route.path} in ${route.controllerFile}:${route.lineNumber}. All endpoints must use the OpenAPI middleware with an explicit operationId in snake_case.`,
			);
		}
		const baseSecurity = route.explicitSecurity?.map((scheme) => ({[scheme]: []})) ?? this.buildSecurity(route);
		const security = this.applyOAuth2ScopeSecurity(baseSecurity, route);
		const parameters = this.buildParameters(route);
		const requestBody = this.buildRequestBody(route);
		const responses = this.buildResponses(route, security.length > 0);
		const operation: OpenAPIOperation = {
			operationId: this.getUniqueOperationId(route.explicitOperationId),
			summary: route.explicitSummary,
			tags: route.explicitTags,
			responses,
		};
		if (route.explicitDescription) {
			operation.description = route.explicitDescription;
		}
		if (route.explicitDeprecated) {
			operation.deprecated = route.explicitDeprecated;
		}
		if (route.explicitExternalDocs) {
			operation.externalDocs = route.explicitExternalDocs;
		}
		if (security.length > 0) {
			operation.security = security;
		}
		if (parameters.length > 0) {
			operation.parameters = parameters;
		}
		if (requestBody) {
			operation.requestBody = requestBody;
		}
		return operation;
	}
	private getUniqueOperationId(baseId: string): string {
		if (this.usedOperationIds.has(baseId)) throw new Error(`Duplicate operationId: ${baseId}`);
		this.usedOperationIds.add(baseId);
		return baseId;
	}
	private buildSecurity(route: ExtractedRoute): Array<Record<string, Array<string>>> {
		if (route.path.startsWith('/admin/') || route.middlewares.includes('requireAdminACL')) {
			return [{adminApiKey: []}];
		}
		if (route.path === '/applications/@me') {
			return [{botToken: []}];
		}
		if (route.path === '/users/@me' || route.path.startsWith('/users/@me/')) {
			return [{bearerToken: []}, {sessionToken: []}];
		}
		if (!route.hasLoginRequired && !route.hasLoginRequiredAllowSuspicious && !route.hasDefaultUserOnly) {
			return [];
		}
		if (route.hasDefaultUserOnly) {
			return [{bearerToken: []}, {sessionToken: []}];
		}
		return [{botToken: []}, {bearerToken: []}, {sessionToken: []}];
	}
	private applyOAuth2ScopeSecurity(
		security: Array<Record<string, Array<string>>>,
		route: ExtractedRoute,
	): Array<Record<string, Array<string>>> {
		if (!route.oauth2RequiredScopes || route.oauth2RequiredScopes.length === 0 || !route.oauth2ScopeMode) {
			if (route.oauth2BearerTokenRequired) {
				return security.map((entry) => {
					if (!('bearerToken' in entry)) {
						return entry;
					}
					return {oauth2Token: []};
				});
			}
			return security.filter((entry) => !('bearerToken' in entry));
		}
		if (route.oauth2ScopeMode === 'all') {
			const scopes = [...route.oauth2RequiredScopes].sort();
			return security.map((entry) => {
				if (!('bearerToken' in entry)) {
					return entry;
				}
				return {oauth2Token: scopes};
			});
		}
		const sortedScopes = [...route.oauth2RequiredScopes].sort();
		const transformed: Array<Record<string, Array<string>>> = [];
		for (const entry of security) {
			if (!('bearerToken' in entry)) {
				transformed.push(entry);
				continue;
			}
			for (const scope of sortedScopes) {
				transformed.push({oauth2Token: [scope]});
			}
		}
		return transformed;
	}
	private buildParameters(route: ExtractedRoute): Array<OpenAPIParameter> {
		const parameters = new Map<string, OpenAPIParameter>();
		for (const validator of route.validators) {
			if (validator.target === 'json' || validator.target === 'form') continue;
			const location = validator.target === 'param' ? 'path' : validator.target;
			const schemaName = validator.schemaName;
			const schema = this.schemaRegistry.get(schemaName);
			if (!schema || schema.type !== 'object' || !schema.properties) {
				throw new Error(`Parameter schema must be an object: ${schemaName}`);
			}
			const required = new Set(schema.required ?? []);
			for (const [name, property] of Object.entries(schema.properties)) {
				const propertySchema: OpenAPISchema = typeof property === 'boolean' ? (property ? {} : {not: {}}) : property;
				const key = `${location}:${name}`;
				if (parameters.has(key)) throw new Error(`Duplicate parameter ${key} for ${route.method} ${route.path}`);
				parameters.set(key, {
					name,
					in: location,
					required: location === 'path' || required.has(name),
					schema: propertySchema,
					...(propertySchema.description ? {description: propertySchema.description} : {}),
				});
			}
		}
		for (const name of extractPathParameterNames(route.path)) {
			if (!parameters.has(`path:${name}`)) {
				throw new Error(`Path parameter ${name} has no validator for ${route.method.toUpperCase()} ${route.path}`);
			}
		}
		return [...parameters.values()];
	}

	private buildRequestBody(route: ExtractedRoute): OpenAPIRequestBody | undefined {
		const jsonValidator = route.validators.find((validator) => validator.target === 'json');
		const formValidator = route.validators.find((validator) => validator.target === 'form');
		const jsonSchemaName = route.explicitRequestSchemaName ?? jsonValidator?.schemaName;
		const formSchemaName = route.explicitRequestFormSchemaName ?? formValidator?.schemaName;
		if (!jsonSchemaName && !formSchemaName) return undefined;
		const requestBody: OpenAPIRequestBody = {
			required:
				route.explicitRequestBodyRequired ??
				!(jsonValidator && jsonSchemaName && this.schemaRegistry.acceptsEmptyObject(jsonSchemaName)),
			content: {},
		};
		for (const [contentType, schemaName] of [
			['application/json', jsonSchemaName],
			['multipart/form-data', formSchemaName],
		] as const) {
			if (!schemaName) continue;
			requestBody.content[contentType] = {schema: this.schemaRegistry.getRef(schemaName)};
		}
		return requestBody;
	}

	private buildResponses(route: ExtractedRoute, requiresAuth: boolean): Record<string, OpenAPIResponse> {
		const responses: Record<string, OpenAPIResponse> = {};
		const statusCodes = successStatusCodes(route);
		for (const code of route.bodylessStatusCodes) {
			if (!statusCodes.includes(code)) {
				throw new Error(`Bodyless status ${code} is not declared for ${route.method.toUpperCase()} ${route.path}`);
			}
		}
		for (const code of statusCodes) {
			if (code === 304) {
				responses['304'] = {description: 'Not Modified'};
				continue;
			}
			if (
				code === 204 ||
				route.bodylessStatusCodes.includes(code) ||
				(route.hasNoContent && !route.responseSchemaName)
			) {
				responses[String(code)] = {description: code === 204 ? 'No Content' : 'Success'};
				continue;
			}
			if (!route.responseSchemaName) {
				throw new Error(`Missing response schema for ${route.method.toUpperCase()} ${route.path}`);
			}
			responses[String(code)] = {
				description: 'Success',
				content: {
					[route.responseContentType]: {schema: this.schemaRegistry.getRef(route.responseSchemaName, 'output')},
				},
			};
		}
		Object.assign(responses, getErrorResponses(requiresAuth));
		return responses;
	}
}
