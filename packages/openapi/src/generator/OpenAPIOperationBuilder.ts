// SPDX-License-Identifier: AGPL-3.0-or-later
import {SnowflakeTypeRef} from '@fluxer/openapi/src/converters/BuiltInSchemas';
import {analyzeSecurityRequirements} from '@fluxer/openapi/src/extractors/MiddlewareAnalyzer';
import type {
	ExtractedRoute,
	ExtractedValidator,
	OpenAPIOperation,
	OpenAPIParameter,
	OpenAPIRequestBody,
	OpenAPIResponse,
	OpenAPISchema,
	OpenAPISchemaOrRef,
} from '@fluxer/openapi/src/OpenAPITypes';
import {extractPathParameters} from '@fluxer/openapi/src/registry/ParameterRegistry';
import {getErrorResponses, getNoContentResponse} from '@fluxer/openapi/src/registry/ResponseRegistry';
import type {LoadedSchema} from '@fluxer/openapi/src/registry/SchemaLoader';
import type {SchemaRegistry} from '@fluxer/openapi/src/registry/SchemaRegistry';

interface OpenAPIOperationBuilderDependencies {
	readonly schemaRegistry: SchemaRegistry;
	readonly loadedSchemas: Map<string, LoadedSchema>;
	readonly usedOperationIds: Set<string>;
}
export class OpenAPIOperationBuilder {
	private readonly schemaRegistry: SchemaRegistry;
	private readonly loadedSchemas: Map<string, LoadedSchema>;
	private readonly usedOperationIds: Set<string>;
	constructor(dependencies: OpenAPIOperationBuilderDependencies) {
		this.schemaRegistry = dependencies.schemaRegistry;
		this.loadedSchemas = dependencies.loadedSchemas;
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
		const security = route.explicitSecurity
			? this.buildSecurityFromExplicit(route.explicitSecurity, route)
			: this.buildSecurity(route);
		const parameters = this.buildParameters(route);
		const requestBody = this.buildRequestBody(route);
		const responses = this.buildResponses(route, route.explicitStatusCodes);
		const operation: OpenAPIOperation = {
			operationId: this.getUniqueOperationId(route.explicitOperationId),
			summary: route.explicitSummary,
			tags: route.explicitTags,
			responses,
			'x-mint': {metadata: {title: route.explicitSummary}},
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
		let operationId = baseId;
		let counter = 2;
		while (this.usedOperationIds.has(operationId)) {
			operationId = `${baseId}${counter}`;
			counter++;
		}
		this.usedOperationIds.add(operationId);
		return operationId;
	}
	private buildSecurityFromExplicit(
		explicitSecurity: Array<string>,
		route: ExtractedRoute,
	): Array<Record<string, Array<string>>> {
		const baseSecurity = explicitSecurity.map((scheme) => ({[scheme]: []}));
		return this.applyOAuth2ScopeSecurity(baseSecurity, route);
	}
	private buildSecurity(route: ExtractedRoute): Array<Record<string, Array<string>>> {
		let baseSecurity: Array<Record<string, Array<string>>>;
		if (route.path.startsWith('/admin/') || route.middlewares.includes('requireAdminACL')) {
			baseSecurity = [{adminApiKey: []}];
			return this.applyOAuth2ScopeSecurity(baseSecurity, route);
		}
		if (route.path === '/applications/@me') {
			baseSecurity = [{botToken: []}];
			return this.applyOAuth2ScopeSecurity(baseSecurity, route);
		}
		if (route.path === '/users/@me' || route.path.startsWith('/users/@me/')) {
			baseSecurity = [{bearerToken: []}, {sessionToken: []}];
			return this.applyOAuth2ScopeSecurity(baseSecurity, route);
		}
		const security = analyzeSecurityRequirements(route);
		if (security.type !== 'bearer') {
			return [];
		}
		if (route.hasDefaultUserOnly) {
			baseSecurity = [{bearerToken: []}, {sessionToken: []}];
			return this.applyOAuth2ScopeSecurity(baseSecurity, route);
		}
		baseSecurity = [{botToken: []}, {bearerToken: []}, {sessionToken: []}];
		return this.applyOAuth2ScopeSecurity(baseSecurity, route);
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
		const parameters: Array<OpenAPIParameter> = [];
		const seenParameters = new Set<string>();
		function addParameter(parameter: OpenAPIParameter): void {
			const key = `${parameter.in}:${parameter.name}`;
			if (seenParameters.has(key)) {
				return;
			}
			seenParameters.add(key);
			parameters.push(parameter);
		}
		for (const pathParameter of extractPathParameters(route.path)) {
			addParameter(pathParameter);
		}
		for (const validator of route.validators) {
			if (validator.target !== 'query') {
				continue;
			}
			if (validator.schemaName) {
				for (const parameter of this.extractQueryParametersFromSchema(validator.schemaName)) {
					addParameter(parameter);
				}
				continue;
			}
			if (validator.inlineSchema) {
				for (const parameter of this.extractQueryParameters(validator)) {
					addParameter(parameter);
				}
			}
		}
		return parameters;
	}
	private extractQueryParameters(validator: ExtractedValidator): Array<OpenAPIParameter> {
		if (!validator.inlineSchema) {
			return [];
		}
		const parameters: Array<OpenAPIParameter> = [];
		const matches = validator.inlineSchema.matchAll(/(\w+):\s*([^,}]+)/g);
		for (const match of matches) {
			const name = match[1];
			const typeString = match[2].trim();
			const isOptional = typeString.includes('.optional()') || typeString.includes('.nullish()');
			parameters.push({
				name,
				in: 'query',
				required: !isOptional,
				schema: this.inferSchemaFromTypeString(typeString),
			});
		}
		return parameters;
	}
	private extractQueryParametersFromSchema(schemaName: string): Array<OpenAPIParameter> {
		const loadedSchema = this.loadedSchemas.get(schemaName);
		if (!loadedSchema) {
			return [];
		}
		const schema = loadedSchema.openAPISchema;
		if (schema.type !== 'object' || !schema.properties) {
			return [];
		}
		const required = new Set(schema.required ?? []);
		const parameters: Array<OpenAPIParameter> = [];
		for (const [name, propertySchema] of Object.entries(schema.properties)) {
			parameters.push({
				name,
				in: 'query',
				required: required.has(name),
				schema: propertySchema,
			});
		}
		return parameters;
	}
	private inferSchemaFromTypeString(typeString: string): OpenAPISchemaOrRef {
		function parseNumericSchema(targetTypeString: string): OpenAPISchema {
			const isInteger = targetTypeString.includes('.int()');
			const schema: OpenAPISchema = {
				type: isInteger ? 'integer' : 'number',
			};
			const minMatch = targetTypeString.match(/\.min\((\d+)\)/);
			const maxMatch = targetTypeString.match(/\.max\((\d+)\)/);
			const defaultMatch = targetTypeString.match(/\.default\((\d+)\)/);
			if (minMatch) {
				schema.minimum = Number.parseInt(minMatch[1], 10);
			}
			if (maxMatch) {
				schema.maximum = Number.parseInt(maxMatch[1], 10);
			}
			if (defaultMatch) {
				schema.default = Number.parseInt(defaultMatch[1], 10);
			}
			return schema;
		}
		if (typeString.includes('UnsignedInt64Type')) {
			return {type: 'string', format: 'int64', pattern: '^[0-9]+$'};
		}
		if (typeString.includes('NonNegativeSafeIntegerType')) {
			return {type: 'integer', minimum: 0, maximum: 9007199254740991, format: 'int53'};
		}
		if (typeString.includes('Int64StringType')) {
			return {type: 'string', format: 'int64', pattern: '^-?[0-9]+$'};
		}
		if (typeString.includes('Int64Type')) {
			return {type: 'string', format: 'int64', pattern: '^-?[0-9]+$'};
		}
		if (typeString.includes('PermissionStringType') || typeString.includes('BitflagStringType')) {
			return {type: 'string', format: 'int64', pattern: '^[0-9]+$'};
		}
		if (typeString.includes('SnowflakeStringType') || typeString.includes('SnowflakeType')) {
			return SnowflakeTypeRef;
		}
		if (typeString.includes('z.coerce.number()') || typeString.includes('z.number()')) {
			return parseNumericSchema(typeString);
		}
		if (typeString.includes('QueryBooleanType') || typeString.includes('z.boolean()')) {
			return {type: 'boolean'};
		}
		if (typeString.includes('z.string()')) {
			return {type: 'string'};
		}
		return {type: 'string'};
	}
	private getResponseSchema(schemaNameOrExpression: string):
		| OpenAPISchema
		| {
				$ref: string;
		  }
		| null {
		const trimmed = schemaNameOrExpression.trim();
		const {baseExpression, isNullable} = this.stripNullability(trimmed);
		const baseSchema = this.getBaseResponseSchema(baseExpression);
		if (!baseSchema) {
			return null;
		}
		if (!isNullable) {
			return baseSchema;
		}
		return {anyOf: [baseSchema, {type: 'null'}]};
	}
	private getBaseResponseSchema(schemaNameOrExpression: string):
		| OpenAPISchema
		| {
				$ref: string;
		  }
		| null {
		if (this.schemaRegistry.has(schemaNameOrExpression)) {
			return this.schemaRegistry.getRef(schemaNameOrExpression);
		}
		const trimmed = schemaNameOrExpression.trim();
		if (/^z\s*\.null\(\)/.test(trimmed)) {
			return {type: 'null'};
		}
		if (/^z\s*\.string\(\)/.test(trimmed)) {
			return {type: 'string'};
		}
		if (/^z\s*\.array\(/.test(trimmed)) {
			const inner = this.extractFirstCallArgument(trimmed);
			if (!inner) {
				return null;
			}
			const itemSchema = this.getResponseSchema(inner);
			if (!itemSchema) {
				return null;
			}
			return {type: 'array', items: itemSchema};
		}
		if (/^z\s*\.record\(/.test(trimmed)) {
			const args = this.extractFirstCallArgument(trimmed);
			if (!args) {
				return null;
			}
			const [keyArg, valueArg] = this.splitTopLevel(args, ',', 2);
			if (!keyArg || !valueArg) {
				return null;
			}
			const valueSchema = this.getResponseSchema(valueArg);
			if (!valueSchema) {
				return null;
			}
			return {type: 'object', additionalProperties: valueSchema};
		}
		if (/^z\s*\.object\(/.test(trimmed)) {
			return this.parseInlineSchema(trimmed);
		}
		return null;
	}
	private stripNullability(expression: string): {
		baseExpression: string;
		isNullable: boolean;
	} {
		let baseExpression = expression.trim();
		let isNullable = false;
		while (true) {
			const match = baseExpression.match(/\.(nullable|nullish)\(\)\s*$/);
			if (!match) {
				break;
			}
			isNullable = true;
			baseExpression = baseExpression.slice(0, match.index).trim();
		}
		return {baseExpression, isNullable};
	}
	private splitTopLevel(value: string, delimiter: string, maxParts?: number): Array<string> {
		const parts: Array<string> = [];
		let start = 0;
		let parenDepth = 0;
		let braceDepth = 0;
		let bracketDepth = 0;
		let inSingleQuote = false;
		let inDoubleQuote = false;
		let inTemplate = false;
		function pushPart(end: number): void {
			parts.push(value.slice(start, end).trim());
			start = end + delimiter.length;
		}
		for (let i = 0; i < value.length; i++) {
			const char = value[i];
			if (!inDoubleQuote && !inTemplate && char === "'" && value[i - 1] !== '\\') {
				inSingleQuote = !inSingleQuote;
			} else if (!inSingleQuote && !inTemplate && char === '"' && value[i - 1] !== '\\') {
				inDoubleQuote = !inDoubleQuote;
			} else if (!inSingleQuote && !inDoubleQuote && char === '`' && value[i - 1] !== '\\') {
				inTemplate = !inTemplate;
			}
			if (inSingleQuote || inDoubleQuote || inTemplate) {
				continue;
			}
			if (char === '(') {
				parenDepth++;
			} else if (char === ')') {
				parenDepth--;
			} else if (char === '{') {
				braceDepth++;
			} else if (char === '}') {
				braceDepth--;
			} else if (char === '[') {
				bracketDepth++;
			} else if (char === ']') {
				bracketDepth--;
			}
			if (char === delimiter && parenDepth === 0 && braceDepth === 0 && bracketDepth === 0) {
				pushPart(i);
				if (maxParts && parts.length >= maxParts - 1) {
					break;
				}
			}
		}
		parts.push(value.slice(start).trim());
		return parts;
	}
	private findTopLevelChar(value: string, target: string): number {
		let parenDepth = 0;
		let braceDepth = 0;
		let bracketDepth = 0;
		let inSingleQuote = false;
		let inDoubleQuote = false;
		let inTemplate = false;
		for (let i = 0; i < value.length; i++) {
			const char = value[i];
			if (!inDoubleQuote && !inTemplate && char === "'" && value[i - 1] !== '\\') {
				inSingleQuote = !inSingleQuote;
			} else if (!inSingleQuote && !inTemplate && char === '"' && value[i - 1] !== '\\') {
				inDoubleQuote = !inDoubleQuote;
			} else if (!inSingleQuote && !inDoubleQuote && char === '`' && value[i - 1] !== '\\') {
				inTemplate = !inTemplate;
			}
			if (inSingleQuote || inDoubleQuote || inTemplate) {
				continue;
			}
			if (char === '(') {
				parenDepth++;
			} else if (char === ')') {
				parenDepth--;
			} else if (char === '{') {
				braceDepth++;
			} else if (char === '}') {
				braceDepth--;
			} else if (char === '[') {
				bracketDepth++;
			} else if (char === ']') {
				bracketDepth--;
			}
			if (char === target && parenDepth === 0 && braceDepth === 0 && bracketDepth === 0) {
				return i;
			}
		}
		return -1;
	}
	private extractFirstCallArgument(expression: string): string | null {
		const openIndex = expression.indexOf('(');
		if (openIndex === -1) {
			return null;
		}
		let depth = 0;
		for (let i = openIndex; i < expression.length; i++) {
			const char = expression[i];
			if (char === '(') {
				depth++;
			} else if (char === ')') {
				depth--;
				if (depth === 0) {
					return expression.slice(openIndex + 1, i);
				}
			}
		}
		return null;
	}
	private buildRequestBody(route: ExtractedRoute): OpenAPIRequestBody | undefined {
		const jsonValidator = route.validators.find((validator) => validator.target === 'json');
		const formValidator = route.validators.find((validator) => validator.target === 'form');
		if (!jsonValidator && !formValidator) {
			if (route.explicitRequestSchemaName || route.explicitRequestFormSchemaName) {
				const requestBody: OpenAPIRequestBody = {required: true, content: {}};
				if (route.explicitRequestSchemaName) {
					requestBody.content['application/json'] = {
						schema: this.schemaRegistry.has(route.explicitRequestSchemaName)
							? this.schemaRegistry.getRef(route.explicitRequestSchemaName)
							: {type: 'object'},
					};
				}
				if (route.explicitRequestFormSchemaName) {
					requestBody.content['multipart/form-data'] = {
						schema: this.schemaRegistry.has(route.explicitRequestFormSchemaName)
							? this.schemaRegistry.getRef(route.explicitRequestFormSchemaName)
							: {type: 'object'},
					};
				}
				return requestBody;
			}
			return undefined;
		}
		function isOptionalSchema(schema: string | null | undefined): boolean {
			if (!schema) {
				return false;
			}
			return /\.(nullable|nullish|optional)\(\)\s*$/.test(schema.trim());
		}
		const requestBody: OpenAPIRequestBody = {
			required: !(isOptionalSchema(jsonValidator?.inlineSchema) || isOptionalSchema(formValidator?.inlineSchema)),
			content: {},
		};
		const setContent = (
			contentType: 'application/json' | 'multipart/form-data',
			validator: ExtractedValidator,
		): void => {
			if (validator.schemaName) {
				requestBody.content[contentType] = {
					schema: this.schemaRegistry.has(validator.schemaName)
						? this.schemaRegistry.getRef(validator.schemaName)
						: {type: 'object'},
				};
				return;
			}
			if (validator.inlineSchema) {
				requestBody.content[contentType] = {
					schema: this.parseInlineSchema(validator.inlineSchema),
				};
			}
		};
		if (jsonValidator) {
			setContent('application/json', jsonValidator);
		}
		if (formValidator) {
			setContent('multipart/form-data', formValidator);
		}
		return requestBody;
	}
	private parseInlineSchema(schemaString: string): OpenAPISchema {
		const objectArgument = this.extractFirstCallArgument(schemaString);
		if (!objectArgument) {
			return {type: 'object'};
		}
		const trimmed = objectArgument.trim();
		if (!trimmed.startsWith('{') || !trimmed.endsWith('}')) {
			return {type: 'object'};
		}
		const body = trimmed.slice(1, -1).trim();
		if (body.length === 0) {
			return {type: 'object', properties: {}};
		}
		const properties: Record<string, OpenAPISchemaOrRef> = {};
		const required: Array<string> = [];
		for (const entry of this.splitTopLevel(body, ',')) {
			if (!entry) {
				continue;
			}
			const colonIndex = this.findTopLevelChar(entry, ':');
			if (colonIndex === -1) {
				continue;
			}
			const key = entry.slice(0, colonIndex).trim();
			const value = entry.slice(colonIndex + 1).trim();
			if (!key || !value) {
				continue;
			}
			const rawName = key.startsWith("'") || key.startsWith('"') ? key.slice(1, -1) : key;
			const name = rawName.trim();
			if (!name) {
				continue;
			}
			properties[name] = this.inferSchemaFromTypeString(value);
			const isOptional =
				/\.(optional|nullish)\(\)/.test(value) || /\.(default|catch)\(/.test(value) || /^z\.optional\(/.test(value);
			if (!isOptional) {
				required.push(name);
			}
		}
		const result: OpenAPISchema = {
			type: 'object',
			properties,
		};
		if (required.length > 0) {
			result.required = required;
		}
		return result;
	}
	private buildResponses(
		route: ExtractedRoute,
		explicitStatusCodes: Array<number> | null,
	): Record<string, OpenAPIResponse> {
		const requiresAuth = this.buildSecurity(route).length > 0;
		const responses: Record<string, OpenAPIResponse> = {};
		const successStatusCodes =
			explicitStatusCodes && explicitStatusCodes.length > 0
				? explicitStatusCodes
				: route.successStatusCodes.length > 0
					? route.successStatusCodes
					: route.hasNoContent
						? [204]
						: [200];
		for (const code of successStatusCodes) {
			if (code === 204 || (route.hasNoContent && !route.responseSchemaName)) {
				responses['204'] = getNoContentResponse();
				continue;
			}
			let responseSchema:
				| OpenAPISchema
				| {
						$ref: string;
				  } = {type: 'object'};
			if (route.responseSchemaName) {
				const resolved = this.getResponseSchema(route.responseSchemaName);
				if (resolved) {
					responseSchema = resolved;
				} else {
					console.warn(
						`Warning: Response schema '${route.responseSchemaName}' not found for ${route.method.toUpperCase()} ${route.path}`,
					);
				}
			}
			responses[String(code)] = {
				description: 'Success',
				content: {
					'application/json': {
						schema: responseSchema,
					},
				},
			};
		}
		Object.assign(responses, getErrorResponses(requiresAuth));
		return responses;
	}
}
