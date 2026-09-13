// SPDX-License-Identifier: AGPL-3.0-or-later
import {
	EMPTY_SCOPE,
	getIdentifierDeclarations,
	type Scope,
	StaticPathResolver,
	type StaticValue,
	UNRESOLVED,
} from '@fluxer/openapi/src/extractors/StaticPathResolver';
import type {
	ExtractedRoute,
	ExtractedValidator,
	HttpMethod,
	OpenAPIExternalDocs,
	ValidatorTarget,
} from '@fluxer/openapi/src/OpenAPITypes';
import {
	type CallExpression,
	type FunctionDeclaration,
	Node,
	type ObjectLiteralExpression,
	Project,
	type SourceFile,
} from 'ts-morph';
import {z} from 'zod';

const HTTP_METHODS: ReadonlySet<string> = new Set(['get', 'post', 'put', 'patch', 'delete']);
const SCHEMA_METADATA_FIELDS: ReadonlySet<string> = new Set(['responseSchema', 'requestSchema', 'requestFormSchema']);
const MetadataStringList = z
	.union([z.string(), z.array(z.string())])
	.transform((value) => (typeof value === 'string' ? [value] : value));
const MetadataStatusCode = z.int().min(100).max(599);
const MetadataSecurityScheme = z.enum(['botToken', 'oauth2Token', 'bearerToken', 'sessionToken', 'adminApiKey']);
const RouteMetadata = z.strictObject({
	operationId: z.string().regex(/^[a-z][a-z0-9_]*$/),
	summary: z.string().min(1),
	description: z.string(),
	responseSchema: z.string().min(1).nullable(),
	responseContentType: z.string().optional(),
	requestSchema: z.string().min(1).optional(),
	requestFormSchema: z.string().min(1).optional(),
	requestBodyRequired: z.boolean().optional(),
	statusCode: z
		.union([MetadataStatusCode, z.array(MetadataStatusCode)])
		.transform((value) => (typeof value === 'number' ? [value] : value))
		.optional(),
	bodylessStatusCodes: z.array(MetadataStatusCode).optional(),
	security: z
		.union([MetadataSecurityScheme, z.array(MetadataSecurityScheme)])
		.transform((value) => (typeof value === 'string' ? [value] : value))
		.optional(),
	tags: MetadataStringList,
	deprecated: z.boolean().optional(),
	externalDocs: z.strictObject({url: z.string(), description: z.string().optional()}).optional(),
});
const RouteOptionsMetadata = RouteMetadata.pick({description: true, responseContentType: true}).extend({
	description: z.string().min(1),
});
function isHttpMethod(method: string): method is HttpMethod {
	return HTTP_METHODS.has(method);
}
function isValidatorTarget(target: string): target is ValidatorTarget {
	return ['json', 'query', 'param', 'form', 'header', 'cookie'].includes(target);
}
function extractStringLiteral(node: Node): string | null {
	if (Node.isStringLiteral(node)) {
		return node.getLiteralValue();
	}
	if (Node.isNoSubstitutionTemplateLiteral(node)) {
		return node.getLiteralValue();
	}
	return null;
}
function isHttpStatusCode(value: unknown): value is number {
	return typeof value === 'number' && Number.isInteger(value) && value >= 100 && value <= 599;
}
function extractOAuth2ScopeArgs(args: ReadonlyArray<Node>): Array<string> | null {
	const scopes: Array<string> = [];
	for (const arg of args) {
		const value = extractStringLiteral(arg);
		if (!value) {
			return null;
		}
		scopes.push(value);
	}
	return scopes.length > 0 ? scopes : null;
}
interface MetadataContext {
	readonly resolver: StaticPathResolver;
	readonly scope: Scope;
}
function resolveMetadataValue(node: Node, context: MetadataContext): StaticValue {
	const value = context.resolver.resolve(node, context.scope);
	if (value === UNRESOLVED) throw new Error(`Unresolved OpenAPI metadata expression: ${node.getText()}`);
	return value;
}
function extractSchemaName(node: Node, context: MetadataContext): string | null {
	const value = context.resolver.resolve(node, context.scope);
	if (value === null) return null;
	if (value === UNRESOLVED && Node.isIdentifier(node)) return node.getText();
	throw new Error(`OpenAPI schemas must use a named schema export: ${node.getText()}`);
}
function parseObjectLiteralMetadata(
	objLiteral: ObjectLiteralExpression,
	context: MetadataContext,
): Record<string, unknown> {
	const metadata = new Map<string, unknown>();
	for (const property of objLiteral.getProperties()) {
		if (!Node.isPropertyAssignment(property) && !Node.isShorthandPropertyAssignment(property)) {
			throw new Error(`Unsupported OpenAPI metadata property: ${property.getText()}`);
		}
		const name = context.resolver.resolvePropertyName(property.getNameNode(), context.scope);
		if (name === null) throw new Error(`Unresolved OpenAPI metadata key: ${property.getName()}`);
		if (
			Node.isPropertyAssignment(property) &&
			name === '__proto__' &&
			!Node.isComputedPropertyName(property.getNameNode())
		) {
			throw new Error('OpenAPI metadata cannot set an object prototype');
		}
		const initializer = Node.isPropertyAssignment(property) ? property.getInitializerOrThrow() : property.getNameNode();
		metadata.set(
			name,
			SCHEMA_METADATA_FIELDS.has(name)
				? extractSchemaName(initializer, context)
				: resolveMetadataValue(initializer, context),
		);
	}
	return Object.fromEntries(metadata);
}
function extractValidatorInfo(callExpr: CallExpression): ExtractedValidator | null {
	const expression = callExpr.getExpression();
	if (!Node.isIdentifier(expression) || expression.getText() !== 'Validator') return null;
	const [targetArg, schemaArg] = callExpr.getArguments();
	if (!targetArg || !schemaArg) throw new Error(`Validator requires a target and named schema: ${callExpr.getText()}`);
	const target = extractStringLiteral(targetArg);
	if (!target || !isValidatorTarget(target)) throw new Error(`Unsupported validator target: ${targetArg.getText()}`);
	if (!Node.isIdentifier(schemaArg)) {
		throw new Error(`Validator must use a named schema export: ${schemaArg.getText()}`);
	}
	return {target, schemaName: schemaArg.getText()};
}
interface MiddlewareInfo {
	middlewareName: string;
	rateLimitConfig?: string;
	responseSchemaName?: string;
	responseContentType?: string;
	hasNoContent?: boolean;
	bodylessStatusCodes?: Array<number> | null;
	explicitRequestSchemaName?: string;
	explicitRequestFormSchemaName?: string;
	explicitRequestBodyRequired?: boolean;
	explicitSummary?: string;
	explicitOperationId?: string;
	explicitDescription?: string;
	explicitStatusCodes?: Array<number> | null;
	explicitSecurity?: Array<string> | null;
	oauth2RequiredScopes?: Array<string> | null;
	oauth2ScopeMode?: 'all' | 'any';
	oauth2BearerTokenRequired?: boolean;
	explicitTags?: Array<string> | null;
	explicitDeprecated?: boolean;
	explicitExternalDocs?: OpenAPIExternalDocs;
}
function extractOpenAPIMetadata(args: ReadonlyArray<Node>, context: MetadataContext): MiddlewareInfo {
	const [first, summary, responseSchema, options] = args;
	if (!first) throw new Error('OpenAPI requires route metadata');
	if (options !== undefined && !Node.isObjectLiteralExpression(options)) {
		throw new Error(`OpenAPI options must be an object literal: ${options.getText()}`);
	}
	const value = Node.isObjectLiteralExpression(first)
		? parseObjectLiteralMetadata(first, context)
		: {
				...(options ? RouteOptionsMetadata.parse(parseObjectLiteralMetadata(options, context)) : {}),
				operationId: resolveMetadataValue(first, context),
				summary: summary ? resolveMetadataValue(summary, context) : undefined,
				responseSchema: responseSchema ? extractSchemaName(responseSchema, context) : undefined,
				tags: [],
			};
	const parsed = RouteMetadata.safeParse(value);
	if (!parsed.success) {
		throw new Error(`Invalid OpenAPI metadata at line ${first.getStartLineNumber()}`, {cause: parsed.error});
	}
	const metadata = parsed.data;
	return {
		middlewareName: 'OpenAPI',
		responseSchemaName: metadata.responseSchema ?? undefined,
		hasNoContent: metadata.responseSchema === null,
		bodylessStatusCodes: metadata.bodylessStatusCodes,
		responseContentType: metadata.responseContentType,
		explicitRequestSchemaName: metadata.requestSchema,
		explicitRequestFormSchemaName: metadata.requestFormSchema,
		explicitRequestBodyRequired: metadata.requestBodyRequired,
		explicitSummary: metadata.summary,
		explicitOperationId: metadata.operationId,
		explicitDescription: metadata.description,
		explicitStatusCodes: metadata.statusCode,
		explicitSecurity: metadata.security,
		explicitTags: metadata.tags,
		explicitDeprecated: metadata.deprecated,
		explicitExternalDocs: metadata.externalDocs,
	};
}
function extractMiddlewareInfo(callExpr: CallExpression, context: MetadataContext): MiddlewareInfo | null {
	const expression = callExpr.getExpression();
	if (!Node.isIdentifier(expression)) return null;
	const name = expression.getText();
	const args = callExpr.getArguments();
	switch (name) {
		case 'RateLimitMiddleware':
			return {middlewareName: name, rateLimitConfig: args[0]?.getText()};
		case 'ResponseType':
			return {middlewareName: name, responseSchemaName: args[0]?.getText()};
		case 'NoContent':
			return {middlewareName: name, hasNoContent: true};
		case 'OpenAPI':
			return extractOpenAPIMetadata(args, context);
		case 'requireOAuth2Scope':
		case 'requireOAuth2ScopeForBearer':
			return {middlewareName: name, oauth2RequiredScopes: extractOAuth2ScopeArgs(args), oauth2ScopeMode: 'all'};
		case 'requireAnyOAuth2Scope':
		case 'requireAnyOAuth2ScopeForBearer':
			return {middlewareName: name, oauth2RequiredScopes: extractOAuth2ScopeArgs(args), oauth2ScopeMode: 'any'};
		case 'requireOAuth2BearerToken':
			return {middlewareName: name, oauth2BearerTokenRequired: true};
		default:
			return {middlewareName: name};
	}
}
function extractSuccessStatusCodes(handler: Node): Array<number> {
	const codes = new Set<number>();
	handler.forEachDescendant((node) => {
		if (!Node.isCallExpression(node)) return;
		const expression = node.getExpression();
		if (!Node.isPropertyAccessExpression(expression)) return;
		const target = expression.getExpression();
		if (!Node.isIdentifier(target) || target.getText() !== 'ctx') return;
		const method = expression.getName();
		if (method !== 'json' && method !== 'body' && method !== 'text') return;
		const args = node.getArguments();
		if (args.length < 2) return;
		const statusArg = args[1];
		if (!Node.isNumericLiteral(statusArg)) return;
		const parsed = statusArg.getLiteralValue();
		if (!isHttpStatusCode(parsed)) throw new Error(`Invalid HTTP response status: ${statusArg.getText()}`);
		if (parsed >= 200 && parsed <= 299) {
			codes.add(parsed);
		}
	});
	return Array.from(codes).sort((a, b) => a - b);
}
interface RegistrationCall {
	readonly call: CallExpression;
	readonly methods: ReadonlyArray<HttpMethod>;
	readonly pathArgument: Node;
	readonly middlewareArguments: ReadonlyArray<Node>;
}
interface UnresolvedRegistration {
	readonly filePath: string;
	readonly lineNumber: number;
	readonly methods: string;
	readonly expression: string;
}
function methodsFromOnArgument(node: Node, resolver: StaticPathResolver, scope: Scope): Array<HttpMethod> | null {
	const value = resolver.resolve(node, scope);
	if (value === UNRESOLVED) {
		return null;
	}
	const entries: Array<StaticValue> = Array.isArray(value) ? [...value] : [value];
	const methods: Array<HttpMethod> = [];
	for (const entry of entries) {
		if (typeof entry !== 'string') {
			return null;
		}
		const lowered = entry.toLowerCase();
		if (lowered === 'head') {
			continue;
		}
		if (!isHttpMethod(lowered)) {
			return null;
		}
		methods.push(lowered);
	}
	return methods.length > 0 ? methods : null;
}
function isHonoReceiver(receiver: Node): boolean {
	if (!Node.isIdentifier(receiver)) return false;
	const declarations = getIdentifierDeclarations(receiver);
	if (declarations.length !== 1) return false;
	const declaration = declarations[0];
	if (!Node.isParameterDeclaration(declaration) && !Node.isVariableDeclaration(declaration)) return false;
	const typeNode = declaration.getTypeNode();
	if (typeNode !== undefined && Node.isTypeReference(typeNode)) {
		const typeName = typeNode.getTypeName();
		if (Node.isIdentifier(typeName) && (typeName.getText() === 'HonoApp' || typeName.getText() === 'Hono')) {
			return true;
		}
	}
	if (!Node.isVariableDeclaration(declaration)) return false;
	const initializer = declaration.getInitializer();
	if (initializer === undefined || !Node.isNewExpression(initializer)) return false;
	const constructorExpression = initializer.getExpression();
	return Node.isIdentifier(constructorExpression) && constructorExpression.getText() === 'Hono';
}
function isRegistrationCall(callExpr: CallExpression): boolean {
	const expression = callExpr.getExpression();
	if (!Node.isPropertyAccessExpression(expression)) {
		return false;
	}
	if (!isHonoReceiver(expression.getExpression())) {
		return false;
	}
	const name = expression.getName().toLowerCase();
	const args = callExpr.getArguments();
	if (isHttpMethod(name)) {
		return args.length >= 2;
	}
	return name === 'on' && args.length >= 3;
}
function pathArgumentOf(callExpr: CallExpression): Node | null {
	const expression = callExpr.getExpression();
	if (!Node.isPropertyAccessExpression(expression)) {
		return null;
	}
	const args = callExpr.getArguments();
	return expression.getName().toLowerCase() === 'on' ? (args[1] ?? null) : (args[0] ?? null);
}
function readRegistrationCall(
	callExpr: CallExpression,
	resolver: StaticPathResolver,
	scope: Scope,
): RegistrationCall | null {
	if (!isRegistrationCall(callExpr)) {
		return null;
	}
	const expression = callExpr.getExpression();
	if (!Node.isPropertyAccessExpression(expression)) {
		return null;
	}
	const name = expression.getName().toLowerCase();
	const args = callExpr.getArguments();
	if (isHttpMethod(name)) {
		return {
			call: callExpr,
			methods: [name],
			pathArgument: args[0],
			middlewareArguments: args.slice(1),
		};
	}
	const methods = methodsFromOnArgument(args[0], resolver, scope);
	if (methods == null) {
		return null;
	}
	return {
		call: callExpr,
		methods,
		pathArgument: args[1],
		middlewareArguments: args.slice(2),
	};
}
function buildRoute(
	registration: RegistrationCall,
	method: HttpMethod,
	routePath: string,
	sourceFile: SourceFile,
	resolver: StaticPathResolver,
	scope: Scope,
): ExtractedRoute {
	const route: ExtractedRoute = {
		method,
		path: routePath,
		controllerFile: sourceFile.getFilePath(),
		lineNumber: registration.call.getStartLineNumber(),
		validators: [],
		middlewares: [],
		hasLoginRequired: false,
		hasDefaultUserOnly: false,
		hasLoginRequiredAllowSuspicious: false,
		rateLimitConfig: null,
		responseSchemaName: null,
		responseContentType: 'application/json',
		hasNoContent: false,
		bodylessStatusCodes: [],
		successStatusCodes: [],
		explicitRequestSchemaName: null,
		explicitRequestFormSchemaName: null,
		explicitRequestBodyRequired: null,
		explicitSummary: null,
		explicitOperationId: null,
		explicitDescription: null,
		explicitStatusCodes: null,
		explicitSecurity: null,
		oauth2RequiredScopes: null,
		oauth2ScopeMode: null,
		oauth2BearerTokenRequired: false,
		explicitTags: null,
		explicitDeprecated: false,
		explicitExternalDocs: null,
	};
	const context = {resolver, scope};
	for (const arg of registration.middlewareArguments) {
		if (Node.isIdentifier(arg)) {
			const name = arg.getText();
			route.middlewares.push(name);
			if (name === 'LoginRequired') route.hasLoginRequired = true;
			if (name === 'DefaultUserOnly') route.hasDefaultUserOnly = true;
			if (name === 'LoginRequiredAllowSuspicious') route.hasLoginRequiredAllowSuspicious = true;
			continue;
		}
		if (Node.isArrowFunction(arg) || Node.isFunctionExpression(arg)) {
			route.successStatusCodes = extractSuccessStatusCodes(arg);
			continue;
		}
		if (!Node.isCallExpression(arg)) continue;
		const validator = extractValidatorInfo(arg);
		if (validator) {
			route.validators.push(validator);
			continue;
		}
		const middleware = extractMiddlewareInfo(arg, context);
		if (!middleware) continue;
		route.middlewares.push(middleware.middlewareName);
		if (middleware.rateLimitConfig) route.rateLimitConfig = middleware.rateLimitConfig;
		if (middleware.responseSchemaName) route.responseSchemaName = middleware.responseSchemaName;
		if (middleware.responseContentType) route.responseContentType = middleware.responseContentType;
		if (middleware.hasNoContent) route.hasNoContent = true;
		if (middleware.bodylessStatusCodes) route.bodylessStatusCodes = middleware.bodylessStatusCodes;
		if (middleware.explicitRequestSchemaName) route.explicitRequestSchemaName = middleware.explicitRequestSchemaName;
		if (middleware.explicitRequestFormSchemaName) {
			route.explicitRequestFormSchemaName = middleware.explicitRequestFormSchemaName;
		}
		if (middleware.explicitRequestBodyRequired !== undefined) {
			route.explicitRequestBodyRequired = middleware.explicitRequestBodyRequired;
		}
		if (middleware.explicitSummary) route.explicitSummary = middleware.explicitSummary;
		if (middleware.explicitOperationId) route.explicitOperationId = middleware.explicitOperationId;
		if (middleware.explicitDescription) route.explicitDescription = middleware.explicitDescription;
		if (middleware.explicitStatusCodes) route.explicitStatusCodes = middleware.explicitStatusCodes;
		if (middleware.explicitSecurity) route.explicitSecurity = middleware.explicitSecurity;
		if (middleware.oauth2RequiredScopes && middleware.oauth2ScopeMode) {
			if (route.oauth2ScopeMode && route.oauth2ScopeMode !== middleware.oauth2ScopeMode) {
				throw new Error(
					`Cannot combine OAuth2 scope middleware modes on ${method.toUpperCase()} ${routePath} in ${route.controllerFile}:${route.lineNumber}`,
				);
			}
			route.oauth2ScopeMode = middleware.oauth2ScopeMode;
			route.oauth2RequiredScopes = [
				...new Set([...(route.oauth2RequiredScopes ?? []), ...middleware.oauth2RequiredScopes]),
			];
		}
		if (middleware.oauth2BearerTokenRequired) route.oauth2BearerTokenRequired = true;
		if (middleware.explicitTags) route.explicitTags = middleware.explicitTags;
		if (middleware.explicitDeprecated) route.explicitDeprecated = true;
		if (middleware.explicitExternalDocs) route.explicitExternalDocs = middleware.explicitExternalDocs;
	}
	return route;
}
function owningFunction(node: Node): FunctionDeclaration | null {
	for (const ancestor of node.getAncestors()) {
		if (Node.isFunctionDeclaration(ancestor)) {
			return ancestor;
		}
	}
	return null;
}
function bindParameters(
	fn: FunctionDeclaration,
	args: ReadonlyArray<Node>,
	callerScope: Scope,
	resolver: StaticPathResolver,
): Scope {
	const scope = new Map(callerScope);
	for (const declaration of scope.keys()) {
		if (declaration.getAncestors().includes(fn)) scope.delete(declaration);
	}
	const firstSpreadIndex = args.findIndex(Node.isSpreadElement);
	fn.getParameters().forEach((parameter, index) => {
		if (parameter.isRestParameter()) return;
		if (firstSpreadIndex !== -1 && index >= firstSpreadIndex) return;
		const arg = args[index];
		if (arg == null) {
			return;
		}
		const value = resolver.resolve(arg, callerScope);
		if (value === UNRESOLVED) {
			return;
		}
		const nameNode = parameter.getNameNode();
		if (Node.isIdentifier(nameNode)) {
			scope.set(parameter, value);
			return;
		}
		if (Node.isObjectBindingPattern(nameNode) && typeof value === 'object' && value !== null && !Array.isArray(value)) {
			const record = value as {readonly [key: string]: StaticValue};
			for (const element of nameNode.getElements()) {
				if (element.getDotDotDotToken() !== undefined) continue;
				const key = resolver.resolvePropertyName(element.getPropertyNameNode() ?? element.getNameNode(), scope);
				if (key !== null && Object.hasOwn(record, key)) {
					scope.set(element, record[key]);
				}
			}
		}
	});
	return scope;
}
function scopesForFunction(
	fn: FunctionDeclaration,
	sourceFile: SourceFile,
	resolver: StaticPathResolver,
	visiting: Set<FunctionDeclaration>,
): Array<Scope> {
	if (visiting.has(fn) || fn.getName() === undefined) {
		return [EMPTY_SCOPE];
	}
	visiting.add(fn);
	try {
		const scopes: Array<Scope> = [];
		sourceFile.forEachDescendant((node) => {
			if (!Node.isCallExpression(node)) {
				return;
			}
			const callee = node.getExpression();
			if (!Node.isIdentifier(callee) || !getIdentifierDeclarations(callee).includes(fn)) {
				return;
			}
			const enclosing = owningFunction(node);
			const outerScopes =
				enclosing == null || enclosing === fn
					? [EMPTY_SCOPE]
					: scopesForFunction(enclosing, sourceFile, resolver, visiting);
			for (const outerScope of outerScopes) {
				for (const loopScope of expandLoops(node, enclosing, outerScope, resolver)) {
					scopes.push(bindParameters(fn, node.getArguments(), loopScope, resolver));
				}
			}
		});
		return scopes.length > 0 ? scopes : [EMPTY_SCOPE];
	} finally {
		visiting.delete(fn);
	}
}
function expandLoops(
	node: Node,
	stopAt: FunctionDeclaration | null,
	baseScope: Scope,
	resolver: StaticPathResolver,
): Array<Scope> {
	const loops: Array<Node> = [];
	for (const ancestor of node.getAncestors()) {
		if (ancestor === stopAt || Node.isSourceFile(ancestor)) {
			break;
		}
		if (Node.isForOfStatement(ancestor)) {
			loops.push(ancestor);
		}
	}
	let scopes: Array<Scope> = [baseScope];
	for (const loop of loops.reverse()) {
		if (!Node.isForOfStatement(loop)) {
			continue;
		}
		const initializer = loop.getInitializer();
		if (!Node.isVariableDeclarationList(initializer) || initializer.getDeclarationKind() !== 'const') {
			return scopes;
		}
		const declaration = initializer.getDeclarations()[0];
		const nameNode = declaration?.getNameNode();
		if (nameNode == null || !Node.isIdentifier(nameNode)) {
			return scopes;
		}
		const expanded: Array<Scope> = [];
		for (const scope of scopes) {
			const iterated = resolver.resolve(loop.getExpression(), scope);
			if (!Array.isArray(iterated)) {
				return scopes;
			}
			for (const element of iterated) {
				const next = new Map(scope);
				next.set(declaration, element);
				expanded.push(next);
			}
		}
		scopes = expanded;
	}
	return scopes;
}
function findRoutesInSourceFile(
	sourceFile: SourceFile,
	resolver: StaticPathResolver,
	unresolved: Array<UnresolvedRegistration>,
): Array<ExtractedRoute> {
	const registrations: Array<CallExpression> = [];
	sourceFile.forEachDescendant((node) => {
		if (Node.isCallExpression(node)) {
			registrations.push(node);
		}
	});
	const byOwner = new Map<FunctionDeclaration | null, Array<CallExpression>>();
	for (const call of registrations) {
		if (!isRegistrationCall(call)) {
			continue;
		}
		const owner = owningFunction(call);
		const bucket = byOwner.get(owner);
		if (bucket == null) {
			byOwner.set(owner, [call]);
		} else {
			bucket.push(call);
		}
	}
	const routes: Array<ExtractedRoute> = [];
	for (const [owner, calls] of byOwner) {
		const scopes = owner == null ? [EMPTY_SCOPE] : scopesForFunction(owner, sourceFile, resolver, new Set());
		for (const call of calls) {
			const seen = new Set<string>();
			let resolvedAny = false;
			for (const scope of scopes) {
				const registration = readRegistrationCall(call, resolver, scope);
				if (registration == null) {
					continue;
				}
				const routePath = resolver.resolveString(registration.pathArgument, scope);
				if (routePath == null) {
					continue;
				}
				resolvedAny = true;
				for (const method of registration.methods) {
					const key = `${method} ${routePath}`;
					if (seen.has(key)) {
						continue;
					}
					seen.add(key);
					routes.push(buildRoute(registration, method, routePath, sourceFile, resolver, scope));
				}
			}
			if (!resolvedAny) {
				const expression = call.getExpression();
				const methodName = Node.isPropertyAccessExpression(expression) ? expression.getName().toUpperCase() : '?';
				const pathArgument = pathArgumentOf(call);
				unresolved.push({
					filePath: sourceFile.getFilePath(),
					lineNumber: call.getStartLineNumber(),
					methods: methodName === 'ON' ? `ON ${call.getArguments()[0].getText()}` : methodName,
					expression: (pathArgument ?? call).getText().replace(/\s+/gu, ' '),
				});
			}
		}
	}
	return routes;
}
export function extractRoutesFromControllers(controllerPaths: Array<string>): Array<ExtractedRoute> {
	const project = new Project({
		skipAddingFilesFromTsConfig: true,
		skipFileDependencyResolution: true,
		compilerOptions: {noResolve: true, noLib: true, types: []},
	});
	const resolver = new StaticPathResolver(project);
	const routes: Array<ExtractedRoute> = [];
	const unresolved: Array<UnresolvedRegistration> = [];
	const sourceFiles = controllerPaths.map((controllerPath) => project.addSourceFileAtPath(controllerPath));
	for (const sourceFile of sourceFiles) {
		try {
			const fileRoutes = findRoutesInSourceFile(sourceFile, resolver, unresolved);
			routes.push(...fileRoutes);
		} catch (error) {
			throw new Error(`Could not extract routes from ${sourceFile.getFilePath()}`, {cause: error});
		}
	}
	if (unresolved.length > 0) {
		const lines = unresolved.map(
			(entry) => `  ${entry.filePath}:${entry.lineNumber.toString()}  ${entry.methods}  ${entry.expression}`,
		);
		throw new Error(
			[
				`The route extractor could not read ${unresolved.length.toString()} route path(s). A path it cannot read is a route`,
				'that would vanish from openapi.json and from the docs coverage gate without a trace, so extraction',
				'stops here instead. Give the path a literal, or a const the resolver can follow, or teach',
				'packages/openapi/src/extractors/StaticPathResolver.ts to read the expression.',
				...lines,
			].join('\n'),
		);
	}
	return routes;
}
export function discoverControllerFiles(apiPackagePath: string): Array<string> {
	const project = new Project({
		tsConfigFilePath: `${apiPackagePath}/tsconfig.json`,
		skipAddingFilesFromTsConfig: true,
	});
	const sourceFiles = project.addSourceFilesAtPaths([
		`${apiPackagePath}/src/**/*.ts`,
		`!${apiPackagePath}/src/**/*.test.ts`,
		`!${apiPackagePath}/src/**/tests/**`,
	]);
	return sourceFiles.map((sf) => sf.getFilePath());
}
