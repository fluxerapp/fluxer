// SPDX-License-Identifier: AGPL-3.0-or-later
import {
	EMPTY_SCOPE,
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

const HTTP_METHODS: ReadonlySet<string> = new Set(['get', 'post', 'put', 'patch', 'delete']);
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
function extractNumberArray(value: unknown): Array<number> | null {
	if (typeof value === 'number') return [value];
	if (Array.isArray(value)) {
		if (!value.every((entry): entry is number => typeof entry === 'number')) {
			throw new Error('OpenAPI status-list metadata must contain only numbers');
		}
		return value;
	}
	return null;
}
function extractStringArray(value: unknown): Array<string> | null {
	if (typeof value === 'string') return [value];
	if (Array.isArray(value)) {
		if (!value.every((entry): entry is string => typeof entry === 'string')) {
			throw new Error('OpenAPI string-list metadata must contain only strings');
		}
		return value;
	}
	return null;
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
function resolveMetadataText(node: Node, context: MetadataContext): string | null {
	const value = context.resolver.resolve(node, context.scope);
	return typeof value === 'string' ? value : null;
}
function extractObjectLiteralValue(node: Node, context: MetadataContext): unknown {
	if (Node.isStringLiteral(node) || Node.isNoSubstitutionTemplateLiteral(node)) {
		return node.getLiteralValue();
	}
	if (Node.isTemplateExpression(node) || Node.isConditionalExpression(node)) {
		return resolveMetadataText(node, context);
	}
	if (Node.isNumericLiteral(node)) {
		return Number.parseFloat(node.getText());
	}
	if (Node.isTrueLiteral(node)) {
		return true;
	}
	if (Node.isFalseLiteral(node)) {
		return false;
	}
	if (Node.isNullLiteral(node)) {
		return null;
	}
	if (Node.isIdentifier(node)) {
		return node.getText();
	}
	if (Node.isPropertyAccessExpression(node)) {
		return resolveMetadataText(node, context) ?? node.getText();
	}
	if (Node.isCallExpression(node)) {
		return node.getText();
	}
	if (Node.isArrayLiteralExpression(node)) {
		const values: Array<unknown> = [];
		for (const element of node.getElements()) {
			if (Node.isSpreadElement(element)) {
				const spread = context.resolver.resolve(element.getExpression(), context.scope);
				if (spread == null || spread === UNRESOLVED || !Array.isArray(spread)) {
					values.push(null);
					continue;
				}
				values.push(...spread);
				continue;
			}
			values.push(extractObjectLiteralValue(element, context));
		}
		return values;
	}
	if (Node.isObjectLiteralExpression(node)) {
		return parseObjectLiteralMetadata(node, context);
	}
	return null;
}
function parseObjectLiteralMetadata(
	objLiteral: ObjectLiteralExpression,
	context: MetadataContext,
): Record<string, unknown> {
	const metadata: Record<string, unknown> = {};
	for (const property of objLiteral.getProperties()) {
		if (!Node.isPropertyAssignment(property)) continue;
		const initializer = property.getInitializer();
		if (initializer) metadata[property.getName()] = extractObjectLiteralValue(initializer, context);
	}
	return metadata;
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
function metadataString(value: unknown): string | undefined {
	return typeof value === 'string' ? value : undefined;
}
function extractExternalDocs(value: unknown): OpenAPIExternalDocs | undefined {
	if (!value || typeof value !== 'object' || !('url' in value) || typeof value.url !== 'string') return undefined;
	return {url: value.url, description: 'description' in value ? metadataString(value.description) : undefined};
}
function extractOpenAPIMetadata(args: ReadonlyArray<Node>, context: MetadataContext): MiddlewareInfo {
	const [first, summary, responseSchema, options] = args;
	if (!first) throw new Error('OpenAPI requires route metadata');
	const metadata = Node.isObjectLiteralExpression(first)
		? parseObjectLiteralMetadata(first, context)
		: {
				...(options && Node.isObjectLiteralExpression(options) ? parseObjectLiteralMetadata(options, context) : {}),
				operationId: extractStringLiteral(first),
				summary: summary ? extractStringLiteral(summary) : undefined,
				responseSchema: responseSchema?.getText(),
			};
	const schemaName = metadataString(metadata.responseSchema);
	return {
		middlewareName: 'OpenAPI',
		responseSchemaName: schemaName,
		hasNoContent: schemaName === undefined || schemaName === 'null',
		bodylessStatusCodes: extractNumberArray(metadata.bodylessStatusCodes),
		responseContentType: metadataString(metadata.responseContentType),
		explicitRequestSchemaName: metadataString(metadata.requestSchema),
		explicitRequestFormSchemaName: metadataString(metadata.requestFormSchema),
		explicitRequestBodyRequired:
			typeof metadata.requestBodyRequired === 'boolean' ? metadata.requestBodyRequired : undefined,
		explicitSummary: metadataString(metadata.summary),
		explicitOperationId: metadataString(metadata.operationId),
		explicitDescription: metadataString(metadata.description),
		explicitStatusCodes: extractNumberArray(metadata.statusCode),
		explicitSecurity: extractStringArray(metadata.security),
		explicitTags: extractStringArray(metadata.tags),
		explicitDeprecated: metadata.deprecated === true,
		explicitExternalDocs: extractExternalDocs(metadata.externalDocs),
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
		const parsed = Number.parseInt(statusArg.getText(), 10);
		if (!Number.isFinite(parsed)) return;
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
const HONO_TYPE_PATTERN = /\bHono(App|Env)?\b/u;
function isHonoReceiver(receiver: Node): boolean {
	if (!Node.isIdentifier(receiver)) {
		return false;
	}
	const name = receiver.getText();
	for (const ancestor of receiver.getAncestors()) {
		if (
			Node.isFunctionDeclaration(ancestor) ||
			Node.isArrowFunction(ancestor) ||
			Node.isFunctionExpression(ancestor) ||
			Node.isMethodDeclaration(ancestor)
		) {
			for (const parameter of ancestor.getParameters()) {
				const nameNode = parameter.getNameNode();
				if (Node.isIdentifier(nameNode) && nameNode.getText() === name) {
					return HONO_TYPE_PATTERN.test(parameter.getTypeNode()?.getText() ?? '');
				}
			}
		}
		if (Node.isBlock(ancestor) || Node.isSourceFile(ancestor)) {
			for (const statement of ancestor.getStatements()) {
				if (!Node.isVariableStatement(statement)) {
					continue;
				}
				for (const declaration of statement.getDeclarations()) {
					const nameNode = declaration.getNameNode();
					if (Node.isIdentifier(nameNode) && nameNode.getText() === name) {
						const annotation = declaration.getTypeNode()?.getText() ?? '';
						const initializer = declaration.getInitializer()?.getText() ?? '';
						return HONO_TYPE_PATTERN.test(`${annotation} ${initializer}`);
					}
				}
			}
		}
	}
	return false;
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
	const scope = new Map<string, StaticValue>();
	fn.getParameters().forEach((parameter, index) => {
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
			scope.set(nameNode.getText(), value);
			return;
		}
		if (Node.isObjectBindingPattern(nameNode) && typeof value === 'object' && value !== null && !Array.isArray(value)) {
			const record = value as {readonly [key: string]: StaticValue};
			for (const element of nameNode.getElements()) {
				const key = element.getPropertyNameNode()?.getText() ?? element.getName();
				if (key in record) {
					scope.set(element.getName(), record[key]);
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
	if (visiting.has(fn)) {
		return [EMPTY_SCOPE];
	}
	const name = fn.getName();
	if (name == null) {
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
			if (!Node.isIdentifier(callee) || callee.getText() !== name) {
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
		if (!Node.isVariableDeclarationList(initializer)) {
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
				next.set(nameNode.getText(), element);
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
	});
	const resolver = new StaticPathResolver(project);
	const routes: Array<ExtractedRoute> = [];
	const unresolved: Array<UnresolvedRegistration> = [];
	for (const controllerPath of controllerPaths) {
		try {
			const sourceFile = project.addSourceFileAtPath(controllerPath);
			const fileRoutes = findRoutesInSourceFile(sourceFile, resolver, unresolved);
			routes.push(...fileRoutes);
		} catch (error) {
			throw new Error(`Could not extract routes from ${controllerPath}`, {cause: error});
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
