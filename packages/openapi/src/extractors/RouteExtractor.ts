// SPDX-License-Identifier: AGPL-3.0-or-later
import {
	EMPTY_SCOPE,
	type Scope,
	StaticPathResolver,
	type StaticValue,
	UNRESOLVED,
} from '@fluxer/openapi/src/extractors/StaticPathResolver';
import type {ExtractedRoute, ExtractedValidator, HttpMethod, ValidatorTarget} from '@fluxer/openapi/src/Types';
import {type CallExpression, type FunctionDeclaration, Node, Project, type SourceFile} from 'ts-morph';

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
		const numbers = value.filter((v): v is number => typeof v === 'number');
		return numbers.length > 0 ? numbers : null;
	}
	return null;
}
function extractStringArray(value: unknown): Array<string> | null {
	if (typeof value === 'string') return [value];
	if (Array.isArray(value)) {
		const strings = value.filter((v): v is string => typeof v === 'string');
		return strings.length > 0 ? strings : null;
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
function resolveMetadataText(node: Node, context: MetadataContext | null): string | null {
	if (context == null) {
		return null;
	}
	const value = context.resolver.resolve(node, context.scope);
	return typeof value === 'string' ? value : null;
}
function extractObjectLiteralValue(node: Node, context: MetadataContext | null): unknown {
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
				const spread = context?.resolver.resolve(element.getExpression(), context.scope);
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
		const result: Record<string, unknown> = {};
		for (const prop of node.getProperties()) {
			if (Node.isPropertyAssignment(prop)) {
				const key = prop.getName();
				const initializer = prop.getInitializer();
				if (initializer) {
					result[key] = extractObjectLiteralValue(initializer, context);
				}
			}
		}
		return result;
	}
	return null;
}
function parseObjectLiteralMetadata(objLiteral: Node, context: MetadataContext | null): Record<string, unknown> {
	if (!Node.isObjectLiteralExpression(objLiteral)) return {};
	return extractObjectLiteralValue(objLiteral, context) as Record<string, unknown>;
}
function extractValidatorInfo(callExpr: CallExpression): ExtractedValidator | null {
	const expression = callExpr.getExpression();
	if (!Node.isIdentifier(expression) || expression.getText() !== 'Validator') {
		return null;
	}
	const args = callExpr.getArguments();
	if (args.length < 2) {
		return null;
	}
	const targetArg = args[0];
	const schemaArg = args[1];
	const target = extractStringLiteral(targetArg);
	if (!target || !isValidatorTarget(target)) {
		return null;
	}
	let schemaName: string | null = null;
	let inlineSchema: string | null = null;
	if (Node.isIdentifier(schemaArg)) {
		schemaName = schemaArg.getText();
	} else if (Node.isCallExpression(schemaArg)) {
		const callText = schemaArg.getText();
		if (callText.startsWith('z.object')) {
			inlineSchema = callText;
		} else {
			const callExpressionName = schemaArg.getExpression();
			if (Node.isPropertyAccessExpression(callExpressionName)) {
				const propName = callExpressionName.getName();
				if (propName === 'merge' || propName === 'pick' || propName === 'omit' || propName === 'partial') {
					const obj = callExpressionName.getExpression();
					if (Node.isIdentifier(obj)) {
						schemaName = obj.getText();
					} else {
						inlineSchema = callText;
					}
				} else {
					inlineSchema = callText;
				}
			} else {
				inlineSchema = callText;
			}
		}
	} else if (Node.isPropertyAccessExpression(schemaArg)) {
		schemaName = schemaArg.getText();
	} else {
		inlineSchema = schemaArg.getText();
	}
	return {target, schemaName, inlineSchema};
}
interface MiddlewareInfo {
	middlewareName: string;
	rateLimitConfig: string | null;
	responseSchemaName: string | null;
	hasNoContent: boolean;
	explicitRequestSchemaName?: string | null;
	explicitRequestFormSchemaName?: string | null;
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
	explicitExternalDocs: {
		url: string;
		description?: string;
	} | null;
}
function extractMiddlewareInfo(callExpr: CallExpression, context: MetadataContext | null): MiddlewareInfo | null {
	const expression = callExpr.getExpression();
	if (Node.isIdentifier(expression)) {
		const name = expression.getText();
		if (name === 'RateLimitMiddleware') {
			const args = callExpr.getArguments();
			if (args.length > 0) {
				const configArg = args[0];
				const configText = configArg.getText();
				return {
					middlewareName: name,
					rateLimitConfig: configText,
					responseSchemaName: null,
					hasNoContent: false,
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
			}
		}
		if (name === 'ResponseType') {
			const args = callExpr.getArguments();
			if (args.length > 0) {
				const schemaArg = args[0];
				let schemaName: string | null = null;
				if (Node.isIdentifier(schemaArg)) {
					schemaName = schemaArg.getText();
				} else if (Node.isPropertyAccessExpression(schemaArg)) {
					schemaName = schemaArg.getText();
				} else if (Node.isCallExpression(schemaArg)) {
					schemaName = schemaArg.getText();
				}
				return {
					middlewareName: name,
					rateLimitConfig: null,
					responseSchemaName: schemaName,
					hasNoContent: false,
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
			}
		}
		if (name === 'NoContent') {
			return {
				middlewareName: name,
				rateLimitConfig: null,
				responseSchemaName: null,
				hasNoContent: true,
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
		}
		if (name === 'OpenAPI') {
			const args = callExpr.getArguments();
			if (args.length === 0) return null;
			const firstArg = args[0];
			if (Node.isObjectLiteralExpression(firstArg)) {
				const metadata = parseObjectLiteralMetadata(firstArg, context);
				const operationId = typeof metadata.operationId === 'string' ? metadata.operationId : null;
				const summary = typeof metadata.summary === 'string' ? metadata.summary : null;
				const description = typeof metadata.description === 'string' ? metadata.description : null;
				const deprecated = typeof metadata.deprecated === 'boolean' ? metadata.deprecated : false;
				let schemaName: string | null = null;
				if (metadata.responseSchema != null) {
					schemaName = String(metadata.responseSchema);
				}
				const requestSchemaName =
					metadata.requestSchema != null && typeof metadata.requestSchema === 'string' ? metadata.requestSchema : null;
				const requestFormSchemaName =
					metadata.requestFormSchema != null && typeof metadata.requestFormSchema === 'string'
						? metadata.requestFormSchema
						: null;
				const statusCodes = extractNumberArray(metadata.statusCode);
				const security = extractStringArray(metadata.security);
				const tags = extractStringArray(metadata.tags);
				let externalDocs: {
					url: string;
					description?: string;
				} | null = null;
				if (
					metadata.externalDocs &&
					typeof metadata.externalDocs === 'object' &&
					'url' in metadata.externalDocs &&
					typeof metadata.externalDocs.url === 'string'
				) {
					externalDocs = {
						url: metadata.externalDocs.url,
						description:
							'description' in metadata.externalDocs && typeof metadata.externalDocs.description === 'string'
								? metadata.externalDocs.description
								: undefined,
					};
				}
				return {
					middlewareName: name,
					rateLimitConfig: null,
					responseSchemaName: schemaName,
					hasNoContent: schemaName === null || schemaName === 'null',
					explicitRequestSchemaName: requestSchemaName,
					explicitRequestFormSchemaName: requestFormSchemaName,
					explicitSummary: summary,
					explicitOperationId: operationId,
					explicitDescription: description,
					explicitStatusCodes: statusCodes,
					explicitSecurity: security,
					oauth2RequiredScopes: null,
					oauth2ScopeMode: null,
					oauth2BearerTokenRequired: false,
					explicitTags: tags,
					explicitDeprecated: deprecated,
					explicitExternalDocs: externalDocs,
				};
			}
			if (args.length >= 2) {
				const secondArg = args[1];
				let operationId: string | null = null;
				let summary: string | null = null;
				let schemaName: string | null = null;
				let description: string | null = null;
				if (Node.isStringLiteral(firstArg) || Node.isNoSubstitutionTemplateLiteral(firstArg)) {
					operationId = firstArg.getLiteralValue();
				}
				if (Node.isStringLiteral(secondArg) || Node.isNoSubstitutionTemplateLiteral(secondArg)) {
					summary = secondArg.getLiteralValue();
				}
				if (args.length > 2) {
					const thirdArg = args[2];
					if (Node.isIdentifier(thirdArg)) {
						schemaName = thirdArg.getText();
					} else if (Node.isPropertyAccessExpression(thirdArg)) {
						schemaName = thirdArg.getText();
					} else if (Node.isCallExpression(thirdArg)) {
						schemaName = thirdArg.getText();
					}
				}
				if (args.length > 3) {
					const fourthArg = args[3];
					if (Node.isObjectLiteralExpression(fourthArg)) {
						const properties = fourthArg.getProperties();
						for (const prop of properties) {
							if (Node.isPropertyAssignment(prop)) {
								const propName = prop.getName();
								if (propName === 'description') {
									const initializer = prop.getInitializer();
									if (initializer) {
										description = extractStringLiteral(initializer);
									}
								}
							}
						}
					}
				}
				return {
					middlewareName: name,
					rateLimitConfig: null,
					responseSchemaName: schemaName,
					hasNoContent: schemaName === null || schemaName === 'null',
					explicitSummary: summary,
					explicitOperationId: operationId,
					explicitDescription: description,
					explicitStatusCodes: null,
					explicitSecurity: null,
					oauth2RequiredScopes: null,
					oauth2ScopeMode: null,
					oauth2BearerTokenRequired: false,
					explicitTags: null,
					explicitDeprecated: false,
					explicitExternalDocs: null,
				};
			}
		}
		if (name === 'requireOAuth2Scope' || name === 'requireOAuth2ScopeForBearer') {
			const scopes = extractOAuth2ScopeArgs(callExpr.getArguments());
			return {
				middlewareName: name,
				rateLimitConfig: null,
				responseSchemaName: null,
				hasNoContent: false,
				explicitSummary: null,
				explicitOperationId: null,
				explicitDescription: null,
				explicitStatusCodes: null,
				explicitSecurity: null,
				oauth2RequiredScopes: scopes,
				oauth2ScopeMode: 'all',
				oauth2BearerTokenRequired: false,
				explicitTags: null,
				explicitDeprecated: false,
				explicitExternalDocs: null,
			};
		}
		if (name === 'requireAnyOAuth2Scope' || name === 'requireAnyOAuth2ScopeForBearer') {
			const scopes = extractOAuth2ScopeArgs(callExpr.getArguments());
			return {
				middlewareName: name,
				rateLimitConfig: null,
				responseSchemaName: null,
				hasNoContent: false,
				explicitSummary: null,
				explicitOperationId: null,
				explicitDescription: null,
				explicitStatusCodes: null,
				explicitSecurity: null,
				oauth2RequiredScopes: scopes,
				oauth2ScopeMode: 'any',
				oauth2BearerTokenRequired: false,
				explicitTags: null,
				explicitDeprecated: false,
				explicitExternalDocs: null,
			};
		}
		if (name === 'requireOAuth2BearerToken') {
			return {
				middlewareName: name,
				rateLimitConfig: null,
				responseSchemaName: null,
				hasNoContent: false,
				explicitSummary: null,
				explicitOperationId: null,
				explicitDescription: null,
				explicitStatusCodes: null,
				explicitSecurity: null,
				oauth2RequiredScopes: null,
				oauth2ScopeMode: null,
				oauth2BearerTokenRequired: true,
				explicitTags: null,
				explicitDeprecated: false,
				explicitExternalDocs: null,
			};
		}
		return {
			middlewareName: name,
			rateLimitConfig: null,
			responseSchemaName: null,
			hasNoContent: false,
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
	}
	return null;
}
function extractHandlerInfo(arg: Node): {
	handlerSource: string;
	responseMapperName: string | null;
	successStatusCodes: Array<number>;
} | null {
	if (!Node.isArrowFunction(arg) && !Node.isFunctionExpression(arg)) {
		return null;
	}
	const handlerSource = arg.getText();
	let responseMapperName: string | null = null;
	const mapperMatch = handlerSource.match(/\b(map\w+To\w+)\s*\(/);
	if (mapperMatch) {
		responseMapperName = mapperMatch[1];
	}
	const successStatusCodes = extractSuccessStatusCodes(arg);
	const truncatedSource =
		handlerSource.length > 2000 ? `${handlerSource.slice(0, 2000)}\n// ... truncated` : handlerSource;
	return {handlerSource: truncatedSource, responseMapperName, successStatusCodes};
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
	const validators: Array<ExtractedValidator> = [];
	const middlewares: Array<string> = [];
	let hasLoginRequired = false;
	let hasDefaultUserOnly = false;
	let hasLoginRequiredAllowSuspicious = false;
	let hasSudoMode = false;
	let rateLimitConfig: string | null = null;
	let handlerSource: string | null = null;
	let responseMapperName: string | null = null;
	let responseSchemaName: string | null = null;
	let hasNoContent = false;
	let successStatusCodes: Array<number> = [];
	let explicitRequestSchemaName: string | null = null;
	let explicitRequestFormSchemaName: string | null = null;
	let explicitSummary: string | null = null;
	let explicitOperationId: string | null = null;
	let explicitDescription: string | null = null;
	let explicitStatusCodes: Array<number> | null = null;
	let explicitSecurity: Array<string> | null = null;
	let oauth2RequiredScopes: Array<string> | null = null;
	let oauth2ScopeMode: 'all' | 'any' | null = null;
	let oauth2BearerTokenRequired = false;
	let explicitTags: Array<string> | null = null;
	let explicitDeprecated = false;
	let explicitExternalDocs: {
		url: string;
		description?: string;
	} | null = null;
	for (const arg of registration.middlewareArguments) {
		if (Node.isIdentifier(arg)) {
			const name = arg.getText();
			middlewares.push(name);
			if (name === 'LoginRequired') {
				hasLoginRequired = true;
			} else if (name === 'DefaultUserOnly') {
				hasDefaultUserOnly = true;
			} else if (name === 'LoginRequiredAllowSuspicious') {
				hasLoginRequiredAllowSuspicious = true;
			} else if (name === 'SudoModeMiddleware') {
				hasSudoMode = true;
			}
		} else if (Node.isCallExpression(arg)) {
			const validatorInfo = extractValidatorInfo(arg);
			if (validatorInfo) {
				validators.push(validatorInfo);
			} else {
				const middlewareInfo = extractMiddlewareInfo(arg, {resolver, scope});
				if (middlewareInfo) {
					middlewares.push(middlewareInfo.middlewareName);
					if (middlewareInfo.rateLimitConfig) {
						rateLimitConfig = middlewareInfo.rateLimitConfig;
					}
					if (middlewareInfo.responseSchemaName) {
						responseSchemaName = middlewareInfo.responseSchemaName;
					}
					if (middlewareInfo.hasNoContent) {
						hasNoContent = true;
					}
					if (middlewareInfo.explicitRequestSchemaName) {
						explicitRequestSchemaName = middlewareInfo.explicitRequestSchemaName;
					}
					if (middlewareInfo.explicitRequestFormSchemaName) {
						explicitRequestFormSchemaName = middlewareInfo.explicitRequestFormSchemaName;
					}
					if (middlewareInfo.explicitSummary) {
						explicitSummary = middlewareInfo.explicitSummary;
					}
					if (middlewareInfo.explicitOperationId) {
						explicitOperationId = middlewareInfo.explicitOperationId;
					}
					if (middlewareInfo.explicitDescription) {
						explicitDescription = middlewareInfo.explicitDescription;
					}
					if (middlewareInfo.explicitStatusCodes) {
						explicitStatusCodes = middlewareInfo.explicitStatusCodes;
					}
					if (middlewareInfo.explicitSecurity) {
						explicitSecurity = middlewareInfo.explicitSecurity;
					}
					if (middlewareInfo.oauth2RequiredScopes && middlewareInfo.oauth2ScopeMode) {
						if (oauth2ScopeMode && oauth2ScopeMode !== middlewareInfo.oauth2ScopeMode) {
							throw new Error(
								`Cannot combine OAuth2 scope middleware modes on ${method.toUpperCase()} ${routePath} in ${sourceFile.getFilePath()}:${registration.call.getStartLineNumber()}`,
							);
						}
						oauth2ScopeMode = middlewareInfo.oauth2ScopeMode;
						const combinedScopes: Array<string> = [
							...(oauth2RequiredScopes ?? []),
							...middlewareInfo.oauth2RequiredScopes,
						];
						oauth2RequiredScopes = Array.from(new Set<string>(combinedScopes));
					}
					if (middlewareInfo.oauth2BearerTokenRequired) {
						oauth2BearerTokenRequired = true;
					}
					if (middlewareInfo.explicitTags) {
						explicitTags = middlewareInfo.explicitTags;
					}
					if (middlewareInfo.explicitDeprecated) {
						explicitDeprecated = middlewareInfo.explicitDeprecated;
					}
					if (middlewareInfo.explicitExternalDocs) {
						explicitExternalDocs = middlewareInfo.explicitExternalDocs;
					}
				}
			}
		} else if (Node.isArrowFunction(arg) || Node.isFunctionExpression(arg)) {
			const handlerInfo = extractHandlerInfo(arg);
			if (handlerInfo) {
				handlerSource = handlerInfo.handlerSource;
				responseMapperName = handlerInfo.responseMapperName;
				successStatusCodes = handlerInfo.successStatusCodes;
			}
		}
	}
	return {
		method,
		path: routePath,
		controllerFile: sourceFile.getFilePath(),
		lineNumber: registration.call.getStartLineNumber(),
		validators,
		middlewares,
		hasLoginRequired,
		hasDefaultUserOnly,
		hasLoginRequiredAllowSuspicious,
		hasSudoMode,
		rateLimitConfig,
		handlerSource,
		responseMapperName,
		responseSchemaName,
		hasNoContent,
		successStatusCodes,
		explicitRequestSchemaName,
		explicitRequestFormSchemaName,
		explicitSummary,
		explicitOperationId,
		explicitDescription,
		explicitStatusCodes,
		explicitSecurity,
		oauth2RequiredScopes,
		oauth2ScopeMode,
		oauth2BearerTokenRequired,
		explicitTags,
		explicitDeprecated,
		explicitExternalDocs,
	};
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
			console.warn(`Warning: Could not parse ${controllerPath}:`, error);
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
