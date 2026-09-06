// SPDX-License-Identifier: AGPL-3.0-or-later
import {discoverControllerFiles, extractRoutesFromControllers} from '@fluxer/openapi/src/extractors/RouteExtractor';
import {isExcludedRoutePath, OpenAPIGeneratorCatalog} from '@fluxer/openapi/src/generator/OpenAPIGeneratorCatalog';
import {OpenAPIOperationBuilder} from '@fluxer/openapi/src/generator/OpenAPIOperationBuilder';
import {collectReferencedSchemaNames} from '@fluxer/openapi/src/generator/OpenAPISchemaReferenceCollector';
import {loadSchemasIntoRegistry} from '@fluxer/openapi/src/generator/OpenAPISchemaRegistryLoader';
import type {
	OpenAPIGenerationResult,
	OpenAPIGeneratorOptions,
	OpenAPIRouteScope,
	SkippedRoute,
} from '@fluxer/openapi/src/OpenAPIGenerationTypes';
import type {ExtractedRoute, OpenAPIDocument, OpenAPIPathItem, OpenAPISchema} from '@fluxer/openapi/src/OpenAPITypes';
import {convertPathToOpenAPI} from '@fluxer/openapi/src/registry/ParameterRegistry';
import {SchemaRegistry} from '@fluxer/openapi/src/registry/SchemaRegistry';

interface PathBuildResult {
	readonly paths: Record<string, OpenAPIPathItem>;
	readonly operationCount: number;
	readonly skippedRoutes: ReadonlyArray<SkippedRoute>;
	readonly untemplatableRoutes: ReadonlyArray<SkippedRoute>;
}
function hasOpenAPIPathTemplate(routePath: string): boolean {
	return !routePath.includes('*') && !routePath.includes('{');
}
interface GeneratorSettings {
	readonly basePath: string;
	readonly title: string;
	readonly version: string;
	readonly description: string;
	readonly serverUrl: string;
	readonly routeScope: OpenAPIRouteScope;
}
function createGeneratorSettings(options: OpenAPIGeneratorOptions): GeneratorSettings {
	return {
		basePath: options.basePath,
		title: options.title ?? 'Fluxer API',
		version: options.version ?? '1.0.0',
		description: options.description ?? 'The Fluxer API',
		serverUrl: options.serverUrl ?? 'https://api.fluxer.app',
		routeScope: options.routeScope ?? 'public',
	};
}
function describeRoute(route: ExtractedRoute, reason: string): SkippedRoute {
	return {
		method: route.method.toUpperCase(),
		path: route.path,
		source: `${route.controllerFile}:${route.lineNumber.toString()}`,
		reason,
	};
}
function isAdminRoute(route: ExtractedRoute): boolean {
	return route.path === '/admin' || route.path.startsWith('/admin/');
}
export class OpenAPIGenerator {
	private readonly settings: GeneratorSettings;
	private readonly schemaRegistry: SchemaRegistry;
	constructor(options: OpenAPIGeneratorOptions) {
		this.settings = createGeneratorSettings(options);
		this.schemaRegistry = new SchemaRegistry();
	}
	public async generate(): Promise<OpenAPIDocument> {
		const result = await this.generateWithStats();
		return result.document;
	}
	public async generateWithStats(): Promise<OpenAPIGenerationResult> {
		const controllerFiles = discoverControllerFiles(`${this.settings.basePath}/fluxer_api`);
		const routes = this.filterRoutesForScope(extractRoutesFromControllers(controllerFiles));
		let registeredSchemaCount = 0;
		let loadedSchemas = new Map();
		try {
			const schemaLoadResult = await loadSchemasIntoRegistry(this.settings.basePath, this.schemaRegistry);
			registeredSchemaCount = schemaLoadResult.totalRegisteredSchemas;
			loadedSchemas = schemaLoadResult.loadedSchemas;
		} catch (error) {
			console.warn('Warning: Could not load some schemas:', error);
			registeredSchemaCount = Object.keys(this.schemaRegistry.getAllSchemas()).length;
		}
		const operationBuilder = new OpenAPIOperationBuilder({
			schemaRegistry: this.schemaRegistry,
			loadedSchemas,
			usedOperationIds: new Set(),
		});
		const pathBuildResult = this.buildPaths(routes, operationBuilder);
		const allSchemas = this.schemaRegistry.getAllSchemas();
		const referencedSchemas = collectReferencedSchemaNames(pathBuildResult.paths, allSchemas);
		const publishedSchemas = this.filterPublishedSchemas(allSchemas, referencedSchemas);
		const tags = this.buildTags(routes);
		const document: OpenAPIDocument = {
			openapi: '3.1.0',
			info: {
				title: this.settings.title,
				version: this.settings.version,
				description: this.settings.description,
				contact: {
					name: 'Fluxer Platform AB',
					email: 'support@fluxer.app',
				},
				license: {
					name: 'AGPL-3.0',
					url: 'https://www.gnu.org/licenses/agpl-3.0.html',
				},
			},
			servers: [{url: this.settings.serverUrl, description: 'Production API'}],
			paths: pathBuildResult.paths,
			components: {
				schemas: publishedSchemas,
				securitySchemes: OpenAPIGeneratorCatalog.securitySchemes,
			},
			tags,
		};
		return {
			document,
			stats: {
				controllerCount: controllerFiles.length,
				routeCount: routes.length,
				operationCount: pathBuildResult.operationCount,
				skippedRouteCount: pathBuildResult.skippedRoutes.length,
				skippedRoutes: pathBuildResult.skippedRoutes,
				untemplatableRoutes: pathBuildResult.untemplatableRoutes,
				registeredSchemaCount,
				publishedSchemaCount: Object.keys(publishedSchemas).length,
				tagCount: tags.length,
			},
		};
	}
	private filterRoutesForScope(routes: Array<ExtractedRoute>): Array<ExtractedRoute> {
		switch (this.settings.routeScope) {
			case 'all':
				return routes;
			case 'admin':
				return routes.filter(isAdminRoute);
			case 'public':
				return routes.filter((route) => !isAdminRoute(route));
		}
	}
	private buildPaths(routes: Array<ExtractedRoute>, operationBuilder: OpenAPIOperationBuilder): PathBuildResult {
		const paths: Record<string, OpenAPIPathItem> = {};
		let operationCount = 0;
		const skippedRoutes: Array<SkippedRoute> = [];
		const untemplatableRoutes: Array<SkippedRoute> = [];
		for (const route of routes) {
			if (isExcludedRoutePath(route.path)) {
				continue;
			}
			if (!route.responseSchemaName && !route.hasNoContent) {
				skippedRoutes.push(describeRoute(route, 'no responseSchema and no NoContent()'));
				continue;
			}
			if (!hasOpenAPIPathTemplate(route.path)) {
				untemplatableRoutes.push(describeRoute(route, 'the Hono path has no OpenAPI path template'));
				continue;
			}
			const openApiPath = convertPathToOpenAPI(route.path);
			paths[openApiPath] ??= {};
			paths[openApiPath][route.method] = operationBuilder.buildOperation(route);
			operationCount++;
		}
		const sortedPaths: Record<string, OpenAPIPathItem> = {};
		for (const key of Object.keys(paths).sort()) {
			sortedPaths[key] = paths[key];
		}
		return {
			paths: sortedPaths,
			operationCount,
			skippedRoutes,
			untemplatableRoutes,
		};
	}
	private filterPublishedSchemas(
		allSchemas: Record<string, OpenAPISchema>,
		referencedSchemas: Set<string>,
	): Record<string, OpenAPISchema> {
		const publishedSchemas: Record<string, OpenAPISchema> = {};
		for (const name of referencedSchemas) {
			if (allSchemas[name]) {
				publishedSchemas[name] = allSchemas[name];
			}
		}
		return publishedSchemas;
	}
	private buildTags(routes: Array<ExtractedRoute>): Array<{
		name: string;
		description?: string;
	}> {
		const usedTags = new Set<string>();
		for (const route of routes) {
			if (isExcludedRoutePath(route.path)) {
				continue;
			}
			if (!route.explicitTags) {
				continue;
			}
			for (const tag of route.explicitTags) {
				usedTags.add(tag);
			}
		}
		const orderIndex = new Map<string, number>();
		for (const [index, tag] of OpenAPIGeneratorCatalog.tags.order.entries()) {
			orderIndex.set(tag, index);
		}
		return Array.from(usedTags)
			.sort((a, b) => (orderIndex.get(a) ?? 1000000) - (orderIndex.get(b) ?? 1000000))
			.map((name) => ({
				name,
				description: OpenAPIGeneratorCatalog.tags.descriptions[name],
			}));
	}
}
