// SPDX-License-Identifier: AGPL-3.0-or-later

import type {NotFound, Redirect, RouteConfig, RouteContext} from '@app/features/platform/components/router/RouterTypes';

type RouteGuard = (ctx: RouteContext) => undefined | Redirect | NotFound;

interface RouteBuilderConfig {
	id?: string;
	path?: string;
	component?: RouteConfig['component'];
	layout?: RouteConfig['layout'];
	onEnter?: RouteGuard;
	onLeave?: RouteConfig['onLeave'];
	preload?: RouteConfig['preload'];
	staticData?: unknown;
}

let routeIdCounter = 0;

function generateRouteId(): string {
	return `__route_${routeIdCounter++}`;
}

export class RouteBuilder {
	private config: RouteBuilderConfig;
	private children: Array<RouteBuilder> = [];
	readonly id: string;

	constructor(config: RouteBuilderConfig) {
		this.id = config.id ?? generateRouteId();
		this.config = {...config, id: this.id};
	}

	addChildren(children: Array<RouteBuilder>): this {
		for (const child of children) {
			this.children.push(child);
		}
		return this;
	}

	private collectRoutes(parentId?: string): Array<RouteConfig> {
		const routes: Array<RouteConfig> = [];
		const thisRoute: RouteConfig = {
			id: this.id,
			path: this.config.path,
			parentId,
			component: this.config.component,
			layout: this.config.layout,
			onEnter: this.config.onEnter,
			onLeave: this.config.onLeave,
			preload: this.config.preload,
			staticData: this.config.staticData,
		};
		routes.push(thisRoute);
		for (const child of this.children) {
			routes.push(...child.collectRoutes(this.id));
		}
		return routes;
	}

	build(): Array<RouteConfig> {
		return this.collectRoutes();
	}
}

interface RootRouteConfig {
	component?: RouteConfig['component'];
	layout?: RouteConfig['layout'];
	onEnter?: RouteGuard;
	onLeave?: RouteConfig['onLeave'];
	staticData?: unknown;
}

export function createRootRoute(config: RootRouteConfig = {}): RouteBuilder {
	return new RouteBuilder({
		id: '__root',
		path: '/',
		...config,
	});
}

interface CreateRouteConfig<TParent extends RouteBuilder = RouteBuilder> {
	getParentRoute?: () => TParent;
	id?: string;
	path?: string;
	component?: RouteConfig['component'];
	layout?: RouteConfig['layout'];
	onEnter?: RouteGuard;
	onLeave?: RouteConfig['onLeave'];
	preload?: RouteConfig['preload'];
	staticData?: unknown;
}

export function createRoute<TParent extends RouteBuilder>(config: CreateRouteConfig<TParent>): RouteBuilder {
	const builder = new RouteBuilder({
		id: config.id,
		path: config.path,
		component: config.component,
		layout: config.layout,
		onEnter: config.onEnter,
		onLeave: config.onLeave,
		preload: config.preload,
		staticData: config.staticData,
	});
	return builder;
}
