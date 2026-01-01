/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {NotFound, Redirect, RouteConfig, RouteContext} from './types';

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
	private parent: RouteBuilder | null = null;
	readonly id: string;

	constructor(config: RouteBuilderConfig) {
		this.id = config.id ?? generateRouteId();
		this.config = {...config, id: this.id};
	}

	addChildren(children: Array<RouteBuilder>): this {
		for (const child of children) {
			child.parent = this;
			this.children.push(child);
		}
		return this;
	}

	getParent(): RouteBuilder | null {
		return this.parent;
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
