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

import type * as React from 'react';

export type SearchParamsInput = Record<string, string | number | boolean | null | undefined>;

export type To =
	| string
	| {
			to: string;
			search?: SearchParamsInput;
			hash?: string;
			state?: unknown;
	  };

export type ScrollBehavior = 'preserve' | 'top';

export interface NavigateOptions {
	replace?: boolean;
	state?: unknown;
	scroll?: ScrollBehavior;
	from?: string;
}

export type RouteParams = Record<string, string>;

export interface Match {
	route: Route;
	params: RouteParams;
	search: URLSearchParams;
}

export interface RouterState {
	location: URL;
	matches: Array<Match>;
	navigating: boolean;
	pending?: Array<Match> | null;
	error?: unknown;
	historyState?: unknown;
}

export interface RouteContext {
	url: URL;
	params: RouteParams;
	search: URLSearchParams;
	state: unknown;
	route: Route;
	matches: Array<Match>;
	router: Router;
}

export interface RouteComponentProps {
	match: Match;
	params: RouteParams;
	search: URLSearchParams;
	url: URL;
}

export interface RouteLayoutProps extends RouteComponentProps {
	children: React.ReactNode;
}

export interface RouteConfig {
	id: string;
	path?: string;
	pattern?: URLPattern;
	parentId?: string;
	component?: React.ComponentType<RouteComponentProps>;
	layout?: React.ComponentType<RouteLayoutProps>;
	onEnter?: (ctx: RouteContext) => undefined | Redirect | NotFound;
	onLeave?: (ctx: RouteContext) => void;
	preload?: (ctx: RouteContext) => Promise<unknown> | unknown;
	staticData?: unknown;
}

export interface Route extends Omit<RouteConfig, 'pattern'> {
	pattern: URLPattern;
}

export interface RouterOptions {
	routes: Array<RouteConfig>;
	history?: HistoryAdapter;
	baseHref?: string;
	notFoundRouteId?: string;
	scrollRestoration?: ScrollBehavior;
}

export interface Router {
	getState(): RouterState;
	subscribe(listener: () => void): () => void;
	navigate(to: To, opts?: NavigateOptions): Promise<void>;
	preload(to: To): Promise<void>;
	resolveTo(to: To, from?: URL): URL;
	getRoutes(): Array<Route>;
	destroy(): void;
}

export interface RouterProviderProps {
	router: Router;
	children?: React.ReactNode;
	linkContainerRef?: React.RefObject<HTMLElement>;
}

export interface HistoryLocation {
	url: URL;
	state: unknown;
}

export interface HistoryAdapter {
	getLocation(): HistoryLocation;
	push(url: URL, state?: unknown): void;
	replace(url: URL, state?: unknown): void;
	listen(listener: (location: HistoryLocation, action: 'pop') => void): () => void;
	go(delta: number): void;
	back(): void;
	readonly location: URL;
}

export class Redirect extends Error {
	readonly to: To;
	readonly replace?: boolean;

	constructor(to: To, options?: {replace?: boolean}) {
		super('Redirect');
		this.to = to;
		this.replace = options?.replace;
	}
}

export class NotFound extends Error {
	constructor(message = 'Not Found') {
		super(message);
	}
}
