// SPDX-License-Identifier: AGPL-3.0-or-later

import type * as React from 'react';

export type SearchParamsInput = Record<string, string | ReadonlyArray<string>>;
type NavigateDestination =
	| string
	| {
			to: string;
			search?: SearchParamsInput;
			hash?: string;
			state?: unknown;
	  };
export type To = string | NavigateDestination;
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

interface RouteLayoutProps extends RouteComponentProps {
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
	canHandle(to: To, from?: URL): boolean;
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

	constructor(
		to: To,
		options?: {
			replace?: boolean;
		},
	) {
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
