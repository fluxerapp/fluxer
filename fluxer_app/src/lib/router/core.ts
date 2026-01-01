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

import {createBrowserHistory} from './history';
import {
	type Match,
	type NavigateOptions,
	NotFound,
	Redirect,
	type Route,
	type RouteContext,
	type Router,
	type RouterOptions,
	type RouterState,
	type ScrollBehavior,
	type To,
} from './types';

export class RouterImpl implements Router {
	private readonly routes: Array<Route>;
	private readonly routeById: Map<string, Route>;
	private readonly history;
	private readonly options: RouterOptions;
	private state: RouterState;
	private listeners = new Set<() => void>();
	private unsubscribeHistory?: () => void;
	private preloadCache = new Map<string, Promise<unknown> | 'done'>();

	constructor(options: RouterOptions) {
		this.options = options;

		this.history = options.history ?? createBrowserHistory();

		this.routes = options.routes.map((cfg) => ({
			...cfg,
			pattern:
				cfg.pattern ?? (cfg.path != null ? new URLPattern({pathname: cfg.path}) : new URLPattern({pathname: '*'})),
		}));

		this.routeById = new Map(this.routes.map((r) => [r.id, r]));

		const loc = this.history.getLocation();
		const normalizedInitialUrl = this.normalizeUrl(loc.url);
		if (normalizedInitialUrl.toString() !== loc.url.toString()) {
			this.history.replace(normalizedInitialUrl, loc.state);
		}
		const matches = this.matchUrl(normalizedInitialUrl);

		this.state = {
			location: normalizedInitialUrl,
			matches,
			navigating: false,
			pending: null,
			error: undefined,
			historyState: loc.state,
		};

		this.unsubscribeHistory = this.history.listen((location) => {
			void this.handlePop(location.url, location.state);
		});

		this.runInitialGuards();
	}

	getState(): RouterState {
		return this.state;
	}

	getRoutes(): Array<Route> {
		return this.routes;
	}

	subscribe(listener: () => void): () => void {
		this.listeners.add(listener);
		return () => {
			this.listeners.delete(listener);
		};
	}

	destroy(): void {
		if (this.unsubscribeHistory) {
			this.unsubscribeHistory();
			this.unsubscribeHistory = undefined;
		}
		this.listeners.clear();
	}

	private notify() {
		for (const l of this.listeners) l();
	}

	private runInitialGuards(): void {
		const {location, historyState} = this.state;

		void this.transitionTo(location, {
			replace: true,
			scroll: 'preserve',
			historyState,
			isPop: true,
			forceEnter: true,
		}).catch((err) => {
			if (typeof console !== 'undefined') {
				console.error('[router] initial guard check failed', err);
			}
		});
	}

	private normalizeUrl(url: URL): URL {
		if (url.pathname.length <= 1 || !url.pathname.endsWith('/')) {
			return url;
		}

		const normalized = new URL(url.toString());
		const trimmedPath = url.pathname.replace(/\/+$/, '') || '/';
		normalized.pathname = trimmedPath;
		return normalized;
	}

	resolveTo(to: To, from?: URL): URL {
		const base = from ?? this.state.location ?? new URL(window.location.href);

		if (typeof to === 'string') {
			return this.normalizeUrl(new URL(to, base));
		}

		const url = new URL(to.to, base);

		if (to.search) {
			const sp = new URLSearchParams();
			for (const [k, v] of Object.entries(to.search)) {
				if (v === undefined) continue;
				if (v === null) {
					sp.set(k, '');
					continue;
				}
				sp.set(k, String(v));
			}
			const s = sp.toString();
			url.search = s ? `?${s}` : '';
		}

		if (to.hash) {
			url.hash = to.hash.startsWith('#') ? to.hash : `#${to.hash}`;
		}

		return this.normalizeUrl(url);
	}

	async navigate(to: To, opts: NavigateOptions = {}): Promise<void> {
		const from = this.state.location;
		const targetUrl = this.resolveTo(to, from);

		let historyState =
			opts.state ?? (typeof to === 'object' && 'state' in to ? to.state : undefined) ?? this.state.historyState;

		if (historyState === undefined) historyState = null;

		const pendingMatches = this.matchUrl(targetUrl);

		this.state = {
			...this.state,
			navigating: true,
			pending: pendingMatches,
			error: undefined,
			historyState,
		};
		this.notify();

		await this.transitionTo(targetUrl, {
			replace: opts.replace,
			scroll: opts.scroll ?? this.options.scrollRestoration ?? 'top',
			historyState,
			isPop: false,
		});
	}

	async preload(to: To): Promise<void> {
		const url = this.resolveTo(to, this.state.location);
		const matches = this.matchUrl(url);
		if (!matches.length) return;

		const leaf = matches[matches.length - 1];
		if (!leaf.route.preload) return;

		const key = this.preloadKey(leaf.route, url);
		const existing = this.preloadCache.get(key);
		if (existing) {
			if (existing === 'done') return;
			await existing;
			return;
		}

		const ctx: RouteContext = {
			url,
			params: leaf.params,
			search: leaf.search,
			state: this.state.historyState,
			route: leaf.route,
			matches,
			router: this,
		};

		const promise = Promise.resolve(leaf.route.preload(ctx)).then(
			() => {
				this.preloadCache.set(key, 'done');
			},
			(err) => {
				this.preloadCache.delete(key);
				if (typeof console !== 'undefined') {
					console.error('[router] preload failed', err);
				}
			},
		);

		this.preloadCache.set(key, promise);
		await promise;
	}

	private preloadKey(route: Route, url: URL): string {
		return `${route.id}|${url.pathname}|${url.search}`;
	}

	private async handlePop(url: URL, state: unknown): Promise<void> {
		const normalizedUrl = this.normalizeUrl(url);
		if (normalizedUrl.toString() !== url.toString()) {
			this.history.replace(normalizedUrl, state);
			return;
		}

		await this.transitionTo(normalizedUrl, {
			isPop: true,
			historyState: state,
			scroll: 'preserve',
		});
	}

	private matchUrl(url: URL): Array<Match> {
		const routesWithPaths = this.routes
			.filter((r) => r.path != null)
			.sort((a, b) => {
				const aSegments = (a.path ?? '').split('/').filter(Boolean);
				const bSegments = (b.path ?? '').split('/').filter(Boolean);
				if (bSegments.length !== aSegments.length) {
					return bSegments.length - aSegments.length;
				}
				const aWildcards = aSegments.filter((s) => s.startsWith(':') || s === '*').length;
				const bWildcards = bSegments.filter((s) => s.startsWith(':') || s === '*').length;
				if (aWildcards !== bWildcards) {
					return aWildcards - bWildcards;
				}
				if (a.parentId && !b.parentId) return -1;
				if (!a.parentId && b.parentId) return 1;
				return 0;
			});

		for (const route of routesWithPaths) {
			const exec = route.pattern.exec(url);
			if (!exec) continue;

			const search = url.searchParams;

			const chain: Array<Match> = [];
			let current: Route | undefined = route;

			while (current) {
				const currentExec =
					current === route ? exec : (current.pattern.exec(url) ?? ({pathname: {groups: {}}} as URLPatternResult));

				const params = (currentExec.pathname.groups ?? {}) as Record<string, string>;

				chain.push({
					route: current,
					params,
					search,
				});

				if (!current.parentId) break;
				current = this.routeById.get(current.parentId);
			}

			chain.reverse();
			return chain;
		}

		const notFoundRouteId = this.options.notFoundRouteId;
		if (notFoundRouteId) {
			const nf = this.routeById.get(notFoundRouteId);
			if (nf) {
				return [
					{
						route: nf,
						params: {},
						search: url.searchParams,
					},
				];
			}
		}

		return [];
	}

	private async transitionTo(
		url: URL,
		opts: {
			replace?: boolean;
			scroll?: ScrollBehavior;
			historyState?: unknown;
			isPop?: boolean;
			forceEnter?: boolean;
		},
	): Promise<void> {
		const prevState = this.state;
		const prevMatches = opts.forceEnter ? [] : prevState.matches;
		const nextMatches = this.matchUrl(url);
		const historyState = opts.historyState ?? prevState.historyState ?? null;

		const mkCtx = (match: Match): RouteContext => ({
			url,
			params: match.params,
			search: match.search,
			state: historyState,
			route: match.route,
			matches: nextMatches,
			router: this,
		});

		const firstDifferentIndex =
			opts.forceEnter === true
				? 0
				: (() => {
						let i = 0;
						while (
							i < prevMatches.length &&
							i < nextMatches.length &&
							prevMatches[i].route.id === nextMatches[i].route.id
						) {
							i++;
						}
						return i;
					})();

		try {
			for (let i = firstDifferentIndex; i < nextMatches.length; i++) {
				const match = nextMatches[i];
				const route = match.route;
				if (route.onEnter) {
					const res = route.onEnter(mkCtx(match));
					if (res instanceof Redirect) {
						const redirectUrl = this.resolveTo(res.to, url);
						await this.transitionTo(redirectUrl, {
							replace: res.replace ?? true,
							scroll: opts.scroll,
							historyState,
							isPop: false,
						});
						return;
					}
					if (res instanceof NotFound) {
						await this.handleNotFound(url, historyState);
						return;
					}
				}
			}
		} catch (err) {
			if (err instanceof Redirect) {
				const redirectUrl = this.resolveTo(err.to, url);
				await this.transitionTo(redirectUrl, {
					replace: err.replace ?? true,
					scroll: opts.scroll,
					historyState,
					isPop: false,
				});
				return;
			}
			if (err instanceof NotFound) {
				await this.handleNotFound(url, historyState);
				return;
			}

			this.state = {
				...prevState,
				navigating: false,
				pending: null,
				error: err,
				historyState,
			};
			this.notify();
			throw err;
		}

		for (let i = prevMatches.length - 1; i >= firstDifferentIndex; i--) {
			const prevMatch = prevMatches[i];
			const route = prevMatch.route;
			if (route.onLeave) {
				route.onLeave({
					url: prevState.location,
					params: prevMatch.params,
					search: prevState.location.searchParams,
					state: prevState.historyState,
					route,
					matches: prevMatches,
					router: this,
				});
			}
		}

		const isSamePath =
			prevState.location.pathname === url.pathname &&
			prevState.location.search === url.search &&
			prevState.location.hash === url.hash;

		if (!opts.isPop) {
			if (opts.replace || isSamePath) {
				this.history.replace(url, historyState);
			} else {
				this.history.push(url, historyState);
			}
		}

		this.state = {
			location: url,
			matches: nextMatches,
			navigating: false,
			pending: null,
			error: undefined,
			historyState,
		};
		this.notify();

		const behavior = opts.scroll ?? this.options.scrollRestoration ?? 'top';
		queueMicrotask(() => {
			if (behavior === 'preserve') return;
			if (url.hash) {
				const id = url.hash.slice(1);
				const el = document.getElementById(id);
				if (el) {
					el.scrollIntoView();
					return;
				}
			}
			if (behavior === 'top') {
				window.scrollTo({top: 0, left: 0});
			}
		});
	}

	private async handleNotFound(url: URL, historyState: unknown): Promise<void> {
		const notFoundRouteId = this.options.notFoundRouteId;
		if (!notFoundRouteId) {
			this.state = {
				...this.state,
				location: url,
				matches: [],
				navigating: false,
				pending: null,
				error: new NotFound(),
				historyState,
			};
			this.notify();
			return;
		}

		const nf = this.routeById.get(notFoundRouteId);
		if (!nf) {
			this.state = {
				...this.state,
				location: url,
				matches: [],
				navigating: false,
				pending: null,
				error: new NotFound(),
				historyState,
			};
			this.notify();
			return;
		}

		const match: Match = {
			route: nf,
			params: {},
			search: url.searchParams,
		};

		this.state = {
			location: url,
			matches: [match],
			navigating: false,
			pending: null,
			error: undefined,
			historyState,
		};
		this.notify();
	}
}

export function createRouter(options: RouterOptions): Router {
	return new RouterImpl(options);
}
