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

import * as React from 'react';
import type {
	Match,
	NavigateOptions,
	RouteComponentProps,
	RouteParams,
	Router,
	RouterProviderProps,
	RouterState,
	ScrollBehavior,
	SearchParamsInput,
	To,
} from './types';

const RouterContext = React.createContext<Router | null>(null);

export const RouterProvider: React.FC<RouterProviderProps> = ({router, children, linkContainerRef}) => {
	React.useEffect(() => {
		const container = linkContainerRef?.current ?? document.body;

		const onClick = (event: MouseEvent) => {
			if (event.defaultPrevented) return;
			if (event.button !== 0) return;
			if (event.metaKey || event.altKey || event.ctrlKey || event.shiftKey) return;

			const target = event.target as HTMLElement | null;
			if (!target || !('closest' in target)) return;

			const anchor = target.closest('a[href]') as HTMLAnchorElement | null;
			if (!anchor) return;
			if (anchor.hasAttribute('download')) return;
			if (anchor.getAttribute('rel') === 'external') return;
			if (anchor.target && anchor.target !== '_self') return;
			if (anchor.dataset.routerIgnore === 'true') return;

			const href = anchor.getAttribute('href');
			if (!href) return;
			if (href.startsWith('mailto:') || href.startsWith('tel:')) return;

			const url = new URL(anchor.href, window.location.href);
			if (url.origin !== window.location.origin) return;

			event.preventDefault();
			void router.navigate(url.toString(), {scroll: 'top'});
		};

		container.addEventListener('click', onClick);
		return () => container.removeEventListener('click', onClick);
	}, [router, linkContainerRef]);

	React.useEffect(
		() => () => {
			router.destroy();
		},
		[router],
	);

	return <RouterContext.Provider value={router}>{children ?? <Outlet />}</RouterContext.Provider>;
};

export function useRouter(): Router {
	const ctx = React.useContext(RouterContext);
	if (!ctx) throw new Error('useRouter must be used within a RouterProvider');
	return ctx;
}

export function useRouterState<T>(selector: (state: RouterState) => T): T {
	const router = useRouter();
	return React.useSyncExternalStore(
		React.useCallback((onStoreChange) => router.subscribe(onStoreChange), [router]),
		React.useCallback(() => selector(router.getState()), [router, selector]),
		React.useCallback(() => selector(router.getState()), [router, selector]),
	);
}

export function useLocation(): URL {
	return useRouterState((s) => s.location);
}

export function useMatches(): Array<Match> {
	return useRouterState((s) => s.matches);
}

export function useMatch(): Match | undefined {
	const matches = useMatches();
	return matches[matches.length - 1];
}

export function useParams(): RouteParams {
	const match = useMatch();
	return match?.params ?? {};
}

export function useSearch(): URLSearchParams {
	const match = useMatch();
	return match?.search ?? new URLSearchParams();
}

export function useNavigate(): (to: To, opts?: NavigateOptions) => Promise<void> {
	const router = useRouter();
	return React.useCallback((to: To, opts?: NavigateOptions) => router.navigate(to, opts), [router]);
}

export interface LinkProps extends Omit<React.AnchorHTMLAttributes<HTMLAnchorElement>, 'href'> {
	to: string;
	search?: SearchParamsInput;
	hash?: string;
	state?: unknown;
	replace?: boolean;
	preload?: 'intent' | 'render' | 'none';
	scroll?: ScrollBehavior;
}

export const Link = React.forwardRef<HTMLAnchorElement, LinkProps>(function Link(
	{to, search, hash, state, replace, preload = 'intent', scroll, onClick, onPointerEnter, onFocus, ...rest},
	ref,
) {
	const router = useRouter();
	const location = useLocation();

	const hrefUrl = React.useMemo(
		() => router.resolveTo({to, search, hash}, location),
		[router, to, search, hash, location],
	);

	const handleIntentPreload = () => {
		if (preload === 'intent') {
			void router.preload({to, search, hash});
		}
	};

	const handleClick = (event: React.MouseEvent<HTMLAnchorElement>) => {
		onClick?.(event);
		if (event.defaultPrevented) return;
		if (event.button !== 0) return;
		if (event.metaKey || event.altKey || event.ctrlKey || event.shiftKey) return;
		const target = event.currentTarget.getAttribute('target');
		if (target && target !== '_self') return;
		event.preventDefault();
		void router.navigate({to, search, hash, state}, {replace, scroll});
	};

	const handlePointerEnter = (event: React.PointerEvent<HTMLAnchorElement>) => {
		onPointerEnter?.(event);
		if (!event.defaultPrevented) handleIntentPreload();
	};

	const handleFocus = (event: React.FocusEvent<HTMLAnchorElement>) => {
		onFocus?.(event);
		if (!event.defaultPrevented) handleIntentPreload();
	};

	return (
		<a
			{...rest}
			ref={ref}
			href={hrefUrl.toString()}
			onClick={handleClick}
			onPointerEnter={handlePointerEnter}
			onFocus={handleFocus}
			data-router-link="true"
		/>
	);
});

export const Outlet: React.FC = () => {
	const matches = useMatches();
	const location = useLocation();

	if (!matches.length) return null;

	const renderAt = (index: number): React.ReactNode => {
		const match = matches[index];
		const props: RouteComponentProps = {
			match,
			params: match.params,
			search: match.search,
			url: location,
		};
		const isLeaf = index === matches.length - 1;

		const child = isLeaf
			? match.route.component
				? React.createElement(match.route.component, props)
				: null
			: renderAt(index + 1);

		const Layout = match.route.layout;
		if (Layout) {
			return <Layout {...props}>{child}</Layout>;
		}

		return child;
	};

	return <>{renderAt(0)}</>;
};
